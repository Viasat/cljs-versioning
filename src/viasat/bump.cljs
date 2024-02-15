;; Copyright (c) 2024, Viasat, Inc
;; Licensed under EPL 2.0

(ns viasat.bump
  (:require [cljs-bean.core :refer [->clj]]
            [promesa.core :as P]
            [clojure.string :as S]
            [viasat.util :refer [read-file load-yaml Eprintln Eprn]]
            [viasat.apis.aws.core :as aws]
            [viasat.apis.artifactory :as artifactory]
            [viasat.apis.docker :as docker]
            [viasat.apis.rpm :as rpm]
            [viasat.apis.npm :as npm]
            [viasat.voom :as voom]
            ["fs" :as fs]
            ["util" :refer [promisify]]))

(def spec-format "
Version spec format:
  VARIABLE_NAME:
    type:               'rpm' | 'image' | 'npm' | 'git' | 'literal'
    image:              STRING        # 'image' type only
    registry:           STRING        # 'image' and 'npm' types only
    name:               STRING        # 'rpm' and 'npm' types only
    namespace:          STRING        # 'image' type only (docker hub)
    repo:               STRING        # 'rpm' type only
    paths:              STRING-LIST   # 'git' type only
    version-literal:    STRING        # 'literal' type only
    version-default:    STRING        # 'image' and 'rpm' types only
    version:            STRING
    version-start:      STRING
    version-regex:      REGEX-STRING
    alt-version:        STRING
    alt-version-start:  STRING
    alt-version-regex:  REGEX-STRING
    exclude-latest:     true | false  # default to true
    date:               DATE-STRING
    date-before:        DATE-STRING
    date-after:         DATE-STRING
    creators:           STRING-LIST   # git and artifactory only

Version spec keys:
  * type: 'rpm' (RPM ver), 'image' (docker tag), or 'git' (voom version)
  * image: Docker image name
  * registry: Docker image registry or npm package registry
  * name: NPM or RPM package name.
  * namespace: docker hub image namespace (default: 'library')
  * repo: RPM repo URL or repository name in artifactory.
  * paths: paths to generate voom version for.
  * version-default: version to use when skipping remote queryies.
  * version: Match exact version.
  * version-start: Match beginning of version.
  * version-regex: Match version against regex.
  * alt-version: Match exact alternate version/tag.
  * alt-version-start: Match beginning of alternate version/tag.
  * alt-version-regex: Match alternate version against regex.
  * exclude-latest: Do not match 'latest' version/tag (default: true).
  * date: Matches everything earlier or equal to date (same as date-before)
  * date-before: Matches everything earlier or equal to date
  * date-after: Matches everything equal or later than date
  * creators: Match creator (git author) on any names in list.
")

;; A map of :query-api types where the values are maps of fields to
;; where those fields are in queried version data for each query API.
(def VERSION-ROW-SPEC
  {:docker-hub {:spec-name-field  :full-image ;; ...
                :name-field       :full-image ;; ...
                :version-field    :name       ;; ...
                :date-field       :last_updated
                :hash-field       [:digest]
                :ver-delim        ":"
                :creator-field    :last_updater_username}
   :docker-art {:spec-name-field  :full-image
                :name-field       :image
                :version-field    :tag
                :date-field       :lastUpdated
                :hash-field       [:checksums :sha256]
                :ver-delim        ":"
                :creator-field    :createdBy}
   :docker-ecr {:spec-name-field  :full-image
                :name-field       :repositoryName
                :version-field    :tag
                :date-field       :imagePushedAt
                :hash-field       [:imageDigest]
                :ver-delim        ":"}
   :literal    {:spec-name-field  :var-name
                :name-field       :variable
                :version-field    :version
                :date-field       :date-str
                :hash-field       [:variable]}
   :rpm        {:spec-name-field  :name
                :name-field       :name
                :version-field    :rpm-version
                :date-field       :build-date
                :hash-field       [:checksum (keyword "$t")]
                :ver-delim        "-"}
   :npm        {:spec-name-field  :name
                :name-field       :name
                :version-field    :version
                :date-field       :time
                :hash-field       [:dist :shasum]
                :ver-delim        "-"}
   :voom       {:spec-name-field  :var-name
                :name-field       :module
                :version-field    :voom-version
                :date-field       :date-str
                :hash-field       [:sha]
                :creator-field    :author}})

;; General utility functions

(def ECR-REPO-RE #"([^.]*)\.dkr\.ecr\.([^.]*)\.amazonaws\.com")

(defn rpm?     [{:keys [type]}] (= "rpm" type))
(defn image?   [{:keys [type]}] (= "image" type))
(defn npm?     [{:keys [type]}] (= "npm" type))
(defn git?     [{:keys [type]}] (= "git" type))
(defn literal? [{:keys [type]}] (= "literal" type))

(def -access (promisify (.-access fs)))
(defn file-exists? [f]
  (-> (-access f fs.constants.F_OK)
      (P/then (constantly true))
      (P/catch (constantly false))))

(def -stat (promisify (.-stat fs)))
(defn directory? [f]
  (-> (-stat f)
      (P/then #(.isDirectory %))
      (P/catch (constantly false))))

(defn find-files
  "Check each path in roots. If the path is a directory then search
  for the first path+suffix that exists in that directory."
  [dirs-or-files suffixes]
  (P/then
    (P/all
      (for [path dirs-or-files]
        (P/let [dir? (directory? path)
                paths (if dir?
                        (map #(str path "/" %) suffixes)
                        [path])
                paths (P/all
                        (for [p paths]
                          (P/let [exists? (file-exists? p)]
                            (when exists? p))))]
          (first (filter identity paths)))))
    #(filter identity %)))

;; "version-literal" is chosen over simply "version", because "version"
;; means something to other vspec types.  For example, if we merge and
;; override a string literal with a full {type: image, ..} spec, we
;; don't want to accidentally pin that "version" from 'underneath'.
(defn desugar-vspec
  "Removes version spec 'sugar'.  String literals are replaced with the
  expanded format: `{type: literal, version-literal: $STR-LITERAL}`"
  [vspec]
  (if (string? vspec)
    {:type "literal" :version-literal vspec}
    vspec))

(defn load-default-file [path]
  (letfn [(desugar-named-defaults [m]
            (reduce (fn [res [k v]]
                      (assoc res k (desugar-vspec v)))
                    {} m))]
    (P/-> (load-yaml path)
          (update :by-name desugar-named-defaults))))

(defn load-version-spec [path]
  (P/->> (load-yaml path)
         (map (fn [[k v]]
                [(name k) (desugar-vspec v)]))
         (into {})))

(defn validation-error
  "Check that vspec has required fields for the type its type. Returns
  nil spec validates or an error string otherwise"
  [vname vspec]
  (let [check-missing (fn [v ks]
                        (keep #(when-not (get v %)
                                 (str "missing " (name %)))
                              ks))
        errors (cond-> []
                 true             (into (check-missing vspec [:type]))
                 (rpm? vspec)     (into (check-missing vspec [:name :repo]))
                 (image? vspec)   (into (check-missing vspec [:image]))
                 (npm? vspec)     (into (check-missing vspec [:name]))
                 (literal? vspec) (into (check-missing vspec [:version-literal]))
                 (git? vspec)     (into (check-missing vspec [:paths])))]
    (when-not (empty? errors)
      (str vname ": " (S/join ", " errors)))))

(defn merge-defaults-1
  "Merge a single version-spec item (see merge-defaults docstring)."
  [all-defaults vname vspec]
  (let [all-merged (apply merge (map :all all-defaults))
        ;; :by-name
        vkname (keyword vname)
        name-seq (map #(-> % :by-name (get vkname)) all-defaults)
        vspec (merge (apply merge name-seq) vspec)
        ;;:by-type (fallback to :type from :all)
        vtype (keyword (get vspec :type (get all-merged :type)))
        type-seq (map #(-> % :by-type (get vtype)) all-defaults)
        vspec (merge (apply merge type-seq) vspec)
        ;; :all
        vspec (merge all-merged vspec)]
    (merge {:exclude-latest true} vspec)))

(defn merge-defaults
  "Merge defaults from all-defaults into each spec item in order of
  preference: vspec, :by-name, :by-type, :all. The top-level keys are:
    * :all: defaults that apply to all other spec values.
    * :by-type: sub-keys are :image, :rpm, or :git that each contain
      defaults for all spec values of that type. If the :type value is
      not already set in the vspec or with :by-name, then it will use
      :type from :all.
    * :by-name: sub-keys are spec value names with default that will be
      applied to the matching spec value by name. If the named spec
      value does not exist in a specified version-spec file then it
      will be ignored."
  [all-defaults spec]
  (reduce
    (fn [res [k v]]
      (assoc res k (merge-defaults-1 all-defaults k v)))
    spec spec))

(defn enrich-spec-1
  "Enrich a single version-spec item (see enrich-spec docstring)."
  [vname vspec]
  (let [{:keys [image namespace registry artifactory-api]} vspec
        [[_ _ ecr-region]] (re-seq ECR-REPO-RE (or registry ""))]
    (cond-> vspec
      true                (assoc :var-name vname)
      (rpm? vspec)        (assoc :query-api :rpm)
      (image? vspec)      (assoc :query-api :docker-hub
                                 :full-image (if namespace
                                               (str namespace "/" image)
                                               (str image)))
      (and (image? vspec)
           artifactory-api) (assoc :query-api :docker-art)
      (and (image? vspec)
           ecr-region)    (assoc :query-api :docker-ecr
                                 :ecr-repo-region ecr-region)
      (npm? vspec)        (assoc :query-api :npm)
      (literal? vspec)    (assoc :query-api :literal)
      (git? vspec)        (assoc :query-api :voom))))

(defn enrich-spec
  "Enrich each spec value with additional values. :var-name and
  :query-api are always added. Other values are specific to the
  query/version API mechanism:
    * docker hub: :full-image
    * artifactory: :full-image
    * ECR: :full-image, :ecr-repo-region"
  [spec]
  (reduce
    (fn [res [vname vspec]] (assoc res vname (enrich-spec-1 vname vspec)))
    spec spec))

(defn canonicalize [path def-root]
  (if (#{"." "/"} (first path))
    (str def-root "/" path)
    path))

(defn query-remote-versions
  "Query remote repos versions and return a map keyed by :query-api
  type: :docker-art, :docker-ecr, :rpm, :npm"
  [{:keys [debug profile no-profile artifactory-base-url] :as opts} versions]
  (when debug (artifactory/enable-debug))
  (P/let
    [repo-rpm-specs (distinct (map :repo (filter rpm? (vals versions))))
     docker-hub-specs (filter #(= :docker-hub (:query-api %)) (vals versions))
     docker-art-specs (filter #(= :docker-art (:query-api %)) (vals versions))
     docker-ecr-specs (filter #(= :docker-ecr (:query-api %)) (vals versions))
     repo-npm-specs (filter npm? (vals versions))

     [repo-rpms docker-hub-images docker-art-images docker-ecr-images repo-npms]
     , (P/all
         [(P/all
            (for [repo repo-rpm-specs]
              (P/catch
                (P/->>
                  (rpm/get-rpms repo {:base-url artifactory-base-url
                                      :dbg (if debug Eprintln identity)})
                  (map #(assoc % :repo repo)))
                (fn [err] (throw (ex-info (str "Error querying " repo
                                               ": " (.-message err))
                                          {:error err}))))))
          (P/all
            (for [{:keys [registry namespace image full-image]} docker-hub-specs]
              (P/catch
                (P/->>
                  (docker/get-image-tags image {:base-url registry
                                                :namespace namespace})
                  (map #(merge % {:full-image full-image})))
                (fn [err] (throw (ex-info (str "Error querying " full-image
                                               " in Docker Hub: " (.-message err))
                                          {:error err}))))))
          (P/all
            (for [{:keys [artifactory-api registry full-image]} docker-art-specs]
              (P/catch
                (artifactory/get-full-images
                  registry [full-image] {:artifactory-api artifactory-api
                                         :axios-opts {:headers (artifactory/get-auth-headers opts)}})
                (fn [err] (throw (ex-info (str "Error querying " full-image " in " registry
                                               ": " (.-message err))
                                          {:error err}))))))
          (P/all
            (for [{:keys [ecr-repo-region full-image]} docker-ecr-specs]
              (P/catch
                (P/->>
                  (aws/invoke :ECR :DescribeImages
                              (merge {:region ecr-repo-region
                                      :repositoryName full-image
                                      :debug debug}
                                     (if (and profile (not no-profile))
                                       {:profile profile}
                                       {:no-profile true})))
                  :imageDetails)
                (fn [err] (throw (ex-info (str "Error querying " full-image " in " ecr-repo-region
                                               ": " (.-message err))
                                          {:error err}))))))
          (P/all
            (for [{:keys [registry name]} repo-npm-specs]
              (P/catch
                (npm/get-versions name {:base-url registry
                                        :dbg (if debug Eprintln identity)})
                (fn [err] (throw (ex-info (str "Error querying " name " in " registry
                                               ": " (.-message err))
                                          {:error err}))))))
          ])

     ;; Add :rpm-version and parsed :build-date and sort by :build-date
     rpms (->> (apply concat repo-rpms)
               (map #(merge % {:rpm-version (str (-> % :version :ver)
                                                 "-" (-> % :version :rel))
                               :build-date (-> % :time :build
                                               js/parseInt (* 1000)
                                               js/Date.)}))
               (sort-by :build-date))

     ;; hoist amd64 image up into main map and sort by :last_updated
     hub-images (->> (apply concat docker-hub-images)
                     (map (fn [tag]
                            (->> (:images tag)
                                 (filter #(= (:architecture %) "amd64"))
                                 first
                                 (merge tag))))
                     (sort-by :last_updated))

     ;; Parse :lastUpdated and sort by it
     art-images (->> (apply concat docker-art-images)
                     (filter :lastUpdated)
                     (map #(update % :lastUpdated (fn [d] (js/Date. d))))
                     (sort-by :lastUpdated))

     ecr-images (->> (apply concat docker-ecr-images)
                     (mapcat #(for [t (:imageTags %)]
                                (-> % (assoc :tag t) (dissoc :imageTags))))
                     (sort-by :imagePushedAt))

     ;; Sort by :time
     npms (->> (apply concat repo-npms)
               (sort-by :time))]
    {:docker-hub hub-images
     :docker-art art-images
     :docker-ecr ecr-images
     :rpm        rpms
     :npm        npms}))

(defn query-local-versions
  "Query local versions and return a map keyed by version type:
  :voom, :literal"
  [{:keys [debug root-dir dirty-suffix all-local]
    :or {root-dir "." dirty-suffix "_DIRTY"}} versions]
  (P/let
    [voom-versions
     , (P/all
         (for [[vname vspec] (filter (comp git? val) versions)]
           (P/let [paths (map #(canonicalize % root-dir) (:paths vspec))
                   versions (voom/voom-versions-data paths dirty-suffix all-local)]
             (map #(assoc % :module vname) versions))))
     literal-versions
     , (for [[vname vspec] (filter (comp literal? val) versions)]
         ;; Only a single version exists for a literal, the literal version
         [{:variable vname
           :date-str (.toISOString (js/Date.))
           :version (:version-literal vspec)}])]
    {:voom (apply concat voom-versions)
     :literal (apply concat literal-versions)}))

(defn query-versions
  "Query remote (if :resolve-remote is set) and local versions and
  return a merged map with :query-api types as top-level keys."
  [{:keys [resolve-remote] :as cfg} versions]
  (P/let [[remote-versions local-versions]
          , (P/all [(when resolve-remote
                      (query-remote-versions cfg versions))
                    (query-local-versions cfg versions)])]
    (merge remote-versions local-versions)))



(defn enrich-rows-1
  "Add :version, :full-version, and :hash to all rows using the
  defintion from a single version-spec item."
  [vspec {:keys [spec-name-field name-field version-field hash-field ver-delim] :as row-spec}
   rows]
  (let [vname (get vspec spec-name-field nil)]
    (for [row rows
          :let [version (get row version-field)]
          :when version
          :let [full-ver (if ver-delim (str vname ver-delim version) version)
                hashval (get-in row hash-field)]]
      (merge row {:version version
                  :full-version full-ver
                  :hash hashval}))))


(defn filter-rows-1
  "Filter version rows according to a single version-spec item and
  sorted by date-field (see resolve-versions docstring)."
  [vspec row-spec rows]
  (let [{:keys [exclude-latest creators
                date date-before date-after
                version version-start version-regex
                alt-version alt-version-start alt-version-regex]} vspec
        {:keys [spec-name-field name-field version-field date-field
                hash-field creator-field ver-delim]} row-spec
        vname (get vspec spec-name-field nil)

        ;; Group by image/package hash
        grouped-rows (->> rows
                          (group-by :hash)
                          (map (fn [[hash v]]
                                 [hash (map :version v)]))
                          (into {}))
        ;; Add :all-versions to each row that has the matching hash
        rows (for [row rows]
               (assoc row :all-versions (get grouped-rows (:hash row))))

        ;; Coerce date inputs
        date (when date (js/Date. date))
        date-before (when date-before (js/Date. date-before))
        date-after (when date-after (js/Date. date-after))

        ;; Filter the rows
        ver-re (js/RegExp. version-regex)
        alt-ver-re (js/RegExp. alt-version-regex)
        rows (sort-by date-field rows)
        rows (cond->> rows
               vname (filter #(= vname (get % name-field)))

               version (filter #(= (get % version-field) version))

               version-start (filter #(.startsWith (get % version-field) version-start))

               version-regex (filter #(re-seq ver-re (get % version-field)))

               alt-version (filter (fn [r] (some #(= % alt-version)
                                                 (:all-versions r))))

               alt-version-start (filter (fn [r] (some #(.startsWith % alt-version)
                                                       (:all-versions r))))

               alt-version-regex (filter (fn [r] (some #(re-seq alt-ver-re %)
                                                       (:all-versions r))))

               exclude-latest (filter #(not= "latest" (get % version-field)))

               date (filter #(<= (get % date-field) date))

               date-before (filter #(<= (get % date-field) date-before))

               date-after (filter #(>= (get % date-field) date-after))

               (and creator-field
                    (not (empty? creators)))
               , (filter #((set creators) (get % creator-field))))]
    rows))

(defn resolve-versions-1
  "Resolve versions for a single version-spec item (see
  resolve-versions docstring)."
  [{:keys [query-api version-default] :as vspec} versions]
  (let [row-spec (get VERSION-ROW-SPEC query-api)
        {:keys [name-field spec-name-field version-field]} row-spec
        ;; Get just the rows for this query/version API type
        vname (get vspec spec-name-field nil)
        rows (get versions query-api)
        _ (when (and (empty? rows) (empty? version-default))
            (throw (js/Error.
                     (str vname " must have :query-api or :version-default"))))
        rows (cond
               (empty? rows)
               (->> [{name-field vname
                      version-field version-default}]
                    (enrich-rows-1 vspec row-spec))

               :else
               (->> rows
                   (enrich-rows-1 vspec row-spec)
                   (filter-rows-1 vspec row-spec)))]

    (assoc vspec :version-details rows)))

(defn resolve-versions
  "Takes version spec and queried version rows. Returns an updated
  spec where each spec items has added version details that are
  matched/filtered according to the spec and sorted by date-field.

  Filters according to the following row/version selection algorithm:
    - filters rows where spec-name-field in spec equals name-field in
      rows
    - if version spec specified, then filters rows where version
      matches the beginning of version-field in rows
    - if version-regex spec specified, then filters rows where
      version-regex is a RegExp match on version-field in rows
    - if exclude-latest is specified, then filter rows where
      version-field is not equal to 'latest' (defaults to true).
    - if date is specified, then filters rows where date-field is less
      than date.
    - if creators specified (and is not 'all'), then filters
      rows where creator-field is one of creators

  Last returned version string is the most recent version matching all
  criteria. Version strings are the row name-field and version-field,
  concatenated together using ver-delim."
  [spec versions]
  (reduce
    (fn [res [svname vspec]]
      (assoc res svname (resolve-versions-1 vspec versions)))
    spec spec))

(defn normalize-spec
  "Merge defaults into a version-spec and return it. If checked? is
  set then check spec validity (throw an exception if invalid)."
  [defaults version-spec checked?]
  (let [;; add defaults before validation checks
        full-spec (merge-defaults defaults version-spec)

        validation-errors (keep #(apply validation-error %)
                                ;; for spec values in original spec files
                                ;; (i.e. not from defaults), sanity check
                                ;; those with the merged defaults
                                (select-keys full-spec (keys version-spec)))]
    (when (and checked? (not (empty? validation-errors)))
      (throw (ex-info (str "Invalid specs found:\n"
                           (S/join "\n" validation-errors))
                      {:validation-errors validation-errors})))
    full-spec))

(defn resolve-spec
  "Query API versions and then resolve (filter/match/merge) those
  against each item of the version-spec. If checked? is set then check
  to make sure all version-spec items resolved to a version (throw if
  any unresolved)."
  [cfg version-spec checked?]
  (P/let
    [;; Add additional type specific derived values
     enriched-spec (enrich-spec version-spec)

     ;; Query the API to get version details
     queried-versions (query-versions cfg enriched-spec)

     ;; Resolve/filter versions for each version spec on command line
     resolved-spec (resolve-versions enriched-spec queried-versions)

     unresolved (filter (comp empty? :version-details) (vals resolved-spec))]

    (when (and checked? (not (empty? unresolved)))
      (throw (ex-info (str "Could not resolve versions for: "
                           (S/join ", " (map :var-name unresolved)))
                      {:unresolved unresolved})))
    resolved-spec))

