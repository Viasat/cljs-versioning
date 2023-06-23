(ns viasat.bump
  (:require [cljs-bean.core :refer [->clj]]
            [promesa.core :as P]
            [clojure.string :as S]
            [viasat.util :refer [read-file load-yaml Eprintln Eprn]]
            [viasat.apis.aws.core :as aws]
            [viasat.apis.artifactory :as artifactory]
            [viasat.apis.rpm :as rpm]
            [viasat.voom :as voom]
            ["fs" :as fs]
            ["util" :refer [promisify]]))

(def spec-format "
Version spec format:
  VARIABLE_NAME:
    type:               'rpm' | 'image' | 'git'
    image:              STRING        # 'image' type only
    registry:           STRING        # 'image' type only
    name:               STRING        # 'rpm' type only
    repo:               STRING        # 'rpm' type only
    paths:              STRING-LIST   # 'git' type only
    version-default:    STRING        # 'image' and 'rpm' types only
    version:            STRING
    version-regex:      REGEX-STRING
    alt-version:        STRING
    alt-version-regex:  REGEX-STRING
    exclude-latest:     true | false  # default to true
    date:               DATE-STRING
    creators:           STRING-LIST   # git and artifactory only

Version spec keys:
  * type: 'rpm' (RPM ver), 'image' (docker tag), or 'git' (voom version)
  * image: Docker image name
  * registry: Docker image registry
  * name: RPM package name.
  * repo: RPM repository in artifactory.
  * paths: paths to generate voom version for.
  * version-default: version to use when skipping remote queryies.
  * version: Match beginning of version.
  * version-regex: Match version against regex.
  * alt-version: Match beginning of alternate version/tag.
  * alt-version-regex: Match alternate version against regex.
  * exclude-latest: Do not match 'latest' version/tag (default: true).
  * date: Matches everything earlier or equal to date
  * creators: Match creator (git author) on any names in list.
")

;; General utility functions

(def ECR-REPO-RE #"([^.]*)\.dkr\.ecr\.([^.]*)\.amazonaws\.com")

(defn rpm? [{:keys [type]}] (= "rpm" type))
(defn image? [{:keys [type]}] (= "image" type))
(defn git? [{:keys [type]}] (= "git" type))

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

(defn load-version-spec [path]
  (P/->> (load-yaml path)
         (map (fn [[k v]] [(name k) v]))
         (into {})))


(defn validation-error [vname vspec]
  (let [check-missing (fn [v ks]
                        (keep #(when-not (get v %)
                                 (str "missing " (name %)))
                              ks))
        errors (cond-> []
                 true           (into (check-missing vspec [:type]))
                 (rpm? vspec)   (into (check-missing vspec [:name :repo]))
                 (image? vspec) (into (check-missing vspec [:registry :image]))
                 (git? vspec)   (into (check-missing vspec [:paths])))]
    (when-not (empty? errors)
      (str vname ": " (S/join ", " errors)))))

(defn merge-defaults-1
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
  preference: vspec, :by-name, :by-type, :all. For :by-type, if the
  :type value is not already set vspec or :by-name, then use :type
  from :all."
  [all-defaults spec]
  (reduce
    (fn [res [k v]]
      (assoc res k (merge-defaults-1 all-defaults k v)))
    spec spec))

(defn enrich-spec-1
  [vname vspec]
  (let [{:keys [image namespace registry artifactory-api]} vspec
        [[_ ecr-acct ecr-region]] (re-seq ECR-REPO-RE (or registry ""))]
    (cond-> vspec
      true                (assoc :var-name vname)
      (rpm? vspec)        (assoc :remote-api :rpm)
      (and (image? vspec)
           artifactory-api) (assoc :remote-api :docker-art
                                   :image-repo (if namespace
                                                 (str namespace "/" image)
                                                 (str image)))
      (and (image? vspec)
           ecr-acct)      (assoc :remote-api :docker-ecr
                                 :ecr-repo-acct ecr-acct
                                 :ecr-repo-region ecr-region
                                 :ecr-repo (if namespace
                                             (str namespace "/" image)
                                             (str image)))
      (git? vspec)        (assoc :remote-api :voom))))

(defn enrich-spec
  "Enrich each spec value with additional type specific variables."
  [spec]
  (reduce
    (fn [res [vname vspec]] (assoc res vname (enrich-spec-1 vname vspec)))
    spec spec))

(defn canonicalize [path def-root]
  (if (#{"." "/"} (first path))
    (str def-root "/" path)
    path))

(defn query-remote-versions
  "Query remote repos versions and return a map keyed by API type:
  :docker-art, :docker-ecr, :rpm"
  [{:keys [debug profile artifactory-base-url] :as cfg} versions]
  (when debug (artifactory/enable-debug))
  (P/let
    [art-headers (artifactory/get-auth-headers cfg)
     aws-opts (if profile
                {:debug debug :profile profile}
                {:debug debug :no-profile true})
     [repo-rpms repo-art-images repo-ecr-images]
     , (P/all
         [(P/all
            (for [repo (distinct (map :repo (filter rpm? (vals versions))))]
              (P/->>
                (rpm/get-rpms repo {:base-url artifactory-base-url
                                    :dbg (if debug Eprintln identity)})
                (map #(assoc % :repo repo)))))
          (P/all
            (for [{:keys [artifactory-api registry image-repo]} (filter :artifactory-api (vals versions))]
              (artifactory/get-full-images
                registry [image-repo] {:artifactory-api artifactory-api
                                       :axios-opts {:headers art-headers}})))
          (P/all
            (for [{:keys [ecr-repo-acct ecr-repo-region ecr-repo]}
                  , (filter :ecr-repo-acct (vals versions))]
              (P/->>
                (aws/invoke :ECR :DescribeImages
                            (merge aws-opts {:region ecr-repo-region
                                             :repositoryName ecr-repo}))
                :imageDetails)))])

     ;; Add :rpm-version and parsed :build-date and sort by :build-date
     rpms (->> (apply concat repo-rpms)
               (map #(merge % {:rpm-version (str (-> % :version :ver)
                                                 "-" (-> % :version :rel)
                                                 "." (-> % :arch))
                               :build-date (-> % :time :build
                                               js/parseInt (* 1000)
                                               js/Date.)}))
               (sort-by :build-date))

     ;; Parse :lastUpdated and sort by it
     art-images (->> (apply concat repo-art-images)
                     (filter :lastUpdated)
                     (map #(update % :lastUpdated (fn [d] (js/Date. d))))
                     (sort-by :lastUpdated))

     ecr-images (->> (apply concat repo-ecr-images)
                     (mapcat #(for [t (:imageTags %)]
                                (-> % (assoc :tag t) (dissoc :imageTags))))
                     (sort-by :imagePushedAt))]
    {:docker-art art-images
     :docker-ecr ecr-images
     :rpm        rpms}))

(defn query-local-versions
  "Query local versions and return a map keyed by version type:
  :voom (currently only :voom is supported)"
  [{:keys [debug root-dir all-local] :or {:root-dir "."}} versions]
  (P/let
    [voom-versions
     , (P/all
         (for [[vname vspec] (filter (comp git? val) versions)]
           (P/let [paths (map #(canonicalize % root-dir) (:paths vspec))
                   versions (voom/voom-versions-data paths all-local)]
             (map #(assoc % :module vname) versions))))]
    {:voom (apply concat voom-versions)}))

(defn query-versions
  "Query remote and local versions and return a combined map."
  [{:keys [resolve-remote] :as cfg} versions]
  (P/let [[remote-versions local-versions]
          , (P/all [(when resolve-remote
                      (query-remote-versions cfg versions))
                    (query-local-versions cfg versions)])]
    (merge remote-versions local-versions)))


(defn get-version-row-spec [kind]
  (condp = kind
    :docker-art {:spec-name-field  :image-repo
                 :name-field       :image
                 :version-field    :tag
                 :date-field       :lastUpdated
                 :hash-field       [:checksums :sha256]
                 :ver-delim        ":"
                 :creator-field    :createdBy}
    :docker-ecr {:spec-name-field  :ecr-repo
                 :name-field       :repositoryName
                 :version-field    :tag
                 :date-field       :imagePushedAt
                 :hash-field       [:imageDigest]
                 :ver-delim        ":"}
    :rpm        {:spec-name-field  :name
                 :name-field       :name
                 :version-field    :rpm-version
                 :date-field       :build-date
                 :hash-field       [:checksum (keyword "$t")]
                 :ver-delim        "-"}
    :voom       {:spec-name-field  :var-name
                 :name-field       :module
                 :version-field    :voom-version
                 :date-field       :date-str
                 :hash-field       [:sha]
                 :creator-field    :author}))

(defn enrich-rows-1
  "Add :version, :full-version, and :hash to all rows"
  [version-spec {:keys [spec-name-field name-field version-field hash-field ver-delim] :as row-spec}
   rows]
  (let [vname (get version-spec spec-name-field nil)]
    (for [row rows
          :let [version (get row version-field)]
          :when version
          :let [full-ver (if ver-delim (str vname ver-delim version) version)
                hashval (get-in row hash-field)]]
      (merge row {:version version
                  :full-version full-ver
                  :hash hashval}))))


(defn filter-rows-1
  [filter-spec row-spec rows]
  (let [{:keys [exclude-latest date creators version
                version-regex alt-version alt-version-regex]} filter-spec
        {:keys [spec-name-field name-field version-field date-field
                hash-field creator-field ver-delim]} row-spec
        vname (get filter-spec spec-name-field nil)

        ;; Group by image/package hash
        grouped-rows (->> rows
                          (group-by :hash)
                          (map (fn [[hash v]]
                                 [hash (map :version v)]))
                          (into {}))
        ;; Add :all-versions to each row that has the matching hash
        rows (for [row rows]
               (assoc row :all-versions (get grouped-rows (:hash row))))

        ;; Filter the rows
        date (if date (js/Date. date) nil)
        ver-re (js/RegExp. version-regex)
        alt-ver-re (js/RegExp. alt-version-regex)
        rows (sort-by date-field rows)
        rows (cond->> rows
               vname (filter #(= vname (get % name-field)))

               version (filter #(.startsWith (get % version-field) version))

               version-regex (filter #(re-seq ver-re (get % version-field)))

               alt-version (filter (fn [r] (some #(.startsWith % alt-version)
                                                 (:all-versions r))))

               alt-version-regex (filter (fn [r] (some #(re-seq alt-ver-re %)
                                                       (:all-versions r))))

               exclude-latest (filter #(not= "latest" (get % version-field)))

               date (filter #(<= (get % date-field) date))

               (and creator-field
                    (not (empty? creators)))
               , (filter #((set creators) (get % creator-field))))]
    rows))

(defn resolve-versions-1
  [{:keys [remote-api version-default] :as version-spec} versions]
  (let [row-spec (get-version-row-spec remote-api)
        {:keys [name-field spec-name-field version-field]} row-spec
        ;; Get just the rows for this remote/upstream API type
        vname (get version-spec spec-name-field nil)
        rows (get versions remote-api)
        _ (when (and (empty? rows) (empty? version-default))
            (throw (js/Error.
                     (str vname " must have :remote-api or :version-default"))))
        rows (cond
               (empty? rows)
               (->> [{name-field vname
                      version-field version-default}]
                    (enrich-rows-1 version-spec row-spec))

               :else
               (->> rows
                   (enrich-rows-1 version-spec row-spec)
                   (filter-rows-1 version-spec row-spec)))]
    (assoc version-spec :version-details rows)))

(defn resolve-spec-versions
  "Takes version spec and remote version rows. Returns an updated spec
  where each spec items has added version details that are
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
  "Merge defaults and check spec validity"
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
  "Query remote API versions and then filter/match/merge those against
  the spec."
  [cfg version-spec checked?]
  (P/let
    [;; Add additional type specific derived values
     enriched-spec (enrich-spec version-spec)

     ;; Query the remote API versions
     queried-versions (query-versions cfg enriched-spec)

     ;; Resolve/filter versions for each version spec on command line
     resolved-spec (resolve-spec-versions enriched-spec queried-versions)

     unresolved (filter (comp empty? :version-details) (vals resolved-spec))]

    (when (and checked? (not (empty? unresolved)))
      (throw (ex-info (str "Could not resolve versions for: "
                           (S/join ", " (map :var-name unresolved)))
                      {:unresolved unresolved})))
    resolved-spec))

