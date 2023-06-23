#!/usr/bin/env nbb

(ns bump
  (:require [cljs.pprint :refer [pprint]]
            [cljs-bean.core :refer [->clj]]
            [promesa.core :as P]
            [clojure.string :as S]
            [viasat.util :refer [parse-opts read-file
                                 Eprintln Eprn Epprint fatal]]
            [viasat.apis.aws.core :as aws]
            [viasat.apis.artifactory :as artifactory]
            [viasat.apis.rpm :as rpm]
            ["fs" :as fs]
            ["util" :refer [promisify]]
            ["yaml$default" :as yaml]))

(def usage "
Bump/update/output version variables that represent artifacts by
looking at their source repositories and filtering smartly.

Usage:
  bump [options] <version-spec-files>

Options:
  -h --help                           Show help/usage.
  --verbose                           Show verbose output (stderr)
                                      [env: VERBOSE]
  --debug                             Show debug/trace output (stderr)
                                      [env: DEBUG]
  --defaults-files DEFAULTS-FILES     Comma separated files and/or directories with
                                      default spec data. Not used for selecting
                                      which spec names to query/resolve.
  --enumerate                         List all available versions that match each
                                      variable version spec
  --print-full-spec                   Output the merged version spec with defaults
  --print-resolved-spec               Output the full spec with upstream version details
  --output-format FORMAT              Formats: dotenv, json, yaml [default: dotenv]
                                      Defaults to json for --enumerate, --print-*-spec

  --profile PROFILE                   AWS profile for ECR access [env: PROFILE]
  --artifactory-base-url URL          Artifactory base URL
                                      [env: ARTIFACTORY_BASE_URL]
  --artifactory-username USERNAME     Artifactory username
                                      [env: ARTIFACTORY_USERNAME]
  --artifactory-identity-token TOKEN  Artifactory identity token
                                      [env: ARTIFACTORY_IDENTITY_TOKEN]

The version-spec-files argument is a comma separated list of
directories and/or files. For directories bump looks for
version-spec.(yaml|yml) files in those directories.  Empty directories
and non-existent files are skipped.  Found version spec files are
merged one level deep, such that each variable's version specs are
shallow merged.

Version spec format:
  VARIABLE_NAME:
    type:               'rpm' | 'image'
    image:              STRING        # 'image' type only
    registry:           STRING        # 'image' type only
    name:               STRING        # 'rpm' type only
    repo:               STRING        # 'rpm' type only
    version:            STRING
    version-regex:      REGEX-STRING
    alt-version:        STRING
    alt-version-regex:  REGEX-STRING
    exclude-latest:     true | false  # default to true
    date:               DATE-STRING
    image-creators:     STRING-LIST   # artifactory only

Version spec keys:
  * type: Rest of spec is for RPM ('rpm') or docker image ('image')
  * image: Docker image name
  * registry: Docker image registry
  * name: RPM package name.
  * repo: RPM repository in artifactory.
  * version: Match beginning of version.  * version-regex: Match version against regex.
  * alt-version: Match beginning of alternate version/tag.
  * alt-version-regex: Match alternate version against regex.
  * exclude-latest: Do not match 'latest' version/tag (default: true).
  * date: Matches everything earlier or equal to date
  * image-creators: Match creator on any names in list.

The --defaults-files argument specifies a comma separate list of files
that contain default spec values. Defaults from multiple files are
merged at the spec map level. The top level keys of the defaults
file are:
  * all: defaults that apply to all other spec values.
  * by-type: sub-keys are 'image' and/or 'rpm' that each contain
    defaults for all spec values of that type.
  * by-name: sub-keys are spec value names with default that will be
    applied to the matching spec value by name. If the named spec
    value does not exist in a specified version-spec file then it will
    be ignored.
")

;; General utility functions

(def ECR-REPO-RE #"([^.]*)\.dkr\.ecr\.([^.]*)\.amazonaws\.com")

(defn comma-split [v] (if (string? v) (S/split v #",") v))

(defn rpm? [{:keys [type]}] (= "rpm" type))
(defn image? [{:keys [type]}] (= "image" type))

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

(defn load-yaml [path]
  (P/-> (read-file path "utf8") yaml/parse ->clj))

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
                 (image? vspec) (into (check-missing vspec [:registry :image])))]
    (when-not (empty? errors)
      (str vname ": " (S/join ", " errors)))))

(defn get-spec-defaults [all-defaults vname]
  (reduce
    (fn [spec defaults]
      (let [{:keys [all by-type by-name]} defaults
            ;; type might be set in all
            vtype (get spec :type (get all :type))]
        (merge spec
               all
               (get by-type (keyword vtype))
               (get by-name (keyword vname)))))
    {:exclude-latest true} all-defaults))

(defn enrich-spec
  "Enrich spec values with additional type specific variables."
  [vname vspec]
  (let [{:keys [image namespace registry artifactory-api]} vspec

        [[_ ecr-acct ecr-region]] (re-seq ECR-REPO-RE (or registry ""))]
    (cond-> vspec
      true                (assoc :var-name vname)
      (rpm? vspec)        (assoc :upstream-api :rpm)
      (and (image? vspec)
           artifactory-api) (assoc :upstream-api :docker-art
                                   :image-repo (if namespace
                                                 (str namespace "/" image)
                                                 (str image)))
      (and (image? vspec)
           ecr-acct)      (assoc :upstream-api :docker-ecr
                                 :ecr-repo-acct ecr-acct
                                 :ecr-repo-region ecr-region
                                 :ecr-repo (if namespace
                                             (str namespace "/" image)
                                             (str image))))))

(defn get-upstream-versions
  "Returns a map of versions keyed by api type:
       :docker-art, :docker-ecr, :rpm"
  [{:keys [debug profile artifactory-base-url] :as cfg} versions]
  (when debug (artifactory/enable-debug))
  (P/let
    [art-headers (artifactory/get-auth-headers cfg)
     artifactory-api (str artifactory-base-url "/api")
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

     ;; Add :full-version and parsed :build-date and sort by :build-date
     rpms (->> (apply concat repo-rpms)
               (map #(merge % {:full-version (str (-> % :version :ver)
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
                 :version-field    :full-version
                 :date-field       :build-date
                 :hash-field       [:checksum (keyword "$t")]
                 :ver-delim        "-"}))

(defn resolve-versions
  "Takes version spec and version rows for that spec. Returns string
  versions from matching rows, sorted by date-field.

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
    - if image-creators specified (and is not 'all'), then filters
      rows where creator-field is one of image-creators

  Last returned version string is the most recent version matching all
  criteria. Version strings are the row name-field and version-field,
  concatenated together using ver-delim."
  [{:keys [upstream-api exclude-latest date image-creators
           version version-regex alt-version alt-version-regex] :as spec}
   upstream-versions]
  (let [{:keys [spec-name-field name-field version-field date-field
                hash-field creator-field ver-delim] :as rowspec}
        , (get-version-row-spec upstream-api)
        vname (get spec spec-name-field)
        ;; Get just the rows for this upstream API type
        rows (get upstream-versions upstream-api)

        ;; Enrich the rows
        ;; Add :version-string and :hash to all rows
        rows (for [row rows
                   :let [version (get row version-field)]
                   :when version
                   :let [ver-str (str vname ver-delim version)
                         hashval (get-in row hash-field)]]
               (merge row {:version-string ver-str
                           :matched-version version
                           :hash hashval}))
        ;; Group by image/package hash
        grouped-rows (->> rows
                          (group-by :hash)
                          (map (fn [[hash v]]
                                 [hash (map :matched-version v)]))
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
                    (not (empty? image-creators)))
               , (filter #((set image-creators) (get % creator-field))))]
    (assoc spec :version-details rows)))

(defn print-output [fmt results]
  (case fmt
    "dotenv" (doseq [[k v] results]
               (println (str k "=" v)))
    "json" (print (.stringify js/JSON (clj->js results)))
    "yaml" (print (yaml/stringify (clj->js results)))))

(P/let
  [cfg (parse-opts usage *command-line-args* {:laxPlacement true})
   _ (when (empty? cfg) (fatal 2))
   {:keys [version-spec-files debug verbose defaults-files
           enumerate print-full-spec print-resolved-spec
           output-format artifactory-base-url]} cfg
   verbose (if debug true verbose) ;; debug implies verbose
   _ (when debug (Eprintln "Settings:") (Epprint cfg))

   output-format (if (and (= "dotenv" output-format)
                          (or enumerate print-full-spec print-resolved-spec))
                   "json"
                   output-format)
   defaults-files     (find-files (comma-split defaults-files)
                                  ["version-spec-defaults.yaml"
                                   "version-spec-defaults.yml"])
   version-spec-files (find-files (comma-split version-spec-files)
                                  ["version-spec.yaml"
                                   "version-spec.yml"])

   defaults (when defaults-files
              (when verbose (Eprintln "Loading defaults:" defaults-files))
              (P/all (map load-yaml defaults-files)))
   _ (when verbose (Eprintln "Loading version specs:" (S/join " " version-spec-files)))
   version-spec (P/then (P/all (map load-version-spec version-spec-files))
                        #(apply merge-with merge {} %))

   ;; add defaults before validation checks
   full-spec (reduce
               (fn [res [vname vspec]]
                 (assoc res vname (merge (get-spec-defaults defaults vname)
                                         vspec)))
               version-spec version-spec)

   validation-errors (keep #(apply validation-error %)
                           ;; for spec values in original spec files
                           ;; (i.e. not from defaults), sanity check
                           ;; those with the merged defaults
                           (select-keys full-spec (keys version-spec)))
   _ (when-not (empty? validation-errors)
       (Eprintln "Invalid specs found:")
       (doseq [msg validation-errors]
         (Eprintln msg))
       (when (not print-full-spec)
         (js/process.exit 1)))

   _ (when print-full-spec
       (print-output output-format full-spec)
       (js/process.exit 0))

   ;; Add additional type specific derived values
   enriched-spec (reduce
                   (fn [res [vname vspec]]
                     (assoc res vname (enrich-spec vname vspec)))
                   full-spec full-spec)

   _ (when verbose (Eprintln "Getting docker image tags and RPM versions (in parallel)"))
   upstream-versions (get-upstream-versions cfg enriched-spec)

   ;; Resolve/filter versions for each version spec on command line
   resolved-spec (reduce
                   (fn [res [vname vspec]]
                     (assoc res vname (resolve-versions vspec upstream-versions)))
                   enriched-spec enriched-spec)

   _ (when print-resolved-spec
       (print-output output-format resolved-spec)
       (js/process.exit 0))

   unresolved (filter (comp empty? :version-details) (vals resolved-spec))]

  (assert (empty? unresolved)
          (str "Could not resolve versions for: "
               (S/join ", " (map :var-name unresolved))))

  (->> (for [[k v] resolved-spec
             :let [details (get v :version-details
                                [{:version-string v}])
                   versions (map :version-string details)]]
         [k (if enumerate versions (last versions))])
       (into {})
       (print-output output-format)))
