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
  bump [options] [<version-spec-file>]...

Options:
  -h --help                           Show help/usage.
  --debug                             Show debug/trace output (stderr)
                                      [env: DEBUG]
  --enumerate                         All available versions that match each variable version spec
  --print-full-spec                   Output the full, merged version spec
  --output-format FORMAT              Formats: dotenv, json, yaml [default: dotenv]

  --profile PROFILE                   AWS profile for ECR access [env: PROFILE]
  --artifactory-base-url URL          Artifactory base URL
                                      [env: ARTIFACTORY_BASE_URL]
  --artifactory-username USERNAME     Artifactory username
                                      [env: ARTIFACTORY_USERNAME]
  --artifactory-identity-token TOKEN  Artifactory identity token
                                      [env: ARTIFACTORY_IDENTITY_TOKEN]

version-spec-file arguments can be either directories or files.
In directories bump looks for version-spec.(yaml|yml) files.  Empty
directories and non-existent files are skipped.  Found version spec
files are merged one level deep, such that each variable's version
specs are shallow merged.

Version spec files can define a reserved key 'global' to set
redundant version spec attributes in one place.  Within 'global'
files can define common version specs for 'all' or for specific
types (ex: 'image').
")

;; General utility functions

(def ECR-REPO-RE #"([^.]*)\.dkr\.ecr\.([^.]*)\.amazonaws\.com")

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

(defn find-version-spec [path]
  (P/let [dir? (directory? path)
          paths (if dir?
                  (map #(str path %) ["/version-spec.yaml" "/version-spec.yml"])
                  [path])
          paths (P/all
                  (for [p paths]
                    (P/let [exists? (file-exists? p)]
                      (when exists? p))))]
    (first (filter identity paths))))

(defn load-version-spec [path]
  (P/->> (read-file path "utf8")
         yaml/parse
         ->clj
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

(defn enrich-spec [global vname vspec]
  (let [vspec (merge (:all global)
                     (get global (keyword (:type vspec)))
                     vspec)
        {:keys [image namespace registry artifactory-api]} vspec

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

(defn get-version-row-spec [kind]
  (condp = kind
    :docker-art {:spec-name-field  :image-repo
                 :name-field       :image
                 :version-field    :tag
                 :date-field       :lastUpdated
                 :ver-delim        ":"
                 :creator-field    :createdBy}
    :docker-ecr {:spec-name-field  :ecr-repo
                 :name-field       :repositoryName
                 :version-field    :tag
                 :date-field       :imagePushedAt
                 :ver-delim        ":"}
    :rpm        {:spec-name-field  :name
                 :name-field       :name
                 :version-field    :full-version
                 :date-field       :build-date
                 :ver-delim        "-"}))

(defn resolve-versions
  "Takes version rows and a version spec.  Returns string versions
  from matching rows, sorted by date-field.

  Filters according to the following row/version selection algorithm:
    - filters rows where spec-name-field in spec equals name-field in
      rows
    - if version spec specified, then filters rows where version
      matches the beginning of version-field in rows
    - if version-regex spec specified, then filters rows where
      version-regex is a RegExp match on version-field in rows
    - if date is specified, then filters rows where date-field is less
      than date and version is not equal to 'latest'
    - if latest is specified, then filter rows where version-field is
      not equal to 'latest' (so that latest actual version get selected)
    - if image-creators specified (and is not 'all'), then filters
      rows where creator-field is one of image-creators

  Last returned version string is the most recent version matching all
  criteria. Version strings are the row name-field and version-field,
  concatenated together using ver-delim."
  [rows {:keys [upstream-api latest date version version-regex image-creators] :as spec}]
  (let [{:keys [spec-name-field name-field version-field date-field
                creator-field ver-delim] :as rowspec} (get-version-row-spec upstream-api)
        my-name (get spec spec-name-field)
        date (if date (js/Date. date) nil)
        ver-re (js/RegExp. version-regex)
        rows (sort-by date-field rows)
        rows (cond->> rows
               my-name (filter #(= my-name (get % name-field)))

               version (filter #(.startsWith (get % version-field) version))

               version-regex (filter #(re-seq ver-re (get % version-field)))

               date (filter #(and (not= "latest" (get % version-field))
                                  (<= (get % date-field) date)))

               latest (filter #(not= "latest" (get % version-field)))

               (and creator-field
                    (not (empty? image-creators)))
               , (filter #((set image-creators) (get % creator-field))))]
    (->> rows
         (keep #(get % version-field))
         (mapv #(str my-name ver-delim %)))))

(defn print-output [fmt results]
  (case fmt
    "dotenv" (doseq [[k v] results]
               (println (str k "=" v)))
    "json" (print (.stringify js/JSON (clj->js results)))
    "yaml" (print (yaml/stringify (clj->js results)))))

(P/let
  [
   ;; Setting `:laxPlacement true` with `<version-spec-file>...` (note the ...)
   ;; causes neodoc to drop the default options' values
   cfg (parse-opts usage *command-line-args* {})
   _ (when (empty? cfg) (fatal 2))
   {:keys [version-spec-file debug enumerate print-full-spec output-format
           profile artifactory-base-url]} cfg
   _ (when debug (Eprintln "Settings:") (Epprint cfg))

   _ (assert (not (and (= "dotenv" output-format)
                       (or enumerate print-full-spec)))
             "Cannot use default 'dotenv' format with --enumerate or --print-full-spec")

   version-spec-files (P/then (P/all (map find-version-spec version-spec-file))
                              #(filter identity %))
   _ (when debug (Eprintln "Loading version specs:" (S/join " " version-spec-files)))
   version-spec (P/then (P/all (map load-version-spec version-spec-files))
                        #(apply merge-with merge {} %))
   _ (when print-full-spec
       (print-output output-format version-spec)
       (js/process.exit 0))

   global-spec (get version-spec "global" {})
   version-spec (dissoc version-spec "global")
   versions (reduce
              (fn [res [vname vspec]]
                (assoc res vname (enrich-spec global-spec vname vspec)))
              version-spec version-spec)
   validation-errors (keep #(apply validation-error %) versions)
   _ (when-not (empty? validation-errors)
       (Eprintln "Invalid specs found:")
       (doseq [msg validation-errors]
         (Eprintln msg))
       (js/process.exit 1))

   _ (when debug (Eprintln "Getting RPM versions and docker image tags (in parallel)"))
   art-headers (artifactory/get-auth-headers cfg)
   aws-opts (if profile
              {:debug debug :profile profile}
              {:debug debug :no-profile true})
   _ (when debug (artifactory/enable-debug))
   [repo-rpms repo-art-images repo-ecr-images]
   , (P/all
       [(P/all
          (for [repo (distinct (map :repo (filter rpm? (vals version-spec))))]
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
                   (sort-by :imagePushedAt))

   ;; Resolve updated value for each version spec on command line
   resolved (reduce (fn [res [k vspec]]
                      (let [rows (get {:docker-art art-images
                                       :docker-ecr ecr-images
                                       :rpm rpms} (:upstream-api vspec))]
                        (assoc-in res [k :upstream-versions]
                                  (resolve-versions rows vspec))))
                    versions versions)

   unresolved (filter (comp empty? :upstream-versions) (vals resolved))]

  (assert (empty? unresolved)
          (str "Could not resolve versions for: "
               (S/join ", " (map :var-name unresolved))))

  (->> (for [[k v] resolved
             :let [versions (get-in resolved [k :upstream-versions] [v])]]
         [k (if enumerate versions (last versions))])
       (into {})
       (print-output output-format)))
