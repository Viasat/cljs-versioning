#!/usr/bin/env nbb

(ns bump
  (:require [cljs.pprint :refer [pprint]]
            [cljs-bean.core :refer [->clj]]
            [promesa.core :as P]
            [clojure.string :as S]
            [viasat.util :refer [parse-opts load-yaml
                                 Eprintln Eprn Epprint fatal]]
            [viasat.bump :as bump]
            ["yaml$default" :as yaml]))

(def usage (str "
Bump/update/output version variables that represent artifacts by
looking at their source repositories and filtering smartly.

Usage:
  bump [options] <version-spec-files>

Options:
  -h, --help                          Show help/usage.
  -v, --verbose                       Show verbose output (stderr)
                                      [env: VERBOSE]
  --debug                             Show debug/trace output (stderr)
                                      [env: DEBUG]
  --defaults-files DEFAULTS-FILES     Comma separated files and/or directories with
                                      default spec data. Not used for selecting
                                      which spec names to query/resolve.
  --skip-remote-query                 Skip image/rpm querying and uuse
                                      version-default instead (must be specified fully)
  --enumerate                         List all available versions that match each
                                      variable version spec
  --print-full-spec                   Output the merged version spec with defaults
  --print-resolved-spec               Output the full spec with upstream version details
  --output-format FORMAT              Formats: dotenv, json, yaml [default: dotenv]
                                      Defaults to json for --enumerate, --print-*-spec
  --short-version                     Only emit the version value (omit the path/image prefix)

  --profile PROFILE                   AWS profile for ECR access [env: PROFILE]
  --artifactory-base-url URL          Artifactory base URL
                                      [env: ARTIFACTORY_BASE_URL]
  --artifactory-username USERNAME     Artifactory username
                                      [env: ARTIFACTORY_USERNAME]
  --artifactory-identity-token TOKEN  Artifactory identity token
                                      [env: ARTIFACTORY_IDENTITY_TOKEN]

The version-spec-files argument and the defaults-files option are
comma separated lists of directories and/or files. For directories
bump looks for version-spec.(yaml|yml) files in those directories.
Empty directories and non-existent files are skipped. Found version
spec files are merged one level deep, such that each variable's
version specs are shallow merged.

Version spec files and default files are merged from left to right
(with right most values taking precendence). After the, default values
are merged with specs in the following precendence order (from lowest
to highest):
  * defaults file :all
  * defaults file :by-type
  * defaults file :by-name
  * spec file

Defaults from multiple files are merged at the spec map level. The top
level keys of the defaults file are:
  * all: defaults that apply to all other spec values.
  * by-type: sub-keys are 'image' and/or 'rpm' that each contain
    defaults for all spec values of that type.
  * by-name: sub-keys are spec value names with default that will be
    applied to the matching spec value by name. If the named spec
    value does not exist in a specified version-spec file then it will
    be ignored.

" bump/spec-format))

;; Command line utility functions
(defn comma-split [v] (if (string? v) (S/split v #",") v))

(defn print-output
  "JSON and YAML output might be buffered so this is defined as
  a promise the resolves when everything is flushed"
  [fmt results]
  (P/create
    (fn [resolve reject]
      (let [out (case fmt
                  "dotenv" (str (S/join "\n" (for [[k v] results]
                                               (str k "=" v))) "\n")
                  "json" (.stringify js/JSON (clj->js results))
                  "yaml" (yaml/stringify (clj->js results)))]
        (js/process.stdout.write out resolve)))))

(P/let
  [cfg (parse-opts usage *command-line-args* {:laxPlacement true})
   _ (when (empty? cfg) (fatal 2))
   {:keys [version-spec-files debug verbose defaults-files skip-remote-query
           enumerate print-full-spec print-resolved-spec
           output-format short-version]} cfg
   cfg (merge cfg {:resolve-remote (not skip-remote-query)
                   :all-local true})
   verbose (if debug true verbose) ;; debug implies verbose
   _ (when debug (Eprintln "Settings:") (Epprint cfg))

   output-format (if (and (= "dotenv" output-format)
                          (or enumerate print-full-spec print-resolved-spec))
                   "json"
                   output-format)
   defaults-files     (bump/find-files (comma-split defaults-files)
                                       ["version-spec-defaults.yaml"
                                        "version-spec-defaults.yml"])
   version-spec-files (bump/find-files (comma-split version-spec-files)
                                       ["version-spec.yaml"
                                        "version-spec.yml"])

   defaults (when defaults-files
              (when verbose (Eprintln "Loading defaults:" defaults-files))
              (P/all (map load-yaml defaults-files)))
   _ (when verbose (Eprintln "Loading version specs:" (S/join " " version-spec-files)))
   version-spec (P/then (P/all (map bump/load-version-spec version-spec-files))
                        #(apply merge-with merge {} %))

   full-spec (bump/normalize-spec defaults version-spec (not print-full-spec))

   _ (when print-full-spec
       (P/do (print-output output-format full-spec)
             (js/process.exit 0)))

   _ (when verbose (Eprintln "Querying versions in parallel (docker, RPM, git)"))
   resolved-spec (bump/resolve-spec cfg full-spec (not print-resolved-spec))

   _ (when print-resolved-spec
       (P/do (print-output output-format resolved-spec)
             (js/process.exit 0)))

   version-key (if short-version :version :full-version)]

  (->> (for [[k v] resolved-spec
             :let [details (get v :version-details)
                   versions (map version-key details)]]
         [k (if enumerate versions (last versions))])
       (into {})
       (print-output output-format)))
