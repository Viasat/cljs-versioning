#!/usr/bin/env nbb

;; Copyright (c) 2024, Viasat, Inc
;; Licensed under EPL 2.0

(ns voom-like-version
  (:require [promesa.core :as P]
            [viasat.util :refer [parse-opts exec trim]]
            [viasat.voom :refer [voom-version]]))

(def usage "
Usage:
  voom-like-version [options] <paths>...

Options:
  -h, --help                        Show help/usage.
")

(P/let
  [{:keys [paths]} (parse-opts usage *command-line-args*)
   top-dir (P/-> (exec (str "git rev-parse --show-toplevel")) :stdout trim)
   abs-paths (map #(str top-dir "/" %) paths)
   version (voom-version abs-paths)]
  (println version))
