(ns viasat.voom
  (:require [promesa.core :as P]
            [clojure.string :as S]
            [viasat.util :refer [Eprn exec trim]]
            ["dateformat$default" :as dateformat]))

;; Based on the shell and babashka versions here:
;; https://github.com/LonoCloud/voom-util
;;
;; Differences:
;; - split out core functionality into voom-versions-data which will
;;   return a sequence of maps with more details about each version,
;;   not just the final version. voom-version will pick the most
;;   recent version string and return it.
;; - voom-versions-data has an all? parameter that will return
;;   all historical versions for the given paths.
;; - If all the paths given do not exist in the history then this will
;;   return an error like the original voom commands. However, unlike
;;   the originals, if any path exists in the history then no error
;;   will be returned if some (but not all) of the paths exist.

(defn voom-versions-data
  "Return voom formatted versions for all log entries for the
  specified paths. If there are any local changes or unstaged files at
  those paths then add a final vresion with '_DIRTY' appended to the
  version and with a timestamp/date-str of right now."
  [paths dirty-suffix all?]
  (P/let
    [git-cmd #(P/-> (exec (str "git " (S/join " " %&))) :stdout trim)
     targs (S/join " " paths)
     [logs status]
     , (P/all [(git-cmd (str "log " (when (not all?) "-1")
                             " --pretty='%h,%aE,%cE,%cI' --abbrev=12 -- " targs))
               (git-cmd (str "status --short -- " targs))])
     _ (when (empty? logs)
         (throw (js/Error. (str "No logs found for: " (S/join ", " paths)))))
     versions (for [log (S/split logs #"\n")]
                (let [[sha author committer commit-date] (S/split log #",")
                      date-str (dateformat (js/Date. commit-date) "UTC:yyyymmdd_HHMMss")]
                  {:paths paths
                   :date-str date-str
                   :sha sha
                   :author author
                   :voom-version (str date-str "-g" sha)}))]
    (reverse
      (if (empty? status)
        versions
        (let [curver (first versions)]
          (conj versions
                (merge
                  curver
                  {:date-str (dateformat (js/Date.) "UTC:yyyymmdd_HHMMss")
                   :sha (str (:sha curver) dirty-suffix)
                   :voom-version (str (:voom-version curver) dirty-suffix)})))))))

(defn voom-version [paths]
  (P/-> (voom-versions-data paths "_DIRTY" false) last :voom-version))
