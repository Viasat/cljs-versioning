(ns viasat.docker-compose
  (:require [promesa.core :as P]
            [clojure.string :as S]
            [viasat.util :refer [exec read-file]]
            ["yaml$default" :as yaml]))

(def VAR-RE #"\$\{([^}]+)\}")
(def ONE-VAR-RE #"^\$\{([^}]+)\}$")
(def REG-RE #"^([^/]+)/([^:]+)(:([^/:]+))?$")

(defn get-image-vars
  [dc-raw dc-int]
  (for [[service sdata] (get dc-raw "services")
        :let [image-raw (get-in sdata ["image"])
              image-int (get-in dc-int ["services" service "image"])
              [_ registry image _ tag] (first (re-seq REG-RE image-int))]
        v (map second (re-seq VAR-RE image-raw))]
    {:variable  v
     :service   service
     :location  :image
     :registry  registry
     :image     image
     :image-tag tag
     :image-raw image-raw
     :image-int image-int}))

(defn get-build-vars
  [dc-raw dc-int]
  (P/->>
    (for [[service sdata] (get dc-raw "services")
          :let [{:strs [args context dockerfile]} (get sdata "build")
                args-int (get-in dc-int ["services" service "build" "args"])]
          
          [arg val-raw] args]
      (if (or (empty? val-raw) (re-seq ONE-VAR-RE val-raw))
        [{:variable   arg
          :service    service
          :location   :build
          :value-raw  val-raw
          :value-int  (get args-int arg)}]
        (for [v (map second (re-seq VAR-RE val-raw))]
          {:variable  v
           :service   service
           :location  :build})))
    (apply concat)))

(defn get-vars
  "Loads (async) docker-compose config and parses out variables from
  the image and build args.

  Returns a promise with a map of variable names to data maps where
  each map contains:
      :variable <VARIABLE-NAME>
      :service  <COMPOSE-SERVICE-NAME>
      :location < :image | :build >

  :image location data maps also contain:
      :registry   <DOCKER-REGISTRY>
      :image      <DOCKER-IMAGE>
      :image-tag  <DOCKER-IMAGE-TAG>
      :image-raw  <IMAGE-VALUE-RAW>
      :image-int  <IMAGE-VALUE-INTERPOLATED>

  :build location data maps also contain:
      :value-raw <VARIABLE-VALUE-RAW>
      :value-int <VARIABLE-VALUE-INTERPOLATED>"
  [{:keys [compose-cmd] :or {compose-cmd "docker-compose config"}}]
  (P/let
    [[dc-raw dc-int]
     , (P/all [(P/-> (exec "docker-compose config --no-interpolate")
                     :stdout yaml/parse js->clj)
               (P/-> (exec "docker-compose config")
                     :stdout yaml/parse js->clj)])
     img-vars (get-image-vars dc-raw dc-int)
     bld-vars (get-build-vars dc-raw dc-int)]
  (->> (concat img-vars bld-vars)
       (map (fn [{:keys [variable] :as data}] [variable data]))
       (into {}))))

