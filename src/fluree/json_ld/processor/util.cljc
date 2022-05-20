(ns fluree.json-ld.processor.util
  (:require [clojure.java.io :as io]))

(defn as-seq
  [x]
  (if (sequential? x) x [x]))

(defn read-file
  [filename]
  #?(:cljs (throw (ex-info (str "Loading external resources is not yet supported in Javascript.")
                           {:status 400 :error :json-ld/external-resource}))
     :clj  (try
             (some-> filename
                     io/resource
                     slurp)
             (catch Exception e
               (throw (ex-info
                        (str "" filename)
                        {:code "file not found"
                         :filename filename}
                        e))))))
