(ns fluree.json-ld.processor.expand
  (:require [promesa.core :as promise]
            [fluree.json-ld.processor.schema :as schema]
            [fluree.json-ld.processor.loader :as loader]
            [fluree.json-ld.processor.context :as context]
            [fluree.json-ld.processor.util :as util]
            [malli.core :as m]
            [jsonista.core :as json]))

(defn expand-iri
  [])

(defn expand-value
  [])

(defn expand*
  [active-context active-property element base-url {:keys [frame-expansion?
                                                           ordered?
                                                           from-map?]}]
  (if (nil? element)
    element
    (let [frame-expansion? (if (= active-property "@default") false frame-expansion?)
          property-scoped-context ()])))

(defn expand
  [input {:keys [base expand-context] :as options}]
  (let [p (promise/deferred)]
    (try
      (let [{:keys [document document-url context-url] :as remote-document}
            (cond (m/validate schema/RemoteDocument input) input
                  (string? input) @(loader/static-loader input options)
                  :else           {:document input})

            active-context (merge (context/initial-context options)
                                  {:base-iri          (or document-url base)
                                   :original-base-url (or document-url base)
                                   :term-definitions  []})
            active-context  (if expand-context
                              (context/process active-context
                                               (get expand-context "@context" expand-context)
                                               (:original-base-url active-context)
                                               {})
                              active-context)
            active-context  (if context-url
                              (context/process active-context context-url context-url {})
                              active-context)
            expanded-output (expand* active-context nil document (or document-url base) options)
            expanded-output (cond (nil? expanded-output)
                                  []

                                  (and (= 1 (count expanded-output))
                                       (get expanded-output "@graph"))
                                  (get expanded-output "@graph")

                                  :else (util/as-seq expanded-output))]
        (promise/resolve! p (json/write-value-as-string expanded-output)))
      (catch Exception e
        (promise/reject! p e)))
    p))

(comment
  (let [a {:a 1}]
    (get a :b a))
  {:a 1}

  (def a (expand))
  (promise/resolved? a)
  (promise/pending? a)
  (promise/handle a (fn [result reject] (println "result" result "reject" reject)))





  ,)
