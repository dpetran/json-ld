(ns fluree.json-ld.processor.context
  (:refer-clojure :exclude [resolve])
  (:require [fluree.json-ld.processor.util :as util]
            [fluree.json-ld.impl.external :as external]
            [cheshire.core :as json]
            [fluree.json-ld.processor.loader :as loader]))

(defn initial-context
  [options]
  {:context/processing-mode (:json-ld/processing-mode options)
   :context/mappings {}
   :context/protected? {}})

(def max-context-urls 10)

(defn resolve-remote-context
  [{:keys [active-context context loader base cycles]}]
  (loop [[context & contexts] (util/as-seq (get context "@context" context))])
  (when (> (count cycles) max-context-urls)
    (throw (ex-info "Maximum number of @context URLs exceeded." {:max max-context-urls
                                                                 :code "context overflow"})))
  (when (cycles url)
    (throw (ex-info "Cyclical @context URLs detected." {:url url
                                                        :code "context overflow"})))

  {:resolved (loader url)
   :cycles* (conj cycles url)})

(defn resolve
  [& {:keys [active-context context loader base cycles]}]
  (let [context (if (get context "@context")
                  (get context "@context")
                  context)]
    (loop [[ctx & contexts] (util/as-seq context)
           cycles (or cycles #{})
           all-resolved []]
      (if ctx
        (cond (string? ctx)
              (let [{:keys [resolved cycles*]} (resolve-remote-context :active-context active-context
                                                                       :url ctx :loader loader
                                                                       :base base :cycles cycles)]
                (recur contexts cycles* (into all-resolved (util/as-seq resolved))))

              (map? ctx)
              (recur contexts cycles (conj all-resolved ctx))

              :else
              (throw (ex-info "Invalid JSON-LD syntax: @context" {"@context" ctx
                                                                  :code "invalid local context"})))
        all-resolved))))

(comment
  (def x (resolve :active-context {} :context "https://schema.org"
                  :loader load-bundled-context :base ""))

  x

  ,)

(defn load-bundled-context
  [iri]
  (if (external/external-contexts iri)
    (some-> iri
            external/context->file
            :source
            util/read-file
            json/parse-string)
    (throw (ex-info "Context IRI not bundled." {:url iri :code "loading remote context failed"}))))

(defn load-external-context
  [iri]
  (throw (ex-info "Unable to load external context" {:url iri :code "loading remote context failed"})))

(defn initial-active-context
  []
  {:term-definitions []
   :base-iri ""
   :original-base-url ""})

(defn processing-mode
  ""
  [active-context version]
  ;; if version is > 1.1.
  ;; active-context proc-mode is either not set or is set to > json-ld-{version}
  ;; otherwise
  ;; active-context proc-mode is set to json-ld-1.0
  (=) (::processing-mode active-context)
  (let []))

(defn select-term
  [])

(defn create-inverse-context
  [])

(defn create-term-definition
  {:malli/schema [:=> [:cat :map :map any? :map :map] :map]}
  [active-context local-context term defined
   {:keys [base-url protected override-protected remote-contexts validate-scoped-context] :as options
    :or {base-url nil
         protected false
         override-protected false
         remote-contexts #{}
         validate-scoped-context true}}]
  #_(if (get defined term))

  (cond (true? (get defined term))
        nil
        (false? (get defined term))
        (throw (ex-info "Failed to create term definition." {:code "cyclic IRI mapping"
                                                             :context local-context
                                                             :term term}))
        :else
        (let [defined (assoc defined term false)
              value (get local-context term)]
          (cond (and (= "@type" term)
                     (map? value)
                     (= (or (get value "@container") "@set") "@set"))
                :continue...))))

(defn protected-term-definitions
  [context]
  ;; TODO: this might be wrong? is term-defs a sequence? Revisit after implementing term-defs
  (some #(contains? % :term/protected?) (:context/term-definitions context)))

(defn process
  ([active-context local-context base-url {:keys [remote-contexts override-protected? propagate?
            validate-scopted-context?]
     :or {propagate? true
          remote-contexts #{}
          override-protected? false
          validate-scopted-context? true}
     :as options}]
   (let [propagate? (get local-context "@propagate" propagate?)]
     (loop [[context & contexts] (util/as-seq local-context)
            result (dissoc active-context :context/inverse-context)]
       (let [result (if (and (not propagate?)
                             (not (:context/previous-context result)))
                      (assoc result :context/previous-context active-context)
                      result)]
         (cond (nil? context)
               ;; TODO: 5.1
               result
               #_(if (and override-protected? (protected-term-definitions context))
                   (throw (ex-info "Context processing error."
                                   {:code "invalid context nullification"}))
                   (recur contexts
                          (cond-> {::base-iri base-url ::original-base-url base-url}
                            (false? propagate?) (assoc ::previous-context result))))

               ;; remote context 5.2
               (string? context)
               (let [context (str base-url context)]
                 (cond (and validate-scopted-context? (remote-contexts context))
                       (recur contexts result)

                       (>= (count remote-contexts) max-context-urls)
                       (throw (ex-info (str "Exceeded processor max remote-context limit: " max-context-urls)
                                       {:code "context-overflow"}))

                       :else
                       (let [remote-contexts (conj remote-contexts context)]
                         )))

               ;; context definition

               ;;
               ))))

   #_(let [local-context (if (and (get local-context "@context")
                                  (sequential? (get local-context "@context")))
                           (get local-context "@context")
                           local-context)]
       ;; TODO: fixup this short circuit
       (if-not (not-empty (util/as-seq local-context))
         active-context
         (let [resolved (resolve :active-context active-context :context local-context
                                 :loader load-bundled-context :base base)
               propagate? (or (get (first resolved) "@propagate?") propagate?)
               ])))



   #_(let [propagate? (or (get local-context "@propagate?") propagate?)
           result (assoc active-context :inverse-context nil)
           result (if (and (false? propagate?) (not (:previous-context result)))
                    (assoc result :previous-context active-context))]
       (when (not (boolean? propagate?)) (throw (ex-info "invalid @propagate? value" {"@propagate?" propagate?})))

       (loop [[context & local-contexts] (util/as-seq local-context)
              result result]
         (let []
           (cond
             (nil? context) (if (and override-protected?) :todo)
             ;; remote context
             (string? context) (throw (ex-info "TODO: implement dereferencing remote contexts."))
             ;; context definition
             (map? context) (cond-> {}
                              (get context "@base") (assoc "@base" (get context "@base"))
                              (get context "@direction") (assoc "@direction" (get context "@direction"))
                              (get context "@language") (assoc "@language" (get context "@language"))
                              (get context "@propagate?") (assoc "@propagate?" (get context "@propagate?"))
                              (get context "@version") (assoc "@version" (get context "@version"))
                              (get context "@vocab") (assoc "@vocab" (get context "@vocab"))
                              ;; (get context "@vocab")
                              ;; (throw (ex-info "TODO: implement @import."))
                              (false? (get context "@propogate")) (assoc :previous-context active-context))))))))
