(ns fluree.json-ld.expand
  (:require [fluree.json-ld.iri :as iri]
            [fluree.json-ld.context :as context]
            [fluree.json-ld.external :as external]
            [fluree.json-ld.util :refer [try-catchall sequential]]))

;; TODO - differentiate resolution between @type: @id vs @type: @vocab
;; TODO - support @container: @language indexed value
;; TODO - support for @base - applies only to values, not @type or properties (or where explicit @type: @vocab used)

#?(:clj (set! *warn-on-reflection* true))

(declare node)

(defn match-exact
  "Attempts to do an exact match with a compact-iri.
  If successful returns two-tuple of [full-iri context-map-details].
  Else returns nil."
  [compact-iri context]
  (when-let [exact-match (get context compact-iri)]
    (let [iri (or (:id exact-match)
                  (:reverse exact-match)
                  (throw (ex-info
                           (str "Matching value in context does not contain an @id or @reverse: " compact-iri)
                           {:status 400 :error :json-ld/invalid-iri})))]
      [iri exact-match])))


(defn match-prefix
  "Attempts to do a prefix match with a compact-iri.
  If successful returns two-tuple of [full-iri context-map-details].
  Else returns nil."
  [compact-iri context]
  (when-let [[prefix suffix] (iri/parse-prefix compact-iri)]
    (when-let [prefix-match (get context prefix)]
      [(str (:id prefix-match) suffix) prefix-match])))


(defn match-default
  "If context defines a :vocab (default vocab) and compact-iri does
  not look like a full iri (i.e. not https://schema.org/Movie) returns match.
  If successful returns two-tuple of [full-iri context-map-details].
  Else returns nil."
  [compact-iri context vocab?]
  (when-let [default-match (if vocab?
                             (:vocab context)
                             (:base context))]
    (when-not (or (iri/any-iri? compact-iri)
                  (= \@ (first compact-iri)))
      (let [iri (str default-match compact-iri)]
        [iri {:id iri}]))))


(defn details
  "Attempts to match compact-IRI, and if successful returns a two-tuple of
  the full matched IRI along with a map containing any details provided
  in the original context.

  Used primarily with transactions, as if enough details are provided with the context
  we can auto-generate schemas."
  [compact-iri context vocab?]
  (or (match-exact compact-iri context)
      (match-prefix compact-iri context)
      (match-default compact-iri context vocab?)
      [compact-iri (when (= \@ (first compact-iri))
                     {:id compact-iri})]))


(defn iri
  "Expands a compacted iri string to full iri.

  If the iri is not compacted, returns original iri string."
  [compact-iri context vocab?]
  (first (details compact-iri context vocab?)))


(defmulti parse-node-val (fn [v _ _ _ idx]
                           (cond
                             (map? v) :map
                             (sequential? v) :sequential
                             (string? v) :string
                             (number? v) :number
                             (boolean? v) :boolean
                             (nil? v) :nil
                             :else (throw (ex-info (str "Values in payload must be strings, numbers, maps or vectors. "
                                                        "Provided value: " v " at index: " idx ".")
                                                   {:status 400
                                                    :error  :json-ld/invalid-context})))))

(defmethod parse-node-val :nil
  [v v-info context _ idx]
  nil)

(defmethod parse-node-val :boolean
  [v v-info _ _ idx]
  {:value v
   :type  (:type v-info)                                    ;; type may be defined in the @context
   :idx   idx})

(defmethod parse-node-val :string
  [v {:keys [id type] :as v-info} context _ idx]
  (cond
    (= "@id" id) (iri v context false)
    (= :id type) {:id  (iri v context false)
                  :idx idx}
    :else {:value v
           :type  type
           :idx   idx}))

(defmethod parse-node-val :number
  [v v-info _ _ idx]
  {:value v
   :type  (:type v-info)                                    ;; type may be defined in the @context
   :idx   idx})

(defmethod parse-node-val :map
  [v v-info context externals idx]
  (let [ctx* (if-let [sub-ctx (get v "@context")]
               (context/parse context sub-ctx)
               context)]
    (cond
      (contains? v "@list")
      {:list (-> (get v "@list")
                 (parse-node-val v-info context externals (conj idx "@list")))}

      (contains? v "@set")                                  ;; set is the default container type, so just flatten to regular vector
      (-> (get v "@set")
          (parse-node-val v-info context externals (conj idx "@set")))

      (contains? v "@value")
      (let [val  (get v "@value")
            type (if-let [explicit-type (get v "@type")]
                   (iri explicit-type ctx* true)
                   (:type v-info))]                         ;; if type is defined only in the @context
        (if (= "@id" (get v "@type"))
          {:id  (iri val ctx* false)
           :idx idx}
          {:value val
           :type  type
           :idx   idx}))

      ;; else a sub-value. Top-level @context might have sub-contexts, if so merge
      :else
      (node v (merge ctx* (:context v-info)) externals idx))))

(defmethod parse-node-val :sequential
  [v v-info context externals idx]
  (let [v* (->> v
                (map-indexed #(cond
                                (map? %2) (node %2 context externals (conj idx %1))
                                (sequential? %2) (throw (ex-info (str "Json-ld sequential values within sequential"
                                                                      "values is not allowed. Provided value: " v
                                                                      " at index: " (conj idx %1) ".")
                                                                 {:status 400 :error :json-ld/invalid-context}))
                                :else
                                (let [type (:type v-info)]
                                  (if (= :id type)
                                    {:id  (iri %2 context false)
                                     :idx (conj idx %1)}
                                    {:value %2
                                     :type  type
                                     :idx   (conj idx %1)}))))
                (into []))]
    (if (= :list (:container v-info))
      {:list v*}
      v*)))


(defn- type-sub-context
  "@context can define sub-contexts for certain @type values. Check if exists and merge."
  [context types]
  (reduce
    (fn [context* type]
      (if-let [type-context (get-in context [type :context])]
        (merge context* type-context)
        context*))
    context
    types))


(defn parse-type
  "Parses @type values, returns two-tuple of expanded @type IRIs
  and a (possibly) updated context if there was a type-dependent sub-context present.
  Always return @type as a vector regardless of input."
  [node-map context idx]
  (let [base (if (empty? idx) {:idx []} {:idx idx})]
    (if-let [type-val (get node-map (:type-key context))]
      (let [type-val*     (sequential type-val)
            ;; context may have type-dependent sub-context, update context if so
            context+types (type-sub-context context type-val*)
            expanded      (mapv #(if (string? %)
                                   (iri % context true)
                                   (throw (ex-info (str "@type values must be strings or vectors of strings, provided: "
                                                        type-val " at index: " (conj (:idx base) (:type-key context)) ".")
                                                   {:status 400 :error :json-ld/invalid-context})))
                                type-val*)]
        [(assoc base :type expanded) context+types])
      [base context])))


(defn- node*
  "Does parsing of a node map once normalization happens during 'node' fn.
  node-map should already have @context and @type keys removed as the :type
  will already be included in the base-result."
  [node-map base-result externals context]
  (loop [[[k v] & r] node-map
         context context
         acc     (transient base-result)]
    (if k
      (let [idx* (conj (:idx base-result) k)
            [k* v-info] (details k context true)
            k**  (if (= \@ (first k*))
                   (keyword (subs k* 1))
                   k*)
            v*   (parse-node-val v v-info context externals idx*)]
        (recur r
               (if (= :type k**)
                 (type-sub-context context v)
                 context)
               (assoc! acc k** v*)))
      (persistent! acc))))


(defn node
  "Expands an entire JSON-LD node (JSON object), with optional parsed context
  provided. If node has a local context, will merge with provided parse-context.

  Expands into child nodes."
  ([node-map] (node node-map {} external/external-contexts []))
  ([node-map parsed-context] (node node-map parsed-context external/external-contexts []))
  ([node-map parsed-context externals idx]
   (try-catchall
     (if (sequential? node-map)
       (map-indexed #(node %2 parsed-context externals (conj idx %1)) node-map)
       (let [context (context/parse parsed-context (get node-map "@context"))]
         (if-let [graph (get node-map "@graph")]
           (map-indexed #(node %2 context externals ["@graph" %1]) graph)
           (let [[base-result context*] (parse-type node-map context idx)
                 node-map* (dissoc node-map "@context" (:type-key context))]
             (node* node-map* base-result externals context*)))))
     (catch e
            (if (ex-data e)
              (throw e)
              (throw (ex-info (str "Invalid JSON-LD. Error occurred at JSON object index: " idx
                                   " with error: " (ex-message e))
                              {:status 400
                               :error  :json-ld/invalid}
                              e)))))))
