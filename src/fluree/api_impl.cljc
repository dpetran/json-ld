(ns fluree.api-impl
  (:refer-clojure :exclude [flatten])
  (:require [fluree.json-ld.impl.context :as context]
            [fluree.json-ld :as jld]
            [clojure.string :as str]
            [fluree.json-ld.impl.util :refer [try-catchall sequential]]
            [fluree.json-ld.impl.expand :as expand]
            [fluree.json-ld.impl.external :as external]))


(def Keywords
  [:enum "@base" "@container" "@context" "@direction" "@graph" "@id"
   "@import" "@included" "@index" "@json" "@language" "@list" "@nest" "@none" "@prefix"
   "@propogate" "@protected" "@reverse" "@set" "@type" "@value" "@version" "@vocab"])

(def InitialContext
  [:map
   [:processing-mode [:enum "json-ld.1.0" "json-ld.1.1"]]
   [:mappings :map]

   [:get]])

(def TermDefinition
  [:or :string :map])

(def ActiveContext
  [:map
   [:term-definitions [:sequential TermDefinition]]
   [:base-iri :string]
   [:original-iri :string]
   [:inverse-context :map]
   [:vocab-mapping {:optional true} :string]
   [:default-language {:optional true} :string]
   [:default-base-direction {:optional true} [:enum "ltr" "rtl"]]
   [:previous-context {:optional true} [:enum "ltr" "rtl"]]])

(def LocalContext
  [:or :string :map [:sequential [:or :string :map]]])


(defn as-seq
  [x]
  (if (sequential? x) x) [x])

(defn resolve-context
  ([active-context local-context {:keys [remote-contexts override-protected propagate
                                         validate-scopted-context base]
                                  :or   {remote-contexts          []
                                         base                     ""
                                         override-protected       false
                                         propagate true
                                         validate-scopted-context true}
                                  :as   opts}]

   (let [propagate (or (get local-context "@propagate") propagate)
         result (assoc active-context :inverse-context nil)
         result (if (and (false? propagate) (not (:previous-context result)))
                  (assoc result :previous-context active-context))]
     #_(when (not (boolean? propagate)) (throw (ex-info "invalid @propagate value" {"@propagate" propagate})))

     (loop [[context & local-contexts] (as-seq local-context)
            result    result]
       (let []
         (cond
           (nil? context) (if (and override-protected )
                            :x :y)
           ;; remote context
           (string? context) (throw (ex-info "TODO: implement dereferencing remote contexts." {}))
           ;; context definition
           (map? context)    (cond-> {}
                               (get context "@base") (assoc "@base" (get context "@base"))
                               (get context "@direction") (assoc "@direction" (get context "@direction"))
                               (get context "@language") (assoc "@language" (get context "@language"))
                               (get context "@propagate") (assoc "@propagate" (get context "@propagate"))
                               (get context "@version") (assoc "@version" (get context "@version"))
                               (get context "@vocab") (assoc "@vocab" (get context "@vocab"))
                               ;; (get context "@vocab")
                               ;; (throw (ex-info "TODO: implement @import."))
                               (false? (get context "@propogate")) (assoc :previous-context active-context)
                               )

           )))))
  ([context]
   (context/parse context)))

#_(let [{:keys [propagate]
       :or  {"@propogate" true}}])

(defn expand-iri
  [context iri]
  (let [[base suffix] (str/split iri #":")]
    (if suffix
      (str (get context base) suffix)
      base)))

(defn expand-value
  [v]
  v)

(defn scalar?
  [v])

(let [{ctx "@context" dir "@direction" base "@base" lang "@language"
       prop "@propagate" v "@version" import "@import"}
      {"@context" {"a" "b"}}]
  ctx)


#_(defn expand
  "Expands an entire JSON-LD node (JSON object), with optional parsed context
  provided. If node has a local context, will merge with provided parse-context.

  Expands into child nodes."
  ([json-ld])
  ([json-ld {:keys [base expand-context free-floating-nodes?]
             :or {base "" expand-context {} free-floating-nodes? false}}]
   )

  ([node-map] (expand node-map {} external/external-contexts []))
  ([node-map parsed-context] (expand node-map parsed-context external/external-contexts []))
  ([node-map parsed-context externals idx]
   (try-catchall
     (if (sequential? node-map)
       (expand-nodes parsed-context externals idx node-map)
       (let [context   (context/parse parsed-context (or (get node-map "@context")
                                                         (:context node-map)))
             graph-key (cond
                         (contains? node-map "@graph") "@graph"
                         (contains? node-map :graph) :graph)]
         (if-let [graph (get node-map graph-key)]
           (expand-nodes context externals (conj idx "@graph") graph)
           (let [[base-result context*] (parse-type node-map context idx)
                 node-map* (dissoc node-map "@context" :context (:type-key context))]
             (node* node-map* base-result externals context*)))))
     (catch e
         (if (ex-data e)
           (throw e)
           (throw (ex-info (str "Invalid JSON-LD. Error occurred at JSON object index: " idx
                                " with error: " (ex-message e))
                           {:status 400
                            :error  :json-ld/invalid}
                           e)))))))

(comment
  (def d (process-context "https://schema.org"))

  (expand-iri d "name")
  "name"
  (expand {"@context" {"foaf" "http://xmlns.com/foaf/0.1/"}, "@id" "http://greggkellogg.net/foaf#me", "foaf:name" "Gregg Kellogg"})
  {:idx [], :id "http://greggkellogg.net/foaf#me", "http://xmlns.com/foaf/0.1/name" {:value "Gregg Kellogg", :type nil, :idx ["foaf:name"]}}
  ,)

(defn compact
  "Apply context to an internal json-ld representation, to simplify expression for
  application or human use."
  [context json-ld])

(defn flatten
  ([json-ld])
  ([context json-ld]))

(defn rdf-iri
  [iri]
  (str "<" iri ">"))

(defn ->rdf
  "Translate the given internal json-ld representation into a RDF dataset: a set of quads."
  ([json-ld]
   (->rdf [] #_(expand json-ld)))
  ([dataset graph]
   (let [subject (:id graph)]
     (loop [[[property value] & rst] (dissoc graph :id)
            result dataset]
       (if property
         (if (and value (= :idx property))
           (recur rst result)
           (let [subject (str "<" subject ">")
                 predicate (str "<" property ">")]
             ;; should be able to drop this first case once expand is working correctly
             (cond (= :type property) (recur rst (into result (map #(vector subject "rdf:type" (str "<" % ">")) value)))
                   (:value value) (recur rst (conj result [subject predicate (if (string? (:value value))
                                                                               (str "\"" (:value value) "\"")
                                                                               (:value value))]))
                   (:id value) (into (conj result [subject predicate (str "<" (:id value) ">")]) (->rdf value))
                   :else (recur rst (conj result [subject predicate value])))))
         (->> (sort-by first result)
              (map #(str (str/join #" " %) " ."))))))))


(str "\"" "hey" "\"")


(str (str/join #" " ["<hey>" "<you>" "\"guyes\""]) " .")
"<hey> <you> \"guyes\" ."



#_(defn <-rdf
    [rdf])
