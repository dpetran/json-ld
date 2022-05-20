(ns fluree.json-ld.processer.api
  "Implements a JSON-LD Processor in conformance to https://www.w3.org/TR/json-ld-api."
  (:refer-clojure :exclude [flatten])
  (:require [fluree.api-impl :as impl]
            [malli.core :as m]
            [promesa.core :as promise]
            [fluree.json-ld.processor.expand :as expand]))

(def JsonLdRecord :map)
(def IRI "A string representing an IRI, which can be dereferenced to retrieve a valid remote JSON document." :string)
(def BlankNodeIdentifier :string)
(def RemoteDocument
  [:map
   [::content-type {:doc "The Content-Type of the loaded document, exclusive of any optional parameters."} :string]
   [::context-url {:doc "If available, the value of the HTTP Link Header."} :string]
   [::document {:doc "The retrieved document. This can either be the raw payload or the already parsed document."} :map]
   [::document-url {:doc "The final URL of the loaded document. This is important to handle HTTP redirects properly."} :string]
   [::profile {:doc "The value of any profile parameter retrieved as part of the original contentType."} :string]])

(def JsonLdInput [:or JsonLdRecord [:sequential JsonLdRecord] IRI RemoteDocument])

(def JsonLdContext
  [:or JsonLdRecord [:sequential JsonLdRecord] IRI])

(def JsonLdOptions
  [:map
   [::base {:default nil} :string]
   [::compact-arrays {:default true} :boolean]
   [::compact-to-relative {:default true} :boolean]
   [::document-loader {:default nil} :fn]
   [::expand-context {:default nil} [:or :map :string]]
   [::extract-all-scripts {:default false} :boolean ]
   [::frame-expansion {:default false} :boolean]
   [::ordered {:default false} :boolean]
   [::processing-mode {:default "json-ld-1.1."} :string]
   [::produce-generalized-rdf {:default true} :boolean]
   [::rdf-direction {:default nil} :string]
   [::use-native-types {:default false} :boolean]
   [::use-rdf-type {:default false} :boolean]])

(def RdfLiteral
  [:or
   [:map
    [::value {:doc "The lexical value of the literal."} :string]
    [::datatype {:doc "An absolute IRI denoting the datatype IRI of the literal."} :string]]
   [:map
    [::value {:doc "The lexical value of the literal."} :string]
    [::datatype {:doc "An absolute IRI denoting the datatype IRI of the literal."} [:enum "rdf:langString"]]
    [::language {:doc "Language tag."} :string]]])
(def RdfTriple
  [:catn [::subject :string] [::predicate :string] [::object [:or IRI RdfLiteral BlankNodeIdentifier]]])
(def RdfDataset
  [:sequential RdfTriple])

(def DocumentLoader
  [:=> [:cat [:url :string] [:options [::profile]]] RemoteDocument])

(def DocumentLoaderOptions
  [:map
   [:profile {:default nil} :string]
   [:request-profile {:default nil}
    [:or :string [:sequential :string]]]
   [:extract-all-scripts {:default false} :boolean]])

(defn load-document
  {:malli/schema [:=> [:catn [:url IRI] [:? [:options DocumentLoaderOptions]]]
                  RemoteDocument]}
  [url options])


(defn expand
  "Remove context from an internal json-ld representation, ensuring all values are
  represented in regular form.

  Options:
  - base: base IRI to use.
  - expand-context: a context to expand with.
  - free-floating-nodes?: true to keep free-floating nodes, false not to, default false.
  "
  {:malli/schema [:=> [:cat JsonLdInput [:? [:map [:options {:optional true} JsonLdOptions]]]]
                  [:sequential JsonLdRecord]]}
  [input & {:keys [options]}]
  (expand/expand))

(defn compact
  "Apply context to an internal json-ld representation, to simplify expression for
  application or human use."
  {:malli/schema [:=> [:cat JsonLdInput [:? [:map
                                             [:context {:optional true} JsonLdContext]
                                             [:options {:optional true} JsonLdOptions]]]]
                  JsonLdRecord]}
  ([input & {:keys [context options]}]))

(defn flatten
  {:malli/schema [:=> [:cat JsonLdInput [:? [:map
                                             [:context {:optional true} JsonLdContext]
                                             [:options {:optional true} JsonLdOptions]]]]
                  JsonLdRecord]}
  [context json-ld & options])

(defn ->rdf
  "Translate the given internal json-ld representation into a set of RDF quads."
  {:malli/schema [:=> [:cat JsonLdInput [:? [:map [:options {:optional true} JsonLdOptions]]]]
                  RdfDataset]}
  [input & {:keys [options]}]
  #_ (impl/->rdf json-ld))

(defn <-rdf
  {:malli/schema [:=> [:cat RdfDataset [:? [:map [:options {:optional true} JsonLdOptions]]]]
                  JsonLdRecord]}
  [dataset & {:keys [options]}])

(defn normalize
  {:malli/schema [:=> [:cat [:orn
                             [:dataset [:set :string]]
                             [:json-ld :map]] [:? :map]]
                  [:set :string]]}
  [json-ld])
