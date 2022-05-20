(ns fluree.json-ld.processor.schema)

(def JsonLdRecord :map)
(def IRI "A string representing an IRI, which can be dereferenced to retrieve a valid remote JSON document." :string)
(def BlankNodeIdentifier :string)
(def RemoteDocument
  [:map
   [:remote-doc/content-type {:doc "The Content-Type of the loaded document, exclusive of any optional parameters."} :string]
   [:remote-doc/context-url {:doc "If available, the value of the HTTP Link Header."} :string]
   [:remote-doc/document {:doc "The retrieved document. This can either be the raw payload or the already parsed document."} :map]
   [:remote-doc/document-url {:doc "The final URL of the loaded document. This is important to handle HTTP redirects properly."} :string]
   [:remote-doc/profile {:doc "The value of any profile parameter retrieved as part of the original contentType."} :string]])

(def JsonLdInput [:or JsonLdRecord [:sequential JsonLdRecord] IRI RemoteDocument])

(def JsonLdContext
  [:or JsonLdRecord [:sequential JsonLdRecord] IRI])

(def JsonLdOptions
  [:map
   [:json-ld/base {:default nil} [:maybe :string]]
   [:json-ld/compact-arrays? {:default true} :boolean]
   [:json-ld/compact-to-relative? {:default true} :boolean]
   [:json-ld/document-loader {:default nil} :fn]
   [:json-ld/expand-context {:default nil} [:or JsonLdRecord :string]]
   [:json-ld.loader/extract-all-scripts? {:default false} :boolean ]
   [:json-ld/frame-expansion? {:default false} :boolean]
   [:json-ld/ordered? {:default false} :boolean]
   [:json-ld/processing-mode {:default "json-ld-1.1."} :string]
   [:json-ld/produce-generalized-rdf? {:default true} :boolean]
   [:json-ld/rdf-direction {:default nil} :string]
   [:json-ld/use-native-types? {:default false} :boolean]
   [:json-ld/use-rdf-type {:default false} :boolean]])

(def RdfLiteral
  [:or
   [:map
    [:rdf.lit/value {:doc "The lexical value of the literal."} :string]
    [:rdf.lit/datatype {:doc "An absolute IRI denoting the datatype IRI of the literal."} :string]]
   [:map
    [:rdf.lit/value {:doc "The lexical value of the literal."} :string]
    [:rdf.lit/datatype {:doc "An absolute IRI denoting the datatype IRI of the literal."} [:enum "rdf:langString"]]
    [:rdf.lit/language {:doc "Language tag."} :string]]])
(def RdfTriple
  [:catn [:rdf/subject :string] [:rdf/predicate :string] [:rdf/object [:or IRI RdfLiteral BlankNodeIdentifier]]])
(def RdfGraph
  [:sequential RdfTriple])

(def DocumentLoader
  [:=> [:cat [:url :string] [:options [::profile]]] RemoteDocument])

(def DocumentLoaderOptions
  [:map
   [:loader/profile {:default nil} :string]
   [:loader/request-profile {:default nil}
    [:or :string [:sequential :string]]]
   [:loader/extract-all-scripts? {:default false} :boolean]])

(def TermDefinition
  [:map
   [:term/iri-mapping IRI]
   [:term/prefix? :boolean]
   [:term/protected? :boolean]
   [:term/reverse-property? :boolean]
   [:term/base-url {:optional true} IRI]
   [:term/context {:optional true} IRI]
   [:term/container-mapping {:optional true} [:sequential :string]]
   [:term/direction-mapping {:optional true} [:enum "ltr" "rtl"]]
   [:term/index-mapping {:optional true} :string]
   [:term/language-mapping {:optional true} :string]
   [:term/nest-value {:optional true} :string]
   [:term/type-mapping {:optional true} IRI]])

(def ActiveContext
  [:map
   [:context/term-definitions [:sequential TermDefinition]]
   [:context/base-iri IRI]
   [:context/original-base-url IRI]
   [:context/inverse-context [:maybe :map]]
   [:context/vocabulary-mapping {:optional true} :map]
   [:context/default-language {:optional true} :string]
   [:context/default-base-direction {:optional true} [:enum "ltr" "rtl"]]
   [:context/previous-context {:optional true} :map]
   ])
