(ns fluree.api
  "Implements a JSON-LD Processor in conformance to https://www.w3.org/TR/json-ld-api."
  (:refer-clojure :exclude [flatten])
  (:require [fluree.api-impl :as impl]))

(defn internalize
  "Takes a JSON-LD serialization and returns the internal representation (clojure
  datastructures), suitable for further processing."
  [json-ld-string])

(defn concretize
  "Takes an internal JSON-LD representation and returns a JSON-LD serialization."
  [json-ld])

(defn expand
  "Remove context from an internal json-ld representation, ensuring all values are
  represented in regular form.

  Options:
  - base: base IRI to use.
  - expand-context: a context to expand with.
  - free-floating-nodes?: true to keep free-floating nodes, false not to, default false.
  "
  ([json-ld]
   (impl/expand json-ld))
  ([json-ld options]
   (impl/expand json-ld options)))

(defn compact
  "Apply context to an internal json-ld representation, to simplify expression for
  application or human use."
  [context json-ld])

(defn flatten
  ([json-ld])
  ([context json-ld]))

(defn ->rdf
  "Translate the given internal json-ld representation into a set of RDF quads."
  [json-ld]
  (impl/->rdf json-ld))

(defn <-rdf
  [rdf])

(defn normalize
  [json-ld])

(defn process-context
  [context]
  (impl/process-context context))
