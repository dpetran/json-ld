(ns fluree.json-ld.compact
  (:require [clojure.string :as str]))

(defn- reverse-context
  "Flips context map from prefix -> prefix-map, to iri -> prefix-map"
  [context]
  (reduce-kv #(assoc %1 (:id %3) %2) {} context))

(defn compact-fn
  "Returns a single arity function based on the provided context that will compact any string iri."
  [context]
  (let [flipped    (reverse-context context)                ;; flips context map
        match-iris (->> flipped keys (sort-by #(* -1 (count %))))]
    (fn [iri]
      (or
        (some
          (fn [iri-substr]
            (when (str/starts-with? iri iri-substr)         ;; match
              (let [prefix (get flipped iri-substr)
                    suffix (subs iri (count iri-substr))]
                (cond
                  (= :vocab prefix)                         ;; special vocabulary reference (default context)
                  (subs iri (count iri-substr))

                  (= "" suffix)                             ;; exact match, no prefix needed, just substitute
                  prefix

                  :else
                  (str prefix ":" suffix)))))
          match-iris)
        iri))))


(defn compact
  "Goes through context and attempts to shorten iri if matches context, else returns original IRI.

  Uses query context format where context values have {:iri 'iri-here'}, so must already be parsed."
  [iri context]
  (let [match-fn (compact-fn context)]
    (match-fn iri)))
