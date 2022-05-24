(ns dev
  (:require [fluree.json-ld :as jld]
            [fluree.json-ld.impl.expand :as expand]
            [fluree.json-ld.impl.context :as context]
            [fluree.api :as jld2]))

;; https://www.w3.org/TR/json-ld-api/#features

(def ex1 {"@context" "https://schema.org",
          "@id" "https://www.wikidata.org/wiki/Q836821",
          "@type" ["Movie"],
          "name" "The Hitchhiker's Guide to the Galaxy",
          "disambiguatingDescription" "2005 British-American comic science fiction film directed by Garth Jennings",
          "titleEIDR" "10.5240/B752-5B47-DBBE-E5D4-5A3F-N",
          "isBasedOn" {"@id" "https://www.wikidata.org/wiki/Q3107329",
                       "@type" "Book",
                       "name" "The Hitchhiker's Guide to the Galaxy",
                       "isbn" "0-330-25864-8",
                       "author" {"@id" "https://www.wikidata.org/wiki/Q42",
                                 "@type" "Person",
                                 "name" "Douglas Adams"}}})


(def c (jld2/process-context (ex1 "@context")))

(jld2/->rdf ex1)


(comment
  (count c)
  (keys c)

  ,)

(jld2/expand ex1)

[["@id" nil] ["@type" nil] ["name" nil] ["disambiguatingDescription" nil] ["titleEIDR" nil] ["isBasedOn" [["@id" nil] ["@type" nil] ["name" nil] ["isbn" nil] ["author" [["@id" nil] ["@type" nil] ["name" nil]]]]]]

[["@id" nil] ["@type" nil] ["name" nil] ["disambiguatingDescription" nil] ["titleEIDR" nil] ["isBasedOn" [["@id" nil] ["@type" nil] ["name" nil] ["isbn" nil] ["author" [["@id" nil] ["@type" nil] ["name" nil]]]]]]

[["@id" nil] ["@type" nil] ["name" nil] ["disambiguatingDescription" nil] ["titleEIDR" nil] ["isBasedOn" [["@id" nil] ["@type" nil] ["name" nil] ["isbn" nil] ["author" [["@id" nil] ["@type" nil] ["name" nil]]]]]]


(def ex0 [{"@id" "https://www.wikidata.org/wiki/Q836821",
           "@type" ["http://schema.org/Movie"],
           "http://schema.org/disambiguatingDescription" [{"@value" "2005 British-American comic science fiction film directed by Garth Jennings"}],
           "http://schema.org/isBasedOn" [{"@id" "https://www.wikidata.org/wiki/Q3107329",
                                           "@type" ["http://schema.org/Book"],
                                           "http://schema.org/author" [{"@id" "https://www.wikidata.org/wiki/Q42",
                                                                        "@type" ["http://schema.org/Person"],
                                                                        "http://schema.org/name" [{"@value" "Douglas Adams"}]}],
                                           "http://schema.org/isbn" [{"@value" "0-330-25864-8"}],
                                           "http://schema.org/name" [{"@value" "The Hitchhiker's Guide to the Galaxy"}]}],
           "http://schema.org/name" [{"@value" "The Hitchhiker's Guide to the Galaxy"}],
           "http://schema.org/titleEIDR" [{"@value" "10.5240/B752-5B47-DBBE-E5D4-5A3F-N"}]}])

(def ex3 {"@context" {"name"     "http://xmlns.com/foaf/0.1/name",
                      "homepage" {"@id"   "http://xmlns.com/foaf/0.1/homepage",
                                  "@type" "@id"}},
          "@id"      "http://me.markus-lanthaler.com/",
          "name"     "Markus Lanthaler",
          "homepage" "http://www.markus-lanthaler.com/"})

(def ex4 {"@context" {"website" "http://xmlns.com/foaf/0.1/homepage"},
          "@id" "http://me.markus-lanthaler.com/",
          "http://xmlns.com/foaf/0.1/name" "Markus Lanthaler",
          "website" {"@id" "http://www.markus-lanthaler.com/"}})

(def ex5
  [{"@id"                                "http://me.markus-lanthaler.com/",
    "http://xmlns.com/foaf/0.1/name"     [{"@value" "Markus Lanthaler"}],
    "http://xmlns.com/foaf/0.1/homepage" [{"@id" "http://www.markus-lanthaler.com/"}]}])

[["http://me.markus-lanthaler.com/" "http://xmlns.com/foaf/0.1/name" "Markus Lanthaler"]
 ["http://me.markus-lanthaler.com/" "http://xmlns.com/foaf/0.1/homepage" "http://www.markus-lanthaler.com/"]
 ]


(context/parse (ex4 "@context"))
{:type-key "@type", "website" {:id "http://xmlns.com/foaf/0.1/homepage"}}

(jld2/expand ex3)
{:idx [],
 :id "http://me.markus-lanthaler.com/",
 "http://xmlns.com/foaf/0.1/name" {:value "Markus Lanthaler",
                                   :type nil,
                                   :idx ["name"]},
 "http://xmlns.com/foaf/0.1/homepage" {:id "http://www.markus-lanthaler.com/",
                                       :idx ["homepage"]}}
[["@id" nil]
 ["http://xmlns.com/foaf/0.1/namename" nil]
 ["{\"@id\" \"http://xmlns.com/foaf/0.1/homepage\", \"@type\" \"@id\"}homepage" nil]]

[["@context" [["name" nil]
              ["homepage" [["@id" nil] ["@type" nil]]]]]
 ["@id" nil] ["name" nil] ["homepage" nil]]
[["@context" {"name" "http://xmlns.com/foaf/0.1/name", "homepage" {"@id" "http://xmlns.com/foaf/0.1/homepage", "@type" "@id"}}]
 ["@id" "http://me.markus-lanthaler.com/"]
 ["name" "Markus Lanthaler"]
 ["homepage" "http://www.markus-lanthaler.com/"]]

[["@context" {"name" "http://xmlns.com/foaf/0.1/name", "homepage" {"@id" "http://xmlns.com/foaf/0.1/homepage", "@type" "@id"}}]
 ["@id" "http://me.markus-lanthaler.com/"]
 ["name" "Markus Lanthaler"]
 ["homepage" "http://www.markus-lanthaler.com/"]]

(keys (jld/expand ex1))


(defn ->rdf
  ;; type isn't expanded...
  ;; sorting?
  ;; graph names?
  ;; blank nodes?
  [expanded]
  (let [subject (:id expanded)
        json-ld (dissoc expanded :id)]
    (loop [[[property value] & rst] json-ld
           result []]
      (if property
        (if (= :idx property)
          (recur rst result)
          (let [subject (str "<" subject ">")
                predicate (str "<" property ">")]
            ;; should be able to drop this first case once expand is working correctly
            (cond (= :type property) (recur rst (into result
                                                      (map #(vector subject "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" (str "<" % ">"))
                                                           value)))
                  (:value value) (recur rst (conj result [subject predicate (:value value)]))
                  (:id value) (into (conj result [subject predicate (str "<" (:id value) ">")]) (->rdf value))
                  :else (recur rst (conj result [subject predicate value])))))
        (sort-by first result)))))

#_(let [[[k v] & rst] (sort (dissoc {"a" 1 "d" 3 "1" "hey" :hey :a :idx []} :hey))]
  [k v rst])





(->rdf (jld/expand ex1))
(jld2/->rdf ex1)








(["<>" "<@context>" "https://schema.org"]
 ["<>" "<@id>" "https://www.wikidata.org/wiki/Q836821"]
 ["<>" "<@type>" ["Movie"]]
 ["<>" "<name>" "The Hitchhiker's Guide to the Galaxy"]
 ["<>" "<disambiguatingDescription>" "2005 British-American comic science fiction film directed by Garth Jennings"]
 ["<>" "<titleEIDR>" "10.5240/B752-5B47-DBBE-E5D4-5A3F-N"]
 ["<>" "<isBasedOn>" {"@id" "https://www.wikidata.org/wiki/Q3107329", "@type" "Book", "name" "The Hitchhiker's Guide to the Galaxy", "isbn" "0-330-25864-8", "author" {"@id" "https://www.wikidata.org/wiki/Q42", "@type" "Person", "name" "Douglas Adams"}}])
[["<https://www.wikidata.org/wiki/Q836821>" "rdf:type" "<http://schema.org/Movie>"]
 ["<https://www.wikidata.org/wiki/Q836821>" "<http://schema.org/name>" "The Hitchhiker's Guide to the Galaxy"]
 ["<https://www.wikidata.org/wiki/Q836821>" "<http://schema.org/disambiguatingDescription>" "2005 British-American comic science fiction film directed by Garth Jennings"]
 ["<https://www.wikidata.org/wiki/Q836821>" "<http://schema.org/titleEIDR>" "10.5240/B752-5B47-DBBE-E5D4-5A3F-N"]
 ["<https://www.wikidata.org/wiki/Q836821>" "<http://schema.org/isBasedOn>" "<https://www.wikidata.org/wiki/Q3107329>"]
 ["<https://www.wikidata.org/wiki/Q3107329>" "rdf:type" "<http://schema.org/Book>"]
 ["<https://www.wikidata.org/wiki/Q3107329>" "<http://schema.org/name>" "The Hitchhiker's Guide to the Galaxy"]
 ["<https://www.wikidata.org/wiki/Q3107329>" "<http://schema.org/isbn>" "0-330-25864-8"]
 ["<https://www.wikidata.org/wiki/Q3107329>" "<http://schema.org/author>" "<https://www.wikidata.org/wiki/Q42>"]
 ["<https://www.wikidata.org/wiki/Q42>" "rdf:type" "<http://schema.org/Person>"]
 ["<https://www.wikidata.org/wiki/Q42>" "<http://schema.org/name>" "Douglas Adams"]]



{:idx [],
 :id "https://www.wikidata.org/wiki/Q836821",
 :type ["http://schema.org/Movie"],
 "http://schema.org/name" {:value "The Hitchhiker's Guide to the Galaxy",
                           :type nil,
                           :idx ["name"]},
 "http://schema.org/disambiguatingDescription" {:value "2005 British-American comic science fiction film directed by Garth Jennings",
                                                :type nil,
                                                :idx ["disambiguatingDescription"]},
 "http://schema.org/titleEIDR" {:value "10.5240/B752-5B47-DBBE-E5D4-5A3F-N",
                                :type nil,
                                :idx ["titleEIDR"]},
 "http://schema.org/isBasedOn" {:idx ["isBasedOn"],
                                :id "https://www.wikidata.org/wiki/Q3107329",
                                :type ["http://schema.org/Book"],
                                "http://schema.org/name" {:value "The Hitchhiker's Guide to the Galaxy",
                                                          :type nil,
                                                          :idx ["isBasedOn" "name"]},
                                "http://schema.org/isbn" {:value "0-330-25864-8",
                                                          :type nil,
                                                          :idx ["isBasedOn" "isbn"]},
                                "http://schema.org/author" {:idx ["isBasedOn" "author"],
                                                            :id "https://www.wikidata.org/wiki/Q42",
                                                            :type ["http://schema.org/Person"],
                                                            "http://schema.org/name" {:value "Douglas Adams",
                                                                                      :type nil,
                                                                                      :idx ["isBasedOn" "author" "name"]}}}}

[(jld/expand ex3)
 (jld/expand ex4)]
[{:idx [],
  :id "http://me.markus-lanthaler.com/",
  "http://xmlns.com/foaf/0.1/name" {:value "Markus Lanthaler",
                                    :type nil,
                                    :idx ["name"]},
  "http://xmlns.com/foaf/0.1/homepage" {:id "http://www.markus-lanthaler.com/",
                                        :idx ["homepage"]}}
 {:idx [],
  :id "http://me.markus-lanthaler.com/",
  "http://xmlns.com/foaf/0.1/name" {:value "Markus Lanthaler",
                                    :type nil,
                                    :idx ["http://xmlns.com/foaf/0.1/name"]},
  "http://xmlns.com/foaf/0.1/homepage" {:idx ["website"],
                                        :id "http://www.markus-lanthaler.com/"}}]
