(ns fluree.json-ld.processer-test
  (:require  [clojure.test :as t]
             [fluree.json-ld.processer.api :as jld2]
             #_[fluree.json-ld.processor.api :as jld2]
             [clojure.java.io :as io]
             [jsonista.core :as json]
             [clojure.string :as str]
             [fluree.json-ld :as jld]))


(defn read-file
  [path]
  (slurp (io/resource path)))

(comment
  (def m (.readFileSync fs "/home/dan/projects/json-ld/test-resources/manifest.jsonld" "utf8"))

  (println fs)

  (println readFileSync)

  (println fs/readFileSync)

  ,)

(def tests
  "Loads all the test data into a map of suite-name->manifest."
  (-> (json/read-value (read-file "manifest.jsonld"))
      (get "sequence")
      (->> (map (comp json/read-value read-file))
           (map (fn [suite-manifest]
                  (let [test-defs (get suite-manifest "sequence")
                        test-ids (map #(get % "@id") test-defs)]
                    (assoc suite-manifest
                           :test-ids test-ids
                           :test-id->test-def (reduce (fn [m test-def]
                                                        (assoc m (get test-def "@id") test-def))
                                                      {}
                                                      test-defs)))))
           (reduce (fn [m manifest] (assoc m (get manifest "name") manifest)) {}))))

(defn get-test-def
  [suite test-id]
  (-> (get tests suite)
      :test-id->test-def
      (get test-id)))

(def test-types
  #{"jld:FlattenTest" "jld:ToRDFTest" "jld:ExpandTest" "jld:CompactTest" "jld:FromRDFTest"})

(defmulti run-test (fn [_suite test-def] (let [type (some test-types (get test-def "@type"))]
                                           #_(println "dispatching" (pr-str type))
                                           type)))
(defmethod run-test :default
  [suite test-def]
  (println "Skipping default" (get test-def "@id") "@type" (get test-def "@type")))

(defmethod run-test "jld:ExpandTest"
  [suite test-def]
  (let [{test-name "name" id "@id" purpose "purpose" input-path "input" expect-path "expect" type "@type"}
        test-def
        input (when input-path (json/read-value (read-file input-path)))
        expect (when expect-path (str/split-lines (read-file expect-path)))]
    (cond
      (some #{"jld:NegativeEvaluationTest"} type)
      (println "Skipping" id "@type" (pr-str type))

      (some #{"jld:PositiveEvaluationTest"} type)
      (t/testing (str test-name ":")
        (t/testing (str "\"" id "\"")
          (println "Testing" test-name id (pr-str input))
          (t/is (= expect
                   (jld2/expand input))
                purpose)))))

  (println "Skipping" (get test-def "@id") "@type" (get test-def "@type")))
(defmethod run-test "jld:CompactTest"
  [suite test-def]
  (println "Skipping" (get test-def "@id") "@type" (get test-def "@type")))
(defmethod run-test "jld:FlattenTest"
  [suite test-def]
  (println "Skipping" (get test-def "@id") "@type" (get test-def "@type")))
(defmethod run-test "jld:ToRDFTest"
  [suite test-def]
  (let [{test-name "name" id "@id" purpose "purpose" input-path "input" expect-path "expect" type "@type"}
        test-def
        input (when input-path (json/read-value (read-file input-path)))
        expect (when expect-path (str/split-lines (read-file expect-path)))]
    (cond
      (some #{"jld:NegativeEvaluationTest"} type)
      (println "Skipping" id "@type" (pr-str type))

      (some #{"jld:PositiveEvaluationTest"} type)
      (t/testing (str test-name ":")
        (t/testing (str "\"" id "\"")
          (println "Testing" test-name id (pr-str input))
          (t/is (= expect
                   (jld2/->rdf input))
                purpose))))))
(defmethod run-test "jld:FromRDFTest"
  [suite test-def]
  (println "Skipping" (get test-def "@id") "@type" (get test-def "@type")))


(t/deftest big-processer-test
  (let [suites (keys tests)]
    (doseq [suite suites :when (#{#_"Transform JSON-LD to RDF" "Expansion"} suite)]
      (let [test-defs (get-in tests [suite "sequence"])]
        (println "Suite" suite ":" (count test-defs))
        (t/testing (str "\"" suite "\"" ":")
          (doseq [test-def (take 1 test-defs)]
            (run-test suite test-def)))))))

(comment
  (def test-def (get-test-def "Transform JSON-LD to RDF" "#te071"))

  test-def

  {"@id" "#te071", "@type" ["jld:PositiveEvaluationTest" "jld:ToRDFTest"], "name" "Redefine terms looking like compact IRIs", "purpose" "RDF version of expand-0071", "input" "toRdf/e071-in.jsonld", "expect" "toRdf/e071-out.nq", "option" {"specVersion" "json-ld-1.0"}}


  [{"@id" "#te071", "@type" ["jld:PositiveEvaluationTest" "jld:ToRDFTest"], "name" "Redefine terms looking like compact IRIs", "purpose" "RDF version of expand-0071", "input" "toRdf/e071-in.jsonld", "expect" "toRdf/e071-out.nq", "option" {"specVersion" "json-ld-1.0"}}
   {"@id" "#ter44", "@type" ["jld:NegativeEvaluationTest" "jld:ToRDFTest"], "name" "Redefine terms looking like compact IRIs", "purpose" "Term definitions may look like compact IRIs, but must be consistent.", "input" "toRdf/er44-in.jsonld", "expectErrorCode" "invalid IRI mapping", "option" {"specVersion" "json-ld-1.1"}}]


  (run-test "Transform JSON-LD to RDF" test-def)
  [{"@context" [{"v" "http://example.com/vocab#", "v:term" "v:somethingElse", "v:termId" {"@id" "v:somethingElseId"}} {"v:term" "v:term", "v:termId" {"@id" "v:termId"}}], "v:term" "value of v:term", "v:termId" "value of v:termId"}
   ["_:b0 <http://example.com/vocab#term> \"value of v:term\" ." "_:b0 <http://example.com/vocab#termId> \"value of v:termId\" ."]]
  nil
  [{"@context" [{"v" "http://example.com/vocab#", "v:term" "v:somethingElse", "v:termId" {"@id" "v:somethingElseId"}} {"v:term" "v:term", "v:termId" {"@id" "v:termId"}}], "v:term" "value of v:term", "v:termId" "value of v:termId"}
   ["_:b0 <http://example.com/vocab#term> \"value of v:term\" ."
    "_:b0 <http://example.com/vocab#termId> \"value of v:termId\" ."]]

  (jld2/->rdf {"@context" [{"v" "http://example.com/vocab#", "v:term" "v:somethingElse", "v:termId" {"@id" "v:somethingElseId"}} {"v:term" "v:term", "v:termId" {"@id" "v:termId"}}], "v:term" "value of v:term", "v:termId" "value of v:termId"})


  nil


  ("Compaction" "Expansion" "Flattening" "Transform RDF to JSON-LD" "Remote document" "Transform JSON-LD to RDF" "HTML")
  (get #{"expand-manifest.jsonld" "html-manifest.jsonld" "fromRdf-manifest.jsonld" "compact-manifest.jsonld" "remote-doc-manifest.jsonld" "toRdf-manifest.jsonld" "flatten-manifest.jsonld"} "")



  )




(comment

  (def files (json/parse-string (slurp (io/resource "manifest.jsonld"))))

  files



  (def manifests (->> files
                      (map #(json/parse-string (read-file %)))
                      #_(group-by #(get % "name"))))
  (map #(get % "@id") manifests)


  ("Compaction" "Expansion" "Flattening" "Transform RDF to JSON-LD" "Remote document" "Transform JSON-LD to RDF" "HTML")

  ("Compaction" "Expansion" "Flattening" "Transform RDF to JSON-LD" "Remote document" "Transform JSON-LD to RDF" "HTML")

  (-> (get tests "Transform JSON-LD to RDF")
      (first)
      (get "sequence"))

  (first manifests)

  {"@context" ["context.jsonld" {"@base" "manifest"}],
   "@id" "",
   "@type" "mf:Manifest",
   "name" "JSON-LD Test Suite",
   "description"
   "This manifest loads additional manifests for specific behavior tests for [JSON-LD 1.1 API](https://www.w3.org/TR/json-ld11-api/)",
   "sequence"
   ["compact-manifest.jsonld"
    "expand-manifest.jsonld"
    "flatten-manifest.jsonld"
    "fromRdf-manifest.jsonld"
    "remote-doc-manifest.jsonld"
    "toRdf-manifest.jsonld"
    "html-manifest.jsonld"]}


  (json/parse-string (slurp (io/resource "toRdf-manifest.jsonld")))

  (json/parse-string (slurp (io/resource "toRdf/0001-in.jsonld")))
  {"@id" "http://greggkellogg.net/foaf#me", "http://xmlns.com/foaf/0.1/name" "Gregg Kellogg"}
  "{\n  \"@id\": \"http://greggkellogg.net/foaf#me\",\n  \"http://xmlns.com/foaf/0.1/name\": \"Gregg Kellogg\"\n}"

  (map #(str/split % #" ")) (str/split-lines (slurp (io/resource "toRdf/0001-out.nq")))
  ["<http://greggkellogg.net/foaf#me> <http://xmlns.com/foaf/0.1/name> \"Gregg Kellogg\" ."]
  "<http://greggkellogg.net/foaf#me> <http://xmlns.com/foaf/0.1/name> \"Gregg Kellogg\" .\n"

  (read-file "manifest.jsonld")

  (->> (reduce (fn [types [_ suite]] (into types (map #(get % "@type") (-> suite (get "sequence")))))
               #{}
               tests)
       (group-by second))

  ("jld:FlattenTest" "jld:ToRDFTest" "jld:ExpandTest" "jld:CompactTest" "jld:FromRDFTest")
  (some #{"jld:CompactTest"} ["jld:NegativeEvaluationTest" "jld:CompactTest"])
  "jld:CompactTest"

  (["jld:PositiveEvaluationTest" "jld:CompactTest" "jld:HtmlTest"]
   ["jld:PositiveEvaluationTest" "jld:CompactTest"]
   ["jld:NegativeEvaluationTest" "jld:CompactTest"]
   ["jld:PositiveEvaluationTest" "jld:ExpandTest" "jld:HtmlTest"]
   ["jld:NegativeEvaluationTest" "jld:ExpandTest" "jld:HtmlTest"]
   ["jld:NegativeEvaluationTest" "jld:ExpandTest"]
   ["jld:PositiveEvaluationTest" "jld:ExpandTest"]
   ["jld:NegativeEvaluationTest" "jld:FlattenTest"]
   ["jld:PositiveEvaluationTest" "jld:FlattenTest" "jld:HtmlTest"]
   ["jld:PositiveEvaluationTest" "jld:FlattenTest"]
   ["jld:PositiveEvaluationTest" "jld:FromRDFTest"]
   ["jld:NegativeEvaluationTest" "jld:FromRDFTest"]
   ["jld:NegativeEvaluationTest" "jld:ToRDFTest" "jld:HtmlTest"]
   ["jld:PositiveEvaluationTest" "jld:ToRDFTest"]
   ["jld:PositiveEvaluationTest" "jld:ToRDFTest" "jld:HtmlTest"]
   ["jld:PositiveSyntaxTest" "jld:ToRDFTest"]
   ["jld:NegativeEvaluationTest" "jld:ToRDFTest"]))
