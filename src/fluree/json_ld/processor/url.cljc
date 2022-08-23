(ns fluree.json-ld.processor.url
  (:require [lambdaisland.regal :as reg]
            [clojure.string :as str]))

;; TODO: compreshensive uri regex http://jmrware.com/articles/2009/uri_regexp/URI_regex.html
(defn absolute?
  "Returns true if given v is an absolute IRI or blank node IRI, false if not.

  Note: just a weak check, only checks for a correct starting scheme."
  [v]
  (boolean
    (and (string? v)
         (re-matches (reg/regex
                       [:cat
                        :start
                        [:capture
                         [:alt
                          [:class ["A" "Z"] ["a" "z"]]
                          [:* [:class ["A" "Z"] ["a" "z"] [\0 \9] ["+" "."]]]
                          "_"]]
                        ":"
                        [:* [:not :whitespace]]
                        :end])
                     v))))

(defn relative?
  [v]
  (and (string? v)
       (not (absolute? v))))

(def parsers
  {:simple {:keys [:href :scheme :authority :path :query :fragment]
            :regex (reg/regex [:cat
                               :start
                               [:? [:cat [:capture [:+ [:not ":" "/" "?" "#"]]] ":"]]
                               [:? [:cat "/" "/" [:capture [:* [:not "/" "?" "#"]]]]]
                               [:capture [:* [:not "?" "#"]]]
                               [:? [:cat "?" [:capture [:* [:not "#"]]]]]
                               [:? [:cat "#" [:capture [:* :any]]]]])}
   :full {:keys [:href :protocol :scheme :authority :auth :user :password :hostname :port
                 :path :directory :file :query :fragment]
          :regex (reg/regex [:cat
                             :start
                             [:? [:capture [:capture [:+ [:not ":" "/" "?" "#"]]] ":"]]
                             [:? [:cat "/" "/"
                                  [:capture
                                   [:? [:cat
                                        [:? [:capture
                                             [:capture [:* [:not ":" "@"]]]
                                             [:? [:cat ":" [:capture [:* [:not ":" "@"]]]]]]]
                                        "@"]]
                                   [:capture [:* [:not ":" "/" "?" "#"]]]
                                   [:? [:cat ":" [:capture [:* :digit]]]]]]]
                             [:cat
                              [:capture
                               [:capture [:* [:cat [:* [:not "?" "#" "/"]] "/"]]]
                               [:capture [:* [:not "?" "#"]]]]
                              [:? [:cat "?" [:capture [:* [:not "#"]]]]]
                              [:? [:cat "#" [:capture [:* :any]]]]]])}})

(defn remove-dot-segments
  "Remove dot segments from a URL path."
  [path]
  (if (zero? (count path))
    ""
    (let [output (reduce (fn [output segment]
                           (cond (= "." segment) output
                                 (= ".." segment) (if (pos? (count output))
                                                    (pop output)
                                                    output)
                                 (some? segment) (conj output segment)))
                         []
                         (str/split path #"/"))
          ;; ensure output has trailing "/" when ending with a dot
          output (if (or (str/ends-with? path ".")
                         (not (str/blank? (last output))))
                   (conj output "")
                   output)
          ;; ensure output has leading "/" when path is absolute
          output (if (and (= (first path) \/)
                          (pos? (count output))
                          (not= (first output) ""))
                   (into [""] output)
                   output)]
      (str/join "/" output))))

(defn parse
  "Parse the given uri string into a map of its components.

  :simple parser - [:href :scheme :authority :path :query :fragment]
  :full parser - [:href :protocol :scheme :authority :auth :user :password :hostname :port
                 :path :directory :file :query :fragment]

  "
  ([s]
   (parse s :full))
  ([s parser-type]
   (let [parser (get parsers parser-type)
         matches (re-matches (:regex parser) s)
         {:keys [port scheme href authority path] :as parsed}
         (->> matches
              (map (partial vector) (-> parsers parser-type :keys))
              (into {}))]
     (cond-> (assoc parsed :normalized-path (remove-dot-segments path))
       (or (and (= "https" scheme) (= 443 port))
           (and (= "http" scheme) (= 80 port)))
       ;; remove default ports
       (-> (assoc :href (str/replace href (str ":" port) "")
                  :authority (str/replace authority (str ":" port) ""))
           (dissoc :port))))))

(defn prepend-base
  "Prepends a base IRI to the given relative IRI."
  [base iri]
  (cond (nil? base) iri
        (absolute? iri) iri
        :else
        (let [base (when (string? base) (parse base))
              rel (parse iri)

              transform (cond-> {:protocol (:protocol base "")
                                 :authority (or (:authority rel) (:authority base))
                                 :path (remove-dot-segments
                                         (cond (str/blank? (:path rel))           (:path base)
                                               (str/starts-with? (:path rel) "/") (:path rel)
                                               :else
                                               (str (:directory base) (:path rel))))
                                 :query (or (:query rel) (:query base))}
                          (:authority rel) (assoc :authority (:authority rel)
                                                  :path (:path rel)
                                                  :query (:query rel)))
              rval (str (:protocol transform)
                        (when (:authority transform)
                          (str "//" (:authority transform)))
                        (:path transform)
                        (when (:query transform)
                          (str "?" (:query transform)))
                        (when (:fragment rel)
                          (str "#" (:fragment rel))))]

          (if (str/blank? rval)
            "./"
            rval))))

(defn remove-base
  [base iri]
  (if (nil? base) iri
      (let [base (parse (or base ""))
            root (str (cond (not (str/blank? (:href base)))
                            (str (:protocol base) "//" (:authority base))
                            (not (str/starts-with? iri "//"))
                            "//"))]
        (if-not (str/starts-with? iri root)
          iri                           ; IRI not relative to base
          (let [rel (parse (subs iri (count root)))

                [base-segments iri-segments]
                (loop [base-segments (str/split (:normalized-path base) #"/" -1)
                       iri-segments (str/split (:normalized-path rel) #"/" -1)]
                  (if (and (pos? (count base-segments))
                           (> (count iri-segments) (if (or (:fragment rel) (:query rel))
                                                     0
                                                     1))
                           (= (first base-segments) (first iri-segments)))
                    (recur (rest base-segments) (rest iri-segments))
                    [base-segments iri-segments]))

                rval (str (reduce (fn [rval _] (str rval  "../")) "" (butlast base-segments))
                          (str/join "/" iri-segments)
                          (when (:query rel)
                            (str "?" (:query rel)))
                          (when (:fragment rel)
                            (str "#" (:fragment rel))))]
            (if (str/blank? rval)
              "./"
              rval))))))

(comment

  (remove-base "http://schema.org/ideas/" "http://schema.org/ideas/my/idea")
  "my/idea"
  (prepend-base "http://schema.org/ideas/" "my/idea")
  "http://schema.org/ideas/my/idea"

  (remove-base "http://schema.org/animals/frogs" "http://schema.org/ideas/my/idea")
  "../../ideas/my/idea/"
  (prepend-base "http://schema.org/animals/frogs" "../../ideas/my/idea")
  "http://schema.org/ideas/my/idea/"

  (remove-base "http://schema.org/" "http://schema.org/ideas/my/idea?me=cool#yeah")
  "ideas/my/idea?me=cool#yeah"
  (prepend-base "http://schema.org/" "ideas/my/idea?me=cool#yeah")
  "http://schema.org/ideas/my/idea?me=cool#yeah"


  ,)
