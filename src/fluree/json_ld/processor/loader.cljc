(ns fluree.json-ld.processor.loader
  (:require #?(:clj [org.httpkit.client :as http])
            #?(:clj [jsonista.core :as json])
            #?(:clj [clojure.java.io :as io])
            [promesa.core :as promise]
            [fluree.json-ld.impl.external :as external]
            [clojure.string :as str]))

(defn json?
  [content-type]
  (or (= content-type "application/json")
      (str/ends-with? "+json")))

(def json-mime-types "application/ld+json,application/json")

(defn remote-loader
  [url {:keys [profile request-profile extract-all-scripts]}]
  (let [p (promise/deferred)]
    (future
      (let [{:keys [status headers body]} @(http/get url {:timeout 200 :headers {"Accept" json-mime-types}})]
        (if (and (= 200 status) (json? (:content-type headers)))
          (let [document (try (json/read-value body)
                              (catch Exception e
                                (promise/reject! p (ex-info "Unable to parse json document."
                                                            {:code "loading document failed"
                                                             :url url}))))]
            (promise/resolve! p {:document document
                                 :document-url url
                                 :content-type (:content-type headers)
                                 :profile nil
                                 :context-url nil}))
          (promise/reject! p (ex-info "Unable to load remote document."
                                      {:code "loading document failed"
                                       :url url})))))

    p))

(defn static-loader
  [url {:keys [profile request-profile extract-all-scripts]}]
  (let [p (promise/deferred)]
    (if (external/external-contexts url)
      (future
        (let [body (slurp (io/resource ()))
              document (try (json/read-value body)
                            (catch Exception e
                              (promise/reject! p (ex-info "Unable to parse json document."
                                                          {:code "loading document failed"
                                                           :url url}))))]
          (promise/resolve! p {:document document
                               :document-url url
                               :content-type "application/ld+json"
                               :profile nil
                               :context-url nil})))
      (promise/reject! p (ex-info "Unable to load cached json document."
                                  {:code "loading document failed"
                                   :url url})))
    p))

(comment


  (def jld (slurp (io/resource "contexts/org/schema/latest.jsonld")))
  jld


  (def doc0 @(http/get "https://ns.flur.ee/" {:timeout 200
                                              :headers {"Accept" "application/ld+json,application/json"}}))
  doc0
  (def doc1 @(http/get "http://xmlns.com/foaf/0.1/" {:timeout 200
                                                     :headers {"Accept" "application/ld+json; profile=http://www.w3.org/ns/json-ld#expanded"}}))

  (def doc2 @(http/get "https://schema.org/version/latest/schemaorg-current-https.jsonld"
                       {:timeout 200
                        :headers {"Accept" "application/ld+json,application/json"}}))

  (str/split "application/x+json" #"/")
  ["application" "x+json"]
  ["application" "ld+json"]
  ["application" "json"]


  @(let [p (promise/deferred)
         x (try (json/read-value "<html></html>")

                (catch Exception e
                  (ex-info "Failed to parse remote JSON document." {:code "loading document failed"})))]

     (println x)
     (promise/resolve! x)
     p)

  (str/ends-with? "+json")

  (keys doc)
  (:opts :body :headers :status)
  (:headers doc1)
  {:accept-ranges "bytes", :content-length "25579", :content-type "application/ld+json", :date "Wed, 18 May 2022 20:16:12 GMT", :etag "\"63eb-4fc2cb1269a12\"", :last-modified "Thu, 19 Jun 2014 09:06:27 GMT", :server "Apache/2.4.7 (Ubuntu)", :vary "Accept"}

  (:body doc1)


  ,)
