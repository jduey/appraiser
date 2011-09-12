(ns appraiser.test.scripter
  (:use clojure.test
        compojure.core
        appraiser.scripter)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]))

(def counter (atom 0))

(defn home []
  (swap! counter + 1)
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body (str "counter: " @counter)})

(defroutes main-routes
           (GET "/" [] 
                (home))
           (route/not-found "Page not found"))

(def app
  (handler/site main-routes))

(deftest test-not-found-route
         (script app
                 [_ (click "/wrong")
                  _ (response-code? 404)]
                 true))

(def hit-counter
  (as-script
    [_ (click "/")
     _ (response-code? 200)]
    nil))

(deftest test-counter
         (script app
                 [_ hit-counter
                  _ (body-contains? "counter: 1")

                  _ hit-counter
                  _ (body-contains? "counter: 2")

                  _ hit-counter
                  _ (body-contains? "counter: 3")]
                 true))

