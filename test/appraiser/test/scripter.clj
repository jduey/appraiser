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
         (is (run-script app
                         (script
                           (click "/wrong")
                           (response-code? 404)))))

(def hit-counter
  (script
    (click "/")
    (response-code? 200)))

(deftest test-counter
         (let [test-script (script
                             hit-counter
                             (body-contains? "counter: 1")

                             hit-counter
                             (body-contains? "counter: 2")

                             hit-counter
                             (body-contains? "counter: 3"))]
         (is (run-script app test-script))))

(deftest test-failure
         (let [test-script (script
                             hit-counter
                             (body-contains? "counter: 1")

                             hit-counter
                             (body-contains? "counter: 5")

                             hit-counter
                             (body-contains? "counter: 3"))]
         (reset! counter 0) 
         (is (not
               (run-script app test-script)))))

