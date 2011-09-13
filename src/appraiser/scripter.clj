(ns appraiser.scripter
  (:use [clojure.test])
  (:require [clojure.contrib.monads :as m]))

(def script-m (m/state-t m/maybe-m))

(defmacro as-script
  "A shorthand for 'domonad script-m'"
  [& body]
  `(clojure.contrib.monads/domonad appraiser.scripter/script-m ~@body))

(defn set-val
  "Set a key 'k' in the state to a value 'v'"
  [k v]
  (fn [state]
    (let [new-state (assoc state k v)]
      [new-state new-state])))

(defn get-val
  "Retrieve the value associated with key 'k' in the state"
  [k]
  (fn [state]
    [(get state k) state]))

(defn body-contains?
  "Tests whether the body of the most recent response contains the
   string 'strn'. Uses 'is' from clojure.test."
  [strn]
  (as-script
    [result (get-val :response)]
    (is (.contains (:body result) strn)
        (format "Expected body to contain \"%s\"" strn))))

(defn response-code?
  "Tests whether the most recent response code is 'code' or not.
   Uses 'is' from clojure.test."
  [code]
  (as-script
    [response (get-val :response)]
    (is (= code (:status response))
        (format "Unexpected response code. Expected: %s" code))))

(defn- get-handler [state]
  (let [handler (:handler state)]
    (when-not handler
      (throw (Exception. "could not get handler")))
    [handler state]))

(defn- response-cookies [state]
  (let [cookies (->> state
                  :response
                  :headers
                  (#(get % "Set-Cookie"))
                  (map #(let [[k v] (.split % "=")]
                          [k {:value v}]))
                  (into {}))]
    [cookies state]))

(def set-cookies
  (as-script
    [cookies response-cookies
     _ (set-val :cookies cookies)]
    cookies))

(defn click
  "Simulate the clicking of a link on a webpage. An optional
  query string may be provided."
  [url & [query-string]]
  (as-script
    [handler get-handler
     cookies (get-val :cookies)
     :let [result (handler {:request-method :get
                            :uri url
                            :cookies cookies
                            :query-string query-string})]
     _ (set-val :response result)
     _ set-cookies]
    result))

(defn submit
  "Simulate the submission of a form. 'method' is one of the
  HTTP methods. 'field-vals' is a hash-map whose keys are the
  field id's of the form and whose values are the corresponding
  values."
  [method url field-vals]
  (let [method (keyword (.toLowerCase (name method)))]
    (as-script
      [handler get-handler
       cookies (get-val :cookies)
       :let [result (handler {:request-method method
                              :uri url
                              :cookies cookies
                              :query-string (->> field-vals
                                              (map #(apply format "%s=%s" %))
                                              (interpose "&")
                                              (apply str))})]
       _ (set-val :response result)
       _ set-cookies]
      result)))

(defmacro script
  "Define a script to be run to test a particular Ring handler."
  [handler & steps]
   `((clojure.contrib.monads/domonad
       appraiser.scripter/script-m 
       ~@steps
       true)
       {:handler ~handler}))
