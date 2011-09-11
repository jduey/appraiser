(ns appraiser.scripter
  (:use [clojure.test])
  (:require [clojure.contrib.monads :as m]))

(def script-m (m/state-t m/maybe-m))

(defmacro as-script [& body]
  `(m/domonad appraiser.scripter/script-m ~@body))

(defn set-val [k v]
  (fn [state]
    (let [new-state (assoc state k v)]
      [new-state new-state])))

(defn get-val [k]
  (fn [state]
    [(get state k) state]))

(defn body-contains? [strn]
  (as-script
    [result (get-val :result)]
    (is (.contains (:body result) strn)
        (format "Expected body to contain \"%s\"" strn))))

(defn response-code? [code]
  (as-script
    [response (get-val :result)]
    (is (= code (:code response))
        (format "Unexpected response code. Expected: %s" code))))

(defn get-handler [state]
  (let [handler (:handler state)]
    (when-not handler
      (throw (Exception. "could not get handler")))
    [handler state]))

(defn response-cookies [state]
  (let [cookies (->> state
                  :result
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

(defn request [method url params]
  (as-script
    [handler get-handler
     ;; extract state from previous result
     ;; :let [[result request] (handler {:request-method method
     ;;                                  :uri url
     ;;                                  :headers {"cookie" (str "session-id="
     ;;                                                          session-id)}
     ;;                                  :params params})]
     ;;_ (set-val :result result)
     ;;_ (set-val :request request)
     ;;_ set-cookies
     ]
    :result))

(defn click [url & [query-string]]
  (as-script
    [handler get-handler
     cookies (get-val :cookies)
     :let [result (handler {:request-method :get
                            :uri url
                            :cookies cookies
                            :query-string query-string})]
     _ (set-val :result result)
     _ set-cookies]
    result))

(defn submit [method url field-vals]
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
     _ (set-val :result result)
     _ set-cookies]
    result))

(defmacro script [handler & steps]
   `((clojure.contrib.monads/domonad
       appraiser.scripter/script-m 
       ~@steps)
       {:handler ~handler}))
