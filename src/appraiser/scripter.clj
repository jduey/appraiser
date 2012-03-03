(ns appraiser.scripter
  (:use [clojure.algo.monads :only [domonad m-seq state-t set-val
                                    fetch-val maybe-m with-monad]]))

(def script-m (state-t maybe-m))

(defn script [& lines]
  (with-monad script-m
                (m-seq lines)))

(defmacro script-let [bindings body]
  `(domonad script-m ~bindings ~body))

(defn update-val
  "Set a key 'k' in the state to the result of applying
  'f' to the current value of 'k' and additional arguments."
  [k f & args]
  (fn [state]
    (let [new-state (apply update-in state [k] f args)]
      [new-state new-state])))

(def get-val fetch-val)

(defn- get-handler [state]
  (if-let [handler (:handler state)]
    [handler state]
    (throw (Exception. "could not get handler"))))

(defn- response-cookies [state]
  (let [cookies (->> (get-in state [:response :headers "Set-Cookie"]) 
                  (map #(let [[k v] (.split % "=")]
                          [k {:value v}]))
                  (into {}))]
    [cookies state]))

(def set-cookies
  (script-let
    [cookies response-cookies
     _ (update-val :cookies merge cookies)]
    cookies))

(defn request [method url & [req-map]]
  (let [method (->> method
                 name
                 .toLowerCase
                 keyword)]
    (script-let
      [handler get-handler
       cookies (get-val :cookies)
       :let [request (assoc req-map
                            :request-method method
                            :uri url
                            :cookies cookies)
             result (handler request)]
       _ (set-val :response result)
       _ set-cookies]
      result)))

(defn click
  "Simulate the clicking of a link on a webpage. An optional
  query string may be provided."
  [url & [query-string]]
  (script-let
    [result (request :get url {:query-string query-string})]
    result))

(defn submit
  "Simulate the submission of a form. 'method' is one of the
  HTTP methods. 'field-vals' is a hash-map whose keys are the
  field id's of the form and whose values are the corresponding
  values."
  [method url field-vals]
  (let [method (keyword (.toLowerCase (name method)))]
    (script-let
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

(defn body-contains?
  "Tests whether the body of the most recent response contains the
   string 'strn'."
  [strn]
  (script-let
    [response (get-val :response)
     :when (.contains (:body response) strn)]
    true))

(defn response-code?
  "Tests whether the most recent response code is 'code' or not."
  [code]
  (script-let
    [response (get-val :response)
     :when (= code (:status response))]
    true))

(defn run-script
  "Run a script to test a particular Ring handler."
  [handler script]
  (-> (script {:handler handler})
    first   ;; discard the final state
    last    ;; get the value returned from the last action
    boolean))  ;; convert it to a boolean
