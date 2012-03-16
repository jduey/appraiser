(ns appraiser.forms
  (:use [clojure.algo.monads :exclude [update-val]]
        [korma.core :exclude [fields]])
  (:require [hiccup.form-helpers :as fh])
  (:import java.text.SimpleDateFormat
           java.sql.Date
           org.mindrot.jbcrypt.BCrypt))

(defmacro update-val
  "Preferable way to update a value in the state monad."
  [k f & args]
  `(clojure.algo.monads/update-val
     ~k (fn [v#] (~f v# ~@args))))

(defn make-key
  "Convert a field name to a canonical form."
  [field-name]
  (-> field-name
    name
    (.replaceAll " |-" "_")
    .toLowerCase
    keyword))

(defn- transformation
  "Add a function to transform a field coming from the database"
  [trans-fn]
  (fn [frm]
    [nil (update-in frm [:transforms] conj trans-fn)]))

(defn- preparation
  "Add a function to prepare a field to be written to the database"
  [prep-fn]
  (fn [frm]
    [nil (update-in frm [:prepares] conj prep-fn)]))

(defmacro fields
  "Compose multiple fields into one field-like item."
  [& body]
  `(clojure.algo.monads/with-monad
     clojure.algo.monads/state-m
     (m-seq [~@body])))

(defmacro state-let [bindings result]
  `(clojure.algo.monads/domonad
     clojure.algo.monads/state-m
     ~bindings ~result))

(defn db-only
  "Define fields that will not have HTML code generated for them."
  [& db-fields]
  (state-let
    [html-fns (fetch-val ::html-fn)
     _ (m-seq db-fields)
     _ (set-val ::html-fn html-fns)]
    nil))

(defn- html-tag
  "Used to implement simple HTML elements."
  [& body]
  (with-monad state-m
              (update-val ::html-fn
                          conj (fn [tuple]
                                 (vec body)))))

(defn label [& tag-info]
  (apply html-tag (apply fh/label tag-info)))

(defn h3 [& args]
  (apply html-tag :h3 args))

(defn br [& args]
  (apply html-tag :br args))

 (defn- html-block
   "Used to implement HTML tags that may wrap other HTML elements."
   [tag attrs & html-fields]
   (let [html-fields (if (map? attrs)
                       html-fields
                       (cons attrs html-fields))
         head (if (map? attrs)
                [tag attrs]
                [tag])]
    (state-let
      [html-fns (fetch-val ::html-fn)
       _ (set-val ::html-fn [])
       _ (m-seq html-fields)
       new-html-fns (fetch-val ::html-fn)
       :let [block-html-fn (fn [tuple]
                             ;; given a tuple, generate the html
                             ;; for the block
                             (into head
                                   (map #(% tuple) new-html-fns)))]
       _ (set-val ::html-fn (conj html-fns block-html-fn))]
      nil)))

;; some block level html elements
(def div (partial html-block :div))
(def para (partial html-block :p))
(def span (partial html-block :span))

(defn- html-generator
  "Add a function to generate a piece of html for the form. 'html-fn'
  expects a hash map of field keys to values."
  [html-fn]
  (fields
    (update-val ::html-fn conj html-fn)))

(defn- decoder
  "Add a function to decode a field coming from the form. 'decode-fn'
  expects a hash map of form fields to form values."
  [decode-fn]
  (fields
    (update-val ::decoders conj decode-fn)))

(defn- field
  "Add a function to decode a field coming from the form. 'decode-fn'
  expects a hash map of form fields to form values."
  [field-key]
  (fields
    (update-val ::fields conj field-key)))

(defn raw-text
  "Insert literal text into html."
  [strn]
  (html-generator (constantly strn)))

(defn string
  "A text field."
  [field-name]
   (let [field-key (make-key field-name)]
     (fields
       (field field-key)
       (html-generator #(fh/text-field field-key (get % field-key)))
       (decoder #(hash-map field-key (get % (name field-key)))))))

(defn hash-password [pwd]
  (when pwd
    (BCrypt/hashpw pwd (BCrypt/gensalt))))

(defn password
  "A password field. The password is automatically hashed before storing"
  [field-name]
   (let [field-key (make-key field-name)]
     (fields
       (field field-key)
       (preparation #(update-in % [field-key] hash-password))
       (html-generator (constantly (fh/password-field field-key "")))
       (decoder #(hash-map field-key (get % (name field-key)))))))

(defn integer
  "A integer field."
  [field-name]
   (let [field-key (make-key field-name)]
     (fields
       (field field-key)
       (html-generator #(fh/text-field field-key (get % field-key)))
       (decoder #(let [num-str (get % (name field-key))]
                   {field-key (when num-str
                                (read-string num-str))})))))

(def dateFormatter (doto (SimpleDateFormat.)
                     (.setLenient false)
                     (.applyPattern "yyyy-MM-dd")))

(defn date-to-sqldate [date]
  (.format dateFormatter date))

(defn sqldate-to-date [sql-date]
  (try
    (.parse dateFormatter (str sql-date))
    (catch Exception e
      nil)))

(defn parse-date [date-parser date-str]
  (try
    (.parse date-parser date-str)
    (catch Exception e
      nil)))

(defn- format-date-field [date-parser date-str]
  (when date-str
    (.format date-parser date-str)))

(defn date
  "Date field where the input conforms to 'pattern'"
  [field-name pattern]
  (let [field-key (make-key field-name)
        date-parser (doto (SimpleDateFormat.)
                      (.setLenient false)
                      (.applyPattern pattern))]
    (fields
      (field field-key)
      (preparation #(update-in % [field-key] date-to-sqldate))
      (transformation #(update-in % [field-key] sqldate-to-date))
      (html-generator #(->> field-key
                         (get %)
                         (format-date-field date-parser)
                         (fh/text-field field-key)))
      (decoder #(let [date-str (get % (name field-key))]
                  {field-key (when date-str
                               (parse-date date-parser
                                           date-str))})))))

(defn selection
  "A drop down selector."
  [field-name selection-map]
  (let [field-key (make-key field-name)
        sel-lookup (into {} (map (fn [[k v]] [v k])
                                 selection-map))]
    (fields
      (field field-key)
      (transformation #(update-in % [field-key] read-string))
      (preparation #(update-in % [field-key] str))
      (html-generator #(->> (get % field-key)
                         (get selection-map)
                         (fh/drop-down field-key (vals selection-map))))
      (decoder #(hash-map field-key (->> (name field-key)
                                      (get %)
                                      (get sel-lookup)))))))

(defn radio-button
  "'field-name' is the field in the table that holds the selected value
   'choice' is the value put in the table if this button is checked
   'value' is the HTML value of this radio button"
  [field-name choice value]
  (let [field-key (make-key field-name)]
    (fields
      (field field-key)
      (transformation #(update-in % [field-key] read-string))
      (preparation #(update-in % [field-key] str))
      (html-generator #(fh/radio-button field-key
                                        (= choice (get % field-key))
                                        value))
      (decoder #(when (= value (get % (name field-key)))
                  {field-key choice})))))

(defn radio-group
  "'field-name' is the field in the database that holds the selected value
   'button-map' is hash map from from values to store in db to HTML values
   'button-format' is a function that creates the fields for each button.
   It takes the same parameters as the radio-button function.
   ex: (fn [field-name choice value]
           (span
              ;; some label field
              (radio-button field-name choice value)))"
  [field-name button-map button-format]
  (let [field-key (make-key field-name)]
    (state-let
      [transforms (fetch-val :transforms)
       prepares (fetch-val :prepares)
       field-list (fetch-val ::fields)
       _ (decoder (constantly {field-key nil})) ;; the default value of the field
       _ (m-seq (map #(apply button-format field-name %)
                     button-map))
       _ (set-val :transforms transforms)
       _ (set-val :prepares prepares)
       _ (set-val ::fields field-list)
       _ (field field-key)
       _ (transformation #(update-in % [field-key] read-string))
       _ (preparation #(update-in % [field-key] str))]
      nil)))

(defn check-box
  "'field-name' is the field in the table that holds the selected value
   'option' is the value put in the table if this button is checked
   'value' is the HTML value of this radio button"
  [group-name & [option value]]
  (let [field-name (if value
                     (str (name group-name) "_" option)
                     (name group-name))
        field-key (make-key field-name)]
    (fields
      (field field-key)
      (transformation #(update-in % [field-key] read-string))
      (preparation #(update-in % [field-key] str))
      (html-generator #(if (nil? option)
                         (fh/check-box field-key
                                         (get % field-key false))
                         (fh/check-box field-key
                                         (contains?
                                           (get % group-name #{})
                                           option))))
      (decoder #(hash-map field-key
                          (read-string
                            (get % (name field-key) "false")))))))

(defn- option-decoder [field-key field-prefix]
  (fn [tuple]
    (let [ops (->> tuple
                (filter
                  #(and (.startsWith (first %) field-prefix)
                        (read-string (second %))))
                (map first)
                (map #(.replace % field-prefix ""))
                (map read-string)
                set)]
      {field-key ops})))

(defn options
  "'field-name' is the field in the database that holds the selected options
   'options-map' is hash map from from values to store in db to HTML values
   'option-format' is a function that creates the fields for each button.
   It takes the same parameters as the check-box function.
   ex: (fn [group-name option value]
           (span
              ;; some label field
              (check-box group-name option value)))"
  [field-name options-map option-format]
  (let [field-key (make-key field-name)
        field-prefix (str (name field-key) "_") ]
    (state-let
      [transforms (fetch-val :transforms)
       prepares (fetch-val :prepares)
       decoders (fetch-val ::decoders)
       field-list (fetch-val ::fields)
       _ (m-seq (map #(apply option-format field-name %)
                     options-map))
       _ (set-val :transforms transforms)
       _ (set-val :prepares prepares)
       _ (set-val ::decoders decoders)
       _ (set-val ::fields field-list)
       _ (field field-key)
       _ (transformation #(update-in % [field-key]
                                     (fn [t]
                                       (read-string
                                         (format "#{%s}" t)))))
       _ (preparation #(update-in % [field-key]
                                  (fn [t]
                                    (apply str
                                           (interpose "," t)))))
       _ (decoder (option-decoder field-key field-prefix))]
      nil)))

(defn create-form
  "The korma-entity should not have any :prepares or :transforms set"
  [table-name & fields]
  (let [korma-entity (create-entity table-name)
        add-fields (with-monad state-m
                               (m-seq fields))]
    (->> (assoc korma-entity
                ::fields []
                ::html-fn []
                ::decoders [])
      add-fields
      second)))

(defmacro defform [form-name & fields]
  `(def ~form-name
     (create-form (name '~form-name)
                  ~@fields)))

(defn form-html
  "build the html for a frm populated with a record from the
   database table, if it exits"
  ([frm] (form-html frm nil nil))
  ([frm query-map] (form-html frm query-map nil))
  ([frm query-map addtl-map]
   (let [query-fields (fn [q]
                        (apply korma.core/fields q (::fields frm)))
         tuple (when query-map
                 (first
                   (select frm
                           query-fields
                           (where query-map))))
         tuple (merge tuple addtl-map)]
     (map #(% tuple) (::html-fn frm)))))

(defn decode
  "Converts a request tuple for a form into a clojure hash-map."
  [frm tuple]
  (apply merge (map #(% tuple) (::decoders frm))))

