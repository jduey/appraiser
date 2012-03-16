(ns appraiser.test.forms
  (:use appraiser.forms
        clojure.test
        [korma.core :exclude [fields]]
        korma.db)
  (:import java.sql.Date
           java.text.SimpleDateFormat))

(deftest test-make-key
         (is (= :some_key
                (make-key "Some Key")))
         (is (= :some_key
                (make-key "Some-Key"))))

(deftest test-label
         (let [lbl-fn (-> {}
                        ((label :name "First:"))
                        second
                        :appraiser.forms/html-fn
                        first)]
           (is (= [:label {:for "name"} "First:"]
                  (lbl-fn {})))))

(deftest test-div
         (let [div-fn (-> {}
                        ((div (label :name "First:")))
                        second
                        :appraiser.forms/html-fn
                        first)]
           (is (= [:div [:label {:for "name"} "First:"]]
                  (div-fn {}))))
         (let [div-fn (-> {}
                        ((div {:id "name-div"}
                              (label :name "First:")))
                        second
                        :appraiser.forms/html-fn
                        first)]
           (is (= [:div {:id "name-div"}
                   [:label {:for "name"} "First:"]]
                  (div-fn {})))))

(deftest test-string
         (let [string-field (second ((string :some_key) {}))
               decoder (first (:appraiser.forms/decoders string-field)) 
               html-gen (first (:appraiser.forms/html-fn string-field)) 
               html [:input {:type "text" :name "some_key"
                             :id "some_key" :value nil}]]
           (is (= {:some_key "upset"} (decoder {"some_key" "upset"})))
           (is (= html
                 (html-gen nil)))
           (is (= (assoc-in html [1 :value] "howdy")
                  (html-gen {:some_key "howdy"})))
           (is (= html
                  (html-gen {:different "howdy"})))))

(deftest test-integer
         (let [int-field (second ((integer :some_key) {}))
               decoder (first (:appraiser.forms/decoders int-field)) 
               html-gen (first (:appraiser.forms/html-fn int-field)) 
               html [:input {:type "text" :name "some_key"
                             :id "some_key" :value nil}]]
           (is (= {:some_key 24}
                  (decoder {"some_key" "24"})))
           (is (= html
                  (html-gen nil)))
           (is (= (assoc-in html [1 :value] 24)
                  (html-gen {:some_key 24})))
           (is (= html
                  (html-gen {:different 24})))))

(deftest test-date
         (let [date-parser (doto (SimpleDateFormat.)
                             (.setLenient false)
                             (.applyPattern "M/d/yy"))
               date-field (second ((date "Some Key" "M/d/yy") {}))
               decoder (first (:appraiser.forms/decoders date-field)) 
               html-gen (first (:appraiser.forms/html-fn date-field)) 
               html [:input {:type "text" :name "some_key"
                             :id "some_key" :value nil}]]
           (is (= {:some_key (Date/valueOf "2011-10-09")}
                  (decoder {"some_key" "10/9/11"})))
           (is (= html
                  (html-gen nil)))
           (is (= (assoc-in html [1 :value] "5/19/11")
                  (html-gen {:some_key (parse-date date-parser "5/19/11")})))
           (is (= html
                  (html-gen {:different (parse-date date-parser "5/19/11")})))))

(deftest test-selection
         (let [select-field (second ((selection "Select Key"
                                          (sorted-map
                                            :first "First"
                                            :second "Second"))
                                 {}))
               decoder (first (:appraiser.forms/decoders select-field)) 
               html-gen (first (:appraiser.forms/html-fn select-field)) 
               html [:select {:name "select_key" :id "select_key"}
                      [[:option {:selected false} "First"]
                       [:option {:selected false} "Second"]]]]
           (is (= {:select_key :second}
                  (decoder {"select_key" "Second"})))
           (is (= html
                  (html-gen nil)))
           (is (= (assoc-in html [2 0 1 :selected] true)
                  (html-gen {:select_key :first})))
           (is (= html
                  (html-gen {:different :third})))))

(deftest test-radio-group
         (let [choice-field (create-form {}
                                         (radio-group "Choice Key"
                                                      (sorted-map :first "First"
                                                                  :second "Second") 
                                                      radio-button))
               decoder (fn [tuple]
                         (apply merge
                                (map #(% tuple)
                                     (:appraiser.forms/decoders choice-field)))) 
               html-gen (fn [tuple]
                         (map #(% tuple)
                              (:appraiser.forms/html-fn choice-field)))
               html [[:input {:type "radio"
                              :name "choice_key"
                              :id "choice_key-First"
                              :value "First"
                              :checked false}]
                     [:input {:type "radio"
                              :name "choice_key"
                              :id "choice_key-Second"
                              :value "Second"
                              :checked false}]]]
           (is (= 1 (count (:transforms choice-field))))
           (is (= 1 (count (:prepares choice-field))))
           (is (= {:choice_key :first}
                  (decoder {"choice_key" "First"})))
           (is (= {:choice_key nil}
                  (decoder {"another_key" "Second"})))
           (is (= html
                  (html-gen nil)))
           (is (= (assoc-in html [0 1 :checked] true)
                  (html-gen {:choice_key :first})))
           (is (= html
                  (html-gen {"different" :third})))))

(deftest test-options
         (let [option-field (create-form {}
                                         (options :opts
                                                  {:opt1 "Option1"
                                                   :opt2 "Option2"
                                                   :opt3 "Option3"}
                                                  check-box)) 
               decoder (first (:appraiser.forms/decoders option-field))
               html-gen (fn [tuple]
                         (map #(% tuple)
                              (:appraiser.forms/html-fn option-field)))
               html [[:input {:type "checkbox"
                              :name "opts_:opt1"
                              :id "opts_:opt1"
                              :value "true"
                              :checked false}]
                     [:input {:type "checkbox"
                              :name "opts_:opt2"
                              :id "opts_:opt2"
                              :value "true"
                              :checked false}]
                     [:input {:type "checkbox"
                              :name "opts_:opt3"
                              :id "opts_:opt3"
                              :value "true"
                              :checked false}]]]
           (is (= 1 (count (:transforms option-field))))
           (is (= 1 (count (:prepares option-field))))
           (is (= {:opts #{:opt1 :opt3}}
                  (decoder {"opts_:opt1" "true" "opts_:opt3" "true"})))
           (is (= html 
                  (html-gen {})))
           #_(do
             (println " ") 
             (doseq [diff (remove
                            #(apply = %)
                            (map vector
                                 (assoc-in html [1 1 :checked] true) 
                                 (html-gen {:opts #{:opt2}})))]
               (prn (first diff))
               (prn (second diff))
               (println " "))) 
           (is (= (assoc-in html [1 1 :checked] true) 
                  (html-gen {:opts #{:opt2}})))))

(defdb test_db {:classname "com.mysql.jdbc.Driver"
                :subprotocol "mysql"
                :delimiters "`"
                :subname "//localhost:3306/testing"
                :user "root"
                :password "bogus"})

(defform test_form 
         (db-only
           (string "uid"))
         (div
           (label :name "First Name:")
           (string :name))
         (div
           (label "city" "Metro Area:")
           (selection "city" (sorted-map :kc "KC" :stl "STL")))
         (span 
           (raw-text "Gender")
           (radio-group "gender"
                        (sorted-map-by (comparator (fn [x y] (> (compare x y) 0)))
                                       :male "Male" :female "Female")
                        (fn [g c v]
                          (fields
                            (label (str g "-" v) (str v ":"))
                            (radio-button g c v))))) 
         (div
           (check-box :monday)
           (label :monday "Monday"))
         (div
           (check-box :tuesday)
           (label :tuesday "Tuesday"))
         (div
           (check-box :wednesday)
           (label :wednesday "Wednesday"))
         (div
           (check-box :thursday)
           (label :thursday "Thursday"))
         (div
           (label :b_day "B-Day")
           (date :b_day "M/d/yy"))
         (password "pwd")
         (span
           (options :opts
                    (sorted-map :opt1 "Option1" :opt2 "Option2")
                    (fn [g o v]
                      (fields
                        (check-box g o v) 
                        (label (str (name g) "_" o) v))))) 
         (div
           (label :age "Age:")
           (integer :age)))

(deftest test-defform
         (delete test_form)
         (is (= (:appraiser.forms/fields test_form)
                [:uid :name :city :gender :monday :tuesday :wednesday
                 :thursday :b_day :pwd :opts :age]))
         (let [date-parser (doto (SimpleDateFormat.)
                             (.setLenient false)
                             (.applyPattern "M/d/yy"))
               check (fn [day]
                       [:input {:type "checkbox"
                                :name day
                                :id day
                                :value "true"
                                :checked false}])
               html [[:div
                      [:label {:for "name"} "First Name:"]
                      [:input {:type "text"
                               :name "name"
                               :id "name"
                               :value nil}]]
                     [:div
                      [:label {:for "city"} "Metro Area:"]
                      [:select {:name "city"
                                :id "city"}
                       [[:option {:selected false} "KC"]
                        [:option {:selected false} "STL"]]]]
                     [:span "Gender"
                      [:label {:for "gender-Male"} "Male:"]
                      [:input {:type "radio"
                               :name "gender"
                               :id "gender-Male"
                               :value "Male"
                               :checked false}]
                      [:label {:for "gender-Female"} "Female:"]
                      [:input {:type "radio"
                               :name "gender"
                               :id "gender-Female"
                               :value "Female"
                               :checked false}]]
                     [:div
                      (check "monday")
                      [:label {:for "monday"} "Monday"]]
                     [:div
                      (check "tuesday")
                      [:label {:for "tuesday"} "Tuesday"]]
                     [:div
                      (check "wednesday")
                      [:label {:for "wednesday"} "Wednesday"]]
                     [:div
                      (check "thursday")
                      [:label {:for "thursday"} "Thursday"]]
                     [:div
                      [:label {:for "b_day"} "B-Day"]
                      [:input {:type "text"
                               :name "b_day"
                               :id "b_day"
                               :value nil}]]
                     [:input {:type "password"
                              :name "pwd"
                              :id "pwd"
                              :value ""}]
                     [:span
                      (check "opts_:opt1")
                      [:label {:for "opts_:opt1"} "Option1"]
                      (check "opts_:opt2")
                      [:label {:for "opts_:opt2"} "Option2"]]
                     [:div
                      [:label {:for "age"} "Age:"]
                      [:input {:type "text" :name "age"
                               :id "age" :value nil}]]]]
           (is (= html (form-html test_form nil)))
           (is (= {:uid nil
                   :name "Jim"
                   :pwd "clever"
                   :city :kc
                   :gender :male
                   :monday false
                   :tuesday true
                   :wednesday false
                   :thursday false
                   :b_day (parse-date date-parser "10/7/11")
                   :opts #{:opt1}
                   :age 25}
                  (decode test_form
                          {"name" "Jim"
                           "city" "KC"
                           "gender" "Male"
                           "monday" "false"
                           "tuesday" "true"
                           "wednesday" "false"
                           "thursday" "false"
                           "b_day" "10/7/11"
                           "pwd" "clever"
                           "opts_:opt1" "true"
                           "opts_:opt2" "false"
                           "age" "25"}))) 
           (insert test_form
                   (values {:uid "user1"
                            :name "Jim"
                            :pwd "clever"
                            :city :kc
                            :gender :male
                            :monday false
                            :tuesday true
                            :wednesday false
                            :thursday false
                            :b_day (parse-date date-parser "10/7/11")
                            :opts #{:opt1}
                            :age 25}))
           (is (= [{:uid "user1"
                    :name "Jim"
                    :city :kc
                    :gender :male
                    :monday false
                    :tuesday true
                    :wednesday false
                    :thursday false
                    :b_day (parse-date date-parser "10/7/11")
                    :opts #{:opt1}
                    :age 25}]
                  (map #(dissoc % :pwd) (select test_form))))
           (is (= (-> html
                    (assoc-in [0 2 1 :value] "Jim")
                    (assoc-in [1 2 2 0 1 :selected] true)
                    (assoc-in [2 3 1 :checked] true)
                    (assoc-in [4 1 1 :checked] true)
                    (assoc-in [7 2 1 :value] "10/7/11")
                    (assoc-in [9 1 1 :checked] true)
                    (assoc-in [10 2 1 :value] 25))
                  (form-html test_form
                             {:uid "user1"})))))

