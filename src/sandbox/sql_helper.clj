(ns sandbox.sql-helper
  [:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.instant :refer [read-instant-timestamp]]])

(defrecord Table [name columns])
(defrecord Column [name type conformer])
(def table-container (atom {}))
(def types-conformers {"string" ::->string
                       "int"    ::->int
                       "date"   ::->date})

(defmacro column
  [name type]
  (let [result (s/conform ::valid-str-params [name type])]
    (if-not (= ::s/invalid result)
      (let [[name-string type-string] result]
        (->Column name-string type-string (get types-conformers type-string)))
      (throw (ex-info (s/explain-str ::valid-str-params [name type]) {})))))

(defmacro deftable
  [name & forms]
  (let [name-string (s/conform ::valid-str-param name)
        columns (mapv (fn [form] (macroexpand-1 form)) forms)]
    (if-not (= ::s/invalid name-string)
      (swap! table-container assoc name-string (->Table name-string columns))
      (throw (ex-info (s/explain-str ::valid-str-params [name type]) {})))))

(defmacro from
  [& [table & fields]]
  (let [selected-fields (if-not (empty? fields)
                          (str/join ", " (filter #(complement (str/blank? %)) (map name fields)))
                          "*")]
    `(str "select " ~selected-fields " from " ~(name table))))

(defmacro where
  [& [column op value]]
  `(let [[column-name# op# value#] (s/conform ::non-blank-strings [~(name column) ~(str op) ~(str value)])]
     (str " where " column-name# " " op# " " value#)))

(defmacro gensql
  [& body]
  (if (s/valid? ::gensql body)
    (let [locals (into {} (for [local (keys &env)] [(name local) local]))]
      `(let [table# (get @table-container ~(name (second (first body))))
             column-conformer# (reduce merge (map (fn [col#] {(:name col#) (:conformer col#)}) (:columns table#)))
             column-parameter# (merge {} ~@(map (fn [[q column op value-param]]
                                                  (when (= q 'where) {(name column) (str value-param)}))
                                                (rest body)))
             mapped-vals# (for [[k# v#] column-parameter#]
                            [v# (str (s/conform (get column-conformer# k#) (get ~locals v#)))])
             query# (reduce str ~@body)]
         (reduce #(apply str/replace %1 %2) query# mapped-vals#)))
    (throw (ex-info (s/explain-str ::gensql body) {}))))

(defmacro with-conformer
  [bind & body]
  `(s/conformer
     (fn [~bind]
       (try
         ~@body
         (catch Exception e#
           ::s/invalid)))))

(s/def ::non-blank-string (every-pred string? (complement str/blank?)))
(s/def ::non-blank-strings (s/coll-of ::non-blank-string))
(s/def ::valid-str-param (s/and ::->string ::non-blank-string))
(s/def ::valid-str-params (s/coll-of ::valid-str-param))
(s/def ::symbol symbol?)

(s/def ::valid-args-count
  (fn [[args valid-count]] (= valid-count (count args))))

(s/def ::valid-conformers
  (fn [[columns clause-columns]]
    (every? #(some? (:conformer %))
            (filter #((set (map str clause-columns)) (:name %)) columns))))

(s/def ::valid-selects (fn [[columns selected]]
                         (every? (set (map #(:name %) columns)) (map str selected))))

(s/def ::->string
  (s/and some?
         (with-conformer value (str value))))

(s/def ::->int
  (s/and ::non-blank-string
         (with-conformer value (Integer/parseInt value))))

(s/def ::->date
  (s/and ::non-blank-string
         (with-conformer value (inst-ms (read-instant-timestamp value)))))

(s/def ::->column-type (with-conformer [conformer value] (s/conform conformer value)))

(s/def ::->columns-str
  (s/and ::valid-selects
         (with-conformer [columns selected] (str/join ", " selected))))

(s/def ::->table
  (s/and ::valid-str-param
   (with-conformer name (get @table-container name))))

(s/def ::->select
  (s/and
    (s/cat :_ #(= % 'from)
           :from (s/* ::symbol))
    (with-conformer from (:from from))
    (s/cat :table ::->table
           :fields (s/* ::symbol))
    (with-conformer schema [(:columns (:table schema)) (:fields schema)])
    ::valid-selects
    (with-conformer columns-fields (first columns-fields))))

(s/def ::->where
  (s/and
    (s/cat :_ #(= % 'where)
           :where (s/* any?))
    (with-conformer res [(:where res) 3])
    ::valid-args-count
    (with-conformer args-count (first (first args-count)))))

(s/def ::gensql
  (s/and
    (s/cat :select ::->select
           :where-columns  (s/* ::->where))
    (with-conformer result [(:select result) (:where-columns result)])
    ::valid-selects
    ::valid-conformers))

(defmacro test-gensql
  []
  (let [name-val 1 date-val "2021-01-01"]
    (deftable users (column name string) (column created date))
    (println (gensql (from users created) (where name = name-val))) ; select * from users where name="1"
    (println (gensql (from users) (where created = date-val))))) ; select * from users where date=111111, date converted to unix-time

