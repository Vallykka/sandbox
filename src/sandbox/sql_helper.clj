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
  (let [[name-string type-string] (s/conform ::valid-str-params [name type])]
    (when (s/valid? ::valid-str-params [name type])
      (->Column name-string type-string (get types-conformers type-string)))))

(defmacro deftable
  [name & forms]
  (let [name-string (s/conform ::valid-str-param name)
        columns (mapv (fn [form] (macroexpand-1 form)) forms)]
    (when-not (= ::s/invalid name-string)
      (swap! table-container assoc name-string (->Table name-string columns)))))

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
  (if-not (s/valid? ::gensql body)
    (s/explain-data ::gensql body)
    (let [locals (into {} (for [local (keys &env)] [(name local) local]))]
      `(let [table# (get @table-container ~(name (second (first body))))
             column-conformer# (apply merge (map (fn [col#] {(:name col#) (:conformer col#)}) (:columns table#)))
             column-parameter# (merge {} ~@(map (fn [[q column op value-param]]
                                                  (when (= (str q) "where") {(name column) (str value-param)}))
                                                (rest body)))
             mapped-vals# (for [[k# v#] column-parameter#]
                            [v# (str (s/conform (get column-conformer# k#) (get ~locals v#)))])
             query# (apply str ~@body)]
         (reduce #(apply str/replace %1 %2) query# mapped-vals#)))))

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

(s/def ::valid-selects (fn [[columns selected]]
                         (every? (set (map #(:name %) columns)) (map str selected))))

(s/def ::->columns-str
  (s/and ::valid-selects
         (with-conformer [columns selected] (str/join ", " selected))))

(s/def ::->select-part
  (with-conformer [query & clauses]
                  (let [[query-part table-name & fields] query]
                    (if-let [table (get @table-container (s/conform ::valid-str-param table-name))]
                      (if (s/valid? ::valid-selects [(:columns table) fields])
                        [table clauses]
                        (throw (ex-info "invalid query selected fields" {})))
                      (throw (ex-info (format "table with name %s not found" (str table-name)) {}))))))

(s/def ::->where-params-part
  (with-conformer [table clauses]
                  (let [res (apply into []
                                   (for [[query-part column op value] clauses]
                                     (when (= query-part 'where)
                                       (if-let [params-part (s/conform ::valid-str-params [column op value])]
                                         [params-part]
                                         (throw (ex-info "invalid where clause parameters" {}))))))]

                    [table res])))

(s/def ::->where-columns-part
  (with-conformer [table selected-columns]
                  (if (= ::s/invalid selected-columns)
                    selected-columns
                    (apply into []
                           (for [[column-name op value]  selected-columns]
                             (if-let [column (first (filter #(= column-name (:name %)) (:columns table)))]
                               (when-not (s/valid? some? (:conformer column))
                                 (throw (ex-info (format "no type converter for column %s" column-name) {})))
                               (throw (ex-info (format "there is no such column as %s in table %s" column-name) {}))))))))

(s/def ::gensql
  (s/and
    ::->select-part
    ::->where-params-part
    ::->where-columns-part))

(defmacro test-gensql
  []
  (let [name-val 1 date-val "2021-01-01"]
    (deftable users (column name string) (column created date))
    (println (gensql (from users created) (where name = name-val))) ; select * from users where name="1"
    (println (gensql (from users) (where created = date-val))))) ; select * from users where date=111111, date converted to unix-time

