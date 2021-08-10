(ns sandbox.core)

(def all-patients
  '({:firstname "Adam"
     :lastname  "Smith"
     :diagnosis "COVID-19"
     :treated   true}
    {:firstname "Joseph"
     :lastname  "Goodman"
     :diagnosis "COVID-19"
     :treated   true}
    {:firstname "Werner"
     :lastname  "Ziegler"
     :diagnosis "COVID-19"
     :treated   false}
    {:firstname "Boris"
     :lastname  "Henry"
     :diagnosis "Healthy"
     :treated   false}
     {:firstname "Johnny"
      :lastname  "Grayhand"
      :diagnosis "COVID-76"
      :treated   false}))

(defmacro factor-group
  "Factor group + local bindings macros"
  [data group-name bindings & body]
  (let [binds (apply assoc {} bindings)
        grouped (group-by #((apply juxt (vals binds)) %) @(resolve data))]
    `(for [[group-key# ~group-name] ~grouped
           :let [new-binds# (map (fn [[k# v#]] [k# (get (first ~group-name) v#)]) ~binds)]]
       (do ~@body))))

(comment defmacro test-factor-group
  []
  (assert (= '(2 1 1 1) (factor-group all-patients patients-group [treated? :treated
                                                                    disease-name :diagnosis]
                          (println " начало обработки группы пациентов с диагнозом " disease-name
                                   (if treated? ", подвергавшихся лечению"
                                                 ", НЕ подвергавшихся лечению"))

                          (println " количество пациентов в группе - " (count patients-group))
                          (println " фамилии пациентов - " (clojure.string/join ", " (map :lastname patients-group)))

                          (count patients-group)))))

(def owners [{:owner "Jimmy"
              :pets  (ref [{:name "Rex"
                            :type :dog}
                           {:name "Sniffles"
                            :type :hamster}])}
             {:owner "Jacky"
              :pets  (ref [{:name "Spot"
                            :type :mink}
                           {:name "Puff"
                            :type :magic-dragon}])}])

(defmacro extended->>
  [expr alias & forms]
  (let [steps (map (fn [step] `(when ~alias ~step)) forms)]
    `(let [~alias ~expr
           ~@(interleave (repeat alias) (butlast steps))]
       ~(if (empty? steps)
          alias
          (last steps)))))

(defmacro test-extended->>
  []
  (assert (= :hamster (extended->> owners ! (nth ! 0) (:pets !) (deref !) (! 1) (! :type))))
  (assert (nil? (extended->> owners ! (nth ! 0) (:UNKNOWN-KEY !) (deref !) (! 1) (! :type)))))

(defmacro multitry
  [& [form & rest :as forms]]
  (if-not (empty? forms)
   (let [res `(try
                ~form
                (catch Exception e# :invalid))]
     `(if-not (= ~res :invalid)
        ~res
        (multitry ~@rest)))))

(defmacro test-multitry
  []
  (assert (= "9" (multitry (throw (ex-info "ururu" {})) (throw (ex-info "azaza" {})) (str 9) (+ 5 5))))
  (assert (= "42" (multitry (throw (ex-info "ururu" {})) (throw (ex-info "azaza" {})) (str 42))))
  (assert (= 10 (multitry (throw (ex-info "ururu" {})) (throw (ex-info "azaza" {})) (+ 5 5))))
  (assert (= 10 (multitry (+ 5 5) (throw (ex-info "ururu" {}))))))


