(ns tree-data.query-test
  (:require [clojure.test :as test :refer [deftest is testing]]
            [tree-data.query :as q]
            [tree-data.core :as tree]))

(def twin ["student" {"id" "same"} ["Тёзка"]])

(def doc
  (tree/create-tree "university" {"id" "root"}
    ["введение"
     ["faculty" {"id" "fit" "active" ""}
      [42 ["student" {"id" "1" "note" "a/b]c"} ["Анна" 5]]
       ["group" {"id" "outer"}
        [["group" {"id" "inner"} [["student" {"id" "2"} ["Ирина"]]]]]]
       twin false twin]]
     {"json" ["student" {"id" "hidden"} []]}
     ["faculty" {"id" "other"} [["student" {"id" "3"} ["Ирина"]]]]]))

(defn ids [nodes] (mapv #(get (tree/node-attributes %) "id") nodes))

(deftest paths-and-context
  (is (= [(:root doc)] (q/select-nodes doc "/")))
  (is (= [(:root doc)] (q/select-nodes doc ".")))
  (is (= ["root"] (ids (q/select-nodes doc "//university"))))
  (is (= [] (q/select-nodes doc ".//university")))
  (is (= [] (q/select-nodes doc "/faculty")))
  (is (= ["fit" "other"] (ids (q/select-nodes doc "/university/*"))))
  (is (= ["1" "same" "same" "3"] (ids (q/select-nodes doc "./faculty/student"))))
  (is (= ["1" "2" "same" "same" "3"] (ids (q/select-nodes doc "//student"))))
  (is (= ["other"] (ids (q/select-nodes doc [["faculty" 2]] "."))))
  (is (= ["3"] (ids (q/select-nodes doc [["faculty" 2]] "student"))))
  (is (= ["1"] (ids (q/select-nodes doc [["faculty" 2]] "//student[@id='1']"))))
  (is (= [] (q/select-nodes doc ["missing"] ".")))
  (is (thrown? clojure.lang.ExceptionInfo (q/select-nodes doc [["faculty" 0]] ".")))
  (is (thrown? clojure.lang.ExceptionInfo (q/select-nodes doc ["missing" 0] "."))))

(deftest parsed-conditions
  (let [query (q/parse-query "/university/faculty[@active][@id=\"fit\"]//student")]
    (is (:absolute? query))
    (is (= [:child :child :descendant] (mapv :axis (:steps query))))
    (is (= [[:has "active"] [:attr= "id" "fit"]]
           (:conditions (second (:steps query)))))
    (is (= ["1" "2" "same" "same"] (ids (q/select-nodes doc query)))))
  (is (= ["1"] (ids (q/select-nodes doc "//student[ @note = 'a/b]c' ]"))))
  (is (= ["2" "3"] (ids (q/select-nodes doc "//student[text()='Ирина']"))))
  (is (= [] (q/select-nodes doc "//faculty[text()='Ирина']")))
  (is (= [] (q/select-nodes doc "//student[text()='5']")))
  (is (= ["2"] (ids (q/select-nodes doc "//group//student"))))
  (is (= ["1" "outer" "inner" "2" "same" "same" "3"]
         (ids (q/select-nodes doc "//*/*[@id]/*[@id]"))))
  (let [quoted (tree/create-tree "*" {"a/b" "O'Brien" "" ""} [["имя с /" {} []]])]
    (is (= [(:root quoted)] (q/select-nodes quoted "/'*'[@'a/b'='O''Brien'][@''='']")))
    (is (= ["имя с /"] (mapv tree/node-name (q/select-nodes quoted ".//'имя с /'"))))))

(deftest syntax-errors
  (doseq [[source expected-offset]
          [[""             0]
           ["   "          3]
           ["//"           2]
           ["./"           2]
           ["student/"     8]
           ["a///b"        3]
           ["/a[]"         3]
           ["a[@]"         3]
           ["a[@x=1]"      5]
           ["a[@x='open]"  5]
           ["a[text()]"    8]
           ["a[@x='v'"     8]
           ["a[@x]tail"    5]
           ["a// /b"       4]
           [".student"     1]
           ["a|b"          1]]]
    (let [error (try (q/parse-query source) nil
                     (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= source (:query error)) source)
      (is (= expected-offset (:offset error)) source)))
  (is (thrown? clojure.lang.ExceptionInfo (q/parse-query nil)))
  (is (thrown? clojure.lang.ExceptionInfo (q/select-nodes {:root 42} "//x"))))

(deftest modifications
  (let [query (q/parse-query "/university/faculty[@active]//student")
        result (q/update-nodes doc query #(assoc-in % [1 "status"] "active"))]
    (is (= ["1" "2" "same" "same"] (ids (q/select-nodes result "//student[@status]"))))
    (is (empty? (q/select-nodes doc "//student[@status]")))
    (is (= ["3"] (ids (q/select-nodes result "faculty[@id='other']/student")))))
  (let [result (q/update-nodes doc [["faculty" 2]] "student"
                               #(update % 2 conj ["address" {} ["Новосибирск"]]))]
    (is (= [["address" {} ["Новосибирск"]]] (q/select-nodes result "//address"))))
  (let [result (q/update-nodes doc "//student[@id='same']" (constantly nil))]
    (is (= ["1" "2" "3"] (ids (q/select-nodes result "//student"))))
    (is (= [42 false] (tree/node-content-values (first (q/select-nodes result "faculty")))))
    (is (= 2 (count (q/select-nodes doc "//student[@id='same']")))))
  (let [result (q/update-nodes doc ["faculty" ["student" 3]] "."
                               #(assoc-in % [1 "id"] "changed"))]
    (is (= ["1" "same" "changed"] (ids (q/select-nodes result "faculty[@id='fit']/student")))))
  (is (identical? doc (q/update-nodes doc "//missing" (constantly nil))))
  (is (thrown? clojure.lang.ExceptionInfo (q/update-nodes doc "/" (constantly nil))))
  (is (thrown? clojure.lang.ExceptionInfo (q/update-nodes doc "//student" (constantly 0)))))

(deftest overlapping-matches
  (let [d (tree/create-tree "x" {} [["x" {} [["x" {} []]]] ["x" {} []]])
        result (q/update-nodes d "//x"
                 (fn [node]
                   (let [sizes (map #(get-in % [1 "size"]) (tree/node-content-children node))]
                     (assoc-in node [1 "size"] (str (inc (reduce + 0 (map #(Long/parseLong %) sizes))))))))]
    (is (= ["4" "2" "1" "1"] (mapv #(get-in % [1 "size"]) (q/select-nodes result "//x"))))
    (is (empty? (q/select-nodes d "//x[@size]"))))
  (let [d (tree/create-tree "root" {} [["x" {} [["x" {} [["x" {} []]]]]]])
        count-updates (atom 0)
        result (q/update-nodes d "//x//x"
                 (fn [node] (swap! count-updates inc) (update node 2 conj ["x" {} []])))]
    (is (= 2 @count-updates))
    (is (= 5 (count (q/select-nodes result "//x"))))))

(deftest deeply-nested-tree
  (let [leaf ["leaf" {"id" "deep"} []]
        root (reduce (fn [n _] ["branch" {} [n]]) leaf (range 1500))
        d {:root root}
        result (q/update-nodes d "//branch//leaf" #(assoc-in % [1 "id"] "updated"))]
    (is (= ["updated"] (ids (q/select-nodes result "//leaf"))))
    (is (= [leaf] (q/select-nodes d ".//leaf")))))

(deftest select-node-locations-tests
  (let [locations (q/select-node-locations doc "//student")
        nodes (mapv :node locations)]
    (testing "Находит те же узлы, что и select-nodes"
      (is (= (q/select-nodes doc "//student") nodes))
      (is (= ["1" "2" "same" "same" "3"] (ids nodes))))

    (testing "Возвращает путь для каждого узла"
      (is (every? vector? (map :path locations)))
      (is (every? #(contains? % :node) locations))
      (is (every? #(contains? % :path) locations)))

    (testing "По пути можно получить исходный узел"
      (doseq [{:keys [path node]} locations]
        (is (= node
               (reduce (fn [current index]
                         (nth (tree/node-content current) index))
                       (:root doc)
                       path))))))

  (testing "Работает с контекстом"
    (let [locations (q/select-node-locations doc ["faculty" ["student" 1]] ".")]
      (is (= 1 (count locations)))
      (is (= "1" (get-in (first locations) [:node 1 "id"]))))))

(defn -main [& _]
  (when-not (test/successful? (test/run-tests 'tree-data.query-test))
    (System/exit 1)))
