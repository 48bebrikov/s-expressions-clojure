(ns tree-data.core-test
  (:require [clojure.test :as test]
            [tree-data.core :as tree]))

(test/deftest primitive-tests
  (test/testing "Проверка примитивных значений"
    (test/is (true? (tree/primitive? "Анна")))
    (test/is (true? (tree/primitive? 22)))
    (test/is (true? (tree/primitive? 3.14)))
    (test/is (true? (tree/primitive? true)))
    (test/is (true? (tree/primitive? nil)))
    (test/is (false? (tree/primitive? [1 2 3])))
    (test/is (false? (tree/primitive? {})))))

(test/deftest object-tests
  (test/testing "Проверка объектов"
    (test/is (true? (tree/object? {"name" "Анна"})))
    (test/is (true? (tree/object? {"age" 22})))
    (test/is (true? (tree/object? {})))
    (test/is (false? (tree/object? {:name "Анна"})))
    (test/is (false? (tree/object? {"name" "Анна" 1 2})))
    (test/is (false? (tree/object? ["name" "Анна"])))))

(test/deftest array-tests
  (test/testing "Проверка массивов и списков"
    (test/is (true? (tree/array? [1 2 3])))
    (test/is (true? (tree/array? '("Анна" 22))))
    (test/is (true? (tree/array? [])))
    (test/is (true? (tree/array? '())))
    (test/is (false? (tree/array? "Анна")))
    (test/is (false? (tree/array? {})))
    (test/is (false? (tree/array? 22)))))

(test/deftest content-value-tests
  (test/testing "Проверка допустимых значений содержимого"
    (test/is (true? (tree/content-value? "Анна")))
    (test/is (true? (tree/content-value? 22)))
    (test/is (true? (tree/content-value? nil)))
    (test/is (true? (tree/content-value? ["student" {} []])))
    (test/is (true? (tree/content-value? {"name" "Анна"})))
    (test/is (true? (tree/content-value? [1 2 3])))
    (test/is (true? (tree/content-value? '("Анна" 22))))
    (test/is (false? (tree/content-value? (Object.))))
    (test/is (false? (tree/content-value? {"name" (Object.)})))
    (test/is (false? (tree/content-value? [1 (Object.)])))))

(test/deftest nested-content-tests
  (test/testing "Проверка вложенных структур"
    (let [valid-data
          {"student"
           {"name" "Анна"
            "age" 22
            "grades" [5 4 5]}}

          invalid-data
          {"student"
           {"name" "Анна"
            "grades" [5 4 (Object.)]}}

          nested-node
          ["student" {}
           ["Анна"
            ["address" {} ["Москва"]]]]]

      (test/is (true? (tree/content-value? valid-data)))
      (test/is (false? (tree/content-value? invalid-data)))
      (test/is (true? (tree/content-value? nested-node))))))

(test/deftest node-tests
  (test/testing "Проверка структуры узла"
    (test/is (true? (tree/node? ["student" {} []])))
    (test/is (true? (tree/node? ["student" {"id" "1"} ["Анна"]])))
    (test/is (true? (tree/node? ["student" {} ["Анна" 22]])))
    (test/is (false? (tree/node? "student")))
    (test/is (false? (tree/node? ["student" {}])))
    (test/is (false? (tree/node? ["student" [] {}])))
    (test/is (false? (tree/node? [123 {} []])))))

(test/deftest create-node-tests
  (test/testing "Создание узла с одним аргументом"
    (test/is (= ["student" {} []]
                (tree/create-node "student"))))

  (test/testing "Создание узла с двумя аргументами"
    (test/is (= ["student" {"id" "1"} []]
                (tree/create-node "student" {"id" "1"}))))

  (test/testing "Создание узла с тремя аргументами"
    (test/is (= ["student" {"id" "1"} ["Анна" 22]]
                (tree/create-node "student"
                                  {"id" "1"}
                                  ["Анна" 22]))))

  (test/testing "Создание узла с вложенными структурами"
    (test/is (= ["student" {}
                 ["Анна"
                  {"age" 22}
                  [5 4 5]
                  ["address" {} ["Иркутск"]]]]
                (tree/create-node
                  "student"
                  {}
                  ["Анна"
                   {"age" 22}
                   [5 4 5]
                   ["address" {} ["Иркутск"]]]))))

  (test/testing "Проверка некорректного имени"
    (test/is (thrown? clojure.lang.ExceptionInfo
                      (tree/create-node 123)))
    (test/is (thrown? clojure.lang.ExceptionInfo
                      (tree/create-node nil))))

  (test/testing "Проверка некорректных атрибутов"
    (test/is (thrown? clojure.lang.ExceptionInfo
                      (tree/create-node "student" [])))
    (test/is (thrown? clojure.lang.ExceptionInfo
                      (tree/create-node "student" {"id" 123}))))

  (test/testing "Проверка некорректного содержимого"
    (test/is (thrown? clojure.lang.ExceptionInfo
                      (tree/create-node "student" {} "Анна")))
    (test/is (thrown? clojure.lang.ExceptionInfo
                      (tree/create-node "student" {} [1 (Object.)])))))

(test/deftest traverse-tree-tests
  (test/testing "Обход дерева с одним корневым узлом"
    (let [doc (tree/create-tree "university")]
      (test/is (= [["university" {} []]]
                  (tree/traverse-tree doc)))))

  (test/testing "Обход дерева с дочерними узлами"
    (let [doc (tree/create-tree
                "university"
                {}
                [["faculty" {}
                  [["student" {} ["Анна"]]
                   ["student" {} ["Иван"]]]]
                 ["library" {}
                  [["book" {} ["Clojure"]]]]])]
      (test/is
        (= ["university" "faculty" "student"
            "student" "library" "book"]
           (mapv tree/node-name
                 (tree/traverse-tree doc)))))))

(test/deftest find-node-tests
  (let [doc (tree/create-tree
              "university"
              {}
              [["faculty" {}
                [["student" {"id" "1"} ["Анна"]]
                 ["student" {"id" "2"} ["Иван"]]
                 ["student" {"id" "3"} ["Мария"]]]]
               ["library" {}
                [["book" {"id" "10"} ["Clojure"]]]]])]

    (test/testing "Поиск корневого узла"
      (test/is (= ["university" {}
                   (tree/node-content (:root doc))]
                  (tree/find-node doc []))))

    (test/testing "Поиск дочернего узла по имени"
      (test/is (= ["faculty" {}
                   [["student" {"id" "1"} ["Анна"]]
                    ["student" {"id" "2"} ["Иван"]]
                    ["student" {"id" "3"} ["Мария"]]]]
                  (tree/find-node doc ["faculty"]))))

    (test/testing "Поиск первого одноимённого узла"
      (test/is (= ["student" {"id" "1"} ["Анна"]]
                  (tree/find-node doc ["faculty" "student"]))))

    (test/testing "Поиск узла по индексу"
      (test/is (= ["student" {"id" "2"} ["Иван"]]
                  (tree/find-node doc ["faculty" ["student" 2]])))

      (test/testing "Поиск несуществующего узла"
        (test/is (nil? (tree/find-node doc ["faculty" "teacher"]))))

      (test/testing "Поиск по индексу за пределами количества узлов"
        (test/is (nil? (tree/find-node doc ["faculty" ["student" 5]]))))

      (test/testing "Некорректный путь"
        (test/is (nil? (tree/find-node doc ["faculty" ["student" 0]])))))))

(test/deftest add-child-tests
  (let [doc (tree/create-tree
              "university"
              {}
              [["faculty" {}
                [["student" {"id" "1"} ["Анна"]]
                 ["student" {"id" "2"} ["Иван"]]]]
               ["library" {} []]])]

    (test/testing "Добавление узла в корень"
      (let [result (tree/add-child
                     doc
                     []
                     (tree/create-node "department"))]
        (test/is (= "department"
                    (tree/node-name
                      (last (tree/node-content (:root result))))))))

    (test/testing "Добавление узла внутрь родителя"
      (let [result (tree/add-child
                     doc
                     ["faculty"]
                     (tree/create-node "teacher"))]
        (test/is (= ["student" "student" "teacher"]
                    (mapv tree/node-name
                          (tree/node-content
                            (tree/find-node result ["faculty"])))))))

    (test/testing "Добавление узла внутрь второго одноимённого узла"
      (let [result (tree/add-child
                     doc
                     ["faculty" ["student" 2]]
                     (tree/create-node "address" {} ["Иркутск"]))]
        (test/is
          (= ["Иван" ["address" {} ["Иркутск"]]]
             (tree/node-content
               (tree/find-node result
                               ["faculty" ["student" 2]]))))))

    (test/testing "Исходное дерево не изменяется"
      (let [result (tree/add-child
                     doc
                     []
                     (tree/create-node "department"))]
        (test/is (= 2
                    (count (tree/node-content (:root doc)))))
        (test/is (= 3
                    (count (tree/node-content (:root result)))))))

    (test/testing "Добавление к несуществующему родителю"
      (test/is
        (thrown? clojure.lang.ExceptionInfo
                 (tree/add-child
                   doc
                   ["unknown"]
                   (tree/create-node "student")))))

    (test/testing "Добавление некорректного узла"
      (test/is
        (thrown? clojure.lang.ExceptionInfo
                 (tree/add-child
                   doc
                   []
                   ["student" {} "не вектор"]))))))

(test/deftest remove-node-tests
  (let [doc (tree/create-tree
              "university"
              {}
              [["faculty" {}
                [["student" {"id" "1"} ["Анна"]]
                 ["student" {"id" "2"} ["Иван"]]
                 ["student" {"id" "3"} ["Мария"]]]]
               ["library" {}
                [["book" {"id" "10"} ["Clojure"]]]]])]

    (test/testing "Удаление дочернего узла"
      (let [result (tree/remove-node doc ["library"])]
        (test/is (= ["faculty"]
                    (mapv tree/node-name
                          (tree/node-content (:root result)))))))

    (test/testing "Удаление узла по имени"
      (let [result (tree/remove-node doc ["faculty" "student"])]
        (test/is (= [["Иван"] ["Мария"]]
                    (mapv tree/node-content
                          (tree/node-content
                            (tree/find-node result ["faculty"])))))))

    (test/testing "Удаление второго одноимённого узла"
      (let [result (tree/remove-node
                     doc
                     ["faculty" ["student" 2]])]
        (test/is (= ["1" "3"]
                    (mapv #(get (tree/node-attributes %) "id")
                          (tree/node-content
                            (tree/find-node result ["faculty"])))))))

    (test/testing "Удаление узла не изменяет исходное дерево"
      (let [result (tree/remove-node doc ["library"])]
        (test/is (= 2
                    (count (tree/node-content (:root doc)))))
        (test/is (= 1
                    (count (tree/node-content (:root result)))))))

    (test/testing "Удаление несуществующего узла"
      (test/is
        (thrown? clojure.lang.ExceptionInfo
                 (tree/remove-node doc ["unknown"]))))

    (test/testing "Удаление корневого узла запрещено"
      (test/is
        (thrown? clojure.lang.ExceptionInfo
                 (tree/remove-node doc []))))

    (test/testing "Некорректный путь"
      (test/is
        (thrown? clojure.lang.ExceptionInfo
                 (tree/remove-node doc ["faculty" ["student" 0]]))))))

(test/deftest update-content-tests
  (let [doc (tree/create-tree
              "university"
              {}
              [["faculty" {}
                [["student" {"id" "1"} ["Анна"]]
                 ["student" {"id" "2"} ["Иван"]]]]])]

    (test/testing "Изменение содержимого дочернего узла"
      (let [result (tree/update-content
                     doc
                     ["faculty" ["student" 2]]
                     ["Иван" 23])]
        (test/is (= ["Иван" 23]
                    (tree/node-content
                      (tree/find-node result
                                      ["faculty" ["student" 2]])))))

      (test/testing "Изменение содержимого корневого узла"
        (let [result (tree/update-content
                       doc
                       []
                       ["Новое содержимое"])]
          (test/is (= ["Новое содержимое"]
                      (tree/node-content (:root result))))))

        (test/testing "Исходное дерево не изменяется"
          (let [result (tree/update-content
                         doc
                         ["faculty" ["student" 2]]
                         ["Иван" 23])]
            (test/is (= ["Иван"]
                        (tree/node-content
                          (tree/find-node doc
                                          ["faculty" ["student" 2]]))))
            (test/is (= ["Иван" 23]
                        (tree/node-content
                          (tree/find-node result
                                          ["faculty" ["student" 2]]))))))

          (test/testing "Некорректное содержимое"
            (test/is
              (thrown? clojure.lang.ExceptionInfo
                       (tree/update-content doc ["faculty"] "не вектор"))))

          (test/testing "Изменение несуществующего узла"
            (test/is
              (thrown? clojure.lang.ExceptionInfo
                       (tree/update-content doc ["unknown"] ["Текст"])))))))

(test/deftest update-attributes-tests
  (let [doc (tree/create-tree
              "university"
              {}
              [["faculty" {}
                [["student" {"id" "1"} ["Анна"]]
                 ["student" {"id" "2"} ["Иван"]]]]])]

    (test/testing "Изменение атрибутов дочернего узла"
      (let [result (tree/update-attributes
                     doc
                     ["faculty" ["student" 2]]
                     {"id" "20" "status" "active"})]
        (test/is (= {"id" "20" "status" "active"}
                    (tree/node-attributes
                      (tree/find-node result
                                      ["faculty" ["student" 2]])))))

      (test/testing "Изменение атрибутов корневого узла"
        (let [result (tree/update-attributes
                       doc
                       []
                       {"version" "2"})]
          (test/is (= {"version" "2"}
                      (tree/node-attributes (:root result))))))

        (test/testing "Исходное дерево не изменяется"
          (let [result (tree/update-attributes
                         doc
                         ["faculty" ["student" 2]]
                         {"id" "20"})]
            (test/is (= {"id" "2"}
                        (tree/node-attributes
                          (tree/find-node doc
                                          ["faculty" ["student" 2]]))))
            (test/is (= {"id" "20"}
                        (tree/node-attributes
                          (tree/find-node result
                                          ["faculty" ["student" 2]]))))))

          (test/testing "Некорректные атрибуты"
            (test/is
              (thrown? clojure.lang.ExceptionInfo
                       (tree/update-attributes
                         doc
                         ["faculty"]
                         {"id" 123}))))

          (test/testing "Изменение несуществующего узла"
            (test/is
              (thrown? clojure.lang.ExceptionInfo
                       (tree/update-attributes
                         doc
                         ["unknown"]
                         {"id" "10"})))))))

; захардкоженный путь тут указывать не нужно, есть java.io.File/createTempFile
(test/deftest file-operations-tests
  (let [doc (tree/create-tree
              "university"
              {}
              [["student" {"id" "1"} ["Анна"]]])
        temp-file (java.io.File/createTempFile "test-tree" ".smc")
        file-path (.getAbsolutePath temp-file)]
    (try
      (test/testing "Сохранение и загрузка дерева"
        (tree/save-tree doc file-path)
        (test/is (= doc (tree/load-tree file-path))))

      (test/testing "Сохранение с неправильным расширением"
        (test/is
          (thrown? clojure.lang.ExceptionInfo
                   (tree/save-tree doc "tree.txt"))))

      (test/testing "Загрузка с неправильным расширением"
        (test/is
          (thrown? clojure.lang.ExceptionInfo
                   (tree/load-tree "tree.txt"))))
      (finally
        (.delete temp-file)))))