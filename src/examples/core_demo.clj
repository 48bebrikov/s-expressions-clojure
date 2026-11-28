
(require '[tree-data.core :as tree])

; Подключение библиотеки и создание дерева
(def doc
  (tree/create-tree
    "university"
    {"name" "Технический университет"}
    [["faculty"
      {"name" "Факультет информатики"}
      [["student" {"id" "1" "name" "Анна"} ["Программирование"]]
       ["student" {"id" "2" "name" "Иван"} ["Математика"]]]]]))

(println "Исходное дерево:")
(prn doc)

; Проверка структуры и получение информации об узлах
(println "Корректность дерева:")
(println (tree/node? (:root doc)))

(println "Имя корневого узла:")
(println (tree/node-name (:root doc)))

(println "Атрибуты корневого узла:")
(prn (tree/node-attributes (:root doc)))

(println "Дочерние узлы корня:")
(prn (tree/node-content-children (:root doc)))

; Обход дерева и поиск узла
(println "Все узлы дерева:")
(prn (tree/traverse-tree doc))

(println "Поиск первого студента:")
(prn (tree/find-node doc ["faculty" "student"]))

(println "Поиск второго студента:")
(prn (tree/find-node doc ["faculty" ["student" 2]]))

; Добавление нового узла
(def with-new-student
  (tree/add-child
    doc
    ["faculty"]
    (tree/create-node
      "student"
      {"id" "3" "name" "Мария"}
      ["Базы данных"])))

(println "Дерево после добавления студента:")
(prn (tree/traverse-tree with-new-student))

; Изменение атрибутов и содержимого
(def updated-attributes
  (tree/update-attributes
    with-new-student
    ["faculty" ["student" 1]]
    {"id" "1" "name" "Анна" "course" "2"}))

(println "Студент после изменения атрибутов:")
(prn (tree/find-node updated-attributes
                     ["faculty" "student"]))

(def updated-content
  (tree/update-content
    updated-attributes
    ["faculty" ["student" 1]]
    ["Программирование, Алгоритмы"]))

; Удаление узла
(println "Студент после изменения содержимого:")
(prn (tree/find-node updated-content
                     ["faculty" "student"]))

(def without-student
  (tree/remove-node
    updated-content
    ["faculty" ["student" 2]]))

(println "Дерево после удаления второго студента:")
(prn (tree/traverse-tree without-student))

; Сохранение и загрузка документа
(println "Сохранение дерева:")
(println (tree/save-tree without-student "core_demo_university.smc"))

(def loaded-doc
  (tree/load-tree "core_demo_university.smc"))

(println "Загруженное дерево:")
(prn loaded-doc)


