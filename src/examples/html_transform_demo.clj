
(require '[tree-data.html-transform :as transform])
(require '[tree-data.core :as tree])

; Создание дерева
(def doc
  (tree/create-tree
    "university"
    {"name" "Технический университет"}
    [["faculty"
      {"name" "Факультет информатики"}
      [["student"
        {"id" "1" "name" "Анна"}
        ["Программирование"]]

       ["student"
        {"id" "2" "name" "Иван"}
        ["Математика"]]]]]))

; Создание шаблона HTML-трансформации
(def template
  (transform/create-template
    [{:match "/university"
      :tag "main"
      :attributes {:copy false}}

     {:match "//faculty"
      :tag "section"
      :attributes {:rename {"name" "data-name"}}}

     {:match "//student"
      :tag "li"
      :attributes {:rename {"name" "data-name"}
                   :set {"class" "student"}}}]))

; Преобразование SMC-документа в HTML-дерево
(def html-tree
  (transform/transform-tree doc template))

(println "HTML-дерево:")
(prn html-tree)

; Преобразование HTML-дерева в строку
(def html
  (transform/render-html html-tree))

(println "HTML-разметка:")
(println html)

; Сохранение HTML-строки в файл
(println "\nСохранение HTML-разметки:")

(println
  (transform/transform-to-html
    doc
    template
    "html_transform_university_from_tree.html"))

; Преобразование SMC-файла в HTML-файл
(println "\nПреобразование SMC-файла в HTML-файл:")

(println
  (transform/transform-smc-to-html
    "core_demo_university.smc"
    template
    "html_transform_university_from_file.html"))
