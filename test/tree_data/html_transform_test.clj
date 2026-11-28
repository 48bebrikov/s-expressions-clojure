(ns tree-data.html-transform-test
  (:require [clojure.test :refer [deftest is testing]]
            [tree-data.html-transform :as transform]
            [tree-data.core :as tree]))
(def university
  {:root
   ["university" {}
    [["student" {"id" "1" "type" "teacher"}
      ["Анна"]]
     ["student" {"id" "2" "type" "student"}
      ["Иван"]]
     ["student" {"id" "3" "type" "student"}
      ["Мария"]]]]})

(def university-template
  (transform/create-template
    [{:match "//university"
      :tag "ul"}

     {:match "//student[@type='teacher']"
      :tag "li"
      :attributes
      {:exclude ["type"]
       :rename {"id" "data-id"}
       :set {"class" "teacher"}}}

     {:match "//student"
      :tag "li"
      :attributes
      {:exclude ["type"]
       :rename {"id" "data-id"}}}]))

(deftest create-template-tests
  (testing "Создание корректного шаблона"
    (let [template
          (transform/create-template
            [{:match "//student"
              :tag "li"}])]

      (is (= 1 (count (:templates template))))

      (is (= "//student"
             (:match (first (:templates template)))))

      (is (= "li"
             (:tag (first (:templates template)))))

      (is (= {:copy true
              :exclude []
              :rename {}
              :set {}}
             (:attributes (first (:templates template)))))))

  (testing "Создание шаблона с настройками атрибутов"
    (let [template
          (transform/create-template
            [{:match "//student"
              :tag "div"
              :attributes
              {:copy true
               :exclude ["type"]
               :rename {"id" "data-id"}
               :set {"class" "student"}}}])]

      (is (= ["type"]
             (get-in template [:templates 0 :attributes :exclude])))

      (is (= {"id" "data-id"}
             (get-in template [:templates 0 :attributes :rename])))))

  (testing "Пустой набор правил"
    (is (= {:templates []}
           (transform/create-template []))))

  (testing "Некорректный набор правил"
    (is (thrown? clojure.lang.ExceptionInfo
                 (transform/create-template nil)))

    (is (thrown? clojure.lang.ExceptionInfo
                 (transform/create-template
                   {:match "//student" :tag "li"}))))

  (testing "Некорректное выражение запроса"
    (is (thrown? clojure.lang.ExceptionInfo
                 (transform/create-template
                   [{:match "//"
                     :tag "li"}]))))

  (testing "Некорректное имя HTML-тега"
    (is (thrown? clojure.lang.ExceptionInfo
                 (transform/create-template
                   [{:match "//student"
                     :tag "div>"}]))))

  (testing "Некорректные настройки атрибутов"
    (is (thrown? clojure.lang.ExceptionInfo
                 (transform/create-template
                   [{:match "//student"
                     :tag "li"
                     :attributes {:copy "yes"}}])))

    (is (thrown? clojure.lang.ExceptionInfo
                 (transform/create-template
                   [{:match "//student"
                     :tag "li"
                     :attributes {:exclude "id"}}])))))

(deftest invalid-attribute-options-tests
  (testing "Некорректные имена в :exclude"
    (is
      (thrown? clojure.lang.ExceptionInfo
               (transform/create-template
                 [{:match "//student"
                   :tag "li"
                   :attributes {:exclude ["bad name"]}}]))))

  (testing "Некорректные имена в :rename"
    (is
      (thrown? clojure.lang.ExceptionInfo
               (transform/create-template
                 [{:match "//student"
                   :tag "li"
                   :attributes {:rename {"bad name" "data-id"}}}])))

    (is
      (thrown? clojure.lang.ExceptionInfo
               (transform/create-template
                 [{:match "//student"
                   :tag "li"
                   :attributes {:rename {"id" "bad name"}}}]))))

  (testing "Некорректные имена в :set"
    (is
      (thrown? clojure.lang.ExceptionInfo
               (transform/create-template
                 [{:match "//student"
                   :tag "li"
                   :attributes {:set {"bad name" "value"}}}])))))

(deftest rule-tests
  (testing "Отсутствие обязательных полей"
    (is (thrown? clojure.lang.ExceptionInfo
                 (transform/create-template
                   [{:match "//student"}]))))

  (testing "Неизвестные параметры правила"
    (is (thrown? clojure.lang.ExceptionInfo
                 (transform/create-template
                   [{:match "//student"
                     :tag "li"
                     :unknown "value"}]))))

  (let [doc
        (tree/create-tree
          "university" {}
          [["faculty" {}
            [["student" {"id" "1" "type" "teacher"} ["Анна"]]]]])

        template
        (transform/create-template
          [{:match "//student" :tag "li"}
           {:match "//student[@type='teacher']" :tag "div"}])]

  (testing "Выбирается наиболее специфичное правило"
    (is (= [["div" {"id" "1" "type" "teacher"} ["Анна"]]]
           (transform/transform-tree doc template))))

  (testing "Если правило не найдено, узел пропускается"
    (let [template
          (transform/create-template
            [{:match "//teacher" :tag "div"}])]

      (is (= ["Анна"]
             (transform/transform-tree doc template)))))

  (testing "Одинаковая специфичность вызывает ошибку"
    (let [template
          (transform/create-template
            [{:match "//student" :tag "li"}
             {:match "//student" :tag "div"}])]

      (is (thrown? clojure.lang.ExceptionInfo
                   (transform/transform-tree doc template)))))))

(deftest transform-attributes-tests
  (testing "Копирование, исключение, переименование и добавление атрибутов"
    (let [doc
          {:root
           ["university" {}
            [["student"
              {"id" "15"
               "type" "teacher"
               "name" "Анна"}
              []]]]}

          template
          (transform/create-template
            [{:match "//student"
              :tag "li"
              :attributes
              {:copy true
               :exclude ["type"]
               :rename {"id" "data-student-id"}
               :set {"class" "teacher"}}}])]

      (is (= [["li"
               {"data-student-id" "15"
                "name" "Анна"
                "class" "teacher"}
               []]]
             (transform/transform-tree doc template)))))

  (testing "Отключение копирования оставляет только новые атрибуты"
    (let [doc
          {:root
           ["university" {}
            [["student" {"id" "15"} []]]]}

          template
          (transform/create-template
            [{:match "//student"
              :tag "li"
              :attributes
              {:copy false
               :set {"class" "student"}}}])]

      (is (= [["li" {"class" "student"} []]]
             (transform/transform-tree doc template))))))

(deftest recursive-transformation-tests
  (testing "Преобразование вложенного узла"
    (let [doc
          {:root
           ["university" {}
            [["student" {"id" "15"} ["Анна"]]]]}

          template
          (transform/create-template
            [{:match "//student" :tag "li"}])]

      (is (= [["li" {"id" "15"} ["Анна"]]]
             (transform/transform-tree doc template)))))

  (testing "Узел без правила пропускается, но его потомки преобразуются"
    (let [doc
          {:root
           ["university" {}
            [["group" {}
              [["student" {"id" "15"} ["Анна"]]]]]]}

          template
          (transform/create-template
            [{:match "//student" :tag "li"}])]

      (is (= [["li" {"id" "15"} ["Анна"]]]
             (transform/transform-tree doc template)))))

  (testing "Сохраняется порядок текста и дочерних узлов"
    (let [doc
          {:root
           ["university" {}
            ["Начало "
             ["student" {} ["Анна"]]
             " конец"]]}

          template
          (transform/create-template
            [{:match "//student" :tag "li"}])]

      (is (= ["Начало "
              ["li" {} ["Анна"]]
              " конец"]
             (transform/transform-tree doc template))))))

(deftest transform-tree-tests
  (testing "Преобразование всего документа"
    (let [doc
          {:root
           ["university" {}
            [["group" {}
              [["student" {"id" "15"} ["Анна"]]
               ["student" {"id" "16"} ["Иван"]]]]]]}

          template
          (transform/create-template
            [{:match "//student"
              :tag "li"}])]

      (is
        (=
          [["li" {"id" "15"} ["Анна"]]
           ["li" {"id" "16"} ["Иван"]]]
          (transform/transform-tree doc template)))))
  (testing "Шаблон содержит правило без :match"
    (is
      (thrown? clojure.lang.ExceptionInfo
               (transform/transform-tree
                 university
                 {:templates
                  [{:tag "li"}]}))))

  (testing "Шаблон содержит некорректное имя HTML-тега"
    (is
      (thrown? clojure.lang.ExceptionInfo
               (transform/transform-tree
                 university
                 {:templates
                  [{:match "//student"
                    :tag "123"}]}))))

  (testing "Шаблон содержит некорректное выражение :match"
    (is
      (thrown? clojure.lang.ExceptionInfo
               (transform/transform-tree
                 university
                 {:templates
                  [{:match "//student["
                    :tag "li"}]}))))

  (testing "Принимает корректный SMC-документ"
    (let [doc
          {:root ["university" {}
                  [["student" {} ["Анна"]]]]}

          template
          (transform/create-template
            [{:match "//student"
              :tag "li"}])]

      (is
        (=
          [["li" {} ["Анна"]]]
          (transform/transform-tree doc template)))))

  (testing "Отклоняет документ без корректного корневого узла"
    (let [template (transform/create-template
                     [{:match "//student"
                       :tag "li"}])]

      (is
        (thrown? clojure.lang.ExceptionInfo
                 (transform/transform-tree
                   {:root ["university" {}]}
                   template)))))
  )

(deftest render-html-tests
  (testing "Создание HTML-тега с атрибутами и текстом"
    (is
      (=
        "<li id=\"15\">Анна</li>"
        (transform/render-html
          ["li" {"id" "15"} ["Анна"]]))))
  (testing "Создание вложенных HTML-тегов"
    (is
      (=
        "<ul><li>Анна</li></ul>"
        (transform/render-html
          ["ul" {}
           [["li" {} ["Анна"]]]]))))
  (testing "Экранирование специальных символов в тексте"
    (is
      (=
        "<p>&lt;Анна &amp; Иван&gt;</p>"
        (transform/render-html
          ["p" {} ["<Анна & Иван>"]]))))
  (testing "Преобразование нескольких соседних элементов"
    (is
      (=
        "<li>Анна</li><li>Иван</li>"
        (transform/render-html
          [["li" {} ["Анна"]]
           ["li" {} ["Иван"]]]))))
  (testing "Преобразование SMC-документа непосредственно в HTML"
    (let [doc
          {:root
           ["university" {}
            [["student" {"id" "15"} ["Анна"]]]]}
          template
          (transform/create-template
            [{:match "//student"
              :tag "li"}])]
      (is
        (=
          "<li id=\"15\">Анна</li>"
          (transform/transform-to-html doc template)))))
  (testing "Пустой HTML-элемент не получает закрывающий тег"
    (is
      (=
        "<br>"
        (transform/render-html ["br" {} []]))))
  (testing "Пустой элемент BR"
    (is (= "<BR>"
           (transform/render-html ["BR" {} []]))))

  (testing "Пустой элемент br"
    (is (= "<br>"
           (transform/render-html ["br" {} []]))))
  (testing "Рендеринг завершается ошибкой при недопустимом имени атрибута"
    (is
      (thrown? clojure.lang.ExceptionInfo
               (transform/render-html
                 ["div" {"bad name" "value"} ["Текст"]]))))
  (testing "Корректные атрибуты сохраняются при рендеринге"
    (is
      (=
        "<div id=\"main\" class=\"container\">Текст</div>"
        (transform/render-html
          ["div"
           {"id" "main"
            "class" "container"}
           ["Текст"]]))))
  (testing "Специальные символы в значениях атрибутов экранируются"
    (is
      (=
        "<div title=\"Tom &amp; Jerry &quot;show&quot;\">Текст</div>"
        (transform/render-html
          ["div"
           {"title" "Tom & Jerry \"show\""}
           ["Текст"]]))))
  (testing "Пустой HTML-элемент не должен содержать содержимое"
    (is
      (thrown? clojure.lang.ExceptionInfo
               (transform/render-html
                 ["br" {} ["Какой-то текст"]]))))
  (testing ""
  (is (thrown? clojure.lang.ExceptionInfo
               (transform/render-html ["div>" {} ["Текст"]]))))

  (testing ""
  (is (thrown? clojure.lang.ExceptionInfo
               (transform/render-html ["div" {"id" 123} []]))))

  (testing ""
  (is (thrown? clojure.lang.ExceptionInfo
               (transform/render-html ["div" {} "Текст"]))))

  (testing "Map внутри HTML-содержимого не допускается"
    (is (thrown? clojure.lang.ExceptionInfo
                 (transform/render-html
                   ["div" {} [{"key" "value"}]]))))

  (testing "Некорректный вложенный HTML-узел отклоняется"
    (is (thrown? clojure.lang.ExceptionInfo
                 (transform/render-html
                   ["div" {} [["div>" {} ["Текст"]]]]))))




  )

(deftest university-transform-test
  (testing "Комплексное преобразование дерева университета в HTML"
    (let [result
          (transform/transform-to-html
            university
            university-template)]
      (is
        (=
          "<ul><li data-id=\"1\" class=\"teacher\">Анна</li><li data-id=\"2\">Иван</li><li data-id=\"3\">Мария</li></ul>"
          result))))

  (testing "Сохранение HTML в файл"
    (let [file-path "test-output.html"
          result
          (transform/transform-to-html
            university
            university-template
            file-path)]
      (is (= file-path result))
      (is (=
            "<ul><li data-id=\"1\" class=\"teacher\">Анна</li><li data-id=\"2\">Иван</li><li data-id=\"3\">Мария</li></ul>"
            (slurp file-path)))
      (.delete (java.io.File. file-path)))))