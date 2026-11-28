(ns tree-data.html-transform
  (:require [clojure.string :as str]
            [tree-data.query :as query]
            [tree-data.core :as tree]))

(declare transform-node)

(defn- valid-tag?
  "Проверяет допустимость имени HTML-тега."
  [tag]
  (and
    (string? tag)
    (boolean
      (re-matches #"[A-Za-z][A-Za-z0-9:._-]*" tag))))

(defn- valid-attribute-name?
  "Проверяет допустимость имени HTML-атрибута."
  [name]
  (and
    (string? name)
    (boolean
      (re-matches
        #"[A-Za-z_:][A-Za-z0-9:._-]*"
        name))))

(defn- valid-attributes-options?
  "Проверяет настройки преобразования атрибутов."
  [options]
  (and
    (map? options)

    ;; Разрешаем только известные параметры.
    (every? #{:copy :exclude :rename :set}
            (keys options))

    ;; Копирование должно быть включено или выключено.
    (or (not (contains? options :copy))
        (boolean? (:copy options)))

    ;; Исключения задаются вектором допустимых имён атрибутов.
    (or (not (contains? options :exclude))
        (and
          (vector? (:exclude options))
          (every? valid-attribute-name? (:exclude options))))

    ;; Переименование задаётся отображением допустимых имён.
    (or (not (contains? options :rename))
        (and
          (map? (:rename options))
          (every? (fn [[old-name new-name]]
                    (and
                      (valid-attribute-name? old-name)
                      (valid-attribute-name? new-name)))
                  (:rename options))))

    ;; Новые атрибуты имеют строковые значения.
    (or (not (contains? options :set))
        (and
          (map? (:set options))
          (every? (fn [[name value]]
                    (and
                      (valid-attribute-name? name)
                      (string? value)))
                  (:set options))))

    ;; Нельзя задавать исключение и переименование,
    ;; если копирование отключено.
    (or (not= false (:copy options))
        (and
          (empty? (:exclude options))
          (empty? (:rename options))))))

(defn- normalize-attributes-options
  "Добавляет настройки атрибутов по умолчанию."
  [options]
  (merge
    {:copy true
     :exclude []
     :rename {}
     :set {}}
    options))

(defn- transform-attributes
  "Преобразует атрибуты узла согласно настройкам правила."
  [node rule]
  (let [source-attributes (second node)
        options (:attributes rule)

        copied-attributes
        (if (:copy options)
          source-attributes
          {})

        excluded-attributes
        (apply dissoc copied-attributes (:exclude options))

        renamed-attributes
        (reduce-kv
          (fn [result old-name new-name]
            (if (contains? result old-name)
              (-> result
                  (assoc new-name (get result old-name))
                  (dissoc old-name))
              result))
          excluded-attributes
          (:rename options))]

    (merge renamed-attributes (:set options))))

(defn- validate-rule
  "Проверяет и нормализует одно правило трансформации."
  [rule]
  (when-not (map? rule)
    (throw
      (ex-info "Правило трансформации должно быть map."
               {:rule rule})))

  (when-not (and
              (string? (:match rule))
              (contains? rule :match))
    (throw
      (ex-info "Правило должно содержать строковое поле :match."
               {:rule rule})))

  (when-not (valid-tag? (:tag rule))
    (throw
      (ex-info "Правило должно содержать корректное имя HTML-тега."
               {:rule rule})))

  (when-not (every? #{:match :tag :attributes}
                    (keys rule))
    (throw
      (ex-info "Правило содержит неизвестные параметры."
               {:rule rule})))

  ;; Проверяем синтаксис выражения языка запросов.
  (try
    (query/parse-query (:match rule))
    (catch clojure.lang.ExceptionInfo e
      (throw
        (ex-info "Некорректное выражение :match в правиле."
                 {:rule rule
                  :match (:match rule)}
                 e))))

  (let [attributes (or (:attributes rule) {})]
    (when-not (valid-attributes-options? attributes)
      (throw
        (ex-info "Некорректные настройки атрибутов."
                 {:rule rule
                  :attributes attributes})))

    (assoc rule
      :attributes
      (normalize-attributes-options attributes))))

(defn create-template
  "Создаёт набор шаблонов HTML-трансформации.

   Принимает:
   rules – вектор правил трансформации.

   Возвращает:
   Map с ключом :templates, содержащим проверенные правила.

   Выбрасывает ExceptionInfo, если набор правил некорректен."
  [rules]
  (when-not (vector? rules)
    (throw
      (ex-info "Набор правил должен быть вектором."
               {:rules rules})))

  {:templates (mapv validate-rule rules)})

(defn- rule-matches-node?
  "Проверяет, соответствует ли узел правилу трансформации."
  [doc path rule]
  (boolean
    (some #(= path (:path %))
          (query/select-node-locations doc (:match rule)))))

(defn- rule-specificity
  "Возвращает оценку специфичности правила трансформации."
  [rule]
  (let [{:keys [absolute? steps]}
        (query/parse-query (:match rule))

        conditions-count
        (reduce + (map #(count (:conditions %)) steps))

        exact-names-count
        (count (filter #(not= :* (:name %)) steps))

        steps-count
        (count steps)

        absolute-score
        (if absolute? 1 0)

        child-axes-count
        (count (filter #(= :child (:axis %)) steps))]

    [conditions-count
     exact-names-count
     steps-count
     absolute-score
     child-axes-count]))

(defn- find-matching-rule
  "Находит наиболее специфичное правило для узла.
   Если правило не найдено, выводит предупреждение и возвращает nil.
   Если несколько правил имеют одинаковую максимальную специфичность, выбрасывает ExceptionInfo."
  [doc path template]
  (let [matching-rules
        (filter #(rule-matches-node? doc path %)
                (:templates template))]

    (if (empty? matching-rules)
      nil

      (let [best-specificity
            (reduce (fn [best rule]
                      (let [current (rule-specificity rule)]
                        (if (pos? (compare current best))
                          current
                          best)))
                    (rule-specificity (first matching-rules))
                    (rest matching-rules))

            best-rules
            (filter #(= best-specificity
                        (rule-specificity %))
                    matching-rules)]

        (when (> (count best-rules) 1)
          (throw (ex-info
                   "Обнаружена неоднозначность правил трансформации."
                   {:path path
                    :specificity best-specificity
                    :rules (vec best-rules)})))

        (first best-rules)))))

(defn- transform-content
  "Рекурсивно преобразует содержимое узла."
  [doc path content template]
  (into []
        (mapcat
          (fn [[index item]]
            (if (tree/node? item)
              (transform-node doc
                              (conj path index)
                              item
                              template)
              [item]))
          (map-indexed vector content))))

(defn- transform-node
  "Преобразует один узел исходного дерева в HTML-представление."
  [doc path node template]
  (let [rule (find-matching-rule doc path template)
        transformed-content
        (transform-content doc
                           path
                           (tree/node-content node)
                           template)]
    (if rule
      [[(:tag rule)
        (transform-attributes node rule)
        transformed-content]]
      transformed-content)))

(defn transform-tree
  "Преобразует SMC-документ в HTML-представление согласно заданному шаблону.

   Принимает:
   doc – SMC-документ в формате {:root node}.
   template – шаблон трансформации в формате {:templates [...]}, содержащий правила преобразования узлов.

   Возвращает:
   Вектор HTML-узлов, полученный в результате преобразования документа.
   Узлы, для которых не найдено соответствующее правило, не создают HTML-теги, а их содержимое обрабатывается рекурсивно.

   Выбрасывает исключение, если SMC-документ или шаблон трансформации имеют некорректную структуру."
  [doc template]
  (when-not (and (map? doc)
                 (= #{:root} (set (keys doc)))
                 (tree/node? (:root doc)))
    (throw
      (ex-info "Некорректный SMC-документ."
               {:doc doc})))

  (when-not (and (map? template)
                 (= #{:templates} (set (keys template))))
    (throw
      (ex-info "Некорректный шаблон трансформации."
               {:template template})))

  (let [validated-template
        (create-template (:templates template))

        root (:root doc)]
    (transform-node doc [] root validated-template)))

(defn- escape-html
  "Экранирует специальные символы для HTML."
  [value]
  (-> (str value)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")
      (str/replace "'" "&#39;")))

(def ^:private void-elements
  #{"area" "base" "br" "col" "embed"
    "hr" "img" "input" "link" "meta"
    "param" "source" "track" "wbr"})

(defn- valid-html-node?
  "Проверяет корректность структуры HTML-узла."
  [node]
  (and
    (vector? node)
    (= 3 (count node))

    ;; Имя тега должно быть допустимым.
    (valid-tag? (first node))

    ;; Атрибуты должны быть map с допустимыми именами
    ;; и строковыми значениями.
    (map? (second node))
    (every?
      (fn [[name value]]
        (and
          (valid-attribute-name? name)
          (string? value)))
      (second node))

    ;; Содержимое должно быть вектором.
    (vector? (nth node 2))))

(defn- html-node-candidate?
  "Проверяет, похож ли вектор на структуру HTML-узла."
  [content]
  (and
    (vector? content)
    (= 3 (count content))
    (string? (first content))
    (map? (second content))))

(defn render-html
  "Преобразует HTML-дерево в строку HTML-разметки.

   Принимает:
   content – HTML-узел, вектор или список HTML-узлов, либо текстовое значение (строку, число, boolean или nil).

   Возвращает:
   Строку с HTML-разметкой.
   Специальные HTML-символы в текстовом содержимом и значениях атрибутов экранируются.

   Выбрасывает исключение, если HTML-узел имеет некорректную структуру, недопустимый тип содержимого или пустой HTML-элемент содержит дочерние элементы."
  [content]
  (cond
    (valid-html-node? content)
    (let [[tag attributes children] content
          attributes-string
          (apply str
                 (map (fn [[name value]]
                        (str " " name "=\""
                             (escape-html value)
                             "\""))
                      attributes))]
      (if (contains? void-elements
                     (clojure.string/lower-case tag))
        (do
          (when (seq children)
            (throw
              (ex-info
                "Пустой HTML-элемент не может содержать содержимое."
                {:tag tag
                 :content children})))
          (str "<" tag attributes-string ">"))
        (str "<" tag attributes-string ">"
             (render-html children)
             "</" tag ">")))

    (html-node-candidate? content)
    (throw
      (ex-info
        "Некорректный HTML-узел."
        {:node content}))

    (or (vector? content) (list? content))
    (apply str (map render-html content))

    (nil? content)
    ""

    (or (string? content)
        (number? content)
        (boolean? content))
    (escape-html content)

    :else
    (throw
      (ex-info
        "Недопустимый тип HTML-содержимого."
        {:content content
         :type (type content)}))))

(defn- valid-html-file-extension?
  "Проверяет, имеет ли путь расширение .html."
  [file-path]
  (and
    (string? file-path)
    (.endsWith file-path ".html")))

(defn- save-html
  "Сохраняет HTML-строку в файл."
  [html file-path]
  (when-not (valid-html-file-extension? file-path)
    (throw
      (ex-info "Файл должен иметь расширение .html."
               {:file-path file-path})))

  (when-not (string? html)
    (throw
      (ex-info "Содержимое HTML должно быть строкой."
               {:html html})))

  (spit file-path html)
  file-path)

(defn transform-to-html
  "Преобразует SMC-документ в HTML-разметку.

   Принимает:
   doc – SMC-документ в формате {:root node}.
   template – шаблон трансформации в формате {:templates [...]} с правилами преобразования узлов.
   output-path – путь к HTML-файлу, в который сохраняется результат (необязательный аргумент).

   Возвращает:
   При вызове с двумя аргументами – строку с HTML-разметкой.
   При вызове с тремя аргументами – путь к созданному HTML-файлу.

   Выбрасывает исключение, если документ или шаблон имеют некорректную структуру либо не удалось сохранить HTML-файл."
  ([doc template]
   (render-html
     (transform-tree doc template)))

  ([doc template output-path]
   (save-html
     (transform-to-html doc template)
     output-path)))

(defn transform-smc-to-html
   "Загружает SMC-документ из файла, преобразует его в HTML-разметку согласно заданному шаблону и сохраняет результат в файл.

    Принимает:
    input-path – путь к исходному SMC-файлу.
    template – шаблон трансформации в формате {:templates [...]} с правилами преобразования узлов.
    output-path – путь к HTML-файлу для сохранения результата.

    Возвращает:
    Путь к созданному HTML-файлу.

    Выбрасывает исключение, если исходный файл не удалось загрузить, документ или шаблон имеют некорректную структуру либо не удалось сохранить HTML-файл."
  [input-path template output-path]
  (transform-to-html
    (tree/load-tree input-path)
    template
    output-path))
