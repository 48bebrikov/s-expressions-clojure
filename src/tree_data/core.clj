(ns tree-data.core
  (:require [clojure.edn :as edn]))

(def format-name "SMC") ; S-Expression Markup for Clojure
(def format-version 1)

; Проверка соответствия значений в узле допустимым типам

(defn primitive?
  "Проверяет, является ли значение примитивным типом.

   Принимает:
   value – проверяемое значение.

   Возвращает:
   true, если значение является строкой, числом, boolean или nil;
   иначе false."
  [value]
  (or
    (string? value)
    (number? value)
    (boolean? value)
    (nil? value)))

(defn object?
  "Проверяет, является ли значение объектом.

   Принимает:
   value – проверяемое значение.

   Возвращает:
   true, если значение является map со строковыми ключами;
   false в противном случае."
  [value]
  (and
    (map? value)
    (every? string? (keys value))))

(defn array?
  "Проверяет, является ли значение массивом или списком.

   Принимает:
   value – проверяемое значение.

   Возвращает:
   true, если значение является вектором или списком;
   false в противном случае."
  [value]
  (or
    (vector? value)
    (list? value)))

(defn content-value?
  "Проверяет, является ли значение допустимым содержимым узла."
  [value]
  ;; Узел тоже вектор. Вызов node? здесь повторно обошел бы поддерево
  (loop [pending (list value)]
    (if-let [items (seq pending)]
      (let [item (first items)
            more (rest items)]
        (cond
          (primitive? item) (recur more)
          (object? item)    (recur (into more (vals item)))
          (array? item)     (recur (into more item))
          :else false))
      true)))

(defn attributes?
  "Проверяет корректность атрибутов узла.

   Принимает:
   attributes – набор атрибутов узла.

   Возвращает:
   true, если атрибуты представлены map со строковыми ключами и значениями;
   false в противном случае."
  [attributes]
  (and
    (map? attributes)
    (every? (fn [[key value]]
              (and
                (string? key)
                (string? value)))
            attributes)))

(defn node-shape?
  "Проверяет форму [имя атрибуты содержимое] без обхода потомков."
  [value]
  (and (vector? value)
       (= 3 (count value))
       (string? (first value))
       (attributes? (second value))
       (vector? (nth value 2))))

(defn node?
  "Проверяет, является ли значение корректным узлом дерева.

   Принимает:
   value — значение, которое необходимо проверить.

   Возвращает:
   true, если значение соответствует структуре узла и все его составляющие корректны;
   false в противном случае."
  [value]
  (and (node-shape? value)
       (every? content-value? (nth value 2))))

; Операции для работы с узлами

(defn create-node
  "Создаёт новый узел дерева.

  Принимает:
  name – имя узла в виде строки.
  attributes – (необязательный) набор атрибутов узла в виде map.
  content – (необязательное) содержимое узла в виде вектора.

  Возвращает:
  Вектор, представляющий узел в формате [имя атрибуты содержимое].

  Если атрибуты и содержимое не указаны, создаётся пустой узел."
  ([name]
   (create-node name {} []))

  ([name attributes]
   (create-node name attributes []))

  ([name attributes content]
   (when-not (string? name)
     (throw (ex-info "Имя узла должно быть строкой."
                     {:name name})))

   (when-not (attributes? attributes)
     (throw (ex-info "Некорректные атрибуты узла."
                     {:attributes attributes})))

   (when-not (and (vector? content)
                  (every? content-value? content))
     (throw (ex-info "Некорректное содержимое узла."
                     {:content content})))

   [name attributes content]))

(defn node-name
  "Возвращает имя узла дерева.

   Принимает:
   node – узел дерева в формате [имя атрибуты содержимое].

   Возвращает:
   Имя узла в виде строки."
  [node]
  (first node))

(defn node-attributes
  "Возвращает атрибуты узла дерева.

   Принимает:
   node – узел дерева в формате [имя атрибуты содержимое].

   Возвращает:
   Набор атрибутов узла в виде map.
   Если атрибуты отсутствуют, возвращает пустое отображение."
  [node]
  (second node))

(defn node-content
  "Возвращает всё содержимое узла дерева.

   Принимает:
   node – узел дерева в формате [имя атрибуты содержимое].

   Возвращает:
   Содержимое узла в виде вектора."
  [node]
  (nth node 2))

(defn node-content-values
  "Возвращает значения содержимого узла, исключая дочерние узлы.

   Принимает:
   node — узел дерева в формате [имя атрибуты содержимое].

   Возвращает:
   Вектор значений, не являющихся дочерними узлами."
  [node]
  (filterv (complement node?) (node-content node)))

(defn node-content-children
  "Возвращает дочерние узлы дерева.

   Принимает:
   node – узел дерева в формате [имя атрибуты содержимое].

   Возвращает:
   Вектор дочерних узлов без остальных значений содержимого."
  [node]
  (filterv node? (node-content node)))

; Операции для создания и обхода дерева, удаления/добавления/изменения узлов

(defn create-tree
  "Создаёт дерево с корневым узлом.

   Принимает:
   root-name – имя корневого узла в виде строки.
   attributes – (необязательный) набор атрибутов корневого узла в виде map.
   content – (необязательное) содержимое корневого узла в виде вектора.

   Возвращает:
   Отображение с ключом :root, содержащим корневой узел.

   Если атрибуты и содержимое не указаны, создаётся пустой корневой узел."
  ([root-name]
   {:root (create-node root-name)})

  ([root-name attributes]
   {:root (create-node root-name attributes)})

  ([root-name attributes content]
   {:root (create-node root-name attributes content)}))

(defn traverse-tree
  "Выполняет обход дерева в глубину (DFS).

   Принимает:
   tree – дерево с корневым узлом.

   Возвращает:
   Вектор всех узлов дерева в порядке обхода в глубину."
  [tree]
  (letfn [(traverse-node [node]
            (cons node
                  (mapcat traverse-node
                          (node-content-children node))))]
    (vec (traverse-node (:root tree)))))

(defn find-node
  "Находит узел дерева по заданному пути.

   Принимает:
   tree – дерево с корневым узлом.
   path – вектор, описывающий путь к искомому узлу.
          Строка обозначает имя узла.
          Вектор [имя индекс] обозначает имя узла и его порядковый номер среди одноимённых узлов.
          Пустой путь обозначает корневой узел.

   Возвращает:
   Найденный узел или nil, если узел не найден либо путь некорректен."
  [tree path]
  (when (and (map? tree)
             (node? (:root tree))
             (vector? path))
    (loop [current-node (:root tree)
           remaining-path path]
      (if (empty? remaining-path)
        current-node
        (let [step (first remaining-path)
              children (node-content-children current-node)
              [target-name index]
              (if (string? step)
                [step 1]
                (if (and (vector? step)
                         (= 2 (count step))
                         (string? (first step))
                         (integer? (second step))
                         (pos? (second step)))
                  step
                  [nil nil]))
              matching-nodes (filterv
                               #(= target-name (node-name %))
                               children)]
          (if (and target-name
                   (<= index (count matching-nodes)))
            (recur (nth matching-nodes (dec index))
                   (subvec remaining-path 1))
            nil))))))

(defn add-child
  "Добавляет дочерний узел к указанному родительскому узлу.

   Принимает:
   tree – дерево, в которое необходимо добавить узел.
   parent-path – путь к родительскому узлу.
   child – узел, который необходимо добавить.

   Возвращает:
   Новое дерево с добавленным дочерним узлом.

   Выбрасывает исключение, если дерево, путь или
   добавляемый узел некорректны либо родитель не найден."
  [tree parent-path child]
  (letfn [(valid-path-step? [step]
            (or
              (string? step)
              (and
                (vector? step)
                (= 2 (count step))
                (string? (first step))
                (integer? (second step))
                (pos? (second step)))))

          (update-parent [node path]
            (if (empty? path)
              (update node 2 conj child)

              (let [step (first path)
                    [target-name index]
                    (if (string? step)
                      [step 1]
                      step)

                    content (node-content node)

                    matching-positions
                    (into []
                          (keep-indexed
                            (fn [position item]
                              (when (and
                                      (node? item)
                                      (= target-name
                                         (node-name item)))
                                position))
                            content))]

                (when (or
                        (nil? target-name)
                        (> index (count matching-positions)))
                  (throw
                    (ex-info "Родительский узел не найден."
                             {:path path})))

                (let [position (nth matching-positions
                                    (dec index))
                      updated-child
                      (update-parent
                        (nth content position)
                        (subvec path 1))]

                  (assoc node 2
                              (assoc content position updated-child))))))]

    (when-not (and
                (map? tree)
                (node? (:root tree)))
      (throw
        (ex-info "Некорректное дерево."
                 {:tree tree})))

    (when-not (and
                (vector? parent-path)
                (every? valid-path-step? parent-path))
      (throw
        (ex-info "Некорректный путь к родительскому узлу."
                 {:path parent-path})))

    (when-not (node? child)
      (throw
        (ex-info "Добавляемое значение не является узлом."
                 {:child child})))

    (assoc tree :root
                (update-parent (:root tree) parent-path))))

(defn remove-node
  "Удаляет узел дерева по заданному пути.

   Принимает:
   tree — дерево, из которого необходимо удалить узел.
   path — путь к удаляемому узлу.

   Возвращает:
   Новое дерево без указанного узла.

   Выбрасывает исключение, если путь некорректен, узел не найден или указан путь к корневому узлу."
  [tree path]
  (letfn [(valid-path-step? [step]
            (or
              (string? step)
              (and
                (vector? step)
                (= 2 (count step))
                (string? (first step))
                (integer? (second step))
                (pos? (second step)))))

          (remove-from-parent [parent remaining-path]
            (let [step (first remaining-path)
                  [target-name index]
                  (if (string? step)
                    [step 1]
                    step)

                  content (node-content parent)

                  matching-positions
                  (into []
                        (keep-indexed
                          (fn [position item]
                            (when (and
                                    (node? item)
                                    (= target-name (node-name item)))
                              position))
                          content))]

              (when (or
                      (nil? target-name)
                      (> index (count matching-positions)))
                (throw
                  (ex-info "Удаляемый узел не найден."
                           {:path remaining-path})))

              (let [position (nth matching-positions (dec index))]
                (if (= 1 (count remaining-path))
                  (assoc parent 2
                                (into []
                                      (concat
                                        (subvec content 0 position)
                                        (subvec content (inc position)))))

                  (let [updated-child
                        (remove-from-parent
                          (nth content position)
                          (subvec remaining-path 1))]
                    (assoc parent 2
                                  (assoc content position updated-child)))))))]

    (when-not (and
                (map? tree)
                (node? (:root tree)))
      (throw
        (ex-info "Некорректное дерево."
                 {:tree tree})))

    (when-not (and
                (vector? path)
                (seq path)
                (every? valid-path-step? path))
      (throw
        (ex-info "Некорректный путь к удаляемому узлу."
                 {:path path})))

    (assoc tree :root
                (remove-from-parent (:root tree) path))))

(defn- update-node-at-path
  [tree path update-fn]
  (letfn [(valid-path-step? [step]
            (or
              (string? step)
              (and
                (vector? step)
                (= 2 (count step))
                (string? (first step))
                (integer? (second step))
                (pos? (second step)))))

          (update-at [node remaining-path]
            (if (empty? remaining-path)
              (update-fn node)

              (let [step (first remaining-path)
                    [target-name index]
                    (if (string? step)
                      [step 1]
                      step)

                    content (node-content node)

                    matching-positions
                    (into []
                          (keep-indexed
                            (fn [position item]
                              (when (and
                                      (node? item)
                                      (= target-name (node-name item)))
                                position))
                            content))]

                (when (or
                        (nil? target-name)
                        (> index (count matching-positions)))
                  (throw
                    (ex-info "Узел не найден."
                             {:path remaining-path})))

                (let [position (nth matching-positions (dec index))
                      updated-child
                      (update-at
                        (nth content position)
                        (subvec remaining-path 1))]

                  (assoc node 2
                              (assoc content position updated-child))))))]

    (when-not (and
                (map? tree)
                (node? (:root tree)))
      (throw
        (ex-info "Некорректное дерево."
                 {:tree tree})))

    (when-not (and
                (vector? path)
                (every? valid-path-step? path))
      (throw
        (ex-info "Некорректный путь."
                 {:path path})))

    (assoc tree :root
                (update-at (:root tree) path))))

(defn update-content
  "Изменяет содержимое указанного узла дерева.

   Принимает:
   tree — дерево, в котором необходимо изменить содержимое.
   path — путь к изменяемому узлу.
   content — новое содержимое узла в виде вектора.

   Возвращает:
   Новое дерево с изменённым содержимым узла.

   Выбрасывает исключение, если дерево, путь или содержимое некорректны либо узел не найден."
  [tree path content]
  (when-not (and
              (vector? content)
              (every? content-value? content))
    (throw
      (ex-info "Некорректное содержимое узла."
               {:content content})))

  (update-node-at-path
    tree
    path
    (fn [node]
      (assoc node 2 content))))

(defn update-attributes
  "Изменяет атрибуты указанного узла дерева.

   Принимает:
   tree — дерево, в котором необходимо изменить атрибуты.
   path — путь к изменяемому узлу.
   attributes — новый набор атрибутов в виде map.

   Возвращает:
   Новое дерево с изменёнными атрибутами узла.

   Выбрасывает исключение, если дерево, путь или атрибуты
   некорректны либо узел не найден."
  [tree path attributes]
  (when-not (attributes? attributes)
    (throw
      (ex-info "Некорректные атрибуты узла."
               {:attributes attributes})))

  (update-node-at-path
    tree
    path
    (fn [node]
      (assoc node 1 attributes))))

; Запись и чтение из файла с расширением .smc

(defn- valid-file-extension?
  [file-path]
  (and
    (string? file-path)
    (.endsWith file-path ".smc")))

(defn- valid-file-data?
  [data]
  (and
    (map? data)
    (= #{:format :version :root}
       (set (keys data)))
    (= format-name (:format data))
    (= format-version (:version data))
    (node? (:root data))))

(defn save-tree
  "Сохраняет дерево в файл формата SMC.

   Принимает:
   tree – дерево для сохранения.
   file-path – путь к файлу с расширением .smc.

   Возвращает:
   Путь к сохранённому файлу.

   Выбрасывает исключение, если дерево или расширение файла некорректны."
  [tree file-path]
  (when-not (and
              (map? tree)
              (= #{:root} (set (keys tree)))
              (node? (:root tree)))
    (throw
      (ex-info "Некорректное дерево."
               {:tree tree})))

  (when-not (valid-file-extension? file-path)
    (throw
      (ex-info "Файл должен иметь расширение .smc."
               {:file-path file-path})))

  (let [file-data
        {:format format-name
         :version format-version
         :root (:root tree)}]

    (spit file-path (pr-str file-data))
    file-path))

(defn load-tree
  "Загружает дерево из файла формата SMC.

   Принимает:
   file-path – путь к файлу с расширением .smc.

   Возвращает:
   Дерево в формате {:root node}.

   Выбрасывает исключение, если расширение файла, его формат, версия или структура некорректны."
  [file-path]
  (when-not (valid-file-extension? file-path)
    (throw
      (ex-info "Файл должен иметь расширение .smc."
               {:file-path file-path})))

  (let [file-data (edn/read-string (slurp file-path))]

    (when-not (valid-file-data? file-data)
      (throw
        (ex-info "Файл содержит некорректный формат SMC."
                 {:file-data file-data})))

    {:root (:root file-data)}))