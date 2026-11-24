(ns tree-data.query
  "Запросы к узлам документа SMC."
  (:require [clojure.string :as str]
            [tree-data.core :as tree]))

(defrecord Query [absolute? steps])

(def ^:private name-pattern #"[\p{L}\p{N}_:][\p{L}\p{M}\p{N}_.:-]*")

;; 'O''Brien' в запросе читается как O'Brien.
(def ^:private string-pattern
  #"(?x) '(?: [^']++ | '' )*+'
       | \"(?: [^\"]++ | \"\" )*+\"")

(defn- at-end? [{:keys [source offset]}]
       (= @offset (count source)))

(defn- consume! [{:keys [source offset]} pattern]
       (let [matcher (re-matcher pattern source)]
            (.region matcher @offset (count source))
            (when (.lookingAt matcher)
                  (vreset! offset (.end matcher))
                  (.group matcher))))

(defn- take-token! [cursor pattern]
       (when-let [token (consume! cursor pattern)]
                 (consume! cursor #"\s*")
                 token))

(defn- syntax-error! [{:keys [source offset]} expected]
       (let [position @offset]
            (throw (ex-info (str "Позиция " position ". Ожидается " expected ".")
                            {:query source :offset position :expected expected}))))

(defn- expect-token! [cursor pattern expected]
       (or (take-token! cursor pattern) (syntax-error! cursor expected)))

(defn- take-literal! [cursor]
       (when-let [token (take-token! cursor string-pattern)]
                 (let [delimiter (subs token 0 1)
                       content (subs token 1 (dec (count token)))]
                      (str/replace content (str delimiter delimiter) delimiter))))

(defn- read-name! [cursor]
       (or (take-token! cursor name-pattern)
           (take-literal! cursor)
           (syntax-error! cursor "имя")))

(defn- read-value! [cursor]
       (or (take-literal! cursor) (syntax-error! cursor "строка в кавычках")))

(defn- read-axis! [cursor]
       (case (expect-token! cursor #"//?" "/ или //")
             "/"  :child
             "//" :descendant))

(defn- parse-condition! [cursor]
       (let [condition
             (cond
               (take-token! cursor #"@")
               (let [attribute (read-name! cursor)]
                    (if (take-token! cursor #"=")
                      [:attr= attribute (read-value! cursor)]
                      [:has attribute]))

               (take-token! cursor #"text\(\)")
               (do
                 (expect-token! cursor #"=" "знак =")
                 [:text= (read-value! cursor)])

               :else (syntax-error! cursor "атрибут или text()"))]
            (expect-token! cursor #"\]" "закрывающая скобка ]")
            condition))

(defn- parse-step! [cursor axis]
       (let [node-name (if (take-token! cursor #"\*") :* (read-name! cursor))]
            (loop [conditions []]
                  (if (take-token! cursor #"\[")
                    (recur (conj conditions (parse-condition! cursor)))
                    {:axis axis :name node-name :conditions conditions}))))

(defn- parse-steps! [cursor first-axis]
       (loop [steps [] axis first-axis]
             (let [parsed (conj steps (parse-step! cursor axis))]
                  (if (at-end? cursor)
                    parsed
                    (recur parsed (read-axis! cursor))))))

(defn parse-query
      "Разбирает строку запроса.

       Принимает:
       source — абсолютный или относительный путь, например /root//item или .//item.

       Возвращает:
       Query, который можно передавать в select-nodes и update-nodes.

       При синтаксической ошибке выбрасывает ExceptionInfo.
       Поле :offset в (ex-data e) содержит позицию ошибки в строке, начиная с нуля."
      [source]
      (when-not (string? source)
                (throw (ex-info "Запрос должен быть строкой." {:query source})))
      (let [cursor {:source source :offset (volatile! 0)}]
           (consume! cursor #"\s*")
           (case (take-token! cursor #"//?|\.")
                 "."  (if (at-end? cursor)
                        (->Query false [])
                        (->Query false (parse-steps! cursor (read-axis! cursor))))
                 "/"  (if (at-end? cursor)
                        (->Query true [])
                        (->Query true (parse-steps! cursor :child)))
                 "//" (->Query true (parse-steps! cursor :descendant))
                 (->Query false (parse-steps! cursor :child)))))

(defn- matches-step? [node {:keys [name conditions]}]
       (and (or (= :* name) (= name (tree/node-name node)))
            (every? (fn [[op k v]]
                        (case op
                              :has   (contains? (tree/node-attributes node) k)
                              :attr= (= (get (tree/node-attributes node) k) v)
                              :text= (some #(= k %) (tree/node-content node))))
                    conditions)))

(defn- children [[path node]]
       ;; В содержимом ["текст" ["p" {} []]] у узла p индекс 1.
       (into [] (keep-indexed (fn [i item]
                                  (when (tree/node-shape? item)
                                        [(conj path i) item])))
             (tree/node-content node)))

(defn- context-step [step]
       (cond
         (string? step) [step 1]
         (and (vector? step) (= 2 (count step)) (string? (first step))
              (integer? (second step)) (pos? (second step))) step
         :else (throw (ex-info "Некорректный шаг пути контекста." {:step step}))))

(defn- context-location [root context]
       (reduce (fn [loc [name ordinal]]
                   (let [siblings (filter #(= name (tree/node-name (second %))) (children loc))]
                        (or (first (drop (dec ordinal) siblings)) (reduced nil))))
               [[] root] (mapv context-step context)))

(defn- advance
       "Продвигает множество активных шагов (индексов в steps) на текущем узле.
        Возвращает [hit? next-active]: попал ли узел в результат и какие шаги
        проверять на его детях (шаги // остаются активными на всей глубине)."
       [steps node active]
       (reduce (fn [[hit? next-active] i]
                   (let [step (nth steps i)
                         carry (cond-> next-active (= :descendant (:axis step)) (conj i))]
                        (cond
                          (not (matches-step? node step)) [hit? carry]
                          (= (inc i) (count steps))       [true carry]
                          :else                           [hit? (conj carry (inc i))])))
               [false #{}] active))

(defn- push-children [stack loc active]
       (reduce (fn [s child] (conj s [child active])) stack (rseq (children loc))))

(defn- locations [doc context expression]
       (when-not (vector? context)
                 (throw (ex-info "Путь контекста должен быть вектором." {:context context})))
       (let [{:keys [absolute? steps]} (if (instance? Query expression)
                                         expression (parse-query expression))
             root (:root doc)]
            (when-not (tree/node? root)
                      (throw (ex-info "Документ должен содержать корректный узел :root." {})))
            (let [origin (if absolute? [[] root] (context-location root context))]
                 (cond
                   (nil? origin) []
                   (empty? steps) [origin]
                   :else
                   (loop [stack (if absolute? [[origin #{0}]] (push-children [] origin #{0}))
                          found []]
                         (if (empty? stack)
                           found
                           (let [[loc active] (peek stack)
                                 [hit? next-active] (advance steps (second loc) active)
                                 todo (pop stack)]
                                (recur (if (seq next-active) (push-children todo loc next-active) todo)
                                       (cond-> found hit? (conj loc))))))))))

(defn select-nodes
      "Находит узлы документа по запросу.

       Принимает:
       doc — документ с корневым узлом в :root.
       context — путь от корня к узлу, от которого начинается относительный запрос.
                 По умолчанию [] — корень. Строка в пути выбирает первого ребёнка
                 с таким именем. Шаг [имя номер] выбирает ребёнка среди одноимённых,
                 считая с единицы. Например, [[\"section\" 2]] — вторая секция корня.
       expression — строка запроса или результат parse-query.
                    Абсолютный запрос начинается от корня при любом context.

       Возвращает:
       Вектор узлов в порядке документа. Если совпадений нет, возвращает []."
      ([doc expression] (select-nodes doc [] expression))
      ([doc context expression]
       (mapv second (locations doc context expression))))

(defn- edit-at [root path f]
       (let [[target parents]
             (reduce (fn [[node parents] i]
                         [(nth (tree/node-content node) i) (conj parents [node i])])
                     [root []] path)
             replacement (f target)]
            (when (and (nil? replacement) (empty? path))
                  (throw (ex-info "Удаление корня недопустимо." {:path path})))
            (when-not (or (nil? replacement) (tree/node? replacement))
                      (throw (ex-info "Функция изменения должна вернуть узел или nil." {:path path})))
            (reduce (fn [child [parent i]]
                        (let [content (tree/node-content parent)]
                             (assoc parent 2 (if (nil? child)
                                               (into (subvec content 0 i) (subvec content (inc i)))
                                               (assoc content i child)))))
                    replacement (rseq parents))))

(defn update-nodes
      "Изменяет узлы документа, найденные по запросу.

       Принимает:
       doc — документ с корневым узлом в :root.
       context — путь для относительного запроса, как в select-nodes. По умолчанию [].
       expression — строка запроса или результат parse-query.
       f — функция от одного узла. Возвращает узел для замены или nil для удаления.

       Возвращает:
       Документ с внесёнными изменениями. Если совпадений нет, возвращает исходный doc.

       Если выбраны и родитель, и потомок, f получает родителя после изменения потомка.
       Выбрасывает ExceptionInfo при удалении корня или если f вернула что-либо,
       кроме корректного узла или nil."
      ([doc expression f] (update-nodes doc [] expression f))
      ([doc context expression f]
       (when-not (ifn? f)
                 (throw (ex-info "Ожидается функция изменения узла." {})))
       ;; Идём справа налево, чтобы при удалении не съехали индексы.
       (reduce (fn [result [path _]] (update result :root edit-at path f))
               doc (rseq (locations doc context expression)))))