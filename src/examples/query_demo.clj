(require '[tree-data.core :as tree]
         '[tree-data.query :as q])

(def doc
  (tree/create-tree "root" {}
                    [["section" {"active" "yes"}
                      [["group" {} [["item" {"id" "1"} []]]]]]
                     ["section" {"active" "no"}
                      [["item" {"id" "2"} []]]]]))

(def path "/root/section[@active='yes']//item")

(println "Найденные узлы:")
(prn (q/select-nodes doc path))
;; [["item" {"id" "1"} []]]

(def edited
  (q/update-nodes doc path #(assoc-in % [1 "status"] "done")))

(println "Узлы после изменения:")
(prn (q/select-nodes edited "//item"))
;; [["item" {"id" "1", "status" "done"} []] ["item" {"id" "2"} []]]