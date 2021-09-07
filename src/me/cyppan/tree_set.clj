(ns me.cyppan.tree-set
  (:require [clojure.pprint :as pprint]
            [clojure.walk :as walk]
            [me.cyppan.protocols.orderable :as orderable]))

;; Usage
(comment
  (def t (-> (new-tree-set 5)
             (insert 3)
             (insert 7)
             (insert 10)
             (insert 8)))
  (dump t)
  (member? t 10)
  (member? t 9)
  )

(defrecord TreeSet [left value right])

(defn new-tree-set [value & {:keys [left right]}]
  (->TreeSet left value right))

(defn dump
  "pretty print the tree as a recursive map"
  [tree]
  (pprint/pprint
    (walk/postwalk (fn [form]
                     (if (instance? TreeSet form)
                       (into {} form)
                       form))
                   tree)))

(defn member?
  "true if el is contained in this tree
  instead of doing different comparisons with eq and < at each tree node,
  we use the <= (leq) operator to reduce the total number of operations (worst case)
  from 2*treeDepth to treeDepth + 1"
  ([tree el]
   (member? tree el nil))
  ([{:keys [left value right] :as tree} el eq-candidate]
   (if (orderable/lte el value)
     (if left
       (member? left el value)
       (or (orderable/eq el value)
           (orderable/eq el eq-candidate)))
     ; else el > value
     (if right
       (member? right el)
       (orderable/eq el eq-candidate)))))

(defn insert
  "returns a new tree with the el element inserted
  skip if el already is in the tree"
  [tree el]
  (if (member? tree el)
    ;; avoid copying
    tree
    (letfn [(helper [{:keys [left value right] :as tree} el]
              (if (nil? tree)
                (new-tree-set el)
                (if (orderable/lt el value)
                  (assoc tree :left (helper left el))
                  (assoc tree :right (helper right el)))))]
      (helper tree el))))
