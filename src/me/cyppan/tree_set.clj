(ns me.cyppan.tree-set
  (:require [clojure.pprint :as pprint]
            [clojure.walk :as walk]))

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

;; ordering protocol used by the binary tree
(defprotocol Ordered
  (lt [a b] "true if a < b")
  (eq [a b] "true if a == b")
  (lte [a b] "true if a <= b"))

(extend-protocol Ordered
  Long
  (eq [a b] (= a b))
  (lt [a b] (< a b))
  (lte [a b] (<= a b)))

(defrecord TreeSet [left value right])

(defn new-tree-set [value & {:keys [left right]}]
  (->TreeSet left value right))

(defn dump [tree]
  (pprint/pprint
    (walk/postwalk (fn [form]
                     (if (instance? TreeSet form)
                       (into {} form)
                       form))
                   tree)))

(defn member?
  "true if el is contained in this tree
  instead of doing different comparisons with eq and < at each tree node,
  we use the <= operator to reduce the total number of operations (worst case)
  from 2*treeDepth to treeDepth + 1"
  ([tree el]
   (member? tree el nil))
  ([{:keys [left value right] :as tree} el eq-candidate]
   (if (lte el value)
     (if left
       (member? left el value)
       (or (eq el value)
           (eq el eq-candidate)))
     ; else el > value
     (if right
       (member? right el)
       (eq el eq-candidate)))))

(defn insert
  "returns a new tree with the el element inserted
  skip if el already is in the tree"
  [tree el]
  (if (member? tree el)
    tree
    (letfn [(helper [{:keys [left value right] :as tree} el]
              (if (nil? tree)
                (new-tree-set el)
                (if (lt el value)
                  (assoc tree :left (helper left el))
                  (assoc tree :right (helper right el)))))]
      (helper tree el))))
