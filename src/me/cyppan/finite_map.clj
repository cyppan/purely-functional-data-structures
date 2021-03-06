(ns me.cyppan.finite-map
  ;; import the Ordered protocol and its implementations from tree-set
  (:require [me.cyppan.protocols.orderable :as orderable]))

;; The implementation is very similar to the tree-set (an unbalanced binary tree)

(comment
  (def fm (-> (new-finite-map :c "c")
              (bind :a "a")
              (bind :e "e")
              (bind :d "d")))
  (lookup fm :d)
  (lookup fm :f)
  (lookup (bind fm :f "f") :f)
  )

(defrecord FiniteMap [left key value right])

;; Key should implement the Ordered protocol
(defn new-finite-map [key value & {:keys [left right]}]
  (->FiniteMap left key value right))

(defn lookup
  "lookups the value at the key if any"
  ([tree key-to-lookup]
   (lookup tree key-to-lookup nil nil))
  ([{:keys [left key value right] :as tree} key-to-lookup key-candidate value-candidate]
   (if (orderable/lte key-to-lookup key)
     (if left
       (lookup left key-to-lookup key-candidate value-candidate)
       (cond
         (and key-candidate (orderable/eq key-candidate key-to-lookup)) value-candidate
         (orderable/eq key key-to-lookup) value))
     ; else key-to-lookup > key
     (if right
       (lookup right key-to-lookup key-candidate value-candidate)
       (when (and key-candidate (orderable/eq key-candidate key-to-lookup))
         value-candidate)))))

(defn bind [tree key-to-bind value-to-bind]
  (if (= value-to-bind (lookup tree key-to-bind))
    tree
    (letfn [(helper [{:keys [left key right] :as tree} key-to-bind value-to-bind]
              (if (nil? tree)
                (new-finite-map key-to-bind value-to-bind)
                (if (orderable/lt key-to-bind key)
                  (assoc tree :left (helper left key-to-bind value-to-bind))
                  (assoc tree :right (helper right key-to-bind value-to-bind)))))]
      (helper tree key-to-bind value-to-bind))))
