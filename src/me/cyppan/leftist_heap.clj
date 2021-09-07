(ns me.cyppan.leftist-heap
  (:require [me.cyppan.protocols.orderable :as orderable]))

(comment
  (def h (-> (new-leftist-heap 1000)
             (insert 1)
             (insert 800)
             (insert 500)))
  (find-min h)
  ;; => 1
  (find-min (delete-min h))
  ;; => 500
  )

(defrecord LeftistHeap [r x a b])

(defn rank [{:keys [r] :as h}]
  (or r 0))

(defn new-leftist-heap
  ([x]
   (map->LeftistHeap {:r 1 :x x}))
  ([x a b]
   (if (<= (rank a) (rank b))
     (->LeftistHeap (inc (rank b)) x a b)
     (->LeftistHeap (inc (rank a)) x b a))))

(defn merge [h1 h2]
  (cond
    (nil? (:x h1)) h2
    (nil? (:x h2)) h1
    :else (if (orderable/lte (:x h1) (:x h2))
            (new-leftist-heap (:x h1) (:a h1) (merge (:b h1) h2))
            (new-leftist-heap (:x h2) (:a h2) (merge (:b h2) h1)))))

(defn insert [h x]
  (merge (new-leftist-heap x) h))

(defn find-min [{:keys [x]}]
  x)

(defn delete-min [{:keys [a b]}]
  (merge a b))