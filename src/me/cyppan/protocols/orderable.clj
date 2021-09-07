(ns me.cyppan.protocols.orderable
  (:import (clojure.lang Keyword)))

(defprotocol Orderable
  (lt [a b] "true if a < b")
  (eq [a b] "true if a == b")
  (lte [a b] "true if a <= b"))

(extend-protocol Orderable
  Long
  (eq [a b] (= a b))
  (lt [a b] (< a b))
  (lte [a b] (<= a b))
  Keyword
  ;; using lexicographic ordering
  (eq [a b] (zero? (compare (str a) (str b))))
  (lt [a b] (neg? (compare (str a) (str b))))
  (lte [a b] (<= (compare (str a) (str b)) 0)))

