(ns me.cyppan.stack
  (:refer-clojure :exclude [update])
  (:import (clojure.lang IPersistentList)))


;; Usage
;;-------

(comment
  (dump
    (++
      (list 1 2)
      (list 3 4)))
  ;; => 1 2 3 4

  (dump
    (++
      (-> (new-custom-stack)
          (add 2)
          (add 1))
      (-> (new-custom-stack)
          (add 4)
          (add 3))))
  ;; => 1 2 3 4

  (dump
    (-> (list 1 2 3 4 5)
        (update 0 11)
        (update 4 55)))
  ;; => 11 2 3 4 55

  (doseq [suffix (suffixes (list 1 2 3 4))]
    (dump suffix))
  ;; =>
  ;; 1 2 3 4
  ;; 2 3 4
  ;; 3 4
  ;; 4
  ;;
  )


;;Types structures
;;------------

(defprotocol Stack
  (is-empty [this] "is this stack empty?")
  ;; can't use cons here, not sure what fails exactly
  (add [this el] "push an element on the stack")
  (head [this] "get the head of the stack")
  (tail [this] "get the tail of the stack"))

;; a custom stack implementation (still LIFO)
(defrecord CustomStack [h t]
  Stack
  (is-empty [_] (nil? h))
  (add [this el] (CustomStack. el this))
  (head [_] h)
  (tail [_] (or t (CustomStack. nil nil))))

(defn new-custom-stack []
  (CustomStack. nil nil))

;; making the Clojure list implement the Stack protocol
(extend-type IPersistentList
  Stack
  (is-empty [this] (empty? this))
  (add [this el] (conj this el))
  (head [[h & _]] h)
  (tail [[_ & t]] (or t (list))))


;; Stack API (implementation-independent)
;;-----------

(defn ++
  "concatenate two stacks"
  [a b]
  (if (is-empty a)
    b
    (add (++ (tail a) b) (head a))))

(defn update
  "update the stack replacing the i element"
  [s i el]
  (if (zero? i)
    (add (tail s) el)
    (add (update (tail s) (dec i) el) (head s))))

(defn suffixes
  "Given a stack, returns a sequence of all the stack suffixes in decreasing order
  Ex: 1 2 3 4 => 1 2 3 4, 2 3 4, 3 4, 4, ()
  O(n) in time and space thanks to data sharing"
  [s]
  (letfn [(helper [s res]
            (if (is-empty s)
              (conj res s)
              (recur (tail s) (conj res s))))]
    (helper s [])))

(defn dump
  "dequeue the stack printing elements along the way"
  [s]
  (if-not (is-empty s)
    (do (print (head s) " ")
        (recur (tail s)))
    (print "\n")))
