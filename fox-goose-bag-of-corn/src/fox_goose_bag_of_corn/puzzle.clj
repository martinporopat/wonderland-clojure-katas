(ns fox-goose-bag-of-corn.puzzle
    (:require [clojure.test :refer :all]
              [clojure.set]
              ))

; You must get the fox, goose, and bag of corn safely across the other side of the river
(def start-step [#{:goose :corn :fox :you} #{:boat} #{}])
(def final-step [#{} #{:boat} #{:goose :corn :fox :you}])
(def movable-objects #{:goose :fox :corn :you})

;; ---------------------------------------------------
;; UTILITIES
;; ---------------------------------------------------

; are you here?
(defn are-you? [a]
  (contains? a :you))

; Get position of :you
(defn where-are-you? [step]
  (cond
    (are-you? (get step 0)) 0
    (are-you? (get step 1)) 1
    (are-you? (get step 2)) 2))

; whose with you?
(defn with-you [step]
  (first (filter are-you? step)))

; posibilities to move
(defn with-you-list [step]
  (for [x (with-you step)
        :when (and (not= x :you) (not= x :boat))]
    x))

; Movements
(defn move-you [step direction with]
   (if (= :you with)
     (case (where-are-you? step) ; Move you alone
       0 [(disj (first step) :you)
          (conj (second step) :you)
          (last step)]
       1 (cond
           (= direction "right")
           [(first step)
            (disj (second step) :you)
            (conj (last step) :you)]
           (= direction "left")
           [(conj (first step) :you)
            (disj (second step) :you)
            (last step)])
       2 [(first step)
          (conj (second step) :you)
          (disj (last step) :you)])
     (case (where-are-you? step) ; Move with something
       0 [(disj (first step) :you with)
          (conj (second step) :you with)
          (last step)]
       1 (cond
           (= direction "right")
           [(first step)
            (disj (second step) :you with)
            (conj (last step) :you with)]
           (= direction "left")
           [(conj (first step) :you with)
            (disj (second step) :you with)
            (last step)])
       2 [(first step)
          (conj (second step) :you with)
          (disj (last step) :you with)])))

(defn get-possible-directions [step]
  (case (where-are-you? step)
    0 ["right"]
    1 ["left" "right"]
    2 ["left"]
    nil nil))

(defn get-possible-moves [step]
  (let [movable (clojure.set/intersection 
                 (with-you step)
                 movable-objects) ; col of objects to move
        possible-moves (get-possible-directions step) ; Directions to move
        next-moves (for [direction possible-moves
                         object movable]
                     (move-you step direction object))]
    next-moves))

;; ---------------------------------------------------
;; VALIDATIONS
;; ---------------------------------------------------

; You can only carry 1 item on the boat across with you.
(defn validate-boat [[_ b _]]
  (if (are-you? b) ; if you're on the boat
    (< (count b) 4) ; only one item with you and the boat
    (if (> (count b) 1) false true)))

; The fox cannot be left alone with the goose, (or it will be eaten).
; The goose cannot be left alone with the corn, (or it will be eaten).
(defn validate-eaten [step]
  (letfn [(validate-invalid [c a]
          (some true? (map #(= c (sort %)) a)))]
    (not (or
          (validate-invalid [:fox :goose] step)
          (validate-invalid [:corn :goose] step)
          (validate-invalid [:corn :fox :goose] step)))))

; Step was not previously used
(defn validate-historic [step historic]
  (not (some #(= step %) historic)))

; Run all validations for a step. Return step or nil.
(defn validate-step [step historic]
  (if (and
       (validate-boat step)
       (validate-eaten step)
       (validate-historic step historic))
    step
    nil))

(defn get-next-move [h]
  (filter
   #(validate-step % h)
   (get-possible-moves (last h))))

(defn river-crossing-plan
  ([] (river-crossing-plan start-step []))
  ([step historic]
   (let [h (conj historic step)]
   (if (= step final-step)
     h
    (recur (first (get-next-move h)) h))
     )))
       
(river-crossing-plan)
