(ns tiny-maze.solver
  (:require [clojure.core.async :as async]))

(def maze [[:S 0 1]
           [1 0 1]
           [1 0 :E]])

(def start-pos
  (let [finding-s (map #(.indexOf % :S) maze)
        y  (first (filter (complement neg?) finding-s))
        x (.indexOf finding-s y)]
    [x y])) ; [0 0]

(def step1 (assoc-in maze start-pos :x)) 

(defn next-moves [pos]
  (let [[x y] pos
      right [(+ x 1) y]
      up [x (- y 1)]
      down [x (+ y 1)]
      left [(- x 1) y]      
      moves (filter #(every? (fn [p] (<= 0 p)) %) [right up down left]) ]
    moves))

(next-moves start-pos)

(get-in maze start-pos) ; :S

(defn solve-maze [maze])

(solve-maze maze)

; desde la :S o un 0
; cambiar la :S por :x
; analizar 4 movimientos
; si encuentra la :E, cambiar la :E por :x y devolver el maze
; si encuentra un 0, avanzar!
; sino, terminar
