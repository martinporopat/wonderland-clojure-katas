(ns doublets.solver
  (:require [clojure.java.io :as io]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

; counts differences between 2 words
(defn differences [word1 word2]
  (let [extra (Math/abs (- (count word1) (count word2)))
        comparable (map #(= %1 %2) word1 word2)]
    (+ extra (reduce
          (fn [dif w] (if (false? w) (inc dif) dif))
          0
          comparable))))

; brings words with only 1 letter change (excludes a list of words previously defined)
(defn mutate
  ([word] (mutate word []))
  ([word excludes]
   (remove (fn [x] (some #(= x %) excludes))
            (reduce (fn [r w] (if (= (differences word w) 1) (conj r w) r))
                    #{}
                    words))))

(defn doublets
  ([word1 word2] (doublets word1 word2 (into [] (vector word1))))
  ([word1 word2 solver]
   (if (= word2 (last solver))
     solver
     (let [n (first (for [mutated-word (mutate (last solver) solver)
                          :let [l (doublets word1 word2 (into solver (vector mutated-word)))]
                          :when (not (empty? l))]
                      l))]
       (if (nil? n) [] n)))))
