(ns magic-square.puzzle)

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

                                        ; Casos column:   [0 3 6] [1 4 7] [2 5 8]
                                        ; Casos filas:    [0 1 2] [3 4 5] [6 7 8]
                                        ; Casos diagonal: [0 4 8] [2 4 6]

(defn super+ [a b c col]
  (reduce + [(get col a) (get col b) (get col c)]))  

(defn magic-square [values]
  (let [c (shuffle values)
        magic-number (super+ 0 3 6 c)]
    (if (and
         (= (super+ 1 4 7 c) magic-number)
         (= (super+ 2 5 8 c) magic-number)
         (= (super+ 0 1 2 c) magic-number)
         (= (super+ 3 4 5 c) magic-number)
         (= (super+ 6 7 8 c) magic-number)
         (= (super+ 0 4 8 c) magic-number)
         (= (super+ 2 4 6 c) magic-number)
         )
      [[(get c 0) (get c 1) (get c 2)] [(get c 3) (get c 4) (get c 5)] [(get c 6) (get c 7) (get c 8)]]
      (recur c))))

(magic-square values)
