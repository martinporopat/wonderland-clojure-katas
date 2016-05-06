(ns wonderland-number.finder)

(defn d-numbers [n]
  (sort (distinct (str n)))
  )

(defn wonderland-number []
  (loop [x 100000]
    (if (and
         (= (d-numbers x) (d-numbers (* x 2)))
         (= (d-numbers x) (d-numbers (* x 3)))
         (= (d-numbers x) (d-numbers (* x 4)))
         (= (d-numbers x) (d-numbers (* x 5)))
         (= (d-numbers x) (d-numbers (* x 6)))
         )
      x
      (recur (inc x))
      )
    )
  )


(wonderland-number)

;  - It has six digits
;  - If you multiply it by 2,3,4,5, or 6, the resulting number has all
;  the same digits in at as the original number.  The only difference
;  is the position that they are in.
