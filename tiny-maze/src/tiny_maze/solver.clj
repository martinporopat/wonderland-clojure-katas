(ns tiny-maze.solver)

(defn start-pos [maze]
  (let [finding-s (map #(.indexOf % :S) maze)
        y  (first (filter (complement neg?) finding-s))
        x (.indexOf finding-s y)]
    [x y])) ; [0 0]

; Reciben la posicion [x y]
(defn can-walk? [a maze]
  (let [[x y] a
        value ((maze x) y)]
    (or (= value :E)
        (and (number? value) (zero? value)))))

(defn is-end? [a maze]
  (let [[x y] a
        value ((maze x) y)]
    (= value :E)))

(defn goes-outbounds? [p maze]
  (and (>= (- (count maze) 1) p) (<= 0 p)))

(defn next-moves [pos previous-pos maze]
  (let [[x y] pos
        right [(+ x 1) y]
        up [x (- y 1)]
        down [x (+ y 1)]
        left [(- x 1) y]      
        moves (filter #(can-walk? % maze)
                      (filter (fn [a] (every? #(goes-outbounds? % maze) a))
                       [right up down left]))]
    (filter #(not= previous-pos %) moves)))

(defn convert-to-x [a maze-upd]
  (let [[x y] a]
    (assoc-in maze-upd [x y] :x)))

(defn solve-maze-h [maze pos previous-pos]
  (let [next-positions (remove nil? pos)]
     (for [i next-positions]
       (if (is-end? i maze)
         (convert-to-x i maze)
         (first (remove empty?
                        (solve-maze-h
                         (convert-to-x i maze)
                         (next-moves i previous-pos maze)
                         i)))))))

(defn solve-maze [maze-init]
  (first (solve-maze-h maze-init [(start-pos maze-init)] [0 0])))
