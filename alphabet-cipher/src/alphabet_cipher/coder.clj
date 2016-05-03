(ns alphabet-cipher.coder)
(require '[clojure.string :as str])

(def abc "abcdefghijklmnopqrstuvwxyz")

(defn getline [letter]
  (subs
   (apply str
          (take
           (* 2 (count abc))
           (cycle abc))
          )
   (str/index-of abc letter)
   (+ (str/index-of abc letter) (count abc)))
  )

(defn encode [keyword message]
  (let [keywordcycled (apply str (take (count message) (cycle keyword)))]
    (apply str
           (map
            #(get (getline (str %2)) (str/index-of abc %1))
            keywordcycled
            message)
           )
    )
  )

(encode "scones" "meetmebythetree")

(defn decode [keyword message]
  "decodeme")

(defn decipher [cipher message]
  "decypherme")
