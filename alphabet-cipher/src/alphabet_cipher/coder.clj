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

(defn cycleKeyword [keyword message]
  (apply str (take (count message) (cycle keyword)))
  )

(defn readAbc [f a b]
  (apply str
         (map f a b)
         )
  )

(defn encode [keyword message]
  (let [keywordcycled (cycleKeyword keyword message)]
    (readAbc #(get (getline (str %2)) (str/index-of abc %1))
             keywordcycled
             message
             )
    )
  )

(encode "scones" "meetmebythetree")
           
(defn decode [keyword message]
  (let [keywordcycled (cycleKeyword keyword message)]
    (readAbc #(get abc (str/index-of (getline (str %1)) %2))
             keywordcycled
             message
             )
    )
  )

(decode "scones" "egsgqwtahuiljgs")

(defn decipher [cipher message]
  "decypherme")
