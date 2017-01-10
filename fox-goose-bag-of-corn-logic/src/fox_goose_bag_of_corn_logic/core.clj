(ns fox-goose-bag-of-corn-logic.core)

(use 'clojure.core.logic)

(defne not-membero [x l]
  ([_ []])
  ([_ [?y . ?r]]
    (!= x ?y)
    (not-membero x ?r)))

(defne move [state after]
  ; Move from Left to center with sth
  ([[[a b c :you] [:boat] t]
    [[a b] [:boat :you c] t]] (== [a b] [:corn :fox]))
  ([[[a b c :you] [:boat] t]
    [[a c] [:boat :you b] t]] (== [a c] [:corn :fox]))
  ([[[a b c :you] [:boat] t]
    [[b c] [:boat :you a] t]] (== [b c] [:corn :fox]))
  ([[[a b :you] [:boat] t]
    [[a] [:boat :you b] t]])
  ([[[a b :you] [:boat] t]
    [[b] [:boat :you a] t]])
  ([[[a :you] [:boat] t]
    [[] [:boat :you a] t]])
  ; Move from Left to Center alone
  ([[[a b :you] [:boat] t]
    [[a b] [:boat :you] t]]  (== [a b] [:corn :fox]))
  ([[[a :you] [:boat] t]
    [[a] [:boat :you] t]])
  ; Move from Right to center with sth
  ([[t [:boat] [a b :you]]
    [t [:boat :you a] [b]]])
  ([[t [:boat] [a b :you]]
    [t [:boat :you b] [a]]])
  ([[t [:boat] [a :you]]
    [t [:boat :you a] []]])
  ; Move from Right to Center alone
  ([[t [:boat] [a b :you]]
    [t [:boat :you] [a b]]] (== [a b] [:corn :fox]))
  ([[t [:boat] [a :you]]
    [t [:boat :you] [a]]])
  ; Move from Center to Left with sth
  ([[[b] [:boat :you a] u]
    [[b a :you] [:boat] u]])
  ([[[] [:boat :you a] u]
    [[a :you] [:boat] u]])
  ([[[b] [:boat :you a] u]
    [[b a :you] [:boat] u]])
  ; Move from Center to Left alone  
  ([[[a b] [:boat :you] u]
    [[a b :you] [:boat] u]])
  ([[[a] [:boat :you] u]
    [[a :you] [:boat] u]])
  ([[[] [:boat :you] u]
    [[:you] [:boat] u]])
  ; Move from Center to Right with sth
  ([[u [:boat :you a] [b]]
    [u [:boat] [b a :you]]])
  ([[u [:boat :you a] []]
    [u [:boat] [a :you]]])
  ([[u [:boat :you a] [b]]
    [u [:boat] [b a :you]]])
  ([[u [:boat :you a] [b c]]
    [u [:boat] [b c a :you]]])
  ; Move from Center to Right alone
  ([[u [:boat :you] [a b]]
    [u [:boat] [a b :you]]])
  ([[u [:boat :you] [a]]
    [u [:boat] [a :you]]])
  ([[u [:boat :you] []]
    [u [:boat] [:you]]])
  )
  
(defne river-crossing-plan [state out plan]
  ([[[] [:boat] [_ _ _ :you]] plan])
  ([[_ _ _] _] (fresh [next-state new-plan]
                 (move state next-state)
                 (not-membero next-state plan)
                 (appendo plan [next-state] new-plan)
                 (river-crossing-plan next-state out new-plan))))

; You must get the fox, goose, and bag of corn safely across the other side of the river
(def start-step [[:corn :goose :fox :you] [:boat] []])

(let [a (run* [q] (river-crossing-plan start-step q [start-step]))]
  a)
