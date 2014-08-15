;; poker-hands.clj

(ns poker-hands.core
  (:use [clojure.string :only [split]]))

(def rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14})
(def name-map ["Rules for Draw and Stud Poker" "Ace" "Two" "Three" "Four" "Five" "Six" "Seven" "Eight" "Nine" "Ten" "Jack" "Queen" "King" "Ace"])
(def suit-map {\H :hearts \C :clubs \S :spades \D :diamonds})
(def hand-map {:straight-flush 8 :four-of-a-kind 7 :full-house 6 :flush 5 :straight 4 :three-of-a-kind 3 :two-pairs 2 :pair 1 :high-card 0})

(defn read-card [card-string]
  (let [rank (or (get rank-map (first card-string)) (read-string (subs card-string 0 1)))
        suit (get suit-map (second card-string))
        name (get name-map rank)]
    {:rank rank :suit suit :name name}))

(defn read-hand [hand-string]
  (sort-by :rank (map read-card (split hand-string #" "))))

(defn flush? [cards]
  (= 1 (count (group-by :suit cards))))

(defn straight? [cards]
  (let [ranks (map :rank cards)]
    (= ranks (range (first ranks) (+ 1 (last ranks))))))

(defn group-of? [n sets]
  (some #(= (count (second %1)) n) sets))

(def four-of-a-kind? (partial group-of? 4))
(def three-of-a-kind? (partial group-of? 3))
(def pair? (partial group-of? 2))

(defn count-sets-of [n sets]
  (count (filter #(= (count (second %1)) n) sets)))

(defn hand-type [hand]
  (let [sets (group-by :rank hand)]
    (cond (and (straight? hand) (flush? hand)) :straight-flush
          (four-of-a-kind? sets) :four-of-a-kind
          (and (three-of-a-kind? sets) (pair? 2 sets)) :full-house
          (flush? hand) :flush
          (straight? hand) :straight
          (three-of-a-kind? sets) :three-of-a-kind
          (= 2 (count-sets-of 2 sets)) :two-pairs
          (pair? sets) :pair
          :else :high-card)))

(defn break-tie [hand-a hand-b] true)

(defn hand-> [hand-a hand-b]
  (let [type-a (hand-type hand-a)
        type-b (hand-type hand-b)]
    (or (apply > (map #(get hand-map %) [type-a type-b]))
        (when (= type-a type-b)
          (break-tie hand-a hand-b)))))
