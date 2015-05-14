(ns anagrams
  (:require [clojure.string :as str]))

(def +dict+ 
  (str/split (slurp "/usr/share/dict/american-english") #"\n"))

;; Options:
;; - Map of { letter count }
;;	- Easy membership checks for characters, and removal is something like `reduce decf`
;;	- Word-list words can be used as raw strings
;; - 26-element vector of counts
;;	- Easy membership check for words
;;	- Fast word removal
;; 	- Needs word-list words to be converted to vectors
;; - Sorted array of characters
;;	- Simple

;; We're going with the 26-element vector for reasons.

(defn decode [input]
  (reduce 
   (fn [memo chr]
     (let [ix (- (int chr) 65)]
       (if (>= 25 ix 0)
           (assoc memo ix (inc (get memo ix)))
           memo)))
   (apply vector (repeat 26 0))
   (str/upper-case input)))

;; Example input: "Schoolmaster"

(defn fits? [a b]
  (every? true? (map <= a b)))

(defn anagramify-internal [vector dictionary]
  (loop [vec vector
         words dictionary
         acc []
         an-acc []]
    (prn vec (first words) acc an-acc)
    (prn "===============")
    (if (empty? words)
      (conj an-acc acc)
      (let [wd (decode (first words))]
        (cond (every? zero? vec) (conj an-acc acc)
              (fits? wd vec) (recur (map - vec wd) (rest words) (conj acc (first words)) an-acc)
              :else (recur vec (rest words) acc))))))

(defn anagramify [string]
  (anagramify-internal (decode string) +dict+))

;; (defn anagramify [string]
;;   (loop [vec (decode string)
;;          words +dict+
;;          acc []]
;;     (if (empty? words)
;;       acc
;;       (let [wd (decode (first words))]
;;         (cond (every? zero? vec) acc
;;               (fits? wd vec) (recur (map - vec wd) (rest words) (conj acc (first words)))
;;               :else (recur vec (rest words) acc))))))
