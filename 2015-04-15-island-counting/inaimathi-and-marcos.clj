(ns islands
  (:require [clojure.set :as set]))

(def island #{[0 3] [0 4] [0 5] [12 14]})

(defn island-count [island]
  (if (empty? island)
    0
    (+ 1 (island-count (sink-one island)))))

(defn sink-one [island]
  (if (empty? island)
    island
    (cave island (first island))))

(defn cave [island coord]
  (if (island coord)
    (let [removed (set/difference island #{coord})]
      (reduce 
       cave removed
       (set/intersection (neighbors coord) removed)))
    island))

(defn neighbors [[x y]]
  (set (for [_x [-1 0 1]
             _y [-1 0 1]
             :when (not (= [0 0] [_x _y]))]
         [(+ x _x) (+ y _y)])))
