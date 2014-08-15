(def +blinker+ #{[1 2] [2 2] [3 2]})
(def +glider+ #{[1 0] [2 1] [0 2] [1 2] [2 2]})
(def +gosper-glider-gun+ #{[24 0] [22 1] [24 1] [12 2] [13 2] [20 2] [21 2] [34 2] [35 2]
                           [11 3] [15 3] [20 3] [21 3] [34 3] [35 3] [0 4] [1 4] [10 4] [16 4] [20 4] [21 4] 
                           [0 5] [1 5] [10 5] [14 5] [16 5] [17 5] [22 5] [24 5] 
                           [10 6] [16 6] [24 6] [11 7] [15 7] [12 8] [13 8]})

(defn moore-neighborhood [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not (= [dx dy] [0 0]))]
    [(+ x dx) (+ y dy)]))

(defn step [set-of-cells]
  (set (for [[cell count] (frequencies (mapcat moore-neighborhood set-of-cells))
             :when (or (= 3 count)
                       (and (= 2 count) (contains? set-of-cells cell)))]
         cell)))

(defn print-world 
  ([set-of-cells] (print-world set-of-cells 10))
  ([set-of-cells world-size]
     (let [r (range 0 (+ 1 world-size))]
       (pprint (for [y r] (apply str (for [x r] (if (set-of-cells [x y]) \# \.))))))))

(defn run-life [world-size num-steps set-of-cells]
  (loop [s num-steps 
         cells set-of-cells]
    (print-world cells world-size)
    (when (< 0 s) 
      (recur (- s 1) (step cells)))))
