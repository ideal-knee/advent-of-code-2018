(import [re [match]]
        [sets [Set]] )

(defn load-steps []
  (-> "/data/day-10-input.txt"
    open
    (->>
      (map (fn [line]
             (setv [p-x p-y v-x v-y] (-> line
                                       .strip
                                       (->>
                                         (match "position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>")
                                         .groups
                                         (map int) ) ))
             (iterate (fn [xy]
                        (setv [x y] xy)
                        (, (-> x (+ v-x))
                           (-> y (+ v-y)) ) )
                      (, p-x p-y) ) ))
      list
      (apply zip)
      (map Set) ) ) )

(defn printable-current-positions [current-positions]
  (setv x-min (-> current-positions (->> (map first )) min))
  (setv x-max (-> current-positions (->> (map first )) max))
  (setv y-min (-> current-positions (->> (map second)) min))
  (setv y-max (-> current-positions (->> (map second)) max))

  (if (and (-> x-max (- x-min) (< 275))
           (-> y-max (- y-min) (< 65)) )
    (->> (range (dec y-min) (inc y-max))
      (map (fn [y]
             (->> (range (dec x-min) (inc x-max))
               (map (fn [x]
                      (if (-> (, x y) (in current-positions))
                        "#"
                        "." ) ))
               (.join "") ) ))
      (.join "\n") ) ) )

(for [step (->> (load-steps)
             (map printable-current-positions)
             (zip (iterate inc 0))
             (filter second) )]
  (setv [i printable] step)
  (print i)
  (print printable) )
