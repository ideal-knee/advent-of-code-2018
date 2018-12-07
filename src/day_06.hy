(import [collections [defaultdict]])

(setv coordinates (-> "/data/day-06-input.txt"
                    open
                    (->> (map (fn [line]
                                (-> line
                                  .strip
                                  (.split ", ")
                                  (->>
                                    (map int) )
                                  tuple ) )))
                    list ))

(defn manhattan-distance [a b]
  (setv [a-x a-y] a [b-x b-y] b)
  (+ (-> a-x (- b-x) abs)
     (-> a-y (- b-y) abs) ) )

(setv x-min (-> coordinates (->> (map first)) min))
(setv x-max (-> coordinates (->> (map first)) max))
(setv y-min (-> coordinates (->> (map second)) min))
(setv y-max (-> coordinates (->> (map second)) max))

(defn bounded? [c]
  (setv [x y] c)
  (and (< x-min x x-max)
       (< y-min y y-max) ) )



;; Part 1

(setv bounded-areas (defaultdict (fn [] 0)))

(for [x (range x-min x-max)
      y (range y-min y-max) ]
  (setv [closest second-closest] (-> coordinates
                                   (->>
                                     (map (fn [c] [(manhattan-distance c [x y]) c])) )
                                   (sorted :key first)
                                   (->>
                                     (take 2) ) ))
  (if (and (!= (-> closest first) (-> second-closest first))
           (-> closest second bounded?) )
    (-> bounded-areas
      (get (-> closest second))
      (+= 1) ) ) )

(for [c bounded-areas]
  (print c (-> bounded-areas (get c))) ) ; Take the coordinate with the second biggest value for some reason



;; Part 2

(setv count 0)

(for [x (range x-min x-max)
      y (range y-min y-max) ] ; I think I got lucky that this range is okay
  (print [x y])
  (if (-> coordinates
        (->>
          (map (fn [c] (manhattan-distance [x y] c)))
          (reduce +) )
        (< 10000) )
    (+= count 1) ) )

(print count)
