(import [collections [defaultdict]]
        [re [split]]
        [sets [Set]] )

(setv claimed-squares (defaultdict Set))
(setv ids (Set))

(for [line (-> "/data/day-03-input.txt" open)]
  (setv [id x y width height] (->> line
                                (split "\D+")
                                (drop 1)
                                (take 5)
                                (map int) ) )
  (-> ids (.add id))
  (for [i (range x (+ x width))
        j (range y (+ y height)) ]
    (-> claimed-squares (get (, i j)) (.add id)) ) )

(setv overlapping-claims (-> claimed-squares
                           .values
                           (->>
                             (filter (fn [v] (-> v len (> 1)))) )
                           list ))



;; Part 1

(-> overlapping-claims len print)



;; Part 2

(for [claim overlapping-claims
      id claim ]
    (if (-> id (in ids))
      (-> ids (.remove id)) ) )

(-> ids first print)
