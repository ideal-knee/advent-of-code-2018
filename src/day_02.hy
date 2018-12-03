;; Part 1

(setv two-counts 0)
(setv three-counts 0)

(for [line (-> "/data/day-02-input.txt" open)]
  (setv letter-groups (-> line
                        .strip
                        list
                        sorted
                        group-by
                        (->>
                          (map (fn [kv] (-> kv second list len))) )
                        list ))

  (if (-> letter-groups
        (->>
          (map (fn [n] (= n 2))) )
        any )
    (+= two-counts 1) )

  (if (-> letter-groups
        (->>
          (map (fn [n] (= n 3))) )
        any )
    (+= three-counts 1) ) )

(-> (* two-counts three-counts)
  print )



;; Part 2

(defn match? [a b]
  (setv mismatch-count 0)
  (for [i (-> a len range)]
    (if (!= (-> a (get i))
            (-> b (get i)) )
      (do
        (+= mismatch-count 1)
        (if (-> mismatch-count (> 1))
          (return False) ) ) ) )
  (= mismatch-count 1) )

(setv lines (-> "/data/day-02-input.txt" open list))

(for [i (range (-> lines len (- 1)))
      j (range i (-> lines len)) ]
  (if (match? (-> lines (get i))
              (-> lines (get j)) )
    (do
      (print (-> lines (get i) .strip))
      (print (-> lines (get j) .strip))
      (break) ) ) )
