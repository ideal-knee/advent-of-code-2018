(require [utils [!]])

(import [collections [defaultdict]]
        [sys [argv]]
        [utils [dk-reduce select update!]] )

(setv [n-players n-marbles] (->>
                              argv
                              (drop 1)
                              (take 2)
                              (map int) ))

(defn initial-state [n-players]
  {"circle"             []
   "previous-position"  0
   "scores"             (defaultdict (fn [] 0))
   "n-players"          n-players } )

(defn tick [state current-marble]
  (setv [              circle   previous-position   scores   n-players]
        (select state "circle" "previous-position" "scores" "n-players") )

  (if (-> current-marble (% 100000) zero?)
    (print current-marble) )

  (if (and (-> current-marble (% 23) zero?)
           (-> current-marble (> 0)) )
    (do
      (setv current-position (-> previous-position
                               (- 7)
                               (% (-> circle len)) ))
      (setv marble-at-current-position (-> circle (get current-position)))
      (-> state
        (! .update {"scores" (-> scores
                               (update! (-> current-marble (% n-players))
                                        (fn [score]
                                          (-> score
                                            (+ current-marble
                                               marble-at-current-position ) ) ) ) )
                    "circle" (-> circle (! .pop current-position))
                    "previous-position" current-position }) ) )
    (do
      (setv current-position (if (-> circle len zero?)
                               0
                               (-> previous-position
                                 (+ 2)
                                 (% (-> circle len)) ) ))
      (if (-> current-position zero?)
        (setv current-position (-> circle len)) )
      (-> state
        (! .update {"circle" (-> circle (! .insert current-position current-marble))
                    "previous-position" current-position }) ) ) ) )

(-> (initial-state n-players)
  (dk-reduce tick (-> n-marbles range))
  (get "scores")
  .values
  sorted
  last
  print )
