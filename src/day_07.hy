(require [utils [!]])

(import [re [match]]
        [sets [Set]]
        [utils [dict-filter dk-reduce make-list reduced update!]] )

(defn empty-dependencies-by-step []
  (-> (range 65 (+ 65 26))
    (->>
      (map chr)
      (dk-reduce {}
                 (fn [m c]
                   (-> m
                     (! assoc c (Set)) ) ) ) ) ) )

(defn initial-dependencies-by-step []
  (-> "/data/day-07-input.txt"
    open
    (->>
      (dk-reduce (empty-dependencies-by-step)
                 (fn [dependencies-by-step line]
                   (setv [dependency step] (-> line
                                             (->>
                                               (match "Step (\w) must be finished before step (\w) can begin.") )
                                             (.group 1 2) ))
                   (-> dependencies-by-step
                     (update! step (fn [dependencies]
                                     (-> dependencies (! .add dependency)) )) ) ) ) ) ) )

(defn next-steps-with-no-dependents [dependencies-by-step]
  (->> dependencies-by-step
     (dict-filter (fn [k v]
                    (-> v len zero?) ))
     sorted ) )

(defn next-step-with-no-dependents [dependencies-by-step]
  (-> dependencies-by-step
    next-steps-with-no-dependents
    first ) )

(defn steps-with-dependent [dependencies-by-step dependent]
  (-> dependencies-by-step
    (->>
      (filter (fn [step]
                (-> dependencies-by-step (get step) (->> (in dependent))) )) ) ) )

(defn complete-step! [dependencies-by-step step]
  (dk-reduce (-> dependencies-by-step (! .pop step))
             (fn [dependencies-by-step dependent-step]
               (-> dependencies-by-step (update! dependent-step (fn [dependencies]
                                                                  (-> dependencies (! .remove step)) ))) )
             (-> dependencies-by-step (steps-with-dependent step)) ) )



;; Part 1

(-> (dk-reduce [(initial-dependencies-by-step) ""]
               (fn [state _]
                 (setv [dependencies-by-step result] state)
                 (if (-> dependencies-by-step empty?)
                   (reduced result)
                   (do
                     (setv next-step (-> dependencies-by-step next-step-with-no-dependents))
                     (-> dependencies-by-step
                       (complete-step! next-step)
                       (make-list (-> result (+ next-step))) ) ) ) )
               (repeat None) )  ; reduce until reduced
  print )



;; Part 2

(defn initial-seconds-remaining-by-step []
  (-> (range 26)
    (->>
      (dk-reduce {}
                 (fn [m o]
                   (-> m
                     (! assoc (chr (-> o (+ 65))) (-> o (+ 61))) ) ) ) ) ) )

(defn initial-active-steps [] (Set))

(defn initial-result [] 0)

(defn initial-state []
  [(initial-seconds-remaining-by-step)
   (initial-dependencies-by-step)
   (initial-active-steps)
   (initial-result) ] )

(-> (dk-reduce (initial-state)
               (fn [state _]
                 (setv [seconds-remaining-by-step dependencies-by-step active-steps result] state)
                 (if (-> dependencies-by-step empty?)
                   (-> result (- 1) reduced)
                   (do
                     (setv updated-seconds-remaining-by-step (-> seconds-remaining-by-step
                                                               (dk-reduce (fn [seconds-remaining-by-step active-step]
                                                                            (-> seconds-remaining-by-step (update! active-step dec)) )
                                                                          active-steps ) ))
                     (setv completed-steps (->> updated-seconds-remaining-by-step (dict-filter (fn [_ v] (-> v zero?))) .keys))
                     (setv updated-dependencies-by-step (-> dependencies-by-step (dk-reduce complete-step! completed-steps)))

                     [(->> updated-seconds-remaining-by-step
                         (dict-filter (fn [k v] (-> k (in completed-steps) not))) )
                      updated-dependencies-by-step
                      (-> active-steps
                        (dk-reduce (fn [active-steps completed-step]
                                     (-> active-steps (! .remove completed-step)) )
                                   completed-steps )
                        (dk-reduce (fn [active-steps next-step]
                                     (if (-> active-steps len (< 5))
                                       (-> active-steps (! .add next-step))
                                       active-steps ) )
                                   (next-steps-with-no-dependents updated-dependencies-by-step) ) )
                      (-> result inc) ] ) ) )
               (repeat None) )
  print )
