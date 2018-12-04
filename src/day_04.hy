(import [collections [defaultdict]]
        [re [match]] )

(setv interpreter {"\[.*\] Guard #(\\d+) begins shift" (fn [state match]
                                                         (-> state (assoc :current-guard (-> match (.group 1) int))) )

                   "\[.*?(\\d+)\] falls asleep"        (fn [state match]
                                                         (-> state (assoc :fell-asleep-at (-> match (.group 1) int))) )

                   "\[.*?(\\d+)\] wakes up"            (fn [state match]
                                                         (for [i (range (-> state (get :fell-asleep-at))
                                                                        (-> match (.group 1) int) )]
                                                           (-> state (get :sleep-times (-> state (get :current-guard))) (.append i)) ) )})

(setv state {:sleep-times (defaultdict list)})
(for [line (-> "/data/day-04-input.txt" open sorted)]
  (for [pattern interpreter]
    (setv m (-> pattern (match line)))
    (when m
      ((-> interpreter (get pattern)) state m)
      (break) ) ) )



;; Part 1

(setv max-sleep-guard None)
(setv max-sleep-time 0)
(for [guard (-> state (get :sleep-times))]
  (setv sleep-time (-> state (get :sleep-times guard) len))
  (when (-> sleep-time (> max-sleep-time))
    (setv max-sleep-guard guard)
    (setv max-sleep-time sleep-time) ) )

(setv max-sleep-minute (-> state
                         (get :sleep-times max-sleep-guard)
                         sorted
                         group-by
                         (max :key (fn [kv] (-> kv second list len)))
                         first ))

(-> (* max-sleep-guard max-sleep-minute) print)



;; Part 2

(setv max-sleep-guard None)
(setv max-sleep-minute None)
(setv max-sleep-times 0)

(for [guard (-> state (get :sleep-times))]
  (setv [sleep-minute sleep-times] (-> state
                                     (get :sleep-times guard)
                                     sorted
                                     group-by
                                     (->>
                                       (map (fn [kv] [(-> kv first)
                                                      (-> kv second list len) ])) )
                                     (max :key second) ))
  (when (-> sleep-times (> max-sleep-times))
    (setv max-sleep-guard guard)
    (setv max-sleep-minute sleep-minute)
    (setv max-sleep-times sleep-times) ) )

(-> (* max-sleep-guard max-sleep-minute) print)
