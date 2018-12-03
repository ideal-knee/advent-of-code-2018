(import [sets [Set]])

(setv changes (-> "/data/day-01-input.txt"
                open
                (->>
                  (map (fn [line] (-> line .strip int))) )
                list ))



;; Part 1

(setv current-frequency 0)
(for [change changes]
  (+= current-frequency change) )
(-> current-frequency
  print )



;; Part 2

(setv current-frequency 0)
(setv seen-frequencys (Set [current-frequency]))
(for [change (-> changes cycle)]
  (+= current-frequency change)
  (if (in current-frequency seen-frequencys)
    (break) )
  (-> seen-frequencys (.add current-frequency)) )
(print current-frequency)
