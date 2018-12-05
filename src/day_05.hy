(import [utils [reduced shortcircuiting-reduce peek pop! push!]])

(defn react [p]
  (->> p
    (map ord)
    (shortcircuiting-reduce []
                            (fn [seen-stack current]
                              (if (-> current (- (peek seen-stack 0)) abs (= 32))
                                  (pop! seen-stack)
                                  (push! seen-stack current) ) ) ) ) )



;; Part 1

(setv original-polymer (-> "/data/day-05-input.txt"
                         open
                         .read
                         .strip ))

(-> original-polymer
  react
  len
  print )



;; Part 2

(-> (range 26)
  (->>
    (map (fn [i]
           (-> original-polymer
             (.replace (-> i (+ 65) chr) "")
             (.replace (-> i (+ 97) chr) "")
             react
             len ) )) )
  min
  print )
