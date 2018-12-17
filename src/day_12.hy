(require [utils [!]])

(import [re [match]]
        [sets [Set]]
        [utils [dk-reduce]] )

(defn load-lines []
  (->> "/data/day-12-input.txt"
    open
    list ) )

(defn c->b [c]
  (if (-> c (= "#"))
    1
    0 ) )

(defn five-bit-lshift [a b]
  (-> a
    (<< 1)
    (+ b)
    (& 0b11111) ) )

(defn get-initial-state [lines]
  (-> lines
    first
    (->>
      (match "initial state: ([.#]+)") )
    .groups
    (get 0)
    (->>
      (map c->b) )
    list ) )

(defn get-active-in-next-generation [lines]
  (-> lines
    rest
    rest
    (->>
      (dk-reduce
        (Set)
        (fn [active-in-next-generation line]
          (setv [previous-generation next-generation] (->> line
                                                        (match "([.#]+) => ([.#])")
                                                        .groups ))
          (if (= next-generation ".")
            active-in-next-generation
            (-> previous-generation
              (->>
                (map c->b)
                (reduce five-bit-lshift)
                (! active-in-next-generation .add) ) ) ) ) ) ) ) )

(defn reduce-bit [state current-bit]
  (setv [next-generation current-bits] state)
  (setv current-bits (-> current-bits (five-bit-lshift current-bit)))
  [(-> next-generation
     (! .append (if (-> current-bits (in active-in-next-generation))
                  1
                  0 )) )
   current-bits ] )

(defn get-next-generation [active-in-next-generation state]
  (-> (dk-reduce [[] 0]
                 reduce-bit
                 state )
    (dk-reduce reduce-bit (->> (repeat 0) (take 4)))
    first ) )

(setv initial-state (-> (load-lines)
                      get-initial-state ))

(setv active-in-next-generation (-> (load-lines)
                                  get-active-in-next-generation ))

(setv lines (load-lines))

(->> (range 20)
  (dk-reduce initial-state
             (fn [s _]
               (->> s (get-next-generation active-in-next-generation)) ) )
  (reduce +)
  print )
