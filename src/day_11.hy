(import [collections [defaultdict]]
        [functools [partial]]
        [sys [argv]] )

(defn load-grid-serial-number []
  (->>
    argv
    (drop 1)
    first
    int ) )

(defn memoize [f]
  (setv m {})

  (fn [k]
    (if (-> k (in m) not)
      (-> m (get k) (setv (f k))) )

    (-> m (get k)) ) )

(defn calculate-power-level [grid-serial-number xy]
  (setv [x y] xy)
  (setv rack-id (-> x (+ 10)))
  (-> rack-id
    (* y)
    (+ grid-serial-number)
    (* rack-id)
    (% 1000)
    (/ 100)
    int
    (- 5) ) )

(defn initialize-grid [grid-serial-number]
  (partial calculate-power-level grid-serial-number) )

(defn calculate-square-power-level [grid grid-size x y]
  (->> (range x (-> x (+ grid-size)))
    (map (fn [i]
           (->> (range y (-> y (+ grid-size)))
             (map (fn [j]
                    (->> j
                      (, i)
                      grid ) ))
                (reduce +) ) ))
       (reduce +) ) )

(defn load-grid []
  (-> (load-grid-serial-number)
    initialize-grid ) )

(defn find-max-square [grid grid-size]
  (->> (range 1 (-> 302 (- grid-size)))
    (map (fn [x]
           (->> (range 1 (-> 302 (- grid-size)))
             (map (fn [y]
                    [(calculate-square-power-level grid grid-size x y) x y] ))
             (reduce max) ) ))
    (reduce max) ) )

(setv grid (load-grid))

(for [size (range 1 300)]
  (->> size
    (find-max-square grid)
    (print size) ) )
