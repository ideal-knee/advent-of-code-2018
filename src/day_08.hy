(require [utils [!]])

(import [utils [dk-reduce]])

(defn initial-input []
  (-> "/data/day-08-input.txt"
    open
    .read
    .strip
    (.split " ")
    (->>
      (map int) ) ) )

(defn load-node [input]
  (setv n-children (-> input next))
  (setv n-metadata (-> input next))

  {:children (dk-reduce []
                        (fn [children _]
                          (-> children
                            (! .append (load-node input)) ) )
                        (range n-children) )
   :metadata (->> input (take n-metadata) list) } )



;; Part 1

(defn sum-metadata [node]
  (-> node
    (get :children)
    (->>
      (map sum-metadata)
      (dk-reduce (-> node
                   (get :metadata)
                   (->>
                     (reduce +) ) )
                 + ) ) ) )

(-> (initial-input)
  load-node
  sum-metadata
  print )



;; Part 2

(defn find-value [node]
  (setv metadata (-> node (get :metadata)))
  (setv children (-> node (get :children)))

  (if (-> children len zero?)
    (->> metadata
       (reduce +) )
    (->> metadata
      (map dec)
      (filter (fn [i]
                (and (-> i (>= 0))
                     (-> i (< (-> children len))) ) ))
      (map (fn [i]
             (-> children (get i) find-value) ))
      (reduce +) ) ) )

(-> (initial-input)
  load-node
  find-value
  print )
