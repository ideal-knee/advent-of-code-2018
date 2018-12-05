;; Shortcircuiting reduce

(defclass Reduced [object]
  "Way to know when to shortcircuit"

  (defn --init-- [self o]
    (setv self.o o) )

  (defn deref [self]
    "Return o"
    self.o ) )

(defn reduced [o]
  (Reduced o) )

(defn reduced? [o]
  (-> o (isinstance Reduced)) )

(defn shortcircuiting-reduce [initial-value func itr]
  (setv previous-result initial-value)
  (for [o itr]
    (setv result (func previous-result o))
    (if (-> result reduced?)
      (return (-> result .deref)) )
    (setv previous-result result) )
  previous-result )



;; Stack

(defn peek [s default]
  (if (-> s empty?)
    default
    (-> s (get -1)) ) )

(defn pop! [s]
  (-> s .pop)
  s )

(defn push! [s o]
  (-> s (.append o))
  s )
