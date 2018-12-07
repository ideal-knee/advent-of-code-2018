;; reduce with shortcircuiting and arg order changed

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

;; args: [initial-value] func itr
(defn dk-reduce [arg-1 arg-2 &optional arg-3]
  (if arg-3
    (dk-reduce-aux arg-1 arg-2 arg-3)
    (dk-reduce-aux (first arg-2) arg-1 (rest arg-2)) ) )

(defn dk-reduce-aux [initial-value func itr]
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
