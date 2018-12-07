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
(defn dk-reduce [arg-1 arg-2 &optional [arg-3 :no-arg-3]]
  (if (= arg-3 :no-arg-3)
    (dk-reduce-aux (first arg-2) arg-1 (rest arg-2))
    (dk-reduce-aux arg-1 arg-2 arg-3) ) )

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



;; Make list from args

(defn make-list [&rest elems]
  (list elems) )



;; Make mutating methods functional

(defmacro ! [n f &rest args]
  `(do
     (~f ~n ~@args)
     ~n ) )



;; update

(defn update! [m k f]
  (setv o (-> m (get k)))
  (setv (-> m (get k)) (f o))
  m )



;; Better functional functions for dicts

(defn dict-filter [f d]
  (dk-reduce {}
             (fn [o k]
               (setv v (-> d (get k)))
               (if (f k v)
                 (-> o
                   (! assoc k v) )
                 o ) )
             d ) )
