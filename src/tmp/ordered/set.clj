(ns tmp.ordered.set
  (:use [tmp.ordered.common :only [Compactable compact change!]])
  (:import (clojure.lang IPersistentSet ITransientSet IEditableCollection
                         IPersistentMap ITransientMap ITransientAssociative
                         IPersistentVector ITransientVector
                         Associative SeqIterator Reversible IFn IObj)
           (java.util Set Collection)))

(declare transient-ordered-set)

(deftype OrderedSet [^IPersistentMap k->i
                     ^IPersistentVector i->k]
  IPersistentSet
  (disjoin [this k]
    (if-let [i (.valAt k->i k)]
      (OrderedSet. (dissoc k->i k)
                   (assoc i->k i ::empty))
      this))
  (cons [this k]
    (if-let [i (.valAt k->i k)]
      this
      (OrderedSet. (.assoc ^Associative k->i k (.count i->k))
                   (.cons i->k k))))
  (seq [this]
    (seq (remove #(identical? ::empty %) i->k)))
  (empty [this]
    (OrderedSet. {} []))
  (equiv [this other]
    (.equals this other))
  (get [this k]
    (when (.valAt k->i k) k))
  (count [this]
    (.count k->i))

  IObj
  (meta [this]
    (.meta ^IObj k->i))
  (withMeta [this m]
    (OrderedSet. (.withMeta ^IObj k->i m)
                 i->k))

  Compactable
  (compact [this]
    (OrderedSet. k->i
                 (vec (.seq this))))

  Object
  (hashCode [this]
    (reduce + (map hash (.seq this))))
  (equals [this other]
    (or (identical? this other)
        (and (instance? Set other)
             (let [^Set s (cast Set other)]
               (and (= (.size this) (.size s))
                    (every? #(.contains s %) (.seq this)))))))

  Set
  (iterator [this]
    (SeqIterator. (.seq this)))
  (contains [this k]
    (.containsKey k->i k))
  (containsAll [this ks]
    (every? identity (map #(.contains this %) ks)))
  (size [this]
    (.count this))
  (isEmpty [this]
    (zero? (.count this)))
  (toArray [this dest]
    (reduce (fn [idx item]
              (aset dest idx item)
              (inc idx))
            0, (.seq this))
    dest)
  (toArray [this]
    (.toArray this (object-array (.count this))))

  Reversible
  (rseq [this]
    (seq (remove #(identical? ::empty %) (rseq i->k))))

  IEditableCollection
  (asTransient [this]
    (transient-ordered-set this))
  IFn
  (invoke [this k] (when (.contains this k) k)))

(def ^{:private true,
       :tag OrderedSet} empty-ordered-set (empty (OrderedSet. nil nil)))

(defn ordered-set
  ([] empty-ordered-set)
  ([& xs] (into empty-ordered-set xs)))

(deftype TransientOrderedSet [^{:unsynchronized-mutable true
                                :tag ITransientMap} k->i,
                              ^{:unsynchronized-mutable true
                                :tag ITransientVector} i->k]
  ITransientSet
  (count [this]
    (.count k->i))
  (get [this k]
    (when (.valAt k->i k) k))
  (disjoin [this k]
    (let [i (.valAt k->i k)]
      (when i
        (change! k->i .without k)
        (change! i->k .assocN i ::empty)))
    this)
  (conj [this k]
    (let [i (.valAt k->i k)]
      (when-not i
        (change! ^ITransientAssociative k->i .assoc k (.count i->k))
        (change! i->k conj! k)))
    this)
  (contains [this k]
    (boolean (.valAt k->i k)))
  (persistent [this]
    (OrderedSet. (.persistent k->i)
                 (.persistent i->k))))

(defn transient-ordered-set [^OrderedSet os]
  (TransientOrderedSet. (transient (.k->i os))
                        (transient (.i->k os))))
