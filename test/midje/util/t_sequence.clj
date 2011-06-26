(ns midje.util.t-sequence
  (:use midje.util.sequence)
  (:use midje.sweet))
	
(fact "zips a seq of pairs into two lists of items"
  (zip [1 2] [3 4] [5 6] [7 8]) => [[1 3 5 7] [2 4 6 8]])

(fact "can zip tuples of any length"
  (zip [1 2 3] [4 5 6] [7 8 9] [10 11 12]) => [[1 4 7 10] [2 5 8 11] [3 6 9 12]])

(fact "shortest seq given dictates how many seqs will be in the output"
  (zip [1] [4 5] [7 8 9]) => [[1 4 7]])

(fact "zips two seqs together into a map - maintaining/guaranteeing order matches the original order"
  (keys (ordered-zipmap [:a :b :c :d :e :f :g :h] [1 2 3 4 5 6 7 8])) 
    => [:a :b :c :d :e :f :g :h]
  (vals (ordered-zipmap [:a :b :c :d :e :f :g :h] [1 2 3 4 5 6 7 8])) 
    => [1 2 3 4 5 6 7 8] )

(fact "shortens to the smallest of the two seqs"
  (ordered-zipmap [:a] [1 2 3 4]) => {:a 1}
  (ordered-zipmap [:a :b :c :d] [1]) => {:a 1} )	