(ns midje.util.old-clojure-contrib.core
  (:use [midje.util.old-clojure-contrib.def :only (defmacro-)]))

(defmacro- defnilsafe [docstring non-safe-name nil-safe-name]
  `(defmacro ~nil-safe-name ~docstring
     {:arglists '([~'x ~'form] [~'x ~'form ~'& ~'forms])}
	   ([x# form#]
	     `(let [~'i# ~x#] (when-not (nil? ~'i#) (~'~non-safe-name ~'i# ~form#))))
  	 ([x# form# & more#]
	     `(~'~nil-safe-name (~'~nil-safe-name ~x# ~form#) ~@more#))))
       
(defnilsafe 
  "Same as clojure.core/-> but returns nil as soon as the threaded value is nil itself (thus short-circuiting any pending computation).
   Examples :
   (-?> \"foo\" .toUpperCase (.substring 1)) returns \"OO\"
   (-?> nil .toUpperCase (.substring 1)) returns nil
   "
   -> -?>)
    
(defnilsafe 
  "Same as clojure.core/.. but returns nil as soon as the threaded value is nil itself (thus short-circuiting any pending computation).
   Examples :
   (.?. \"foo\" .toUpperCase (.substring 1)) returns \"OO\"
   (.?. nil .toUpperCase (.substring 1)) returns nil
   "
   .. .?.)

(defnilsafe
  "Same as clojure.core/->> but returns nil as soon as the threaded value is nil itself (thus short-circuiting any pending computation).
   Examples :
   (-?>> (range 5) (map inc)) returns (1 2 3 4 5)
   (-?>> [] seq (map inc)) returns nil
   "
  ->> -?>>)
