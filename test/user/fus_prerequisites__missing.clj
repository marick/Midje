(ns user.fus-prerequisites--missing
  "From https://github.com/marick/Midje/issues/332"
  (:use midje.sweet
        midje.test-util))

(unfinished pre-process)
(unfinished process)

(capturing-failure-output
 (fact "Midje seems to have a problem with lazyseqs"
   (-> (map pre-process [..metaconstant..])
       process) => ..processed..
       (provided
         (pre-process ..metaconstant..) => ..intermediate..
         (process [..intermediate..]) => ..processed..))
 ;; So...
 (fact
   @fact-output => #"You never said #'process would be called with these arguments:"
   @fact-output => #"an unrealized lazy sequence"
   @fact-output => #"pre-process.*never called"
   @fact-output => #"process.*never called"
   @fact-output => #"Expected: \.\.processed\.\."
   @fact-output => #"`process` returned this string"))
