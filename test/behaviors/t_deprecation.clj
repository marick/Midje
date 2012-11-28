(ns behaviors.t-deprecation
  (:use midje.util.deprecation)
  (:use [midje sweet test-util])
  (:require [midje.config :as config]
            [midje.util.ecosystem :as ecosystem]))


(fact "deprecation printing"
  (config/with-temporary-config {:visible-deprecation true}
    (without-previous-deprecations
     (fact "appears first time"
       (with-out-str (deprecate "test message")) => #"test message")
     (fact "but only once..."
       (with-out-str (deprecate "test message")) => "")
     (fact "... unless the config asks for everything"
       (config/with-temporary-config {:visible-deprecation :all}
         (with-out-str (deprecate "test message")) => #"test message")))))
    
(fact "deprecation global message"
  (let [global-message-regex #"(?sm)set configuration variable"]
    (config/with-temporary-config {:visible-deprecation true}
      (fact "normally appears the first time"
        (without-previous-deprecations
         (with-out-str (deprecate "test message")) => global-message-regex))
      (fact "does not appear (no point) if user asks for all deprecations"
        (config/with-temporary-config {:visible-deprecation :all}
          (without-previous-deprecations
           (with-out-str (deprecate "test message")) =not=> global-message-regex))))))
      
(fact "visible deprecation can be turned off"
  (config/with-temporary-config {:visible-deprecation false}
    (without-previous-deprecations
     (with-out-str (deprecate "test message")) => "")))
    
  
  
