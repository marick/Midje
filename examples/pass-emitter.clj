(ns ^{:doc "Adding an emitter that brags about fact results"}
  example.pass-emitter
  (:require [midje.emission.plugins.util :as util]
            [midje.data.fact :as fact]
            [midje.emission.plugins.default :as default]
            [midje.emission.state :as state]))

(defn finishing-top-level-fact [fact]
  (util/emit-one-line (format "Dude! `%s` at line %d of %s totally finished!"
                              (fact/name fact)
                              (fact/line fact)
                              (fact/file fact)))

  ;; Plugins are not responsible for keeping track of successes and
  ;; failures. That happens independently, and you gain access to the
  ;; counts through the `midje.emission.state` namespace.
  (util/emit-one-line (format "We're up to %d passing checks!"
                              (state/output-counters:midje-passes))))

;; The emission map is how you hook your functions into the
;; system. It also makes it convenient to "inherit" from an
;; existing map.
(def emission-map (assoc default/emission-map
                         :finishing-top-level-fact finishing-top-level-fact))

;; Here's where the installation happens.
(state/install-emission-map emission-map)
