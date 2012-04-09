(ns ^{:doc "Reports as JUnit compatible XML."}
  midje.ideas.reporting.junit-xml-format)


(defmulti report-junit-xml :type)

(defmethod report-junit-xml :default [m] )