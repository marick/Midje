(ns ^{:doc "Reports as JUnit compatible XML."}
  midje.ideas.reporting.junit-xml-format)


(defmulti report-junit-xml :type
  )

(defmethod report-junit-xml :default [m] 
  ;; code to create XML for the different types of failure report maps
  ;; see string-format.clj for inspiration
  )

(defn report-junit-xml-summary [exit-after-tests?]
  ;; code to create XML for summary of failed facts
  ;; see string-format.clj for inspiration
  )

(def junit-xml-format-config { :single-fact-fn report-junit-xml
                               :summary-fn report-junit-xml-summary })