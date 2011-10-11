;;  Copyright (c) Stephen C. Gilardi. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  ns-utils
;;
;;  Namespace Utilities
;;
;;    'get-ns'          returns the namespace named by a symbol or throws
;;                      if the namespace does not exist
;;
;;    'ns-vars'         returns a sorted seq of symbols naming public vars
;;                      in a namespace
;;
;;    'print-dir'       prints a sorted directory of public vars in a
;;                      namespace
;;
;;    'print-docs'      prints documentation for the public vars in a
;;                      namespace
;;
;;    'immigrate'       Create a public var in this namespace for each
;;                      public var in the namespaces named by ns-names.
;;                      From James Reeves
;;  Convenience
;;
;;    'vars'            returns a sorted seq of symbols naming public vars
;;                      in a namespace (macro)
;;
;;    'dir'             prints a sorted directory of public vars in a
;;                      namespace (macro)
;;
;;    'docs'            prints documentation for the public vars in a
;;                      namespace (macro)
;;
;;  scgilardi (gmail)
;;  23 April 2008

(ns 
  #^{:author "Stephen C. Gilardi",
     :doc "Namespace utilities"}
  midje.util.old-clojure-contrib.ns-utils
  (:use [midje.util.thread-safe-var-nesting :only [var-root]]))

;; Namespace Utilities

(defn immigrate
  "Create a public var in this namespace for each public var in the
  namespaces named by ns-names. The created vars have the same name, root
  binding, and metadata as the original except that their :ns metadata
  value is this namespace."
  [& ns-names]
  (doseq [ns ns-names]
    (require ns)
    (doseq [[sym var] (ns-publics ns)]
      (let [sym (with-meta sym (assoc (meta var) :ns *ns*))]
        (if (.hasRoot var)
          (intern *ns* sym (var-root var))
          (intern *ns* sym))))))
