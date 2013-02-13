(ns midje.util.t-ecosystem
  (:use midje.sweet
        midje.util.ecosystem
        midje.test-util))

(when-1-3+
 
 (fact "can find paths to load from project.clj"
   (against-background (around :facts (around-initial-paths ?form)))
   (fact "if it exists"
     (leiningen-paths) => ["/test1" "/src1"]
     (provided
       (leiningen-project) => {:test-paths ["/test1"] :source-paths ["/src1"]}))
    
   (fact "and provides a default if it does not"
     (leiningen-paths) => ["test"]
     (provided
       (load-file "./project.clj") =throws=> (new java.io.FileNotFoundException)))
   
   (fact "except that lein-midje can explicitly set the value"
     (set-leiningen-paths! {:test-paths ["t"] :source-paths ["s"]})
     (leiningen-paths) => ["t" "s"])

   (fact "note that test paths come first"
     (set-leiningen-paths! (sorted-map :source-paths ["after"] :test-paths ["before"]))
     (leiningen-paths) => ["before" "after"]))
)
