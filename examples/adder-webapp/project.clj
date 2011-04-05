(defproject adder "0.4.0"
  :description "Add two numbers."
  :dependencies
    [[org.clojure/clojure "1.2.0"]
     [org.clojure/clojure-contrib "1.2.0"]
     [ring/ring-core "0.2.5"]
     [ring/ring-devel "0.2.5"]
     [ring/ring-jetty-adapter "0.2.5"]
     [compojure "0.4.0"]
     [hiccup "0.2.6"]]
  :dev-dependencies
  [[lein-run "1.0.0"]
   [midje "1.1"]
   ])
