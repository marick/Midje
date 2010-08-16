(defproject adder "0.0.1"
  :description "Add two numbers."
  :dependencies
    [[org.clojure/clojure "1.2.0-RC2"]
     [org.clojure/clojure-contrib "1.2.0-RC2"]
     [ring/ring-core "0.2.5"]
     [ring/ring-devel "0.2.5"]
     [ring/ring-jetty-adapter "0.2.5"]
     [compojure "0.4.0"]
     [hiccup "0.2.6"]]
  :dev-dependencies
  [[lein-run "1.0.0-SNAPSHOT"]
   [midje "0.3.0"]
   ])
