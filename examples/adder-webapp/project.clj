(defproject adder "0.4.0"
  :description "Add two numbers."
  :dependencies
    [[org.clojure/clojure "[1.2.0],[1.2.1],[1.3.0]"]
     [ring/ring "1.0.0-beta2"]
     [ring/ring-devel "1.0.0-beta2"]
     [ring/ring-jetty-adapter "1.0.0-beta2"]
     [compojure "0.6.5"]
     [hiccup "0.3.7"]]
  :dev-dependencies
  [[lein-run "1.0.0"]
   [lein-midje "[1.0.0,)"]
   [midje "1.3-alpha5"]
   ])
