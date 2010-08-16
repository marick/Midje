(use 'ring.adapter.jetty)
(require 'adder.core)

(let [port (Integer/parseInt (get (System/getenv) "PORT" "8080"))]
  (run-jetty #'adder.core/app {:port port}))
