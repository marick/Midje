(ns adder.core
  (:use compojure.core)
  (:use hiccup.core)
  (:use hiccup.page-helpers)
  (:use ring.middleware.reload)
  (:use ring.util.response)
  (:use ring.middleware.stacktrace)
  (:use adder.middleware)
  (:use ring.middleware.file)
  (:use ring.middleware.file-info)
)

(defn view-layout [& content]
  (html
   (doctype :xhtml-strict)
   (xhtml-tag "en"
	      [:head
	       [:meta {:http-equiv "Content-type"
		       :content "text/html; charset=utf-8"}]
	       [:title "adder"]
	       [:link {:href "/adder.css" :rel "stylesheet" :type "text/css"}]]
	      [:body content])))

(defn view-input [& [a b]]
  (view-layout
   [:h2 "add two numbers"]
   [:form {:method "post" :action "/"}
    (if (and a b)
      [:p "those are not both numbers!"])
    [:input.math {:type "text" :name "a" :value a}] [:span.math " + "]
    [:input.math {:type "text" :name "b" :value b}] [:br]
    [:input.action {:type "submit" :value "add"}]]))

(defn view-output [a b sum]
  (view-layout
    [:h2 "two numbers added"]
    [:p.math a " + " b " = " sum]
    [:a.action {:href "/"} "add more numbers"]))

(defn parse-input [a b]
  [(Integer/parseInt a) (Integer/parseInt b)])

(defroutes handler
  (GET "/" []
    (view-input))

  (POST "/" [a b]
  (try
    (let [[a b] (parse-input a b)
          sum   (+ a b)]
      (view-output a b sum))
    (catch NumberFormatException e
      (view-input a b))))

  (ANY "/*" [path]
       (redirect "/"))
)


(def production?
  (= "production" (get (System/getenv) "APP_ENV")))

(def development?
  (not production?))

(def app
     (-> #'handler
	 (wrap-file "public")
	 (wrap-file-info)
	 (wrap-request-logging)
	 (wrap-if development? wrap-reload '[adder.middleware adder.core])
	 (wrap-bounce-favicon)
	 (wrap-exception-logging)
	 (wrap-if production? wrap-failsafe)
	 (wrap-if development? wrap-stacktrace)))
