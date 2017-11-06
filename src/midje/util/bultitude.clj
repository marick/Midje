(ns midje.util.bultitude
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [dynapath.util :as dp])
  (:import (java.util.jar JarFile JarEntry)
           (java.util.zip ZipException)
           (java.io File BufferedReader PushbackReader InputStreamReader)
           (clojure.lang DynamicClassLoader)))

(defn- clj? [^File f]
  (and (not (.isDirectory f))
       (.endsWith (.getName f) ".clj")))

(defn- clj-jar-entry? [^JarEntry f]
  (and (not (.isDirectory f))
       (.endsWith (.getName f) ".clj")))

(defn- jar? [^File f]
  (and (.isFile f) (.endsWith (.getName f) ".jar")))

(defn- read-ns-form
  "Given a reader on a Clojure source file, read until an ns form is found."
  [rdr]
  (let [form (try (read rdr false ::done)
                  (catch Exception e ::done))]
    (if (try
          (and (list? form) (= 'ns (first form)))
          (catch Exception _))
      (try
        (str form) ;; force the read to read the whole form, throwing on error
        (second form)
        (catch Exception _))
      (when-not (= ::done form)
        (recur rdr)))))


;;; Classifying files

(defn- starting-classification-for-standalone-file [file]
  {:file file
   :source-type :standalone-file
   :reader-maker #(io/reader file)})

(defn- starting-classification-for-jar-entry [^JarFile jarfile jar-entry]
  {:file jar-entry
   :jarfile jarfile
   :source-type :jar-entry
   :reader-maker #(-> jarfile
                      (.getInputStream jar-entry)
                      InputStreamReader.
                      BufferedReader.)})

(defn- has-valid-namespace? [classification]
  (= (:status classification) :contains-namespace))

(defn- standalone-file? [classification]
  (= (:source-type classification) :standalone-file))
(def jar-entry? (complement standalone-file?))

(defn- readable? [classification]
  (or (jar-entry? classification)
       (boolean (.canRead ^File (:file classification)))))

(defn- describe-namespace-status
  "Produces a map describing whether a file is
   * a clojure file with a namespace
   * a clojure file that doesn't try to have a namespace
   * a file that can't be parsed as Clojure."
  [rdr]
  (letfn [(plausible-ns-form? [form]
            (if (and (list? form) (= 'ns (first form)))
              (if (symbol? (second form))
                true
                (throw (Exception.))) ; not just implausible: flat out impossible/invalid.
              false))

          (next-form []
            (try
              (let [form (read rdr false ::done)]
                (cond (= ::done form)
                      ::done

                      (plausible-ns-form? form)
                      (do
                        (str form) ;; force the read to read the whole form, throwing on error
                        (second form))

                      :else
                      ::boring-form))
              (catch Exception _
                ::broken-namespace)))]
    (loop [form (next-form)]
      (condp = form
        ::done               {:status :no-attempt-at-namespace}
        ::boring-form        (recur (next-form))
        ::broken-namespace   {:status :invalid-clojure-file}
                             {:status :contains-namespace, :namespace-symbol form}))))

(defn- extend-starting-classification [classification]
  (letfn [(grovel-through-bytes []
            (with-open [r (PushbackReader. ((:reader-maker classification)))]
              (describe-namespace-status r)))]
    (if (not (readable? classification))
      (assoc classification :status :unreadable)
      (merge classification (grovel-through-bytes)))))

(defn classify-dir-entries
  "Looks for all Clojure (.clj) files in the directory tree rooted at `dir`, a string.
   Returns a seq of maps.
   Each map will contain one of four values for the `:status` key:
     :contains-namespace   (The namespace is the value of key `:namespace-symbol`.)
     :unreadable
     :no-namespace         (There is no `ns` form.)
     :broken-namespace     (An `ns` entry in the file is malformed.)
   The original java.io.File object is under key `:file`."
  [dir]
  (->> (file-seq (io/file dir))
       (filter clj?)
       (map starting-classification-for-standalone-file)
       (map extend-starting-classification)))

(defn classify-jar-entries [^File jar]
  "Looks for all Clojure (.clj) files in the given jarfile.
   Returns a seq of maps.
   Each map will contain one of three values for the `:status` key:
     :contains-namespace   (The namespace is the value of key `:namespace-symbol`.)
     :no-namespace         (There is no `ns` form.)
     :broken-namespace     (An `ns` entry in the file is malformed.)
   The original JarEntry object is under key `:file` (sic), and the original
   jar is under :jar-file."
  (try
    (let [as-jar-file (JarFile. jar)]
      (->> (enumeration-seq (.entries as-jar-file))
           (filter clj-jar-entry?)
           (map (partial starting-classification-for-jar-entry as-jar-file))
           (map extend-starting-classification)))
  (catch ZipException e
    (throw (Exception. (str "jar file corrupt: " jar) e)))))



(defn- namespaces-in-x [x classifier]
  (->> (classifier x)
       (filter has-valid-namespace?)
       (map :namespace-symbol)))

(defn namespaces-in-dir
  "Return a seq of all namespaces found in Clojure source files in dir."
  [dir]
  (namespaces-in-x dir classify-dir-entries))

(defn- namespaces-in-jar
  "Return a seq of all valid namespaces found in Clojure source files in the given jar."
  [^File jar]
  (namespaces-in-x jar classify-jar-entries))


(defn- split-classpath [^String classpath]
  (.split classpath (System/getProperty "path.separator")))

(defn loader-classpath
  "Returns a sequence of File objects from a classloader."
  [loader]
  (map io/as-file (dp/classpath-urls loader)))

(defn classpath-files
  "Returns a sequence of File objects of the elements on the classpath."
  ([classloader]
     (map io/as-file (dp/all-classpath-urls classloader)))
  ([] (classpath-files (clojure.lang.RT/baseLoader))))

(defn- classpath->collection [classpath]
  (if (coll? classpath)
    classpath
    (split-classpath classpath)))

(defn- classpath->files [classpath]
  (map io/file classpath))

(defn select-subdirectory
  "`dir` is the root of a directory hierarchy. Branches (subdirectories)
  of that hierarchy are described in Clojure namespace notation. The branch
  so described is returned as a ^File.

  A `nil` namespace or empty string means the entire hierarchy.

  Example:
    (extend-directory-with-namespace (io/file \".\") \"a.b-test\")
    => (io/file \"./a/b_test\")"
  [^File dir ^String namespace]
  (if namespace
    (io/file dir (-> namespace
                     (.replaceAll "\\." "/")
                     (.replaceAll "-" "_")))
    dir))

(defn filter-by-prefix
  "Given a list of namespaces, retain only those whose names
   begin with the given prefix. A `nil` prefix means everything
   is to be retained."

  ;; Not describing what `and` does below. As far as I can guess,
  ;; it's just to make the function produce a nil result from a
  ;; nil input (instead of the empty sequence you'd otherwise get.
  [namespaces prefix]
  (if prefix
    (filter #(and % (.startsWith (name %) prefix)) namespaces)
    namespaces))

(defn file->namespaces
  "Map a classpath file to the namespaces it contains. `prefix` allows for
   reducing the namespace search space. For large directories on the classpath,
   passing a `prefix` can provide significant efficiency gains."
  [^String prefix ^File f]
  (cond
    (.isDirectory f) (namespaces-in-dir (select-subdirectory f prefix))
    (jar? f)         (filter-by-prefix (namespaces-in-jar f) prefix)))

(defn namespaces-on-classpath
  "Return symbols of all namespaces matching the given prefix both on disk and
  inside jar files. If :prefix is passed, only return namespaces that begin with
  this prefix. If :classpath is passed, it should be a seq of File objects or a
  classpath string. If it is not passed, default to java.class.path and the
  current classloader, assuming it is a dynamic classloader."
  [& {:keys [prefix classpath] :or {classpath (classpath-files)}}]
  (mapcat
   (partial file->namespaces prefix)
   (->> classpath
        classpath->collection
        classpath->files)))

(defn path-for
  "Transform a namespace into a .clj file path relative to classpath root."
  [namespace]
  (str (-> (str namespace)
           (.replace \- \_)
           (.replace \. \/))
       ".clj"))
