(ns advent.core
  (:require [clojure.java.io :as io]))

(defmacro defcase
  "Define a function binding the *in*
  to the content of the `file` read from classpath, eg:
      (defcase test \"position/in/classpath.txt\"
          (println (read-line)) ;Prints the first line of the file
  "
  [name file body]
  `(defn ~name []
     (binding [*in* (->> ~file
                      io/resource
                      io/input-stream
                      io/reader
                      (new clojure.lang.LineNumberingPushbackReader))]
       ~body)))
