(ns advent.core
  (:require [clojure.java.io :as io]))

(defmacro defcase
  "Define a function letting the `input` a seq with
  the content of the `file` read from classpath, eg:
      (defcase test \"position/in/classpath.txt\" seq-file
          (println (first seq-file)) ;Prints the first line of the file
  "
  [name file input body]
  `(defn ~name []
     (let [~input (->> ~file
                      io/resource
                      io/input-stream
                      io/reader
                      line-seq)]
       ~body)))
(defmacro debug
  "Utility to debug an expression, usage:
      (debug + 5 6) ;evaluate to 11
      ;(+ 5 6) 11
  "
  [& expr]
  `(let [res ~expr]
     (println (quote ~expr) res)
     res))
