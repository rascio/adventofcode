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
      (debug + 5 6) ;print '(+ 5 6) 11'
                    ;evaluate to 11
  "
  [& expr]
  `(let [res# ~expr]
    (println (quote ~expr) res#)
    res#))

(defmacro deflambda
  "define a function whith currying:

  (advent.core/deflambda s [a b c d] (+ a b c d))

  (clojure.core/defn s
   ([a] (partial s a))
   ([a b] (partial s a b))
   ([a b c] (partial s a b c))
   ([a b c d] (+ a b c d)))
  "
  [name args & body]
  `(defn ~name
     ~@(map
         #(seq [% (concat ['partial name] %)])
         (map
           #(vec (take % args))
           (range 1 (count args))))
     ~(cons args body)))
