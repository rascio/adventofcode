(ns advent.core
  (:require [clojure.java.io :as io]))

(defn pdebug [msg x] (println msg x) x)

(defn debugger [active]
    (if active
        (fn [& args]
            (println (clojure.string/join " " args))
            (last args))
        (fn [& args] (last args))))

(defn str->int [s] (Integer/parseInt (str s)))
(defn str->long [s] (Long/parseLong (str s)))

(defn append [coll i v]
   (as-> (count coll) acc
         (- i acc)
         (if (pos? acc)
            (-> coll
                (concat (repeat acc 0))
                (vec))
            coll)
         (assoc acc i v)))

(defn read-input [year day]
    (fn ([] (->> (str year "/day" day ".txt")
                io/resource
                io/input-stream
                io/reader
                line-seq))
        ([v] (->> (str year "/day" day "-" v ".txt")
                io/resource
                io/input-stream
                io/reader
                line-seq))))

(defmacro defcase
  "Define a function letting the `input` a seq with
  the content of the `file` read from classpath, eg:
      (defcase test \"position/in/classpath.txt\" seq-file
          (println (first seq-file)) ;Prints the first line of the file
  "
  [name [input file & utils] & body]
  `(defn ~name []
     (let [~input (->> ~file
                      io/resource
                      io/input-stream
                      io/reader
                      line-seq)
            ~@utils]
       ~@body)))

(defmacro debug
  "Utility to debug an expression, usage:
      (debug + 5 6) ;print '(+ 5 6) 11'
                    ;evaluate to 11
  "
  [& expr]
  `(try 
        (let [res# ~expr]
            (println "Debug:" (first (quote ~expr)))
            (println (quote ~expr))
            (println "aruments:")
            (println ~@(rest expr))
            (println "result:")
            (println res#)
            (println "------------")
            res#)
        (catch Exception e#
            (println "Debug:" (first (quote ~expr)))
            (println (quote ~expr))
            (println "aruments:")
            (println ~@(rest expr))
            (println "error:")
            (println e#)
            (println "------------"))))

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
