(ns advent.core
  (:require [clojure.java.io :as io]))

(defmacro with-input [file body]
    `(binding [*in* (->> ~file
                     io/resource
                     io/input-stream
                     io/reader
                     `(new clojure.lang.LineNumberingPushbackReader))])
     ~body)

(defmacro prova [a]
  `(println ~a))
