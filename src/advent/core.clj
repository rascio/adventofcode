(ns advent.core
  (:require [clojure.java.io :as io]))

(defmacro defcase [name file body]
  `(defn ~name []
     (binding [*in* (->> ~file
                      io/resource
                      io/input-stream
                      io/reader
                      (new clojure.lang.LineNumberingPushbackReader))]
       ~body)))
