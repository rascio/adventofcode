(ns advent.2020.day18
  (:require [advent.core :as a]))

(def reader (a/read-input 2020 18))
#_(def input (reader))
(def input "1 + (2 * 3) + (4 * (5 + 6))")
(def open "(")
(def close ")")
(defn parse [expression]
  (loop [[expr & tail] expression
         acc ()]
    (cond (or (nil? expr)
              (= close expr)) acc
          (= open expr) (conj acc (parse tail))
          (re-matches #"\d+" expr) (->> (a/str->int expr)
                                       (cons acc)
                                       (recur))
          :else (cons expr acc))))
