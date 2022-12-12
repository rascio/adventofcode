(ns advent.2020.day18
  (:require [advent.core :as a]
            [clojure.string :as string]))

(def reader (a/read-input 2020 18))
#_(def input (reader))
(def input "1 + (2 * 3) + (4 * (5 + 6))
1 + 2 * 3 + 4 * 5 + 6")
(def token-regex #"\(|\)|\+|\*|\d+")

(defn parse [expression]
  (loop [[token & tail] expression
         acc nil]
    (if (nil? token)
      acc 
      (condp re-matches token
        #"\d+" (recur tail (if (nil? acc) 
                             (a/str->int token)
                             (concat acc [(a/str->int token)])))
        #"\+" (recur tail [:+ acc])
        #"\*" (recur tail [:* acc])
        #"\(" (let [[v tokens] (parse tail)]
                (recur tokens (concat acc [v])))
        #"\)" [acc tail]))))

(defn eval-expr 
  [expr]
  (if (seq? expr)
    (let [[op a b] expr]
      ((if (= :+ op) + *) (eval-expr a)
          (eval-expr b)))
    expr))
(defn part-1 []
  (doseq [expr (string/split-lines input)]
    (let [parsed (parse (re-seq token-regex expr))
          result (eval-expr parsed)]
      (println expr)
      (println parsed)
      (println result))))

(defn group-parenthesis [tokens]
  (loop [[t & tokens] tokens
         acc []]
    (cond
      (nil? t) acc
      (= t "(") (let [[expr tokens] (group-parenthesis tokens)]
                  (recur tokens
                         (conj acc expr)))
      (= t ")") [acc tokens]
      :else (recur tokens
                   (conj acc t)))))
#_(defn split-expr [op tokens]
  (loop [[t & tokens] tokens
         acc []]
    (when (= "*" op) (a/debug-forms op t))
    (cond
      (nil? t) acc
      (= op t) [(keyword op) acc (split-expr op tokens)]
      (seqable? t) (recur
                    tokens
                    (conj acc (split-expr op t)))
      :else (recur
             tokens
             (conj acc t)))))
(defn split-expr [op tokens]
  (loop [[a f b & tokens] tokens]
    (if (nil? b)
      "OK"
      (do (a/debug-forms a f b)
          (recur (cons [f a b] tokens))))))
(defn parse-special [expression]
  (println "=>" expression)
  (->> expression
       (group-parenthesis)
       (split-expr "+")
       #_(split-expr "*")))

(defn part-2 []
  (doseq [expr (string/split-lines input)]
    (let [parsed (parse-special (re-seq token-regex expr))
          #_#_result (eval-expr parsed)]
      (println expr)
      (println parsed)
      #_(println result))))