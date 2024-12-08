(ns advent.2019.day8
    (:require [advent.core :as a]))

(def reader (a/read-input 2019 8))

(def input (reader))
(comment def input '("000111222" "000111222" "012012012" "012012012"))

(defn parse [w h code]
   (->> code
        (partition w)
        (partition h)))

(a/deflambda n-of-number [number layer]
   (->>  layer
         (mapcat identity)
         (filter #(= number %))
         (count)))
   
(defn part-1 []
   (->>  input
         (mapcat seq)
         (map a/str->int)
         (vec)
         (parse 25 6)
         (apply min-key (n-of-number 0))
         (a/pdebug ":")
         ((juxt (n-of-number 1) (n-of-number 2)))
         (apply *)))

(defn overlap [up bottom]
   (if (= 2 up) bottom up))

(defn part-2 []
   (->>  input
         (mapcat seq)
         (map a/str->int)
         (vec)
         (parse 25 6)
         (reduce (fn [acc layer]
                  (map #(map overlap %1 %2) acc layer)))
         (map (partial map {0 " " 1 "■"}))
         (map clojure.string/join)
         (map println)
         (doall)))
