(ns advent.2022.day6
  (:require [advent.core :as a]))

(def reader (a/read-input 2022 6))
(def input (reader "example"))

(defn detect [packet-size datastream-buffer]
  (->> datastream-buffer
       (partition packet-size 1)
       (map-indexed (fn [index chars]
                      [(+ packet-size index) (set chars)]))
       (filter (fn [[_ chars]] (= packet-size (count chars))))
       (map first)
       (first)))

(defn start-of-packet [datastream-buffer]
  (detect 4 datastream-buffer))

(defn part-1 []
  (->> input
       (map start-of-packet)))

(defn start-of-message [datastream-buffer]
  (detect 14 datastream-buffer))

(defn part-2 []
  (map start-of-message input))