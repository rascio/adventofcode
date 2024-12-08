(ns advent.2022.day7
  (:require [advent.core :as a]
            [clojure.string :as string]
            [clojure.core.match :as m]))

(def reader (a/read-input 2022 7))
(def input (reader))

(defn parse-input [input]
  (->> input
       (map #(string/split % #" "))
       (reduce (fn [[pwd tree] [a b c]]
                 (m/match [a b c]
                   ["$" "ls" _] [pwd tree]
                   ["$" "cd" ".."] [(pop pwd) tree]
                   ["$" "cd" name] [(conj pwd name) tree]
                   ["dir" _ _] [pwd tree]
                   [size name _] [pwd (assoc-in tree 
                                                (conj pwd name) 
                                                (a/str->int size))]))
               [[] nil])
       second))

(defn absolute-files [path tree]
  (cond
    (map? tree) (reduce-kv (fn [acc name v]
                             (->> v
                                  (absolute-files (conj path name))
                                  (concat acc)))
                           ()
                           tree)
    (number? tree) [[path tree]]))

(def sum (fnil + 0))

(defn get-folders-size [tree]
  (reduce (fn [acc [path size]]
            (->> (drop-last path)
                 (reduce (fn [[parent acc] t]
                           (let [dir (conj parent t)]
                             [dir (update acc dir sum size)]))
                         [nil acc])
                 (second)))
          {}
          tree))

(defn part-1 []
  (->> input
       (parse-input)
       (absolute-files [])
       (get-folders-size)
       (map second)
       (filter #(<= % 100000))
       (apply +)))

(def disk-size 70000000)
(def required-disk 30000000)

(defn part-2 []
  (let [files (->> input
                   (parse-input)
                   (absolute-files []))
        folders-sizes (get-folders-size files)
        used-disk (->> files
                       (map second)
                       (apply +))
        free-disk (- disk-size used-disk)
        needed-disk (- required-disk free-disk)]
    (->> folders-sizes
         (map second)
         (filter #(<= needed-disk %))
         (apply min))))