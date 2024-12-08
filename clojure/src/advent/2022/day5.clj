(ns advent.2022.day5
  (:require [advent.core :as a]
            [clojure.string :as string]))

(def reader (a/read-input 2022 5))
(def input (reader))

(defn parse-setup [acc line]
  (if (re-find #"\d+" line)
    [:setup-completed (->> acc 
                           (apply map vector)
                           (map #(filter some? %))
                           (vec))]
    [:init (->> line
                (re-seq #"( {3}|\[([A-Z])\]) ?")
                (map last)
                (conj acc))]))

(defn crate-mover-9000 [acc line]
  (let [[quantity from to] (->> line
                                (re-seq #"\d+")
                                (map a/str->int))
        from (dec from)
        to (dec to)
        from-stack (nth acc from)
        [from-head to-move] (-> from-stack
                                count
                                (- quantity)
                                (split-at from-stack))]
    (-> acc
        (assoc from (vec from-head))
        (assoc to (vec (concat (nth acc to)
                               (reverse to-move)))))))
(defn parse-input [version]
  (->> input
       (reduce
        (fn [[state acc] line]
          (case state
            :init (parse-setup acc line)
            :setup-completed [:instructions acc]
            :instructions [:instructions (version acc line)]))
        [:init])))

(defn part-1 []
  (->> (parse-input crate-mover-9000)
       (second)
       (map last)
       (string/join)))

(defn crate-mover-9001 [acc line]
  (let [[quantity from to] (->> line
                                (re-seq #"\d+")
                                (map a/str->int))
        from (dec from)
        to (dec to)
        from-stack (nth acc from)
        [from-head to-move] (-> from-stack
                                count
                                (- quantity)
                                (split-at from-stack))]
    (-> acc
        (assoc from (vec from-head))
        (assoc to (vec (concat (nth acc to)
                               to-move))))))


(defn part-2 []
  (->> (parse-input crate-mover-9001)
       (second)
       (map last)
       (string/join)))
