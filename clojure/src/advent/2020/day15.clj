(ns advent.2020.day15
  (:require [advent.core :as a]))

(def input [0,13,1,16,6,17])
(def example [1,3,2])

(defn play [starters end-turn]
  (loop [turn (inc (count starters))
         mem (->> starters
                  (drop-last)
                  (map-indexed list)
                  (reduce (fn [acc [i v]]
                            (assoc acc v (inc i)))
                          {}))
         last-number (last starters)]
    (let [last-turn (dec turn)
          next (or (some->> (get mem last-number nil)
                            (- last-turn))
                   0)]
      #_(if (= 0 (mod turn 250))
        (println "Turn" turn
                 ": last " last-number
                 (if (contains? mem last-number)
                   (str "next " next " last seen " (get mem last-number))
                   " never seen")))
      (if (= turn end-turn)
          next
          (recur (inc turn)
                 (assoc mem last-number last-turn)
                 next)))))

(defn part1[]
  (play input 2020))

(defn part2[]
  (play input 30000000))
