(ns advent.2020.day8
  (:require [advent.core :as a]))

(def reader (a/read-input 2020 8))
(def input (reader))

(def op-regex #"(\w+) ([+-]\d+)")
(defn parse-op [line]
  (let [[_ code n] (re-matches op-regex line)]
    (list (keyword code) (a/str->int n))))

(defmulti exec (fn [[code] & _] code))
(defmethod exec :nop [[_ _] idx acc] [(inc idx) acc])
(defmethod exec :acc [[_ n] idx acc] [(inc idx) (+ n acc)])
(defmethod exec :jmp [[_ n] idx acc] [(+ n idx) acc])

(defn find-loop 
  ([instructions] (find-loop instructions [0 0] #{}))
  ([instructions state s]
   (loop [[idx acc] state
          seen s]
     (let [op (get instructions idx)]
       (cond (nil? op) [:res idx acc]
             (contains? seen idx) [:loop idx acc]
             :else (->> (conj seen idx)
                        (recur (exec op idx acc))))))))

(defn part1 []
  (->> input
       (map parse-op)
       (vec)
       (find-loop)
       (last)))

(defn opposite [[op n]]
  (case op
    :jmp [:nop n]
    :nop [:jmp n]
    nil))
(defn fix-graph
  ([instructions] (fix-graph instructions [0 0] #{}))
  ([instructions [idx acc] seen]
   (cond
     (contains? seen idx) [:loop idx acc]
     (nil? (get instructions idx)) [:res idx acc]
     :else (let [op (nth instructions idx)
                 [r :as res] (->> (conj seen idx)
                                  (fix-graph instructions 
                                               (exec op idx acc)))
                 fallback (opposite op)]
             (if (and (= :loop r)
                      (some? fallback))
               (-> (assoc instructions idx fallback)
                   (find-loop [idx acc] seen))
               res)))))

(defn part2 []
  (->> input
       (map parse-op)
       (vec)
       (fix-graph)
       (last)))