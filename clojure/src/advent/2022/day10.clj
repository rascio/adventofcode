(ns advent.2022.day10 
  (:require [advent.core :as a]
            [clojure.string :as string]))

(def reader (a/read-input 2022 10))
(def input (reader))

(def instructions
  {"addx" {:cycles 2
           :args [a/str->int]
           :fn +}
   "noop" {:cycles 1
           :fn identity}})

(defn parse-instruction
  [line]
  (let [[cmd & args] (string/split line #" ")
        cmd (instructions cmd)
        args (->> (map list (cmd :args) args)
                  (map (fn [[f v]] (f v))))]
    (assoc cmd :args args)))

(defn eval-cmd [{:keys [args fn]} state]
  (apply fn state args))

(defn exec-cycle [state [{:keys [cycles] :as cmd} & cmds]]
  (if (= 1 cycles)
    [(eval-cmd cmd state) cmds]
    [state (-> (update cmd :cycles dec) 
               (cons cmds))]))

(defn calculate-signal-strength
  [state cmds cycles]
  (loop [[state cmds] [state cmds]
         cycle 1
         [next-check & cycles] cycles
         strenght 0]
    (cond
      (nil? next-check) strenght
      (= cycle next-check) (recur (exec-cycle state cmds)
                                  (inc cycle)
                                  cycles
                                  (+ strenght (* state cycle)))
      :else (recur (exec-cycle state cmds)
                   (inc cycle)
                   (cons next-check cycles)
                   strenght))))

(defn part-1 []
  (let [state 1
        cmds (map parse-instruction input)]
    (calculate-signal-strength
     state
     cmds
     [20 60 100 140 180 220])))


(defn in-range [idx n]
  (<= (dec idx) n (inc idx)))

(defn crt-print
  [state pos]
  (when (zero? pos)
    (println))
  (if (in-range pos state)
    (print "#")
    (print ".")))

(defn exec-crt-cycle
  [state cmds cycle]
  (let [pos (mod cycle 40)]
    (crt-print state pos)
    (exec-cycle state cmds)))

(defn exec-program
  [cmds]
  (loop [[state cmds] [1 cmds]
         cycle 0]
    (when (not-empty cmds)
      (recur (exec-crt-cycle state cmds cycle)
             (inc cycle)))))

(defn part-2 []
  (->> input
       (map parse-instruction)
       (exec-program))
  (println))