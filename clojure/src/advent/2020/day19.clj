(ns advent.2020.day19
  (:require [advent.core :as a]))

(def reader (a/read-input 2020 19))
(def input (reader "example2"))

(defn check-rule
  ([rules rule-n string idx]
   (check-rule rules rule-n string idx false))
  ([rules rule-n string idx debug]
   (if (= -1 idx)
     -1
     (let [rule (rules rule-n)]
       (when debug
         (println string "check" "[" idx "]" rule-n "=>" rule)
         (println (apply str (concat (repeat idx " ") ["↑"]))))
       (cond
         (= idx (.length string)) -1
         (char? rule) (if (= rule (nth string idx)) (inc idx) -1)
         (seqable? rule) (reduce
                          #(as-> %2 res
                             (reduce
                              (fn [acc r]
                                (-> (check-rule rules r string acc debug)
                                    (as-> $
                                          (if (< -1 $) $ (reduced -1)))))
                              idx
                              res)
                             (if (> res -1)
                               (reduced res)
                               -1))
                          -1
                          rule))))))
(defn parse-rule [line]
  (let [[type _ rule arg] (a/regex-patterns line
                                              :simple #"(\d+): \"(\w+)\""
                                              :complex #"(\d+): ([\d\|\s]+)")]
    (->> (case type
           :simple (first arg)
           :complex (reduce
                     (fn [[last & rest :as acc] item]
                       (case item
                         "|" (cons [] acc)
                         (apply vector
                          (->> item
                               (Integer/parseInt)
                               (conj last))
                          rest)))
                     [[]]
                     (.split arg " ")))
         (vector (Integer/parseInt rule)))))

(defn part1 []
  (->> input
       (reduce
        (fn [[state rules result] line]
            (case state
              :init (if (empty? line)
                      [:check rules 0]
                      (as-> (parse-rule line) $
                        (apply assoc rules $)
                        [state $ result]))
              :check (let [res (check-rule rules 0 line 0)]
                      (if (= res (.length line))
                        [state rules (inc result)]
                        [state rules result]))))
        [:init {} 0])
       (last)))

(defn inject-rules [rules]
  (->> ["8: 42 | 42 8" "11: 42 31 | 42 11 31"]
       (map parse-rule)
       (reduce #(apply assoc %1 %2) rules)))
(defn part2 []
  (->> input
       (reduce
        (fn [[state rules result] line]
          (case state
            :init (if (empty? line)
                    [:check (inject-rules rules) 0]
                    (as-> (parse-rule line) $
                      (apply assoc rules $)
                      [state $ result]))
            :check (let [res (check-rule rules 0 line 0)]
                         (if (= res (.length line))
                           [state rules (inc result)]
                           [state rules result]))
            :debug (do (doseq [r (sort-by first rules)] (println r))
                       (let [res (check-rule rules 0 "babbbbaabbbbbabbbbbbaabaaabaaa" 0 true)]
                         (if (= res (.length line))
                           (do (println "Found" line (rules 8) (rules 11))
                               [:end rules (inc result)])
                           [:end rules result])))
            :end [state rules result]))
        [:init {} 0])
       (last)))