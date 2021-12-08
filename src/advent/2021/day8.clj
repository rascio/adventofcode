(ns advent.2021.day8
  (:require [advent.core :as a]
            [clojure.set :as s]))

(def reader (a/read-input 2021 8))
(def input (->> (reader)
                (map (fn [line]
                       (->> (.split line "\\|")
                            (map #(re-seq #"\w+" %)))))))

(def unique-counts #{2 4 3 7})

(defn part1 []
  (->> input
       (mapcat second)
       (map count)
       (filter unique-counts)
       (count)))


(def digits-schema
  {0 #{:a :b :c :e :f :g}
   1 #{:c :f}                 ;X
   2 #{:a :c :d :e :g}
   3 #{:a :c :d :f :g}
   4 #{:b :c :d :f}           ;X
   5 #{:a :b :d :f :g}
   6 #{:a :b :d :e :f :g}
   7 #{:a :c :f}              ;X
   8 #{:a :b :c :d :e :f :g}  ;X
   9 #{:a :b :c :d :f :g}})
(def digits-reverse-schema (s/map-invert digits-schema))
(def unique-digits [1 7 4 8])

(defn unique-digits-mapping
  "Get the mappings for the unique digits, like:
   {1 \"be\", 7 \"edb\", 4 \"cgeb\", 8 \"cfbegad\"}"
  [digits]
  (->> unique-digits
       (reduce
        (fn [acc digit]
          (as-> (count (digits-schema digit)) $
            (filter #(= $ (count %)) digits)
            (first $)
            (conj acc [digit $])))
        [])))

(defn unique-digits-possibilites
  "From the mappings of unique digits create a list
   of valid possible mappings, sorted from the mapping
   with less possibilities, like:
   ([d #{:a}] [b #{:c :f}] [e #{:c :f}] [c #{:b :d}] [g #{:b :d}] [f #{:e :g}] [a #{:e :g}])"
  [mappings]
  (->> mappings
       (reduce
        (fn [acc [d wires]]
          (->> (for [w (.toCharArray wires)
                     :when (nil? (acc w))]
                 [w (reduce #(s/difference %1 %2)
                            (digits-schema d)
                            (vals acc))])
               (into {})
               (merge acc)))
        {})
       (sort-by #(count (second %)))))

(defn is-valid?
  [m digits]
  (let [wire-mappings (s/map-invert m)]
    (->> digits
         (reduce
          (fn [acc string]
            (let [wires (set (map wire-mappings string))
                  n (digits-reverse-schema wires)]
              (if (nil? n)
                (reduced nil)
                (assoc acc (set string) n))))
          {}))))

(defn create-mappings
  ([digits]
   (as-> digits $
     (unique-digits-mapping $)
     (create-mappings digits (unique-digits-possibilites $) {})))
  ([digits [[c [t' & tries]] & others] mappings]
   (cond
     (nil? c) (is-valid? mappings digits)
     (nil? t') nil
     (not (nil? (mappings t'))) (create-mappings digits
                                                 (cons [c tries] others)
                                                 mappings)
     :else (or
             (create-mappings digits
                              others
                              (assoc mappings t' c))
             (create-mappings digits
                              (cons [c tries] others)
                              mappings)))))

(defn part2 []
  (->> input
       (map
        (fn [[patterns screen]]
          (reduce
           str
           (as-> (create-mappings patterns) $
             (mapv (comp $ set) screen)))))
       (map a/str->int)
       (reduce +)))
