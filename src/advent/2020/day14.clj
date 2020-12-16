(ns advent.2020.day14
  (:require [advent.core :as a]
            [math :as m]
            [clojure.string :as s]))

(def reader (a/read-input 2020 14))
(def input (reader))

(defn binary->str [n]
  (->> (map #(if % "1" "0") n)
       (s/join)))

(defn mask [string]
  (let [mask (->> (.toCharArray string)
                  (mapv #(case %
                          \X identity
                          \0 (constantly false)
                          \1 (constantly true))))]
    (fn [n]
      (->> (map list mask n)
           (map (fn [[f v]] (f v)))))))

(defn num->binary [n]
  (loop [acc ()
         n' n]
    (if (= 0 n')
      (-> (- 36 (count acc))
          (repeat false)
          (concat acc))
      (let [r (long (/ n' 2))
            bit (= 1 (mod n' 2))]
        (recur (cons bit acc) r)))))
(defn str->binary [s]
  (->> (a/str->long s)
       (num->binary)))
(defn binary->num [b]
  (->> (reverse b)
       (map-indexed list)
       (reduce (fn [acc [idx v]]
                 (->> (m/pow 2 idx)
                      (* (if v 1 0))
                      (+ acc)))
               0)))

(defprotocol InstructionsSet
  (set-mask [this mask])
  (set-mem [this [address v]]))

(defn exec-instruction [state cmd]
  (try
    (or (some->> (re-find #"[X01]{36}" cmd)
                 (set-mask state))
        (some->> (re-find #"mem\[(\d+)\] = (\d+)" cmd)
                 (next)
                 (set-mem state)))
    (catch Exception e
      (println cmd)
      (throw e))))

(defn sum-memory [memory]
  (->> (vals memory)
       (map binary->num)
       (reduce +)))
(defn debug-memory [memory]
  (doseq [[address v] memory]
    (println address "]" (binary->str v))))

(defn exec [chip instructions]
  (->> instructions
       (reduce exec-instruction chip)
       (:memory)
       (sum-memory)))


(defrecord DecoderChipV1 [memory mask]
  InstructionsSet
  (set-mask [this string]
    (->> (.toCharArray string)
         (assoc this :mask)))
  (set-mem [this [address v]]
    (assoc-in this
              [:memory (a/str->long address)]
              (->> (str->binary v)
                   (map list mask v)
                   (map (fn [[mask-bit bit]]
                          (case mask-bit
                            \X bit
                            \0 false
                            \1 true)))))))
(defn part1 []
  (exec (DecoderChipV1. {} nil) input))



(defn floating-addresses [mask address]
  (loop [[[bit-mask bit-address] & rest] (map list mask address)
         result [[]]]
    (if (nil? bit-mask)
      result
      (recur rest
             (case bit-mask
               \0 (map #(conj % bit-address) result)
               \1 (map #(conj % true) result)
               \X (mapcat #(list (conj % false)
                                 (conj % true)) result))))))
(defrecord DecoderChipV2 [memory mask]
  InstructionsSet
  (set-mask [this string]
            (->> (.toCharArray string)
                 (assoc this :mask)))
  (set-mem [this [address v]]
           (->> (floating-addresses mask (str->binary address))
                (reduce #(assoc-in %1 [:memory (binary->num %2)] 
                                   (str->binary v))
                        this))))

(defn part2 []
  (exec (DecoderChipV2. {} nil) input))