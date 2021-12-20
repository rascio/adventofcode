(ns advent.2021.day20
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 20))

(def input (->> (reader)
                (filter seq)
                ((fn [[algorithm & image]]
                   {:algorithm algorithm 
                    :image (->> image
                                (vec)
                                (reduce-kv
                                 (fn [acc y row]
                                   (reduce-kv
                                    (fn [acc x char]
                                      (if (= \# char)
                                        (assoc acc [x y] char)
                                        acc))
                                    acc
                                    (vec row)))
                                 {}))}))))

(defn neighbors [[x y]]
  (vec
   (for [y' (range (dec y) (+ 2 y))
         x' (range (dec x) (+ 2 x))]
     [x' y'])))
(defn code-idx [code]
  (->> (vec code)
       (reduce-kv
        (fn [res idx digit]
          (+ res
             (if (= \# digit)
               (Math/pow 2 idx)
               0)))
        0)))
(defn enhanced-pixel-idx [pixel default [[min-x max-x] [min-y max-y]] image]
  (->> (neighbors pixel)
       (map (fn [[x y :as p]] 
              (if (and (<= min-x x max-x)
                       (<= min-y y max-y))
                (get image p \.)
                default)))
       (reverse)
       (code-idx)))

(def min-max-zero [(Long/MAX_VALUE) 0])
(defn get-image-limits
  [image]
  (reduce (fn [[[min-x max-x] [min-y max-y]] [x y]]
            [[(min min-x x) (max max-x x)]
             [(min min-y y) (max max-y y)]])
          [min-max-zero min-max-zero]
          (keys image)))
(defn process
  [image default algorithm]
  (let [[[min-x max-x] [min-y max-y] :as dim] (get-image-limits image)]
    (->>
     (for [y (range (- min-y 2) (+ max-y 3))
           x (range (- min-x 2) (+ max-x 3))]
       [x y])
     (reduce
      (fn [acc pixel]
        (as-> image $
          (enhanced-pixel-idx pixel default dim $)
          (.charAt algorithm $)
          (if (= \# $)
            (assoc acc pixel $)
            acc)))
      {}))))

(defn enhance [image algorithm times]
  (->> (range 0 times)
       (reduce (fn [[default image] _]
                 (vector
                  (.charAt algorithm (code-idx (repeat 9 default)))
                  (process image default algorithm)))
               [\. image])
       (second)))

(defn part1 []
  (->> (enhance (input :image) (input :algorithm) 2)
       (vals)
       (filter #(= \# %))
       (count)))

(defn part2 []
  (->> (enhance (input :image) (input :algorithm) 50)
       (vals)
       (filter #(= \# %))
       (count)))