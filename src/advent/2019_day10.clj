(ns advent.2019-day10
    (:require [advent.core :as a]
              [clojure.math.combinatorics :as comb]))

(def reader (a/read-input 2019 10))
(def debug (a/debugger false))

(def input (reader))
(comment def input (reader "test4"))
(comment def input '(".#..#")   ;(4 0)
             "....."
             "#####"
             "....#"
             "...##") ;best: (3 4)
(comment def input '(".#....#####...#..")
             "##...##.#####..##"
             "##...#...#.#####."
             "..#.....X...###.."
             "..#.#.....#....##")

(defn str->row [s]
   (vec (map #(= \# %) s)))
(defn abs [n]
   (int (Math/abs n)))

(def distance (comp abs -))

(defn mcd [a b]
   (loop [min (min a b)
          max (max a b)]
      (if (zero? min)
         max
         (recur (mod max min) min))))

(a/deflambda in-between [graph [x1 y1] [x2 y2]]
   (let [x-distance (- x2 x1)
         y-distance (- y2 y1)
         step (mcd (abs x-distance) (abs y-distance))
         x-step (quot x-distance step)
         y-step (quot y-distance step)]
      (debug [x1 y1] "to" [x2 y2] "in" step "step" "move x of" x-step "each" y-step "y")
      (debug [x1 y1] "->"
         (->> (range 1 step)
            (map (juxt 
                     (comp (partial + x1) 
                           (partial * x-step))
                     (comp (partial + y1)
                           (partial * y-step))))
            (filter (comp (partial get-in graph) reverse))
            (first)))))


(defn plus [n] (fnil (partial + n) 0))

(defn graph->asteroids [g]
   (transduce
      (map-indexed (fn [y row] 
                     (keep-indexed #(when %2 [%1 y]) row)))
      concat
      g))
(a/deflambda can-see? [graph from to]
   (nil? (in-between graph from to)))

(a/deflambda can-see [graph from asteroids]
   (->> asteroids
      (remove #(= from %))
      (filter (can-see? graph from))))

(defn best-position [graph]
   (let [asteroids (graph->asteroids graph)]
      (loop [[asteroid & others] asteroids
               acc nil]
         (debug "asteroid" asteroid "=>" others)
         (if (nil? asteroid)
            acc
            (as-> (can-see graph asteroid others) ACC
               (reduce #(update %1 %2 (plus 1))
                  (update acc asteroid (plus (count ACC)))
                  ACC)
               (recur others ACC))))))


(defn part-1 []
   (->>  input
         (map str->row)
         (vec)
         (best-position)
         (apply max-key second)))

(a/deflambda ->degrees [[x1 y1] [x2 y2]]
   (-> (Math/atan2 (- x1 x2) (- y1 y2))
       (* (/ 180 Math/PI))))

(defn point-distance [[x1 y1] [x2 y2]]
   (Math/sqrt
      (+ (Math/pow (distance x1 x2) 2)
         (Math/pow (distance y1 y2) 2))))

(a/deflambda relative-degree [from to]
    (-> (->degrees from to)
        (* -1)
        (mod 360)))

(defn count-asteroids [graph]
   (->> (mapcat identity graph)
        (filter identity)
        (count)))

(a/deflambda zero-> [n v] (if (zero? v) n v))

(defn remove-asteroids [from n g]
   (let [[init :as others] (->> (graph->asteroids g)
                              (filter #(not= from %))
                              (map (juxt identity (relative-degree from)))
                              (sort-by second))]
      (loop [graph g
             [asteroid degree] init
             asteroids others
             removed '()]
            (debug "loop" asteroid degree)
            (if (or  (= n (count removed)) 
                     (empty? asteroids))
               (do
                  (debug "removed" (count removed) "remaining" (count asteroids) "asteroids" removed)
                  removed)
               (let [in-between (in-between graph from asteroid)
                     to-remove (or in-between asteroid)]
                  (debug "removing asteroid" to-remove)
                  (recur 
                        (assoc-in graph (reverse to-remove) false)
                        (->> asteroids
                           (sort-by (comp
                                       (zero-> 360)
                                       #(mod % 360) 
                                       #(- % degree)
                                       second))
                           (first))
                        (remove (comp #{to-remove} first) asteroids)
                        (cons to-remove removed)))))))

(def best-asteroid [20 18])
(comment def best-asteroid [11 13])
(defn part-2 [& {n :n :or {n 200}}]
   (let [graph (vec (map str->row input))]
      (->>  input
            (map str->row)
            (vec)
            (remove-asteroids best-asteroid n)
            (first)
            (apply (fn [x y] (+ (* 100 x) y))))))

