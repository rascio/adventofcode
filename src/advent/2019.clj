(ns advent.2019
    (:require [advent.core :as a]
              [clojure.spec.alpha :as s]))

(defn str->int [s] (Integer/parseInt (str s)))
(a/deflambda is-length? [l s] (= l (count s)))

;Day1
(a/defcase day1
    [input "2019/day1.txt"]
    (->> input
        (map #(-> (Integer/parseInt %)
                  (/ 3)
                  (int)
                  (- 2)))
        (reduce +)))

(defn fuel [mass]
    (let [f (/ 3 (int (- 2 mass)))] 
        (if (neg? f)
            0
            (+ f (fuel f)))))
(a/defcase day1-2
    [input "2019/day1.txt"]
    (->> input
        (map (comp fuel str->int))
        (reduce +)))


;Day2
(a/defcase day2
    [input "2019/day2.txt"
     operations {1 + 2 *}
     compute (fn [instructions]
                (let [state (atom instructions)]
                    (loop [index 0]
                        (let [[op arg0 arg1 pos] (subvec @state index (+ index 4))
                              operator (operations op)]
                            (when operator
                                (->> (operator (nth @state arg0) (nth @state arg1))
                                     (swap! state assoc pos)))
                            (if (= 99 op)
                                @state
                                (recur (+ index 4)))))))]
    (let [res (->> input
                   (mapcat #(clojure.string/split % #","))
                   (map str->int)
                   (vec)
                   (compute)
                   (clojure.string/join ","))]
        (println res)))

(defn set-noun-verb [n v i]
    (assoc i 1 n 2 v))
(a/defcase day2-2
    [input "2019/day2.txt"
     operations {1 + 2 *}
     compute (fn [instructions]
                (let [state (atom instructions)]
                    (loop [index 0]
                        (let [[op arg0 arg1 pos] (subvec @state index (+ index 4))
                              operator (operations op)]
                            (when operator
                                (->> (operator (nth @state arg0) (nth @state arg1))
                                     (swap! state assoc pos)))
                            (if (= 99 op)
                                @state
                                (recur (+ index 4)))))))]
    (let [instructions (->> input
                           (mapcat #(clojure.string/split % #","))
                           (map str->int)
                           (vec))]
        (first
            (for [n (range 0 99)
                  v (range 0 99)
                  :let [program (set-noun-verb n v instructions)
                        [out & _] (compute program)]
                  :when (= 19690720 out)]
                (+ (* 100 n) v)))))


;Day3   
(defn direction [[dir v] p]
    (let [op (case dir
                :U (fn [[x y]] [x (inc y)])
                :D (fn [[x y]] [x (dec y)])
                :L (fn [[x y]] [(dec x) y])
                :R (fn [[x y]] [(inc x) y]))]
        (->> (op p)
            (iterate op)
            (take v))))
(defn line->cmds [l]
    (->> (clojure.string/split l #",")
         (map (partial re-matches #"(\w)(\d+)"))
         (map (fn [[_ dir v]] [(keyword dir) (str->int v)]))))
(defn cmds->matrix [cmds]
    (->> cmds
        (reduce (fn [res cmd]
                    (concat res 
                            (direction cmd (last res))))
            [[0 0]])
        (drop 1)))
(defn manhattan-distance [[p1 p2] [q1 q2]]
    (+ (Math/abs (- p1 q1))
       (Math/abs (- p2 q2))))
(a/defcase day3
    [input "2019/day3.txt"]
    (->> input
        (map line->cmds)
        (map cmds->matrix)
        (map set)
        (apply clojure.set/intersection)
        (map #(manhattan-distance [0 0] %))
        (apply min)))
        
(defn find-min-step [w1 w2]
    (->> w1
        (map-indexed (fn [step p]
                        (let [step2 (.indexOf w2 p)]
                            (when (pos? step2)
                                [(inc step) (inc step2)]))))
        (filter #(not (nil? %)))))
(a/defcase day3-2
    [input "2019/day3.txt"]
    (->> input
        (map line->cmds)
        (map cmds->matrix)
        (apply find-min-step)
        (map (partial apply +))
        (apply min)))

;Day4
(s/def ::password (s/and (is-length? 6)
                         #(re-find #"(\d)\1+" %)
                         #(->> (map int %)
                               (apply <=))))
(defn day4 []
    (->> (range 246540 787420)
         (map str)
         (filter #(s/valid? ::password %))
         (count)))

(s/def ::password-v2 (s/and (is-length? 6)
                            #(->> (re-seq #"(\d)\1+" %)
                                  (map first)
                                  (filter (is-length? 2))
                                  (count)
                                  (<= 1))
                            #(->> (map int %)
                                  (apply <=))))
(defn day4-2 []
    (->> (range 246540 787420)
         (map str)
         (filter #(s/valid? ::password-v2 %))
         (count)))

;Day5
(a/defcase day5
    [input "2019/day5.txt"
     read-arg (fn [state v mode]
                (if (zero? mode)
                    (get-in state [:memory v])
                    v))
     process (fn [state]
                (let [[code & [arg0 arg1 arg2]] (subvec (state :memory) 
                                                        (state :index) 
                                                        (min (+ (state :index) 4)
                                                            (count (state :memory))))
                      op (mod code 100)
                      [m0 m1 m2] [(quot (mod code 1000) 100)
                                  (quot (mod code 10000) 1000)
                                  (quot (mod code 100000) 10000)]]
                    (case op
                        1 (-> (assoc-in state [:memory arg2] (+ (read-arg state arg0 m0) 
                                                                (read-arg state arg1 m1)))
                              (update :index #(+ 4 %)))
                        2 (-> (assoc-in state [:memory arg2] (* (read-arg state arg0 m0) 
                                                                (read-arg state arg1 m1)))
                              (update :index #(+ 4 %)))
                        3 (-> (assoc-in state [:memory arg0] 1) ;1 == (read-line)
                              (update :index #(+ 2 %)))
                        4 (do (println (get-in state [:memory arg0]))
                              (update state :index #(+ 2 %)))
                        99 (assoc state :index -1)
                        (throw (ex-info "Not managed" {:value op})))))                  
     compute (fn [instructions]
                (let [state (atom {:memory instructions
                                   :index 0})]
                    (while (<= 0 (@state :index))
                        (swap! state process))
                    @state))]
    (let [res (->> input
                   (mapcat #(clojure.string/split % #","))
                   (map str->int)
                   (vec)
                   (compute))]))
