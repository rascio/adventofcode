(ns advent.2019-day11
    (:require [advent.core :as a]
              [clojure.spec.alpha :as s]
              [clojure.math.combinatorics :as comb]
              [clojure.core.async :as async]))

(def reader (a/read-input 2019 11))
(def debug (a/debugger false))

(def input (reader))

(defprotocol Args
   (read [this pos])
   (write [this pos v])
   (current-operator [this]))

(defn state->args [state]
   (let [code (get-in state [:memory (state :index)])
         op (mod code 100)
         modes #(-> code
                  (mod (Math/pow 10 (+ 3 %)))
                  (quot (Math/pow 10 (+ 2 %)))
                  (int))
         idx->val (fn [idx] (get-in state [:memory idx] 0))
         pos->idx (fn [pos] (+ (inc pos) (state :index)))]
      (reify Args
         (read [_ pos]
            (as-> (pos->idx pos) acc
                  (idx->val acc)
                  (case (modes pos)
                     0 (idx->val acc)
                     1 acc
                     2 (idx->val (+ (state :relative-base) acc)))))
         (write [_ pos v]
            (let [index (as-> (pos->idx pos) acc
                              (idx->val acc)
                              (case (modes pos)
                                 0 acc
                                 1 acc
                                 2 (+ (state :relative-base) acc)))]
               (debug (state :id) "op:" op "(to" pos (modes pos)") = [" index "] <==" v)
               (as-> (a/append (state :memory) index v) acc
                     (assoc state :memory acc))))
         (current-operator [_] op))))

(defn process [state]
   (async/go 
      (let [args (state->args state)]
         (debug (state :id) 
                "exec:" (current-operator args) "@" (state :index) 
                (subvec (state :memory) (state :index) (min (+ (state :index) 4) (count (state :memory))))
                "relative-base:" (state :relative-base) "[" (get-in state [:memory (state :relative-base)]) "]")
         (try
            (case (current-operator args)
                  1 (-> (write args 2 
                           (+ (read args 0) 
                              (read args 1)))
                        (update :index + 4))
                  2 (-> (write args 2 
                           (* (read args 0) 
                              (read args 1)))
                        (update :index + 4))
                  3 (let [in (async/<! (state :input))]
                        (debug (state :id) "\t" in "<== INPUT" (state :input))
                        (-> (write args 0 in)
                            (update :index + 2)))
                  4 (do (debug (state :id) "\t" (read args 0) "==> OUTPUT" (state :output))
                        (async/put! (state :output) (read args 0))
                        (update state :index + 2))
                  5 (->> (if (not (zero? (read args 0)))
                           (read args 1)
                           (+ 3 (state :index)))
                        (assoc state :index))
                  6 (->> (if (zero? (read args 0))
                           (read args 1)
                           (+ 3 (state :index)))
                        (assoc state :index))
                  7 (-> (write args 2
                           (if (< (read args 0) (read args 1)) 
                              1 
                              0))
                        (update :index + 4))
                  8 (-> (write args 2 
                           (if (= (read args 0) (read args 1))
                              1
                              0))
                        (update :index + 4))
                  9 (-> state
                        (update :relative-base + (read args 0))
                        (update :index + 2))
                  99 (do 
                        (async/close! (state :output))
                        (dissoc state :index))
                  {:error true :op (current-operator args) :state state})
            (catch Exception e
               (debug "There was an error" (.getMessage e))
               {:op (current-operator args) :state state :cause e})))))

(defn int-code-eval [conf instructions in out]
    (debug "int-code-eval:" conf (count instructions) in out)
    (async/go
      (try
         (loop [state {:id conf
                       :memory instructions
                       :index 0
                       :relative-base 0
                       :input in
                       :output out}]
            (if (nil? (state :index))
               state
               (->> (process state)
                    (async/<!)
                    (recur))))
         (catch Exception e
            (debug conf "ERROR\n" (.getMessage e) "\n=>" (ex-data e))
            (ex-data e)))))

(defn polar->cartesian [r d]
   (let [radians (a/round (Math/toRadians d) 1)]
      (->> [(Math/cos radians) (Math/sin radians)]
         (map (partial * r))
         (map #(Math/round %)))))

(def rotation {0 -90 1 90})
(defn robot [in out & {init :init :or {init 0}}]
   (async/go-loop [painted {[0 0] init}
                   index [0 0]
                   degree -90]
      (debug "robot-loop" index degree)
      (async/put! out (get painted index 0))
      (debug "waiting")
      (let [paint (async/<! in)
            rotate (async/<! in)]
         (debug "robot>" paint rotate)
         (if (nil? paint)
            painted
            (as-> (+ degree (rotation rotate)) new-angle
               (recur 
                  (assoc painted index paint)
                  (vec (map + index (polar->cartesian 1 new-angle)))
                  new-angle))))))


(defn part-1 []
   (let [in (async/chan)
         out (async/chan)
         instructions (->> (clojure.string/split (first input) #",")
                           (map a/str->long)
                           (vec))
         robot (robot out in)
         program (int-code-eval "0" instructions in out)]
        (do (debug "process:" (async/<!! program)
               (async/<!! (async/into [] out)))
            (->> (async/<!! robot)
                 (count)))))

(defn abs [n]
   (int (Math/abs n)))
(def distance (comp abs -))

(defn print-drawing [points]
   (let [[max-x min-x max-y min-y] (reduce 
                                    (fn [[xmax xmin ymax ymin] [[x y] _]] 
                                       [(max xmax x) (min xmin x) 
                                        (max ymax y) (min ymin y)])
                                    [0 Integer/MAX_VALUE 0 Integer/MAX_VALUE]
                                    points)
         vec-of (fn [n x] (vec (repeat (inc n) x)))
         w (distance min-x max-x)
         h (distance min-y max-y)
         shift-y (abs min-y)
         shift-x (abs min-x)
         matrix (vec-of h (vec-of w " "))]
      (->> points
         (reduce (fn [acc [[x y] paint]]
                   (update acc (+ shift-y y) a/append (+ shift-x x) ({0 " " 1 "█"} paint)))
               matrix)
         (map clojure.string/join)
         (map println)
         (doall))))

(defn part-2 []
   (let [in (async/chan)
         out (async/chan)
         instructions (->> (clojure.string/split (first input) #",")
                           (map a/str->long)
                           (vec))
         robot (robot out in :init 1)
         program (int-code-eval "0" instructions in out)]
        (do (debug "process:" (async/<!! program)
               (async/<!! (async/into [] out)))
            (->> (async/<!! robot)
                 (print-drawing)))))

(defn test-robot [& args]
   (let [in (async/chan)
         out (async/chan)
         robot (robot in out)]
      (->> (map (partial async/put! in) args)
           (doall))
      (debug "Sent all inputs")
      (async/close! in)
      (->> (async/<!! robot)
         (print-drawing))))
