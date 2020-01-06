(ns advent.2019-day9
    (:require [advent.core :as a]
              [clojure.spec.alpha :as s]
              [clojure.math.combinatorics :as comb]
              [clojure.core.async :as async]))

(def reader (a/read-input 2019 9))
(def debug (a/debugger false))

(def input (reader))
(comment def input '("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"))
(comment def input '("1102,34915192,34915192,7,4,7,99,0"))
(comment def input '("104,1125899906842624,99"))
(comment def input '("3,0,1001,2,1,0,4,0,99,-1"))
(comment def input '("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"))

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

(defn compute [conf instructions in out]
    (debug "compute:" conf (count instructions) in out)
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
         
(defn part-1 []
   (let [in (async/chan)
         out (async/chan)
         instructions (->> (clojure.string/split (first input) #",")
                           (map a/str->long)
                           (vec))
         chan (compute "0" instructions in out)]
        (do (async/put! in 1)
            (println "process:" (async/<!! chan))
            (async/<!! (async/into [] out))
 
            (ex-data e))))
         
(defn part-2 []
   (let [in (async/chan)
         out (async/chan)
         instructions (->> (clojure.string/split (first input) #",")
                           (map a/str->long)
                           (vec))
         chan (compute "0" instructions in out)]
        (do (async/put! in 2)
            (println "process:" (async/<!! chan))
            (async/<!! (async/into [] out)))))
 