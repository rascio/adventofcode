(ns advent.2019-day7
    (:require [advent.core :as a]
              [clojure.spec.alpha :as s]
              [clojure.math.combinatorics :as comb]
              [clojure.core.async :as async]))

(def reader (a/read-input 2019 6))
(def debug (a/debugger false))

(def input (reader))

(defn process [state]
   (async/go 
      (let [code (get-in state [:memory (state :index)])
            op (mod code 100)
            modes (fn [idx] (-> (mod code (Math/pow 10 (+ 3 idx)))
                                (quot (Math/pow 10 (+ 2 idx)))
                                (int)))
            arg (fn [idx] (get-in state [:memory (+ (inc idx) (state :index))]))
            read-arg (fn [idx] (if (zero? (modes idx))
                                   (get-in state [:memory (arg idx)])
                                   (arg idx)))]
         (debug (state :id) "exec:" op "@" (state :index) state)
         (case op
               1 (-> (assoc-in state [:memory (arg 2)] (+ (read-arg 0) 
                                                         (read-arg 1)))
                     (update :index + 4))
               2 (-> (assoc-in state [:memory (arg 2)] (* (read-arg 0) 
                                                         (read-arg 1)))
                     (update :index + 4))
               3 (let [in (async/<! (state :input))]
                     (debug (state :id) in "<== INPUT" (state :input))
                     (-> state
                        (assoc-in [:memory (arg 0)] in)
                        (update :index + 2)))
               4 (do (debug (state :id) (read-arg 0) "==> OUTPUT" (state :output))
                     (async/put! (state :output) (read-arg 0))
                     (update state :index + 2))
               5 (->> (if (not= 0 (read-arg 0))
                        (read-arg 1)
                        (+ 3 (state :index)))
                     (assoc state :index))
               6 (->> (if (= 0 (read-arg 0))
                        (read-arg 1)
                        (+ 3 (state :index)))
                     (assoc state :index))
               7 (-> state
                     (assoc-in [:memory (arg 2)] 
                              (if (< (read-arg 0) (read-arg 1)) 
                                 1 
                                 0))
                     (update :index + 4))
               8 (-> state 
                     (assoc-in [:memory (arg 2)] 
                           (if (= (read-arg 0) (read-arg 1))
                              1
                              0))
                     (update :index + 4))
               99 (do 
                     (async/close! (state :output))
                     (dissoc  state :index))
               (throw (ex-info "Not managed" {:value op}))))))

(defn compute [conf instructions in out]
    (debug "compute:" conf instructions in out)
    (async/go-loop [state {:id conf
                           :memory instructions
                           :index 0
                           :input in
                           :output out}]
        (when (state :index)
            (-> (process state)
                (async/<!)
                (recur)))))

(defn run-amplifiers [conf init instructions]
    (let [main (async/chan)
          process (loop [in main
                         [code & settings] conf]
                    (async/put! in code)
                    (if (nil? settings)
                        (compute code instructions in main)
                        (let [output (async/chan)]
                            (compute code instructions in output)
                            (recur output settings))))]
        (debug conf "sending initial input" init)
        (async/put! main init)
        (debug conf "init sent, starting process")
        (async/go (do (async/<! process)
                      [conf (async/<! main)]))))

(defn run-all-settings [settings init instructions]
    (->> settings
         (map #(run-amplifiers % init instructions))
         (async/merge)))

(defn part-1 []
   (->>  (clojure.string/split (first input) #",")
         (map a/str->int)
         (vec)
         (run-all-settings 
            (comb/permutations (range 0 4)) 
            0)
         (async/reduce 
               (partial max-key second)
               [nil -1])
         (async/<!!)))

(defn part-2 []
   (->>  (clojure.string/split (first input) #",")
         (map a/str->int)
         (vec)
         (run-all-settings 
            (comb/permutations (range 5 10)) 
            0)
         (async/reduce (partial max-key second)
                       [nil -1])
         (async/<!!)))
         ;(println "res")))
 
