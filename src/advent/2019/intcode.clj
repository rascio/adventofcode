(ns advent.2019.intcode
  (:require [advent.core :as a]
            [clojure.core.async :as async]
            [clojure.string :as s]
            [clojure.math.combinatorics :as comb]))

(defn input [day]
  (->> ((a/read-input 2019 day))
       (mapcat #(re-seq #"-?\d+" %))
       (mapv a/str->int)))

(def debug false)
(def log (a/debugger debug))

(def int-code-debugger-state (atom :go))
(defn pause-execution [] (reset! int-code-debugger-state :break))
(defn unpause-execution [] (reset! int-code-debugger-state :go))
(def int-code-debugger-chan (async/chan))
(defn debugger-next-step [& [steps]]
  (dotimes [n (or steps 1)]
    (async/>!! int-code-debugger-chan :next)))
(def int-code-debugger-chan-states (async/chan))

(defrecord State [index in out memory id])

(defn int-code
  [{:keys [memory index]}]
  (mod (nth memory index) 100))

(defn debug-state
  [{:keys [index memory id] :as s} & [action & {:keys [force extended]}]]
  (when (or debug force)
    ((if force println log) 
     (if extended
       (str "--- " id " : " (or action "") " ---\n"
            (if (nil? index)
              "!!!INDEX IS NULL!!!\n"
              (str "index:" index
                   " value:" (nth memory index)
                   " opcode:" (int-code s) "\n"))
            (if (nil? memory)
              "!!!MEMORY IS NULL!!!\n"
              (let [len 50]
                (->> (partition-all len memory)
                     (vec)
                     (reduce-kv (fn [s idx partial]
                                  (str s (s/join " " partial) "\n"
                                       (if (= idx (quot index len))
                                         (->
                                          (let [n (mod index len)]
                                            (->> partial
                                                 (take n)
                                                 (reduce #(+ %1 (count (str %2))) n)))
                                          (a/pad-left "↑")
                                          (str "\n"))
                                         "")))
                                ""))))
            "-----------")
       (str id ":exec:" (int-code s)
            " @ " index
            " ..." (s/join " " (subvec memory index (min (+ index 7) (count memory)))) "..."))))
  s)

(defn param-mode
  [opcode idx]
  (-> (mod opcode (Math/pow 10 (+ 2 idx)))
      (quot (Math/pow 10 (+ 1 idx)))
      (int)))

(defn read-arg
  [{:keys [memory index]} arg]
  (let [res
        (case (param-mode (nth memory index) arg)
          0 (nth memory (nth memory (+ index arg)))
          1 (nth memory (+ index arg)))]
    #_(log "READ(" (nth memory (+ index arg)) "):" res "\t[mode:" (param-mode (nth memory index) arg) "]")
    res))

(defn write-arg
  [{:keys [memory index] :as state} arg v]
  (let [pos (nth memory (+ index arg))]
    #_(log "WRITE(" pos ", " v ")")
    (assoc-in state
              [:memory pos]
              v)))

(defn move
  [pos state]
  (update state :index + pos))

(def ids (atom 0))
(defn init-state 
  ([memory] (init-state memory))
  ([memory id] (init-state memory (async/chan) (async/chan) :id id))
  ([memory in out & [id]] (->State 0 in out memory (or id (swap! atom inc)))))

(defn compute
  [state]
  (async/go-loop [state state]
    (when (= :break @int-code-debugger-state)
      (loop []
        (println "Breakpoint enabled, waiting for command!")
        (let [cmd (async/<! int-code-debugger-chan)]
          (case cmd
            :next nil
            :debug-state (do (async/>! int-code-debugger-chan-states state)
                           (recur))
            :halt (throw (ex-info "Halted" {}))
            (throw (ex-info "Not managed" {:cmd cmd}))))))
    (debug-state state "compute" :extended false)
    (as-> (int-code state) $
      (try
        (case $
          99 :halt
          1 (->> (+ (read-arg state 1) (read-arg state 2))
                 (write-arg state 3)
                 (move 4))
          2 (->> (* (read-arg state 1) (read-arg state 2))
                 (write-arg state 3)
                 (move 4))
          3 (->> (async/<! (:in state))
                 (write-arg state 1)
                 (move 2))
          4 (->> (do
                   (->> (read-arg state 1)
                        (async/>! (:out state)))
                   (move 2 state)))
          5 (if (not= 0 (read-arg state 1))
              (assoc state :index (read-arg state 2))
              (move 3 state))
          6 (if (= 0 (read-arg state 1))
              (assoc state :index (read-arg state 2))
              (move 3 state))
          7 (->> (if (< (read-arg state 1) (read-arg state 2))
                   1
                   0)
                 (write-arg state 3)
                 (move 4))
          8 (->> (if (= (read-arg state 1) (read-arg state 2))
                   1
                   0)
                 (write-arg state 3)
                 (move 4))
          :else (ex-info "Error code" {:state state}))
        (catch Exception e
          e))
      (cond
        (= :halt $) state
        (record? $) (recur $)
        :else (do
                (debug-state state "error" :forced true)
                (log "ERROR!")
                (.printStackTrace $))))))

(defn day2-part1 []
  (-> (input 2)
      (assoc 1 12)
      (assoc 2 2)
      (init-state)
      (compute)
      (async/<!!)
      (:memory)
      (first)))

(defn day5-part1 []
  (let [in (async/to-chan [1])
        out (async/chan)
        computation (compute (debug-state
                              (init-state (input 5) in out)
                              "init"
                              :extended true))]
    (do
      (async/go-loop [m (async/<! out)]
        (println (str "Received: " m))
        (when (some? m)
          (recur (async/<! out))))
      (debug-state (async/<!! computation) "END" :force true)
      nil)))

(defn day5-part2 []
  (let [in (async/to-chan [5])
        out (async/chan)
        computation (compute (debug-state
                              (init-state (input 5) in out)
                              "init"
                              :extended true))]
    (async/go-loop [m (async/<! out)]
      (println (str "Received: " m))
      (when (some? m)
        (recur (async/<! out))))
    computation))

(defn day7-part1 []
  (let [input (input 7)
        combinations (comb/permutations (range 0 5))
        run-configuration (fn [configuration]
                            (let [main (async/chan)
                                  res (reduce (fn [in phase]
                                                (let [out (async/chan)
                                                      id (str (reduce str configuration) "-" phase)
                                                      state (init-state input in out id)]
                                                  (async/go 
                                                   (async/<! (compute state))
                                                   (println (str "Config " id " ended"))
                                                   (async/close! in)
                                                   (async/close! out))
                                                  (async/>!! in phase)
                                                  out))
                                              main
                                              configuration)]
                              (async/>!! main 0)
                              res))]
    (async/go
      (->> combinations
           (map run-configuration)
           (async/merge)
           (async/into [])
           (async/<!)
           (sort)
           (last)
           (println)))))

(defn day7-part2 []
  (let [input (input 7)
        combinations (comb/permutations (range 5 10))
        run-configuration (fn [configuration]
                            (let [main (async/chan)
                                  res (reduce (fn [in phase]
                                                (let [out (async/chan)
                                                      id (str (reduce str configuration) "-" phase)
                                                      state (init-state input in out id)]
                                                  (async/go
                                                    (println (str "Config " id " ended"))
                                                    (async/<! (compute state))
                                                    #_(async/close! in)
                                                    (async/close! out))
                                                  (async/>!! in phase)
                                                  out))
                                              main
                                              configuration)]
                              (async/>!! main 0)
                              (async/pipe res main false)
                              main))]
    (async/go
      (->> combinations
           (map run-configuration)
           (async/merge)
           (async/into [])
           (async/<!)
           (sort)
           (last)
           (println)))))