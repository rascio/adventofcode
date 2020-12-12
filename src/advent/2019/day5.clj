(ns advent.2019.day5
    (:require [advent.core :as a]))

(def reader (a/read-input 2019 5))
(def debug (a/debugger true))

(def input (reader))

(def ID (atom 0))

(defn process [state]
   (let [code (get-in state [:memory (state :index)])
         op (mod code 100)
         modes (fn [idx] (-> (mod code (Math/pow 10 (+ 3 idx)))
                           (quot (Math/pow 10 (+ 2 idx)))
                           (int)))
         arg (fn [idx] (get-in state [:memory (+ (inc idx) (state :index))]))
         read-arg (fn [idx] 
                     (as-> (arg idx) v
                        (if (zero? (modes idx))
                           (get-in state [:memory v])
                           v)))
         set-memory (fn [idx v]
                        (debug (str "op:" op) "[" idx "] = " v)
                        (as-> (count (state :memory)) s
                              (- idx s)
                              (if (pos? s)
                                 (-> (state :memory)
                                     (concat (repeat s 0))
                                     (vec))
                                 (state :memory))
                              (assoc s idx v)
                              (assoc state :memory s)))] 
      (debug "exec:" op "@" (state :index) 
             (subvec (state :memory) (state :index) (min (+ (state :index) 4) (count (state :memory)))))
         
      (case op
         1 (-> (set-memory (arg 2) (+ (read-arg 0) 
                                      (read-arg 1)))
               (update :index + 4))
         2 (-> (set-memory (arg 2) (* (read-arg 0) 
                                      (read-arg 1)))
               (update :index + 4))
         3 (-> (set-memory (arg 0) @ID) ;ID == (read-line)
               (update :index + 2))
         4 (do (println (read-arg 0))
               (update state :index + 2))
         5 (->> (if (not= 0 (read-arg 0))
                    (read-arg 1)
                    (+ 3 (state :index)))
                (assoc state :index))
         6 (->> (if (= 0 (read-arg 0))
                    (read-arg 1)
                    (+ 3 (state :index)))
                (assoc state :index))
         7 (-> (assoc-in 
                  state 
                  [:memory (arg 2)] 
                  (if (< (read-arg 0) (read-arg 1)) 1 0))
               (update :index + 4))
         8 (-> (assoc-in state 
                  [:memory (arg 2)] 
                  (if (= (read-arg 0) (read-arg 1)) 1 0))
               (update :index + 4))
         99 (assoc state :index -1)
         (throw (ex-info "Not managed" {:value op})))))                  

(defn compute [instructions]
   (let [state (atom {:memory instructions
                      :index 0})]
      (while (<= 0 (@state :index))
         (swap! state process))
      @state))

(defn part-1 []
   (reset! ID 1)
   (let [res (->> input
                  (map #(->> (clojure.string/split % #",")
                             (map a/str->int)
                             (vec)
                             (compute)))
                  (doall))]
     res))

(defn part-2 []
   (reset! ID 5)
   (let [res (->> input
                  (map #(->> (clojure.string/split % #",")
                             (map a/str->int)
                             (vec)
                             (compute)))
                  (doall))]
     res))

