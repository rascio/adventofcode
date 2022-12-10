(ns advent.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [advent.core :as a])
  (:import [java.io Console]))

(defmacro when->> 
  [predicate & forms]
  (let [[forms [value]] (split-at (dec (count forms)) 
                                  forms)
        v-name `value#
        predicate (if (seq? predicate)
                    (concat predicate [v-name])
                    `(~predicate ~v-name))]
    `(let [~v-name ~value]
       (if ~predicate
         (->> ~v-name ~@forms)
         ~v-name))))

(defn pad-left
  [n string]
  (apply str (concat (repeat n " ") [string])))
(defmacro debug-forms [& forms]
  `(println ~@(mapcat (fn [f] [(str f ":") f]) forms)))
(defn pdebug [msg x] (println msg x) x)

(defmacro trap 
  "Used as:
   (trap :something (/ 5 0))
   print a message with 'trap=> :something' when the last expression throws an exception

   Can be used to debug error values, like:
   (map (fn [x] (trap x (/ 5 x))) a-collection)
   "
  [& forms]
  (let [[form & info] (reverse forms)]
    `(try ~form
          (catch Exception e#
            (println "<trap>")
            ~@(map #(list 'println (str %) %) info)
            (println "</trap>")
            (throw e#)))))

(defn >>debug [msg f arg]
   (pdebug msg (f arg))
   arg)

(defn debugger [active]
    (if active
        (fn [& args]
            (println (s/join " " args))
            (last args))
        (fn [& args] (last args))))
(defmacro log-forms [log & forms]
  `(~log ~@(mapcat (fn [f] [(str f ":") f]) forms)))

(defn re-map
  "Match an transforms regex groups:
   (re-map #\"(\\d+) (\\d+)\" [str->int (comp inc str->int)] \"5 5\") ;=> (5 6)
   "
  ([regex fns]
   (fn [line] (re-map regex fns line)))
  ([regex fns line]
   (->> (re-matches regex line)
        (rest)
        (map list fns)
        (reduce
         (fn [acc [f v]] (conj acc (f v)))
         []))))

(defn regex-patterns [str & patterns]
  (->> (partition 2 patterns)
       (map (fn [[key value]]
                 (when-let [match (re-matches value str)]
                   (cons key match))))
       (filter #(not (nil? %)))
       (first)))
(defn str->int [s] (Integer/parseInt (str s)))
(defn str->long [s] (Long/parseLong (str s)))
(defn round [n decimals]
   (let [m (Math/pow 10 decimals)]
      (-> (* n m)
          (int)
          (/ m))))
(defn seq-map [& mappings]
  (fn [values]
    (loop [[v & vals] values
           [f & fns] mappings
           res []]
      (if (some? v)
        (->> ((or f identity) v)
             (conj res)
             (recur vals fns))
        res))))

(defn append [coll i v]
   (as-> (count coll) acc
         (- i acc)
         (if (pos? acc)
            (-> coll
                (concat (repeat acc 0))
                (vec))
            coll)
         (assoc acc i v)))

(defn read-input [year day]
   (fn ([] (->> (str year "/day" day ".txt")
               io/resource
               io/input-stream
               io/reader
               line-seq))
      ([v] (->> (str year "/day" day "-" v ".txt")
               io/resource
               io/input-stream
               io/reader
               line-seq))))

(defmacro defcase
  "Define a function letting the `input` a seq with
  the content of the `file` read from classpath, eg:
      (defcase test \"position/in/classpath.txt\" seq-file
          (println (first seq-file)) ;Prints the first line of the file
  "
  [name [input file & utils] & body]
  `(defn ~name []
     (let [~input (->> ~file
                      io/resource
                      io/input-stream
                      io/reader
                      line-seq)
            ~@utils]
       ~@body)))

(defmacro debug
  "Utility to debug an expression, usage:
      (debug + 5 6) ;print '(+ 5 6) 11'
                    ;evaluate to 11
  "
  [& expr]
  `(try 
        (let [res# ~expr]
            (println "Debug:" (first (quote ~expr)))
            (println (quote ~expr))
            (println "aruments:")
            (println ~@(rest expr))
            (println "result:")
            (println res#)
            (println "------------")
            res#)
        (catch Exception e#
          (println "Debug:" (first (quote ~expr)))
          (println (quote ~expr))
          (println "aruments:")
          (println ~@(rest expr))
          (println "error:")
          (println e#)
          (println "------------")
          (throw e#))))

(defmacro deflambda
  "define a function whith currying:

  (advent.core/deflambda s [a b c d] (+ a b c d))

  (clojure.core/defn s
   ([a] (partial s a))
   ([a b] (partial s a b))
   ([a b c] (partial s a b c))
   ([a b c d] (+ a b c d)))
  "
  [name args & body]
  `(defn ~name
     ~@(map
         #(seq [% (concat ['partial name] %)])
         (map
           #(vec (take % args))
           (range 1 (count args))))
     ~(cons args body)))


(defn init-day [{:keys [year day]}] 
  (with-open [file (io/writer (str "src/advent/" year "/day" day ".clj"))]
    (let [namespace (symbol (string/join "." ["advent" year (str "day" day)]))
          ns-form (list 'ns namespace '(:require [advent.core :as a]))
          reader-form (list 'def 'reader (list 'a/read-input year day))
          input-form '(def input (reader "example"))]
      (pp/pprint ns-form file)
      (pp/pprint reader-form file)
      (pp/pprint input-form file))))