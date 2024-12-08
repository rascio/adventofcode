(ns advent.2021.day16
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 16))

(def input (->> (first (reader))))

(def sub-packets-n (atom 0))
(def log (a/debugger true))
(defn log-padded [& args] 
  (apply log (apply str (repeat @sub-packets-n "\t")) args))

(declare read-packets)

(defn hex->binary [char]
  (as-> (Integer/parseInt (str char) 16) $
    (Integer/toBinaryString $)
    (format "%4s" $)
    (.replace $ " " "0")))

(defn hex->code [hex] (mapcat hex->binary hex))
(defn binary->int [v] (Long/parseLong (apply str v) 2))


(defn str-io [bits] (apply str bits))
(defn end? [bits] (empty? bits))
(defn read-io 
  ([n io] (read-io n identity io))
  ([n f bits]
   (let [[head tail] (split-at n bits)]
     [(f head) tail])))
(defn read-io-int [n io]
  (read-io n binary->int io))

(defn read-io-literal
  [io]
  (loop [[[control & part] io] (read-io 5 io)
         acc ""]
    (log-padded "control:" control (apply str part))
    (as-> (apply str acc part) $
      (if (= \0 control)
        (do
          (log-padded "literal:" (Long/parseLong $ 2))
          [(Long/parseLong $ 2) io])
        (recur (read-io 5 io) $)))))


(defn read-io-sub-packets
  [IO]
  (swap! sub-packets-n inc)
  (let [[[length-type-id] IO] (read-io 1 IO)]
    (case length-type-id
      \0 (let [[packets-size IO] (read-io-int 15 IO)
               [[packets] IO] (read-io packets-size read-packets IO)]
           (log-padded "packets-size:" packets-size "packets:" packets)
           [packets IO])
      \1 (let [[n-packets IO] (read-io-int 11 IO)]
           (log-padded "packets:" n-packets)
           (read-packets IO n-packets))
      (throw (ex-info (str "Not valid:" length-type-id) {})))))

(defn greater-than [& values] (if (apply < values) 1 0))
(defn less-than [& values] (if (apply > values) 1 0))
(defn equals-to [& values] (if (apply = values) 1 0))

(defn read-packets
  [IO & [max-packets]]
  (loop [state :header
         packets []
         IO IO
         processed 0]
    (log-padded "state:" state "packets" (str processed "/" max-packets) packets "bits:" (str-io IO))
    (case state
      :next (if (or (end? IO)
                    (and (some? max-packets)
                         (>= processed max-packets)))
              [packets IO]
              (recur :header packets IO processed))
      :header (let [[version IO] (read-io-int 3 IO)
                    [type-id IO] (read-io-int 3 IO)]
                (log-padded "version:" version "type-id:" type-id)
                (recur (if (= 4 type-id) :literal :operation)
                       (cons [version type-id] packets)
                       IO
                       processed))
      :literal (let [[value IO] (read-io-literal IO)
                     [current & others] packets]
                 (recur :next
                        (cons (conj current value) others)
                        IO
                        (inc processed)))
      :operation (let [[subpackets IO] (read-io-sub-packets IO)
                       [current & others] packets]
                   (log-padded "subpackets:" subpackets (str-io IO))
                   (recur :next
                          (cons (conj current subpackets) others)
                          IO
                          (inc processed))))))
(defn hex->packet
  [hex]
  (-> (hex->code hex)
      (read-packets 1)
      (first)))
(defn add-version
  [packets]
  (reduce
   (fn [acc [version _ packets]]
     (+ acc
        version
        (if (seqable? packets)
          (add-version packets)
          0)))
   0
   packets))

; This will need to change the read-packets to accumulate them instead of doing the calculation
(defn part1 [& [code]]
  (let [c (or code input)
        packets (hex->packet c)]
    (add-version packets)))

(defn test-part1 []
  (assert (= 16 (part1 "8A004A801A8002F478")))
  (assert (= 12 (part1 "620080001611562C8802118E34")))
  (assert (= 23 (part1 "C0015000016115A2E0802F182340")))
  (assert (= 31 (part1 "A0016C880162017C3686B18A3D4780"))))

(def operations {0 +
                 1 *
                 2 min
                 3 max
                 5 greater-than
                 6 less-than
                 7 equals-to})
(defn exec [[_ type-id value]]
  (if (= 4 type-id)
    value
    (->> value
         (map exec)
         (apply (operations type-id)))))

(defn part2 [& [code]]
  (let [c (or code input)]
    (exec (first (hex->packet c)))))
  
(defn test-part2 []
  (assert (= 3 (part2 "C200B40A82")))
  (assert (= 54 (part2 "04005AC33890")))
  (assert (= 7 (part2 "880086C3E88112")))
  (assert (= 9 (part2 "CE00C43D881120")))
  (assert (= 1 (part2 "D8005AC2A8F0")))
  (assert (= 0 (part2 "F600BC2D8F")))
  (assert (= 0 (part2 "9C005AC2F8F0")))
  (assert (= 1 (part2 "9C0141080250320F1802104A08"))))