(ns advent.2020.day16
  (:require [advent.core :as a]))

(def reader (a/read-input 2020 16))
(def input (reader #_"example2"))

(defn parse-tickets [lines]
  (->> lines
       (map (fn [line] (->> (.split line ",")
                            (map a/str->long))))
       (assoc {} :nearby-tickets)))

(defn parse-my-ticket [[line & lines]]
  (->> (.split line ",")
       (map a/str->long)
       (assoc (parse-tickets (drop 2 lines)) :your-ticket)))

(def rule-regex #"([ \w]+): (\d+)-(\d+) or (\d+)-(\d+)")
(defn parse-rules [lines]
  (loop [[line & others] lines
         rules []]
    (if (empty? line)
      (-> (next others)
          (parse-my-ticket)
          (assoc :rules rules))
      (let [[name & ranges] (next (re-find rule-regex line))
            [rs re rs' re'] (map a/str->int ranges)]
        (recur others
               (cons [name [rs re] [rs' re']]
                     rules))))))

(defn in-range [field [_ [rs re] [rs' re']]]
  (or (<= rs field re)
      (<= rs' field re')))

(defn keep-invalid-fields [rules ticket]
  (->> ticket
       (filter (fn [field]
                 (not-any? #(in-range field %) rules)))))
(defn part1 []
  (let [{:keys [nearby-tickets rules]} (parse-rules input)]
    (->> nearby-tickets
         (mapcat #(keep-invalid-fields rules %))
         (reduce +))))



(defn invalid? [ticket rules]
  (->> ticket
       (every? (fn [field]
                 (some #(in-range field %) rules)))))
(defn valid-group? [values-of-field rule]
  (every? #(in-range % rule) values-of-field))
(defn part2 []
  (let [{:keys [nearby-tickets rules your-ticket]} (parse-rules input)]
    (->> nearby-tickets
         (filter #(invalid? % rules))
         (cons your-ticket)
         (apply map list) ;group field values
         (map (fn [values-of-field]
                (->> rules
                     (filter #(valid-group? values-of-field %))
                     (map first))))
         (map-indexed list)
         (sort-by (comp count second))
         (reduce (fn [[acc removal] [field rules]]
                   (let [rule (first (if (= 1 (count rules))
                                       rules
                                       (remove removal rules)))]
                     [(cons [field rule] acc)
                      (into removal rules)]))
                 [() #{}])
         (first)
         (filter (fn [[_ rule]] (.startsWith rule "departure")))
         (map (fn [[idx _]] (nth your-ticket idx)))
         (reduce *))))