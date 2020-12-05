(ns advent.2020.day4
  (:require [advent.core :as a]
            [clojure.spec.alpha :as s]))

(def reader (a/read-input 2020 4))

(def input (reader))

(def regex-keyval #"(\w+):([\w#]+)")

(defn merge-kv [acc s]
  (->> (re-seq regex-keyval s)
       (reduce (fn [acc [_ k v]]
                 (assoc acc (keyword k) v))
               acc)))
(defn parse-input []
  (loop [[next & tail] input
         acc {}
         res nil]
    (cond
      (nil? next) (if (empty? acc)
                    res
                    (conj res acc))
      (empty? next) (if (empty? acc)
                      (recur tail acc res)
                      (->> (merge-kv acc next)
                           (conj res)
                           (recur tail {})))
      :else (recur tail
                   (merge-kv acc next)
                   res))))

(s/def ::passport (s/keys :opt-un [::cid]
                          :req-un [:v1/byr
                                   :v1/iyr
                                   :v1/eyr
                                   :v1/hgt
                                   :v1/hcl
                                   :v1/ecl
                                   :v1/pid]))
(defn part1 []
  (->> (parse-input)
       (filter #(s/valid? ::passport %))
       (count)))



(s/def ::integer
  (s/conformer (fn [x]
                 (if (re-matches #"\d+" x)
                   (Integer/parseInt x)
                   :clojure.spec.alpha/invalid))))

(s/def ::byr (s/and ::integer #(<= 1920 % 2002)))
(s/def ::iyr (s/and ::integer #(<= 2010 % 2020)))
(s/def ::eyr (s/and ::integer #(<= 2020 % 2030)))
(s/def ::hgt (fn [value] (let [[_ n unit] (re-matches #"(\d+)(cm|in)" value)]
                           (or (and (= unit "cm")
                                    (<= 150 (Integer/parseInt n) 193))
                               (and (= unit "in")
                                    (<= 59 (Integer/parseInt n) 76))))))
(s/def ::hcl #(re-matches #"#[0-9a-f]{6}" %))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid #(re-matches #"[0-9]{9}" %))
(s/def ::passport-v2 (s/keys :opt-un [::cid]
                             :req-un [::byr
                                      ::iyr
                                      ::eyr
                                      ::hgt
                                      ::hcl
                                      ::ecl
                                      ::pid]))

(defn part2 []
  (->> (parse-input)
       (filter #(s/valid? ::passport-v2 %))
       (count)))