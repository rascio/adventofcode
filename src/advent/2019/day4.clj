(ns advent.2019.day4
    (:require [advent.core :as a]
              [clojure.spec.alpha :as s]))

(s/def ::password (s/and (is-length? 6)
                         #(re-find #"(\d)\1+" %)
                         #(->> (map int %)
                               (apply <=))))

(defn part-1 []
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

(defn part-2 []
   (->>  (range 246540 787420)
         (map str)
         (filter #(s/valid? ::password-v2 %))
         (count)))
