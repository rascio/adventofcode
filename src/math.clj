(ns math)

(def rad (/ 180 Math/PI))
(defn deg->rad [degrees]
  (* rad degrees))
(defn sin [degrees]
  (-> (deg->rad degrees)
      (Math/sin)
      (Math/round)
      (int)))
(defn cos [degrees]
  (-> (deg->rad degrees)
      (Math/cos)
      (Math/round)
      (int)))