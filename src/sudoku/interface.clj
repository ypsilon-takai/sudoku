(ns sudoku.interface
  (:use [clojure.pprint]))


(defn read-data [s]
  (->> s
       (map #(if (= % 0) #{} %) ,,)
       (zipmap (for [y (range 9) x (range 9)] [x y]) ,,)))

(defn read-euler-data
  ([] (read-euler-data "http://projecteuler.net/project/sudoku.txt"))
  ([url]
     (with-open [rdr (clojure.java.io/reader url)]
       (->> (line-seq rdr)
            (partition 10 ,,)
            (map (fn [s] (vector (first s)
                                (->> (rest s)
                                     (apply str ,,)
                                     (map #(Character/digit % 10) ,,)
                                     (read-data ,,)))))
            (into [] ,,)))))



(defn print-board [data]
  (for [y (range 9)]
        (for [x (range 9)]
          (get data [x y]))))

(defn print-board [data]
  (clojure.pprint/pprint
   (for [y (range 9)]
        (for [x (range 9)]
          (let [d (get data [x y])]
            (if (set? d)
              \_
              d))))))
