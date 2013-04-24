(ns sudoku.util)

(def test-board
  [0 0 3 0 2 0 6 0 0
   9 0 0 3 0 5 0 0 1
   0 0 1 8 0 6 4 0 0
   0 0 8 1 0 2 9 0 0
   7 0 0 0 0 0 0 0 8
   0 0 6 7 0 8 2 0 0
   0 0 2 6 0 9 5 0 0
   8 0 0 2 0 3 0 0 9
   0 0 5 0 1 0 3 0 0])


(defn read-data [s]
  (->> s
       (map #(if (= % 0) #{} %) ,,)
       (zipmap (for [y (range 9) x (range 9)] [x y]) ,,)))

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
              0
              d))))))
