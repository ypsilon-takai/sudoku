(ns sudoku.pe96
  (:require [sudoku.interface :as io]
            [sudoku.util :as util]
            [sudoku.rules :as rules]
            [sudoku.backtrack :as bt]))

;;
;; solve pe96 only with rules.
;;  -- 6 problems can't solve with 1-3 rules.
(defn solve-pe96 []
  (->> "http://projecteuler.net/project/sudoku.txt"
       (io/read-euler-data ,,)
       (map (fn [[title dat]] [title (rules/apply-rules (util/update-candidate dat))]) ,,)))

;;
;; solve pe96 with backtrack
;;  -- solve them all
(defn solve-pe96-bt []
  (->> "http://projecteuler.net/project/sudoku.txt"
       (io/read-euler-data ,,)
       (map (fn [[title dat]] [title (bt/backtrack (util/update-candidate dat))]) ,,)))


;;
;; output result to file
(defn pe96-print [solver]
  (with-open [outfile (clojure.java.io/writer "/home/yosi/pe86_out.txt")]
    (binding [*out* outfile]
      (dorun (for [[title dat] (solver)]
               (dorun (println title)
                      (io/print-board dat)))))))

;;
;; calc pe86 answer
;; "Elapsed time: 3387.646432 msecs"
(defn pe96 []
  (reduce (fn [sum [title dat]] (+ sum (+ (* 100 (util/get-val dat [0 0]))
                                           (* 10 (util/get-val dat [1 0]))
                                           (* 1 (util/get-val dat [2 0])))))
            0 (solve-pe96-bt)))
