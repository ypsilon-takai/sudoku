(ns sudoku.solve
  (:require [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [sudoku.interface :as io]
            [sudoku.util :as util]
            [sudoku.rules :as rules]))

;;
(defn apply-rules [board]
  (let [new-board (reduce (fn [bd rule] (rule bd)) board rules/rule-list)]
    (if (= board new-board)
      board
      (recur new-board))))


(defn solve-pe96 []
  (->> "d:/userdata/q3197c/workspace/clojure/sudoku/src/sudoku/sudoku.txt"
       (io/read-euler-data ,,)
       (map (fn [[title dat]] [title (apply-rules (util/update-candidate dat))]) ,,)))

(defn pe96 []
  (with-open [outfile (clojure.java.io/writer "d:/userdata/q3197c/Desktop/pe86_out.txt")]
    (binding [*out* outfile]
      (dorun (for [[title dat] (solve-pe96)]
               (dorun (println title)
                      (io/print-board (apply-rules (util/update-candidate dat)))))))))


(defn solve-pe96 []
  (let [problem-list (io/read-euler-data "d:/userdata/q3197c/workspace/clojure/sudoku/src/sudoku/sudoku.txt")]
    ))

