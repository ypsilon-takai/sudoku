(ns sudoku.solve
  (:require [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [sudoku.interface :as io]
            [sudoku.util :as util]
            [sudoku.rules :as rules]))

;;

(defn solve-pe96 []
  (->> "d:/userdata/q3197c/workspace/clojure/sudoku/src/sudoku/sudoku.txt"
       (io/read-euler-data ,,)
       (map (fn [[title dat]] [title (rules/apply-rules (util/update-candidate dat))]) ,,)))

(defn pe96 []
  (with-open [outfile (clojure.java.io/writer "d:/userdata/q3197c/Desktop/pe86_out.txt")]
    (binding [*out* outfile]
      (dorun (for [[title dat] (solve-pe96)]
               (dorun (println title)
                      (io/print-board dat)))))))


