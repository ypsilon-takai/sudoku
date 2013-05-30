(ns sudoku.backtrack
  (:require [clojure.set :as set]
            [sudoku.util :as util]))

(defn non-fixed-pos-priority [board]
  (sort-by #(count (util/get-val board %))
           <
           (util/non-fixed-pos board)))

(defn backtrack [board]
  (let [new-board (rule/apply-rules board)]
    (cond (= :solved (util/status new-board))
          ,,new-board
          (= :fail (util/status new-board))
          ,,false
          :notyet
          ,,(let [tgtpos (first (non-fixed-pos-priority board))]
              (loop [number-list (util/get-val tgtpos)]
                (or (backtrack (util/set-val board tgtpos (first number-list) ))
                    (recur (next number-list))))))))

