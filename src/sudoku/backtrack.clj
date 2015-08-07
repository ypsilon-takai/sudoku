(ns sudoku.backtrack
  (:require [sudoku.util :as util]
            [sudoku.rules :as rule]))

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
          ,,(let [tgt-pos (first (non-fixed-pos-priority new-board))]
              (loop [number-list (util/get-val new-board tgt-pos)]
                (if (seq number-list)
                  (or (backtrack (util/set-val new-board
                                               tgt-pos
                                               (hash-set (first number-list))
                                               :backtrack))
                      (recur (next number-list)))))))))
