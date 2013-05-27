(ns sudoku.solve
  (:require [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [sudoku.interface :as if]
            [sudoku.util :as util]
            [sudoku.rules :as rules]))

;;
(defn apply-rules [board]
  (let [new-board (reduce (fn [bd rule] (rule bd)) board rules/rule-list)]
    (if (= board new-board)
      board
      (recur new-board))))


