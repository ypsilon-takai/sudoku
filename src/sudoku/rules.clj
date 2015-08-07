(ns sudoku.rules
  (:require [sudoku.util :as util]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

;;---------------------------------------------------------
;; rule 1
(defn fix-the-number
  "receives posision whose candidates includes only one number.
   set the number as value of the posision and remove the number
   from related cells"
  [board pos]
  (let [val-at-pos (first (util/get-val board pos))]
    (util/remove-num (util/set-val board pos val-at-pos :rule1-tgt)
                     val-at-pos
                     (util/neibor-pos-list pos)
                     :rule1-delete)))

(defn apply-rule-1 
  "rule 1
     if there is only one possible number, place it."
  [board]
  (let [fixing-pos (->> (util/non-fixed-pos board)
                        (filter #(= (count (util/get-val board %)) 1) ,,))]
    (if (empty? fixing-pos)
      board
      (recur (reduce fix-the-number board fixing-pos)))))


;;---------------------------------------------------------
;; rule 2
(defn not-in-neigbor 
  "Return the number which is not in neigbors.
   Return false if none of it is found. "
  [board pos neigbor-func]
  (->> (neigbor-func pos false)
       (map #(util/get-val board %) ,,)
       (filter set? ,,)
       (reduce clojure.set/union ,,)
       (clojure.set/difference (util/get-val board pos) ,,)
       (#(if (empty? %) false (first %)) ,,)))

(defn check-only-one
  "Check every candidates of the posision whether it is only one in the neibor or not."
  [board pos]
  (loop [pos-fn-list [util/row-pos-list util/col-pos-list util/box-pos-list]]
    (if (empty? pos-fn-list)
      board
      (if-let [only-one-val (not-in-neigbor board pos (first pos-fn-list))]
        (util/remove-num (util/set-val board pos only-one-val :rule2-tgt)
                         only-one-val
                         (util/neibor-pos-list pos)
                         :rule2-delete)
        (recur (next pos-fn-list))))))

(defn apply-rule-2
  "rule 2
   if there is only one cell which specific number is in its candidates, 
   the cell's number is it."
  [board]
  (let [target-pos (util/non-fixed-pos board)
        next-board (reduce check-only-one board target-pos)]
    (cond (empty? target-pos) board
          (= (:board  board) (:board next-board)) board
          :t (recur next-board))))


;;---------------------------------------------------------
;; rule 3
(defn find-same-n
  ([board pos neigbor-func] (find-same-n board pos neigbor-func 2))
  ([board pos neigbor-func cnt]
     (let [ps (->> (neigbor-func pos)
                   (util/non-fixed-pos board ,,)
                   (filter #(= cnt (count (util/get-val board %))) ,,))]
       (loop [pairs (combo/combinations ps cnt)]
         (if (empty? pairs)
           nil
           (if (apply = (map (partial util/get-val board) (first pairs)))
             (first pairs)
             (recur (next pairs))))))))

(defn same-n-cell
  "Check every groups if it is only one in the neibor."
  [board]
  (loop [posfn-key-list [[util/row-pos-list (map #(vector 0 %) (range 9))]
                         [util/col-pos-list (map #(vector % 0) (range 9))]
                         [util/box-pos-list [[0 0] [3 0] [6 0]
                                        [0 3] [3 3] [6 3]
                                        [0 6] [3 6] [6 6]]]]
         bd-lvl-1 board]
    (if (empty? posfn-key-list)
      bd-lvl-1
      (recur (next posfn-key-list)
             (let [[posfn key-pos-list] (first posfn-key-list)]
               (loop [target-pos key-pos-list
                      bd-lvl-2 bd-lvl-1]
                 (if (empty? target-pos)
                   bd-lvl-2
                   (if-let [pairs (find-same-n bd-lvl-2 (first target-pos) posfn 2)]
                     (recur (next target-pos)
                            (reduce #(util/remove-num %1 %2
                                                      (remove (set pairs)
                                                              (posfn (first pairs)))
                                                      :rule3)
                                    bd-lvl-2
                                    (util/get-val bd-lvl-2 (first pairs))))
                     (recur (next target-pos)
                            bd-lvl-2)))))))))

(defn apply-rule-3
  "rule 3
   if two cells have same two and only two candidate number,
   those number shouldn't appear other cells in the group."
  [board]
  (->> (iterate same-n-cell board)
       (partition 2 1 ,,)
       (drop-while #(apply not= %) ,,)
       ((comp first first) ,,)))



;;---------------------------------------------------------
;; apply all rules
(def rule-list [apply-rule-1 apply-rule-2 apply-rule-3])

(defn apply-rules [board]
  (let [new-board (reduce (fn [bd rule] (rule bd)) board rule-list)]
    (if (= board new-board)
      board
      (recur new-board))))
