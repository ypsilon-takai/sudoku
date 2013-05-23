(ns sudoku.solve
  (:use [clojure.pprint]
        [sudoku.util])
  (:require [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))


;;
(defn solved? [board]
  (not-any? coll? (vals board)))

(defn status [board]
  (let [still-as-candidate (filter set? (vals board))]
    (if (empty? still-as-candidate)
      :solved
      (if (some empty? still-as-candidate)
        :fail
        :notyet))))

;; utils
(defn get-val [board pos]
  (get board pos))

(defn set-val [board pos value]
  (assoc board pos value))

;; (defn take-away [board pos num]
;;   (let [new-val (disj (get-val board pos) num)]
;;     (set-val board pos
;;              (if (= (count new-val) 1)
;;                (first new-val)
;;                new-val))))

(defn take-away [board pos num]
  (update-in board [pos] disj num))

;; positions
(defn row-pos-list
  "Get same low positions with the point."
  ([pos] (row-pos-list pos true))
  ([[x y] include-pos?]
     (for [ix (range 9) :when (if include-pos?
                                true
                                (not= ix x))]
       [ix y])))

(defn col-pos-list
  "Get same column positions with the point."
  ([pos] (col-pos-list pos true))
  ([[x y] include-pos?]
     (for [iy (range 9) :when (if include-pos?
                                true
                                (not= iy y))]
       [x iy])))
 
(defn box-pos-list
  "Get positions which in the same box with the point."
  ([pos] (box-pos-list pos true))
  ([[x y] include-pos?]
     (let [x-list (map #(+ (* 3 (quot x 3)) %) (range 3))
           y-list (map #(+ (* 3 (quot y 3)) %) (range 3))]
       (for [iy y-list ix x-list :when (if include-pos?
                                         true
                                         (not (and (= ix x)
                                                   (= iy y))))]
         [ix iy]))))

(defn neibor-pos-list [pos]
  "Get same low, same col and same box posisions."
  (->> ((juxt row-pos-list col-pos-list box-pos-list) pos false)
       (apply concat ,,)
       (distinct ,,)))

;; filter
(defn non-fixed-pos
  ([board] (non-fixed-pos board (keys board)))
  ([board pos-list]
     (->> pos-list
          (filter #(set? (get-val board %)) ,,))))

(defn fixed-pos
  ([board] (fixed-pos board (keys board)))
  ([board pos-list]
     (->> pos-list
          (filter #(number? (get-val board %)) ,,))))


;; create candidate list
(defn candidates [pos board]
  (->> (neibor-pos-list pos)
       (map #(get-val board %) ,,)
       (remove coll? ,,)
       (set ,,)
       (set/difference (set (range 1 10)) ,,)))

(defn update-candidate [board]
  (let [updater (fn [pos]
                  (let [num (get-val board pos)]
                    (if (set? num) 
                      (candidates pos board)
                      num)))]
    (reduce #(set-val %1 %2 (updater %2)) board (non-fixed-pos board))))


;; remove num from candidate
(defn remove-num [board num pos-list]
  (reduce (take-away %1 %2 num)
          board
          (non-fixed-pos pos-list)))


;;
;; rule 1
(defn fix-the-number
  "receives posision whose candidates includes only one number.
   set the number as value of the posision and remove the number
   from related cells"
  [board pos]
  (let [val-at-pos (first (get-val board pos))]
    (remove-num (set-val board pos val-at-pos)
                val-at-pos
                (neibor-pos-list pos ))))

(defn apply-rule-1 
  "rule 1
     if there is only one possible number, place it."
  [board]
  (let [fixing-pos (->> (non-fixed-pos board)
                        (filter #(= (count (get-val board %)) 1) ,,))]
    (if (empty? fixing-pos)
      board
      (recur (reduce fix-the-number board fixing-pos)))))

;;
;; rule 2

(defn not-in-neigbor 
  "return the number which is not in neigbors.
   return false if not found. "
  [board pos neigbor-func]
  (->> (neigbor-func pos false)
       (map #(get-val board %) ,,)
       (filter set? ,,)
       (reduce clojure.set/union ,,)
       (clojure.set/difference (get-val board pos) ,,)
       (#(if (empty? %) false (first %)) ,,)))

(defn check-only-one
  "Check every candidates of the posision if it is only one in the neibor."
  [board pos]
  (loop [pos-fn-list [row-pos-list col-pos-list box-pos-list]]
    (if (empty? pos-fn-list)
      board
      (if-let [only-one-val (not-in-neigbor board pos (first pos-fn-list))]
        (remove-num (set-val board pos only-one-val)
                    only-one-val
                    (neibor-pos-list pos))
        (recur (next pos-fn-list))))))

(defn apply-rule-2
  "rule 2
   if there is only one cell which specific number is in its candidates, 
   the cell's number is it."
  [board]
  (let [target-pos (non-fixed-pos board)
        next-board (reduce check-only-one board target-pos)]
    (cond (empty? target-pos) board
          (= board next-board) board
          :t (recur next-board))))

;;
;; rule 3
;; if there is two cells whose candidates has just two number and same
;; eachother, neigbor cells can't have those number.
(defn find-same-n
  ([board pos neigbor-func] (find-same board pos neigbor-func 2))
  ([board pos neigbor-func cnt]
     (let [ps (->> (neigbor-func pos)
                   (non-fixed-pos board ,,)
                   (filter #(= cnt (count (get-val board %))) ,,))]
       (loop [pairs (combo/combinations ps cnt)]
         (if (empty? pairs)
           nil
           (if (apply = (map (partial get-val board) (first pairs)))
             (first pairs)
             (recur (next pairs))))))))

(defn same-n-cell
  "Check every groups if it is only one in the neibor."
  [board]
  (loop [posfn-key-list [[row-pos-list (map #(vector 0 %) (range 9))]
                         [col-pos-list (map #(vector % 0) (range 9))]
                         [box-pos-list [[0 0] [3 0] [6 0]
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
                            (reduce #(remove-num %1 %2
                                                 (remove (set pairs)
                                                         (posfn (first pairs))))
                                    bd-lvl-2
                                    (get-val bd-lvl-2 (first pairs))))
                     (recur (next target-pos)
                            bd-lvl-2)))))))))

(defn apply-rule-3
  "rule 3
   if two cells have same two and only two candidate number,
   those number shouldn't appear other cells in the group."
  [board]
)

