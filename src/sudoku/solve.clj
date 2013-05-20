(ns sudoku.solve
  (:use [clojure.pprint]
        [sudoku.util])
  (:require [clojure.set :as set]))


;;
(defn solved? [board]
  (not-any? #(or (coll? %) (zero? %)) (vals board)))


;; utils
(defn get-val [board pos]
  (get board pos))

(defn set-val [board pos value]
  (assoc board pos value))

(defn take-away [board pos num]
  (set-val board pos (disj (get-val board pos) num)))

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
  "Get same low, same col, same box posisions."
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
       (remove #(or (coll? %) (zero? %)) ,,)
       (set ,,)
       (set/difference (set (range 1 10)) ,,)))

(defn update-candidate [board]
  (let [updater (fn [pos]
                  (let [num (get-val board pos)]
                    (if (or (set? num)
                            (zero? num))
                      (candidates pos board)
                      num)))]
    (reduce #(set-val %1 %2 (updater %2)) board (non-fixed-pos board))))

;; remove num from candidate
(defn remove-num [board num pos-list]
  (reduce (fn [b p] (if (set? (get-val b p))
                     (take-away b p num)
                     b))
          board
          pos-list))

;;
;; rule 1
(defn fix-the-number [board pos]
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

(defn not-in-neigbor [board pos neigbor-func]
  (->> (neigbor-func pos false)
       (map #(get-val board %) ,,)
       (filter set? ,,)
       (reduce clojure.set/union ,,)
       (clojure.set/difference (get-val board pos) ,,)
       (#(if (empty? %) false (first %)) ,,)))

(defn check-only-one [board pos]
  (loop [pos-fn-list [row-pos-list col-pos-list box-pos-list]]
    (if (empty? pos-fn-list)
      board
      (if-let [only-one-val (not-in-neigbor board pos (first pos-fn-list))]
        (remove-num (set-val board pos only-one-val)
                    only-one-val
                    (neibour-pos-list pos))
        (recur (next pos-fn-list))))))

(defn apply-rule-2
  "rule 2
   if there is only one cell which number n is in candidates, the
   cell's number is it."
  [board]
  (let [target-pos (non-fixed-pos board)
        next-board (reduce check-only-one board target-pos)]
    (cond (empty? target-pos) board
          (= board next-board) board
          :t (recur next-board))))

