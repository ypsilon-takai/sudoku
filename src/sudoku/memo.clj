
(def d1 (update-candidate (sudoku.util/read-data sudoku.util/test-board)))

(def p [4 5])

(defn rowdat [b p]
  (zipmap (row-pos-list p) (map #(get-val b %) (row-pos-list p))))

(defn coldat [b p]
  (zipmap (col-pos-list p) (map #(get-val b %) (col-pos-list p))))

(defn boxdat [b p]
  (zipmap (box-pos-list p) (map #(get-val b %) (box-pos-list p))))
