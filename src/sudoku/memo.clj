
(def d1 (update-candidate (sudoku.util/read-data sudoku.util/test-board)))

(def p [4 5])

(defn rowdat [b p]
  (zipmap (row-pos-list p) (map #(get-val b %) (row-pos-list p))))

(defn coldat [b p]
  (zipmap (col-pos-list p) (map #(get-val b %) (col-pos-list p))))

(defn boxdat [b p]
  (zipmap (box-pos-list p) (map #(get-val b %) (box-pos-list p))))

(def td (second (first (sudoku.interface/read-euler-data "file://d:/msys64/home/q3197c/workspace/yosi/sudoku/src/sudoku/sudoku.txt"))))

(def d (sudoku.util/update-candidate (sudoku.util/create-board td)))


(defn solve-pe96 []
  (->> "file://d:/msys64/home/q3197c/workspace/yosi/sudoku/src/sudoku/sudoku.txt"
       (io/read-euler-data ,,)
       (map (fn [[title dat]] [title (rules/apply-rules (util/update-candidate (util/create-board dat)))]) ,,)))

(defn solve-pe96 []
  (->> "file://d:/msys64/home/q3197c/workspace/yosi/sudoku/src/sudoku/p096_sudoku.txt"
       (io/read-euler-data ,,)
       (map (fn [[title dat]] [title (rules/apply-rules (util/update-candidate (util/create-board dat)))]) ,,)))

(map first (filter #(false? (util/solved? (second %))) ans1))
;;=> ("Grid 06" "Grid 07" "Grid 42" "Grid 48" "Grid 49" "Grid 50")

(first (nth ans1 5))

(count (:history (second (nth ans1 5))))
181

(map first (partition-by identity (map :rule (:history (second (nth ans1 5))))))
