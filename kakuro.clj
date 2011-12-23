(ns kakuro
   (:use
    csp
    (clojure.java io)
    (clojure.contrib combinatorics)
    (clojure set)))

(def MAX-ENTRY 9)

(defn squares-for [empty-cell-set rows cols starty startx dy dx]
     (if (empty-cell-set (+ (* cols starty) startx))
       (cons (+ (* cols starty) startx)
	     (squares-for empty-cell-set rows cols (+ starty dy) (+ startx dx) dy dx))
       []))

(defn load-kakuro [filename]
  (let [inp (reader (file filename))
	[rows cols] (map #(Integer/parseInt %1) (.split (.readLine inp) " "))
	cells (for [i (range rows)]
		(let [line (.readLine inp)]
		  (for [j (range (.length line))]
		    ({\# false \. true} (.charAt line j)))))
	empty-cells (filter (fn [idx]
			      (let [i (quot idx cols)
				    j (mod idx cols)]
				(nth (nth cells i) j)))
			    (range (* rows cols)))
	nums (range 1 (+ 1 MAX-ENTRY))
	board (loop [hsh {} squares empty-cells]
		(if (empty? squares)
		  hsh
		  (recur (assoc hsh (first squares) (set nums)) (rest squares))))
	constraints (loop [hsh {}]
		      (let [s (.readLine inp)]
			(if (nil? s)
			  hsh
			  (let [m (re-matcher #"(\d+) (\d) (.) (\d+)" s)
				_ (re-find m)
				groups (re-groups m)
				y (Integer/parseInt (groups 1))
				x (Integer/parseInt (groups 2))
				[dy dx] ({"E" [0 1] "S" [1 0]} (groups 3))
				val (Integer/parseInt (groups 4))
				constrained (squares-for (set empty-cells)
							 rows cols
							 (+ y dy) (+ x dx)
							 dy dx)
				uniq-c (uniq-constraint 1 constrained)
				hsh (loop [cells constrained h hsh]
				      (if (seq cells)
					(recur (rest cells) 
					       (assoc-poss h (first cells) uniq-c))
					h))
				sum-c (plus-constraint val constrained)
				hsh (loop [cells constrained h hsh]
				      (if (seq cells)
					(recur (rest cells) 
					       (assoc-poss h (first cells) sum-c))
					h))]
			    (recur hsh)))))]
    [board constraints]))
				       
	
	
	
		
		    
