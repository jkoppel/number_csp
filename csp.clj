(ns csp
   (:use 
    (clojure.contrib combinatorics)
    (clojure set)))


(defn assoc-poss [board cell poss]
  (assoc board cell (union (board cell) #{poss})))

(defn compute-changed [board new-board cells]
  (filter #(not (= (board %) (new-board %))) cells))

(defn propagate [board changed units]
  (if (some #(empty? (board %)) changed)
      nil
      (let [constraints (mapcat units changed)
            new-board   (reduce (fn [board con]
                                           (if (nil? board)
                                                nil
                                                (first (con board units))))
                                        board
                                        constraints)]
             (if new-board
                [new-board units]
                 nil))))

;Takes a seq mapping N to a set of all [square possibility]s that can be used
;to sum to N in some way, and updates it with requiring [square num] as a possibility
(defn knapsack-dp-add [xs square num]
    (loop [v (vec (repeat (count xs) nil))
             idx (- (count xs) 1)]
       (if (>= idx num)
            (recur 
                   (if (xs (- idx num))
                       (assoc v idx (union (xs (- idx num)) #{[square num]}))
                       v)
                     (- idx 1))
            v)))
            
;Like knapsack-dp-add, but with multiplication
(defn knapsack-dp-mult [xs square num]
    (loop [v (vec (repeat (count xs) nil))
             idx (- (count xs) 1)]
       (if (>= idx num)
            (recur 
                   (if (and (= (mod idx num) 0) (xs (/ idx num)))
                       (assoc v idx (union (xs (/ idx num)) #{[square num]}))
                       v)
                     (- idx 1))
            v)))

(defn assign-constraint [number cells]
   (fn [board units]
          (if (= (board (first cells)) #{number})
              [board units]
              (propagate (assoc board (first cells) #{number}) cells units))))
                
(defn plus-constraint [number cells]
  (fn [board units]
        (let [knapsack-vec (vec (repeat (+ number 1) nil))
               knapsack-vec (assoc knapsack-vec 0 #{})
               knapsack-vec (loop [v knapsack-vec squares (seq cells)]
                                      (if (seq squares)
                                        (let [sq-name (first squares)
                                               posses (seq (board sq-name))
                                               poss-updates (map (fn [n]
                                                                            (knapsack-dp-add v sq-name n))
                                                                          posses)
                                               merge (partial map (fn [& args]
                                                                           (apply union args)))
                                              update (apply merge (cons knapsack-vec poss-updates))]
                                          (recur (vec update) (rest squares)))
                                        v))
               end-posses (knapsack-vec number)
               
               new-board (loop [xs cells b board]
                                 (if (seq xs)
                                     (recur (rest xs) (assoc b (first xs) #{}))
                                     b))
              new-board (loop [xs end-posses b new-board]
                                  (if (seq xs)
                                      (recur (rest xs)
                                                (assoc-poss b (ffirst xs) (first (nfirst xs))))
                                      b))
              changed (compute-changed board new-board cells)]
          (propagate new-board changed units))))
          
(defn minus-constraint [number cells]
  (fn [board units]
      (let [a (first cells)
             b (fnext cells)
             new-board (assoc board a #{})
             new-board (assoc new-board b #{})
             new-board (reduce (fn [hsh val]
                                          (if ((board a) (+ number val))
                                              (assoc-poss (assoc-poss hsh b val) a (+ number val))
                                              hsh))
                                        new-board
                                        (seq (board b)))
                                        
             new-board (reduce (fn [hsh val]
                                          (if ((board b) (+ number val))
                                              (assoc-poss (assoc-poss hsh a val) b (+ number val))
                                              hsh))
                                        new-board
                                        (seq (board a)))
            
            changed (compute-changed board new-board cells)]
         (propagate new-board changed units))))
         
(defn times-constraint [number cells]
  (fn [board units]
        (let [knapsack-vec (vec (repeat (+ number 1) nil))
               knapsack-vec (assoc knapsack-vec 1 #{})
               knapsack-vec (loop [v knapsack-vec squares (seq cells)]
                                      (if (seq squares)
                                        (let [sq-name (first squares)
                                               posses (seq (board sq-name))
                                               poss-updates (map (fn [n]
                                                                            (knapsack-dp-mult v sq-name n))
                                                                          posses)
                                               merge (partial map (fn [& args]
                                                                           (apply union args)))
                                              update (apply merge (cons knapsack-vec poss-updates))]
                                          (recur (vec update) (rest squares)))
                                        v))
               end-posses (knapsack-vec number)
               
               new-board (loop [xs cells b board]
                                 (if (seq xs)
                                     (recur (rest xs) (assoc b (first xs) #{}))
                                     b))
              new-board (loop [xs end-posses b new-board]
                                  (if (seq xs)
                                      (recur (rest xs)
                                                (assoc-poss b (ffirst xs) (first (nfirst xs))))
                                      b))
              changed (compute-changed board new-board cells)]
          (propagate new-board changed units))))

(defn divide-constraint [number cells]
  (fn [board units]
      (let [a (first cells)
             b (fnext cells)
             new-board (assoc board a #{})
             new-board (assoc new-board b #{})
             new-board (reduce (fn [hsh val]
                                          (if ((board a) (* number val))
                                              (assoc-poss (assoc-poss hsh b val) a (* number val))
                                              hsh))
                                        new-board
                                        (seq (board b)))
                                        
             new-board (reduce (fn [hsh val]
                                          (if ((board b) (* number val))
                                              (assoc-poss (assoc-poss hsh a val) b (* number val))
                                              hsh))
                                        new-board
                                        (seq (board a)))
                                        
            changed (compute-changed board new-board cells)]
         (propagate new-board changed units))))
         
(defn uniq-constraint [number cells]
  (fn [board units]
         (let [set-cells (filter #(= (count (board %)) 1) cells)
                to-rem (map (fn [c]
                                        [(set (for [el set-cells 
                                                           :when (not (= el c))]
                                                   (first (seq (board el)))))
                                          c])
                                  cells)
                new-board (reduce (fn [b rem]
                                             (assoc b (fnext rem)
                                                           (difference (b (fnext rem)) (first rem))))
                                          board
                                          to-rem)
                changed (compute-changed board new-board cells)]
           (propagate new-board changed units))))


(defn min-test [f coll]
  (let [s (seq coll)]
     (reduce (fn [a b]
                  (if (< (f a) (f b))
                       a
                       b))
                 s)))

(defn initial-constrain [board units]
   (first (propagate board (keys board) units)))

(defn search [board units]
  (if board
    (let [test (fn [key]
                    (if (= (count (board key)) 1)
                        100000000
                        (count (board key))))
           to-try (min-test test (keys board))]
        (if (= (count (board to-try)) 1)
           board
           (loop [posses (seq (board to-try))]
              (if (seq posses)
                 (let [attempt (first posses)
                        new-board (assoc board to-try #{attempt})
                         new-board (first (propagate new-board [to-try] units))]
                     (if new-board
                       (if-let [res (search new-board units)]
                               res 
                              (recur (rest posses)))
                         (recur (rest posses))))
                 nil))))
      nil))

(defn solve [board units]
  (search (initial-constrain board units) units))


