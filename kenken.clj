(ns kenken
   (:use 
    csp
    (clojure.contrib combinatorics)
    (clojure set)))

(def constraints {"!"  assign-constraint,
		  "+" plus-constraint,
		  "uniq" uniq-constraint,
		  "-" minus-constraint
		  "*" times-constraint
		  "/" divide-constraint})

(defn load-kenken [filename]
  (let [s (slurp filename)
                             ;Re-pattern instead of literal
                            ;to make life easier with syntax highlighter meant for CL
        m (re-matcher (re-pattern "([#!+*/\\-])\\s+(\\d+)\\s+((\\s*?[A-J]\\d)*)") s)]
    (re-find m)
    (let [size (Integer/parseInt (.group m 2))
           chars (take size "ABCDEFGHI")
           nums (range 1 (+ size 1))
           indices (map #(apply str %) (cartesian-product chars nums))
           board (loop [hsh {} squares indices]
                       (if (seq squares)
                         ;squares can be anything by default
                         (recur (assoc hsh (first squares) (set nums)) (rest squares))
                         hsh))
          jigsaw-units (loop [hsh {}]
                    (if (re-find m)
                        (let [groups (re-groups m)
                               constrained (seq (.split (groups 3) "\\s+"))
                               c ((constraints (groups 1)) (Integer/parseInt (groups 2)) constrained)
                               updated-units (loop [cells constrained h hsh]
                                                     (if (seq cells)
                                                       (recur (rest cells)
                                                         (assoc-poss h (first cells) c))
                                                       h))]
                          (recur updated-units))
                       hsh))
         cols (map (fn [n] (map #(str % n) chars)) nums) 
         rows (map (fn [c] (map #(str c %) nums)) chars) 
         units (loop [hsh jigsaw-units to-add (concat rows cols)]
                                (if (seq to-add)
                                   (let [constrained (first to-add)
                                          c ((constraints "uniq") 1 constrained)
                                          updated-units (loop [cells constrained h hsh]
                                                                (if (seq cells)
                                                                     (recur (rest cells) 
                                                                       (assoc-poss h (first cells) c))
                                                                     h))]
                                       (recur updated-units (rest to-add)))
                                    hsh))]
        [board units])))
