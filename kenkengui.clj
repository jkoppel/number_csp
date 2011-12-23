(ns kenkengui
  (:use kenken
	guiutil
	csp
	(clojure set))
  (:import (javax.swing JPanel JFrame JButton JTextField JOptionPane)
	   (java.awt BorderLayout GridBagLayout Color BasicStroke
		     GridBagConstraints Font)
		     (java.awt.event KeyAdapter MouseAdapter ActionListener)
		     (java.io PrintWriter BufferedWriter FileWriter)))


(def *grid-weight* 5)
(def *ipad* 4)

(defstruct cell :north :east :south :west :op :num :val)


;Now just finish behavior for handling incompletely-solved boards
(defn kenken->str [board]
  (let [board-size (Math/sqrt (count board))
	square-size (+ (* 2 (apply max (map count (vals board)))) 1)
	divider-unit (apply str (concat (replicate square-size "-") ["+"]))
	divider (apply str (concat (cons "+" (replicate board-size divider-unit)) ["\n"]))
	filler-line-unit (apply str (concat (replicate square-size " ") ["|"]))
	filler-line (apply str (concat (cons "|" (replicate board-size filler-line-unit)) ["\n"]))
	letters (take board-size "ABCDEFGHI")
	nums (take board-size "123456789")
	display-lines (map 
		       (fn [letter]
			 (apply str (concat (map 
					     (fn [num]
					       (let [label (str letter num)
						     posses (sort (seq (board label)))]
						 (apply str (cons "| " (map #(str % " ") posses)))))
					     nums)
					    "|\n")))
		       letters)]
    (apply str (cons divider (map
			      #(apply str (concat
					   (cons %
						 (replicate (- square-size 1) filler-line))
					   divider))
			      display-lines)))))


(defn create-board [size]
  (vec  (for [y (range size)]
	  (vec (for [x (range size)]
		 (struct cell true true true true nil nil nil))))))

(defn toggle-wall [board y x dir]
  (let [board (assoc-in board [y x dir] (not (get-in board [y x dir])))
         dir-dydx {:north [-1 0],
                      :east [0 1],
                      :south [1 0],
                      :west [0 -1]}
        [y x] (map + (dir-dydx dir) [y x])
        dir-rev {:north :south,
                   :east :west,
                    :south :north,
                    :west :east}
        dir (dir-rev dir)]
    (if (get-in board [y x])
        (assoc-in board [y x dir] (not (get-in board [y x dir])))
        board)))
        
(defn assoc-cell-constraint [board y x op num]
  (let [board (assoc-in board [y x :op] op)]
    (assoc-in board [y x :num] num)))
        
(defn wall-for [panel size y x ]
  (let [width (quot (.getWidth panel) size)
         height (quot (.getHeight panel) size)
         cell-y (quot y height)
         cell-x (quot x width)
         rel-y (mod y height)
         rel-x (mod x width)
         center-xoff (- (quot width 2) rel-x)
         center-yoff (- (quot height 2) rel-y)
         dir (if (> (Math/abs center-xoff) (Math/abs center-yoff))
                 (if (< center-xoff 0)
                     :east
                     :west)
                 (if (< center-yoff 0)
                     :south
                     :north))]
     [cell-y cell-x dir]))

(def board (atom (create-board 6)))

(def disp-panel (proxy [JPanel] []
                       (paintComponent [g] 
                          (let [height (.getHeight disp-panel)
				width (.getWidth disp-panel)
				size (count @board)
				h-step (quot height size)
				w-step (quot width size)]
			    
			    (proxy-super paintComponent g)
			    
			    (.setColor g Color/BLACK)
			    (.setStroke g (new BasicStroke *grid-weight*))
			    
			    (doseq [y (range size)]
			      (doseq [x (range size)]
				(let [cell ((@board y) x)
                                      tlx (* x w-step)
                                      tly (* y h-step)]
				  (if (cell :north)
                                    (.drawLine g tlx tly (+ w-step tlx) tly))
				  (if (cell :west)
                                    (.drawLine g tlx tly tlx (+ h-step tly)))
				  (if (cell :south)
                                    (.drawLine g tlx (+ h-step tly) (+ w-step tlx) (+ h-step tly)))
				  (if (cell :east)
                                    (.drawLine g (+ w-step tlx) tly (+ w-step tlx) (+ h-step tly)))
				  (if (cell :op)
                                    (.drawString g (str (cell :num) (cell :op))
					;drawString has y be the baseline; we want to refer to the top left corner
						 (+ tlx *ipad*) (+ tly *ipad* (.getHeight (.getFontMetrics g)))))
				  (if (cell :val)
                                    (let [old-font (.getFont g)
					  font (Font. "Arial" Font/BOLD (- 27 size))
					  center-x (+ tlx (quot w-step 2))
					  center-y (+ tly (quot h-step 2))
					  _ (.setFont g font)
					  metrics (.getFontMetrics g)
					  bottom (+ center-y (quot (.getAscent metrics) 2))
					  left (- center-x (quot (.stringWidth metrics (str (cell :val))) 2))]
				      (.drawString g (str (cell :val)) left bottom)
				      (.setFont g old-font))))))))))


(def cur-op (atom nil))

(.addMouseListener disp-panel (proxy [MouseAdapter] []
                                                     (mouseClicked [e]
                                                       (let [[y x dir] (wall-for disp-panel (count @board) (.getY e) (.getX e))]
                                                         (if (= @cur-op nil)
                                                               (reset! board (toggle-wall @board y x dir))
                                                               (if (= @cur-op "c")
                                                                 (reset! board (assoc-cell-constraint @board y x nil nil))
                                                                 (let [num (Integer/parseInt (JOptionPane/showInputDialog "Enter target number"))]
                                                                      (reset! board (assoc-cell-constraint @board y x @cur-op num))))))
                                                       (reset! cur-op nil)
                                                       (.repaint disp-panel))))

(.setBackground disp-panel Color/WHITE)

(defn cur-op-setter-proxy [op]
  (proxy [ActionListener] []
    (actionPerformed [e]
      (reset! cur-op op))))

(defn to-kenken-index [y x]
  (str (.charAt "ABCDEFGHI" y) (+ x 1)))

(defn flood-fill [board y x visited & [op num]]
  (if-let [cell (get-in board [y x])]
      (if (visited [y x])
          [visited #{} op num]
          (let [op (if (cell :op) (cell :op) op)
                 num (if (cell :op) (cell :num) num)
                 visited (union visited #{[y x]})
                 unit #{(to-kenken-index y x)}
                 [visited rest-unit op num] (if (cell :north)
                                                       [visited unit op num]
                                                       (flood-fill board (- y 1) x visited op num))
                 unit (union unit rest-unit)
                 [visited rest-unit op num] (if (cell :south)
                                                       [visited unit op num]
                                                       (flood-fill board (+ y 1) x visited op num))
                 unit (union unit rest-unit)
                 [visited rest-unit op num] (if (cell :west)
                                                       [visited unit op num]
                                                       (flood-fill board y (- x 1) visited op num))
                 unit (union unit rest-unit)
                 [visited rest-unit op num] (if (cell :east)
                                                       [visited unit op num]
                                                       (flood-fill board y (+ x 1) visited op num))
                 unit (union unit rest-unit)]
            [visited unit op num]))
      [visited #{} op num]))

(defn board->kenken-desc [board]
  (let [size (count board)
         indices-seq (for [y (range size) x (range size)] [y x])
         units (loop [visited #{} units '() indices indices-seq]
                   (if (seq indices)
                    (let [[y x] (first indices)
                           [visited cells op num] (flood-fill board y x visited)]
                        (if (empty? cells)
                          (recur visited units (rest indices))
                          (recur visited (cons [cells op num] units) (rest indices))))
                    units))
         header (str "# " size "\n")
         lines (map (fn [[cells op num]]
                          (str op " " num " " (apply str (interpose " " (seq cells)))))
                        units)]
    (str header (apply str (interpose "\n" lines)))))


(defn dump-to-file [str name]
  (let [f (PrintWriter. (BufferedWriter. (FileWriter. name)))]
    (.print f str)
    (.close f)))

(defn assoc-solution [board sol]
  (let [size (count board)
	indices-seq (for [y (range size) x (range size)] [y x])]
    (loop [board board indices indices-seq]
      (if (seq indices)
	(let [[y x] (first indices)]
	  (recur 
	   (assoc-in board [y x :val] (first (seq (sol (to-kenken-index y x)))))
	   (rest indices)))
	board))))

(let [frame (new JFrame "KenKen Solver")]
   (doto frame
      (.setLayout (new BorderLayout))
      (.add disp-panel BorderLayout/CENTER)
      (.setSize 300 300)
      (.setVisible true))
   (let [controls (JPanel.)
	 size-field (JTextField. 7)
	 plus-button (JButton. "+")
	 minus-button (JButton. "-")
	 times-button (JButton. "*")
	 divide-button (JButton. "/")
	 clear-button (JButton. "Clear cell")
	 solve-button (JButton. "Solve")]
          
      (.addKeyListener size-field (proxy [KeyAdapter] []
                                            (keyPressed [e]
                                               (if (= (.getKeyChar e) \newline)
                                                   (do
                                                      (reset! board (create-board (Integer/parseInt (.getText size-field))))
                                                      (.repaint disp-panel))))))
                                                      
      (.addActionListener plus-button (cur-op-setter-proxy "+"))
      (.addActionListener minus-button (cur-op-setter-proxy "-"))
      (.addActionListener times-button (cur-op-setter-proxy "*"))
      (.addActionListener divide-button (cur-op-setter-proxy "/"))
      (.addActionListener clear-button (cur-op-setter-proxy "c"))
      
      (.addActionListener solve-button (proxy [ActionListener] []
                                                      (actionPerformed [e]
                                                        (let [desc (board->kenken-desc @board)]
                                                           (dump-to-file desc "temp_kenken.dat")
                                                           (reset! board (assoc-solution @board (apply solve (load-kenken "temp_kenken.dat"))))
							   (.repaint frame)))))
          
          
      (.setLayout controls (GridBagLayout.))
      (gridbag-add controls size-field "weightx=1.0;gridwidth=2")
      (gridbag-add controls plus-button "weightx=1.0;gridy=1;gridx=0")
      (gridbag-add controls minus-button "weightx=1.0;gridx=1;gridy=1")
      (gridbag-add controls times-button "weightx=1.0;gridx=0;gridy=2")
      (gridbag-add controls divide-button "weightx=1.0;gridx=1;gridy=2")
      (gridbag-add controls clear-button "weightx=1.0;gridx=0;gridy=3;gridwidth=2")
      (gridbag-add controls solve-button "weightx=1.0;gridx=0;gridy=4;gridwidth=2")
      (.add frame controls BorderLayout/WEST))
      (.updateUI (.getContentPane frame))
      (.repaint frame))
