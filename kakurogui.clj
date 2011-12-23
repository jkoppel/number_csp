(ns kakurogui
  (:use guiutil
	kakuro
	csp)
  (:import (javax.swing JPanel JLabel JFrame JButton JTextField JOptionPane)
	   (java.awt BorderLayout GridBagLayout Color BasicStroke
		     GridBagConstraints Font)
		     (java.awt.event KeyAdapter MouseAdapter ActionListener)
		     (java.io PrintWriter BufferedWriter FileWriter)))


(def *border-weight* 2)
(def *slash-weight* 3)
(def *ipad* 4)

(defrecord Cell [filled ssum esum val])

(defn create-board [rows cols]
     (vec (for [y (range rows)]
	    (vec (for [x (range cols)]
		   (Cell. true nil nil nil))))))

(defn assoc-solution [board sol]
  (let [rows (count board)
	cols (count (board 0))
	indices-seq (for [y (range rows) x (range cols)] [y x])]
    (loop [board board indices indices-seq]
      (if (seq indices)
	(let [[y x] (first indices)]
	  (recur 
	   (assoc-in board [y x :val] (first (seq (sol (+ x (* y cols))))))
	   (rest indices)))
	board))))

(defn dump-to-file [str name]
  (let [f (PrintWriter. (BufferedWriter. (FileWriter. name)))]
    (.print f str)
    (.close f)))

(defn kakuro->str [board]
  (let [rows (count board)
	cols (count (board 0))
	s (str rows " " cols "\n")
	s (str s
	       (apply str
		      (interpose "\n"
				 (map (fn [r]
					(apply str (map {true "#" false "."} (map :filled r))))
				      board)))
	       "\n")
	s (str s
	       (apply str (for [i (range rows)
				j (range cols)
				c [((board i) j)]]
			    (str
			     (if (and (:filled c) (:ssum c))
			       (str i " " j " S " (:ssum c) "\n")
			       "")
			     (if (and (:filled c) (:esum c))
			       (str i " " j " E " (:esum c) "\n")
			       "")))))]
    s))

(def board (atom (create-board 10 10)))

(defn cell-for [panel rows cols y x ]
  (let [width (quot (.getWidth panel) cols)
	height (quot (.getHeight panel) rows)
	cell-y (quot y height)
	cell-x (quot x width)
	rel-y (mod y height)
	rel-x (mod x width)
	center-xoff (- (quot width 2) rel-x)
	center-yoff (- (quot height 2) rel-y)
	dir (if (> rel-x rel-y)
	      :esum
	      :ssum)]
    [cell-y cell-x dir]))

(def disp-panel (proxy [JPanel] []
		  (paintComponent [g]
				  (let [height (.getHeight disp-panel)
					width (.getWidth disp-panel)
					rows (count @board)
					cols (count (@board 0))
					h-step (quot height rows)
					w-step (quot width cols)]
				    
				    (proxy-super paintComponent g)
				    
				    (.setColor g Color/BLACK)
				    (.setStroke g (new BasicStroke *border-weight*))

				    (doseq [y (range rows)]
				      (doseq [x (range cols)]
					(let [cell ((@board y) x)
					      tlx (* x w-step)
					      tly (* y h-step)]
					  (.drawLine g tlx tly (+ w-step tlx) tly)
					  (.drawLine g tlx tly tlx (+ h-step tly))
					  (.drawLine g tlx (+ h-step tly) (+ w-step tlx) (+ h-step tly))
					  (.drawLine g (+ w-step tlx) tly (+ w-step tlx) (+ h-step tly))

					  (if (:filled cell)
					    (do 
					      (with-color g Color/GRAY
						       (.fillRect g
								  (+ tlx *border-weight*)
								  (+ tly *border-weight*)
								  (- w-step (* 2 *border-weight*))
								  (- h-step (* 2 *border-weight*))))

					      (if (or (:ssum cell) (:esum cell))
						(with-color g Color/WHITE
						  (with-stroke g (BasicStroke. *slash-weight*)
						    (.drawLine g (+ tlx *slash-weight*)
							       (+ tly *slash-weight*)
							       (+ w-step tlx) (+ h-step tly)))))

					      (if (:ssum cell)
						(.drawString g (str (:ssum cell)) (+ tlx *ipad*) (- (+ tly h-step) *ipad*)))

					      (if (:esum cell)
						(.drawString g (str (:esum cell))
							     (- (+ tlx w-step) *ipad* (.stringWidth (.getFontMetrics g) (str (:esum cell))))
							     (+ tly *ipad* (.getHeight (.getFontMetrics g)))))))

					  (if (:val cell)
					    (let [old-font (.getFont g)
						  font (Font. "Arial" Font/BOLD (- 27 rows))
						  center-x (+ tlx (quot w-step 2))
						  center-y (+ tly (quot h-step 2))
						  _ (.setFont g font)
						  metrics (.getFontMetrics g)
						  bottom (+ center-y (quot (.getAscent metrics) 2))
						  left (- center-x (quot (.stringWidth metrics (str (:val cell))) 2))]
					      (.drawString g (str (:val cell)) left bottom)
					      (.setFont g old-font))))))))))

(def cur-op (atom :toggle-cell))

(.addMouseListener disp-panel (proxy [MouseAdapter] []
				(mouseClicked [e]
					      (let [[y x side] (cell-for disp-panel (count @board) (count (@board 0)) (.getY e) (.getX e))]
						(if (= @cur-op :toggle-cell)
						  (reset! board (assoc-in @board [y x :filled] 
									  (not (get-in @board [y x :filled])))))
						(if (= @cur-op :setnum)
						  (let [num (Integer/parseInt (JOptionPane/showInputDialog "Enter target number"))]
						    (reset! board (assoc-in @board [y x side] num))))
						(if (= @cur-op :clear)
						  (reset! board (assoc-in @board [y x] (Cell. true nil nil nil)))))
					      (.repaint disp-panel))))


(defn cur-op-setter-proxy [op]
  (proxy [ActionListener] []
    (actionPerformed [e]
      (reset! cur-op op))))

(let [frame (new JFrame "Kakuro Solver")]
   (doto frame
      (.setLayout (new BorderLayout))
      (.add disp-panel BorderLayout/CENTER)
      (.setSize 450 400)
      (.setVisible true))
   (let [controls (JPanel.)
          rows-field (JTextField. 7)
          cols-field (JTextField. 7)
          size-button (JButton. "Set Size")
          toggle-button (JButton. "Toggle Walls")
          set-button (JButton. "Set Constraint")
          clear-button (JButton. "Clear cell")
          solve-button (JButton. "Solve")]
          
     (.addActionListener size-button (proxy [ActionListener] []
				       (actionPerformed [e]
							(do
							  (reset! board (create-board (Integer/parseInt (.getText rows-field))
										      (Integer/parseInt (.getText cols-field))))
							  (.repaint disp-panel)))))
     
      (.addActionListener toggle-button (cur-op-setter-proxy :toggle-cell))
      (.addActionListener set-button (cur-op-setter-proxy :setnum))
      (.addActionListener clear-button (cur-op-setter-proxy :clear))
      
      (.addActionListener solve-button (proxy [ActionListener] []
					 (actionPerformed [e]
							  (let [s (kakuro->str @board)]
							    (dump-to-file s "temp_kakuro.dat")
							    (reset! board (assoc-solution @board (apply solve (load-kakuro "temp_kakuro.dat"))))
							    (.repaint frame)))))
          
          
      (.setLayout controls (GridBagLayout.))
      (gridbag-add controls (JLabel. "Rows: ") "weightx=1.0;gridwidth=1")
      (gridbag-add controls rows-field "weightx=1.0;gridwidth=1")
      (gridbag-add controls (JLabel. "Cols: ") "weightx=1.0;gridwidth=1;gridy=2")
      (gridbag-add controls cols-field "weightx=1.0;gridwidth=1;gridy=2")
      (gridbag-add controls size-button "weightx=1.0;gridwidth=1;gridy=3")
      (gridbag-add controls toggle-button "weightx=1.0;gridx=0;gridy=4")
      (gridbag-add controls set-button "weightx=1.0;gridx=1;gridy=4")
      (gridbag-add controls clear-button "weightx=1.0;gridx=0;gridy=5;gridwidth=2")
      (gridbag-add controls solve-button "weightx=1.0;gridx=0;gridy=6;gridwidth=2")
      (.add frame controls BorderLayout/WEST))
      (.updateUI (.getContentPane frame))
      (.repaint frame))
