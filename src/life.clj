(ns life
  (:import (javax.swing JFrame JPanel JButton JTextArea JLabel SwingUtilities JScrollPane)
	   (java.awt Graphics BorderLayout FlowLayout Color Dimension)
	   (java.awt.event MouseListener ActionListener MouseEvent MouseAdapter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; World - board and aquares
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *board-size* 25)
(def *square-size* 20)

(def *board* (ref (vec (repeat *board-size*
			       (vec (repeat *board-size* false))))))

(defn get-square [row col]
  (get-in @*board* [row col]))

(defn draw-board-square [#^Graphics g x y w h vl]
  (doto g
    (.setColor (if vl Color/BLACK Color/LIGHT_GRAY))
    (.fillRect x y w h)))

(defn draw-board [#^Graphics g w h]
  (let [sq-size *square-size*
	sqw sq-size
	sqh sq-size
	rect-size (* (dec *board-size*) sq-size)]
    (doto g
      (.setColor Color/BLACK)
      (.drawRect 0 0 rect-size rect-size))
    (doseq [row (range *board-size*)
	    col (range *board-size*)]
      (let [sqx (* col sq-size)
	    sqy (* row sq-size)]
	(draw-board-square g sqx sqy sqw sqh (get-square row col))))))

(defn render [#^Graphics g w h]
  (doto g
    (.setColor Color/WHITE)
    (.fillRect 0 0 w h))
  (draw-board g w h))

(defn from-coords [x y]
  [(quot y *square-size*) (quot x *square-size*)])

(defn toggle-square [row col]
  (let [vl (get-square row col)]
    (dosync
     (alter *board* assoc-in [row col] (not vl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; That is Life is all about
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn map-2d [fun arr]
  (let [rows (count arr)
	cols (count (get arr 0))]
    (loop [r 0
	   c 0
	   res arr]
      (if (= r rows)
	res
	(if (= c cols)
	  (recur (inc r) 0 res)
	  (recur r (inc c) (assoc-in res [r c] (fun arr r c))))))))

(defn neighbours [board row col]
  [(get-in board [(dec row) (dec col)])
   (get-in board [(dec row) col])
   (get-in board [(dec row) (inc col)])
   (get-in board [row (dec col)])
   (get-in board [row (inc col)]) 
   (get-in board [(inc row) (dec col)])
   (get-in board [(inc row) col])
   (get-in board [(inc row) (inc col)])])

(defn count-neighbours [board row col]
  (count (filter true? (neighbours board row col))))

(defn gen-cell [board row col]
  (let [around (count-neighbours board row col)
	me (get-in board [row col])]
    (cond
     (and (not me) (or (< around 2) (> around 3))) false
     (and (not me) (= around 3)) true
     (and me (or (= around 2) (= around 3))) true
     :aotherwise false)))

(defn next-generation [board]
  (map-2d gen-cell board))

(defn make-step []
  (dosync
   (alter *board* next-generation)))

(def text-area (JTextArea. "Trace console\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simulation Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *is-running* (atom false))

(defn create-simulator [panel]
  (Thread.
   #(do
      (. text-area append (str "Started simulation thread\n"))
      (while true (do
		    (while (not @*is-running*) (Thread/sleep 100))
		    (make-step)
		    (.repaint panel)
		    (Thread/sleep 600))))))

(defn start-simulation []
  (.append text-area "Start simulation\n")
  (reset! *is-running* true))

(defn stop-simulation []
  (.append text-area "Stop simulation\n")
  (reset! *is-running* false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro on-action [comp event & body]
  `(.addActionListener ~comp
		       (proxy [java.awt.event.ActionListener] []
			 (actionPerformed [~event] ~@body))))

(defn mouse-listener []
  (proxy [MouseAdapter] []
    (mouseClicked [#^MouseEvent e]
		  (let [x (.getX e)
			y (.getY e)
			[row col] (from-coords x y)]
		    (toggle-square row col)
		    (-> e .getComponent .repaint)))))

(defn create-board []
  (let [board-panel (proxy [JPanel] []
		      (paintComponent [g]
				      (proxy-super paintComponent g)
				      (render g
					      (.getWidth this)
					      (.getHeight this))))
	simula (create-simulator board-panel)]
    (.start simula)
    (doto board-panel
      (.addMouseListener (mouse-listener)))))

(defn create-panel []
  (let [pane (JPanel. (BorderLayout.))
	button-panel (JPanel. (FlowLayout.))
	board-panel (create-board)
	trace-panel (JScrollPane. text-area)
	start-button (JButton. "Live!")
	stop-button (JButton. "Die!")]
    (on-action start-button e (start-simulation))
    (on-action stop-button e (stop-simulation))
    (doto button-panel
      (.add start-button)
      (.add stop-button))
    (doto trace-panel
      (.setPreferredSize (Dimension. 500 50)))
    (doto pane
      (.setOpaque true)
      (.add button-panel BorderLayout/PAGE_START)
      (.add board-panel BorderLayout/CENTER)
      (.add trace-panel BorderLayout/PAGE_END))))

(defn create-frame []
  (doto (JFrame. "Life Exersice")
    (.setContentPane (create-panel))
    (.setSize 520 630)
    (.setVisible true)
    #_(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)))

(defn run-life []
  (SwingUtilities/invokeLater
   (proxy [Runnable] []
     (run []
	  (create-frame)))))
