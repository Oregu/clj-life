(ns life
  (:import (javax.swing JFrame JPanel JButton JTextArea JLabel SwingUtilities JScrollPane WindowConstants)
	   (java.awt Graphics BorderLayout FlowLayout Color Dimension)
	   (java.awt.event MouseListener ActionListener MouseEvent MouseAdapter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; World - board and aquares
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *board-size* 5)
(def *square-size* 20)

(def *board* (ref (vec (repeat *board-size*
			       (vec (repeat *board-size* false))))))

(defn get-square
  ([row col] (get-square @*board* row col))
  ([board row col] (get-in board [row col])))

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
    (loop [r 0, c 0, res arr]
      (if (= r rows)
	res
	(if (= c cols)
	  (recur (inc r) 0 res)
	  (recur r (inc c) (assoc-in res [r c] (fun arr r c))))))))

(defn subgrid
  "x & y are top left coords, x+ & y+ are spans.
Author: Michał Marczyk"
  [g x y x+ y+]
  (let [x-to (min (+ x x+) (count (g 0)))
	y-to (min (+ y y+) (count g))
	x-from (max 0 x)
	y-from (max 0 y)]
    (vec
     (map #(subvec % x-from x-to)
          (subvec g y-from y-to)))))

(defn neighbours [board row col]
  (-> board
      (subgrid (dec col) (dec row) 3 3)
      ; Here should be code do discount (row, col) square (it's not neighbour - it is myself)
      flatten))

(defn count-neighbours [board row col]
  (count (filter true? (neighbours board row col))))

(defn gen-cell [board row col]
  (let [nc (count-neighbours board row col)
	me (get-square board row col)]
    (cond
     (and (not me) (= nc 3)) true
     (and me (or (= nc 2) (= nc 3))) true
     true false)))

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
    (.setSize 750 850)
    (.setVisible true)
    (.setDefaultCloseOperation WindowConstants/DISPOSE_ON_CLOSE)))

(defn -main [& m]
  (SwingUtilities/invokeLater
   (proxy [Runnable] []
     (run []
	  (create-frame)))))
