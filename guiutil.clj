(ns guiutil
  (:import (java.awt GridBagConstraints Graphics)))


(def gridbag-constraint nil)

(defn gridbag-add [container el constraints]
  (let [c (GridBagConstraints.)
         attrs (seq (.split constraints ";" ))]
      (doseq [s attrs]
        (let [m (re-matcher (re-pattern "(.+)=(.+)") s)
               _  (re-find m)
               [_ prop val] (re-groups m)
               prop (symbol prop)
               val (eval (read-string val))]
            (binding [gridbag-constraint c]   
             (eval `(set! (. gridbag-constraint ~prop) ~val)))))
      (.setConstraints (.getLayout container) el c)
      (.add container el)))

(defmacro with-color [g col & body]
  `(let [oldcol# (.getColor ~g)]
     (.setColor ~g ~col)
     ~@body
     (.setColor ~g oldcol#)))


(defmacro with-stroke [g stroke & body]
  `(let [oldstroke# (.getStroke ~g)]
     (.setStroke ~g ~stroke)
     ~@body
     (.setStroke ~g oldstroke#)))
