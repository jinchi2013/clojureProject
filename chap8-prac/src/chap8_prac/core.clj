(ns chap8-prac.core
  (:gen-class))

(def message "Good job!")
(defmacro with-mischief-more
  [& stuff-to-do]
  (concat (list 'let ['message "oh, big deal!"])
          stuff-to-do))
;; if you call (with-mischief) with argument message here
;; the value of the message will be "oh, big deal", which is the Variable capture issure of macro

(defmacro with-mischief
  [& stuff-to-do]
  (let [macro-message (gensym 'message)]
;; gensym is used here to create a unique symbol for the message in let, which is distinguishing it from the variable in the let instance.
    `(let [~macro-message "Oh, big deal!"]
       ~@stuff-to-do
       (println "I still need to say: " ~macro-message))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Double Evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro report
  [to-try]
  `(if ~to-try
     (println (quote ~to-try) "was successful: " ~to-try)
     (println (quote ~to-try) "was not successful: " ~to-try)))
;; Here if you use the macro report with below function
;; (report (do (Thread/sleep 1000) (+ 1 1)))
;; You will see it actually take 2 secs to show the result
;; first 1 sec~ at the if function
;; second 2 sec~ at the println function
;; the result from if will not be memorized

;; HERE you can use auto-gensym'd symbol to solve this
(defmacro report-1-sec
  [to-try]
  `(let [result# ~to-try]
     (if result#
       (println (quote ~to-try) "was successful: " result#)
       (println (quote ~to-try) "was not successful: " result#))))

