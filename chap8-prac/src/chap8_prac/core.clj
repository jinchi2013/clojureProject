(ns chap8-prac.core
  (:gen-class))

(def message "Good job!")
(defmacro with-mischief-more
  [& stuff-to-do]
  (concat (list 'let ['message "oh, big deal!"])
          stuff-to-do))
;; if you call (with-mischief) with argument message here
;; the value of the message will be "oh, big deal", which is the Variable capture issure of macro

(defmacro without-mischief
  [& stuff-to-do]
  (let [macro-message (gensym 'message)]
;; gensym is used here to create a unique symbol for the message in let, which is distinguishing it from the variable in the let instance.
    `(let [~macro-message "Oh, big deal!"]
       ~stuff-to-do
       (println "I still need to say: " ~macro-message))))

;; (without-mischief (println "I have to say: " message))

;;=>>  `(~@(println "I have to say: " message)) ==>> nil this means no value is turn
;;=>>  `(~(println "I have to say: " message)) ==>> (nil) this means to call nil function which will lead to error

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

;; HERE you can use auto-gensym'd symbol to solve this : result#
(defmacro report-1-sec
  [to-try]
  `(let [result# ~to-try]
     (if result#
       (println (quote ~to-try) "was successful: " result#)
       (println (quote ~to-try) "was not successful: " result#))))

;; Here you create a unique symbol result# in let instance as gensym'd symbol

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros All the Way Down
;; Which means the fact that macro expansion happens before evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; =>(report (= 1 1))
;; => (= 1 1) was successful: true 
;; And then
;; =>(report (= 1 2))
;; => (= 1 2) was not successful: false
;; which has no problem

;; But if you want to simplify it
;; Iterate the report macro call by deseq function
;; => (doseq [code ['(= 1 1) '(= 1 2)]] (report code))
;; => code was successful: (= 1 1);
;; => code was successful: (= 1 2);
;; the result you can see the code 'equal function' are happens before evalustion

;; To resovle this situation, we might write another macro, like this:

(defmacro doseq-macro
  [macroname & args]
  `(do
     ~@(map (fn [arg] 
              (list macroname arg)) 
            args)))

;; Validation functions

(def order-details
  {:name "M B"
   :email "m.b.com"})

(def order-details-validations
  {:name 
   ["please enter a name" not-empty]
   
   :email
   ["Please enter an email address" not-empty
    "Your email address doesn't look like an emali address" #(or (empty? %) (re-seq #"@" %))]})

(defn error-messages-for
  "Return a seq for error message"
  [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate))
                     (partition 2 message-validator-pairs))))

(defn validate
  "Returns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[fieldname validation-check-groups] validation
                  value (get to-validate fieldname)
                  error-messages (error-messages-for value validation-check-groups)]
              (if (empty? error-messages)
                errors
                (assoc errors fieldname error-messages))))
          {}
          validations))

(defmacro if-valid
  "Handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))
