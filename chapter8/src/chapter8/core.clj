(ns chapter8.core
  (:gen-class))

;; use single quote to stop the evaluation, a litte bit feels like escape somehow
;; and just return this symbol
(defmacro my-print
  [expression]
  (list 'let ['result expression]
        (list 'print 'result)
        'result))

(defmacro unless
  "Inverted 'if"
  [test & branches]
  (conj (reverse branches) test 'if))

(defn is-small?
  [number]
  (unless (< number 50)
          "larger than 50"
          "smaller than 50"))

;; macroexpand will return how unless is define in macro
(macroexpand '(unless (done-been slapped? me)
                      (slap me :silly)
                      (say "I reckon that'll learn me")))

(defmacro code-critic-list
  "Phrases are courtesy hermes conrad from futurama"
  [bad good]
  (list 'do
        (list 'println
              "Great squid of Madrid, this is bad code:"
              (list 'quote bad))
        (list 'println
              "Sweet gorilla of Mania, this is good code:"
              (list 'quote good))))

(defmacro code-critic-old1
  "Phrases are courtesy Hermes conrad from futurama"
  [bad good]
  `(do (println "Great squid of madrid, this is bad code:"
                (quote ~bad))
       (println "sweet gorilla of manila, this is good code:"
                (quote ~good))))

;; here you can rewrite the previous function
(defn criticize-code
  [criticism code]
  `(println ~criticism (quote ~code)))

(defmacro code-critic-old2
  [bad good]
  `(do ~(criticize-code "Cursed bacteria of Liberia, this is bad code:" bad)
       ~(criticize-code "Sweet sacred boa of Western and Eastern Samoa, this is good code:" good)))

(defmacro code-critic-with-error
  [bad good]
  `(do ~(map #(apply criticize-code %)
             [["Great squid of Madrid, this is bad code:" bad]
              ["Sweet gorilla of Manila, this is good code:" good]])))

(defmacro code-critic
  [good bad]
  `(do ~@(map #(apply criticize-code %)
        ;; ~@ -- Unquote splicing can break the list
              [["Sweet lion of Zion, this is bad code:" bad]
                ["Great cow of Moscow, this is good code:" good]])))

;; Variable Capture
(def message "Good job!")

(defmacro with-mischief
  [& stuff-to-do]
  (concat (list 'let ['message "oh, big deal!"])
          stuff-to-do))
;; In the code above, in let the value of message in the global can not pass in

;; if you want to introduce let bindings in you macro, you can use a gensym

;;;;;;;;;;;;;;;;;;;;;;;;
;;    gensym
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro without-mischief
  [& stuff-to-do]
  (let [macro-message (gensym 'message)]
                       ;; avoids variable capture by using gensym to create a new, unique symbol
                       ;; that symbol that then gets bound to macro-message
    `(let [~macro-message "oh, big deal!"]
            ;; macro-message here is not quote will resolve to the gensym'd symbol
            ;; this gensym'd symbol is distinct from any symbol within stuff-to-do
            ;; so you avoid variable capture.
       ~@stuff-to-do
       (println "i'm still need to say: " ~macro-message))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;    auto-gensym
;;;;;;;;;;;;;;;;;;;;;;;;

`(let [name# "larry Potter"] name#)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Double Evaluation
;;    Which occurs when a form passed to a macro as an argument gets evaluated more than once.
;;    This mean the result of the argument evaluated in the macro will be save to be used next time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro report-repeat
  [to-try]
  `(if ~to-try
     (println (quote ~to-try) "was successful: " ~to-try)
     (println (quote ~to-try) "was not successful: " ~to-try)))

;; if you call the function above =>
;; => (report (do (Thread/sleep 1000) (+ 1 1)))
;; => it actually take two second to print the line you want
;; => first time is the if condition function 1 sec~
;; => second time is the : " ~to-try 2 sec~

(defmacro report
  [to-try]
  `(let [result# ~to-try]
         ;; the result of ~to-try is reserved here
         ;; so later the form will not evaluated again
     (if result#
       (println (quote ~to-try) "was successful: " result#)
       (println (quote ~to-try) "was not succesfulL " result#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Macros All the Way Down
;;    --macro expansion happens before evaluation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For example, let's say you wanted to doseq using the report macro.

;;INSTEAD OF MULTIPLE CALLS TO REPORT:
(report (= 1 1))
(report (= 1 2))

;; Let's iterate
(doseq [code ['(= 1 1) '(= 1 2)]]
  (report code))
;; the report macro works fine when we pass it functions individually, but when we use deseq to iterate report over multiple functions, it's a worthless failure.
;; Here is what a macro expansion for one of the doseq iterations would look like:

;; (if code
;;  (clojure.core/println 'code "was successful: " code)
;;  (clojure.core/println 'code "was not successful: " code))

;; as you can see, report recevies the  unevaluated symbol code in each iteration, which is truly for if function
;; but we want it to receive whatever code is bound to at evaluation time, and obviously at macro expansion time, just can't access those values.

;; To resolve this situation, we migth write another macro, like this : 
(defmacro doseq-macro
  [macroname & args]
  `(do 
     ~@(map (fn [arg] (list macroname arg)) args)))
;; The solution is another way to iterate the report function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Validation Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some order detials def the below
(def order-details
  {:name "Mitchard Blimmons"
   :email "mitchard.blimmonsgmail.com"})

;; the function we want would be like 
;; => (validate order-details order-details-validations)
;; => {:email ["Your email address doesn't look like an email address."]}

;; ====>>>> order-details-validations
(def order-details-validations
  {
   :name
   ["Please enter a name" not-empty]
   
   :email
   ["Please enter an email" not-empty

    "Your email address doesn't look like an email address"
   #(or (empty? %) (re-seq #"@" %))]})

(defn error-messages-for
  "Return a seq of error messages"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if-valid
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; With our validation code in place, we can now validate records to our hearts's content! Most often, validation with look something like this:

(let [errors (validate order-details order-details-validations)]
  (if (empty? errors)
    (println :success)
    (println :failure errors)))

(defn if-valid-not-work
  "because success-code and failure-code would get evaluated each time"
  [record validations success-code failure-code]
  (let [errors (validate record validations)]
    (if (empty? errors)
      success-code
      failure-code)))

(defmacro if-valid
  "handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))



