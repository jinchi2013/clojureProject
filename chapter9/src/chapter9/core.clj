(ns chapter9.core
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; The Sacred Art of Concurrent and Parallel Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One of the major use cases for concurrent programming is for blocking operations.
;; Blocking really just means waiting for an operation to finish.
;; You'll most often hear it used in relation to I/O operations, like reading a file or waiting for an HTTP request to finish.
;; synchronously and asynchronously

;; Concurrent Programming and Parallel Programming
;; They refer to techniques for decomposing a task into subtaks that can execute in parallel
;;  and mamaging the risks that arise when your program executes more than one task at the same time.

;; Future, Delays, and Promises

;; In Clojure, you can use futures to define a task and place it on another thread without requiring the result immediately.
;; You can create a future with the future macro. 

(future (Thread/sleep 4000)
        (println "I'll print immediately"))

;; Normally, if you evaluated Thread/sleep in your REPL, you wouldn’t be able to evaluate any other statements until the REPL was done sleeping; the thread executing your REPL would be blocked.

;; However, future creates a new thread and places each expression you pass it on the new thread,including Thread/sleep, allowing the REPL's thread to continue, unblocked.

;; the future function returns a reference value that you can use to request the result.
;; you can se the reference value to request a future's result, but if the future isn't done computing the result, 
;; you'll have to wait

;; Requesting a future's result is called dereferencing the future, and you do it with either the deref function or the @ reader macro.

(let [result (future (println "this prints once")
                     (+ 1 1))]
  (println "deref: " (deref result))
  (println "@: " @result))

;; dereferencing a future will block if the future hasn't finished running
(let [result (future (Thread/sleep 3000)
                     (+ 1 1))]
  (println "The result is: " @result)
  (println "It will be at least 3 seconds before I print"))

;; Delay
;; Delay allow you to define a task without having to execute it or require the result immediately.

(def jackson-5-delay
  (delay (let [message "Just call my name and I'll be there"]
           (println "First deref: " message)
           message)))
;; you can get the result of delay by force function
;; => (force jackson-5-delay)
;; like futures, a delay is run only once and its result is cached. 
;; => @jackson-5-delay
;; => "Just call my name and I'll be there"

(def gimli-headshots ["serious.jpg" "fun.jpg" "playful.jpg"])

(defn email-user
  [email-address]
  (println "Sending headshot notification to" email-address))
(defn upload-document
  "Needs to be implemented"
  [headshot]
  true)

(let [notify (delay (email-user "and-my-axe@gmail.com"))]
  (doseq [headshot gimli-headshots]
    (future (upload-document headshot)
            (force notify))))

;; Promise
;; Promise allow you to express that you expect a result without having to define the task that should,
;;   produce it or when that task should run.

;; You create promises using promise and deliver a result to them using deliver.
(def my-promise (promise))
;; create a promise
(deliver my-promise (+ 1 2))
;; use deliver to assign a result to promise
;; ==>> @my-promise
;; ==>> 3

(def yak-butter-international
  {:store "Yak Butter International"
    :price 90
    :smoothness 90})
(def butter-than-nothing
  {:store "Butter Than Nothing"
   :price 150
   :smoothness 83})
;; This is the butter that meets our requirements
(def baby-got-yak
  {:store "Baby Got Yak"
   :price 94
   :smoothness 99})

(defn mock-api-call
  [result]
  (Thread/sleep 1000)
  result)

(defn satisfactory?
  "If the butter meets our criteria, return the butter, else return false"
  [butter]
  (and (<= (:price butter) 100)
       (>= (:smoothness butter) 97)
       butter))

(time
 (let [butter-promise (promise)]
   (doseq [butter [yak-butter-international butter-than-nothing baby-got-yak]]
     (future (if-let [satisfactory-butter (satisfactory? (mock-api-call butter))]
               (deliver butter-promise satisfactory-butter))))
   (println "And the winner is : " @butter-promise)))

;; Each future’s task is to evaluate a yak butter site and to deliver the site’s data to the promise if it’s satisfactory
;; Finally, you dereference butter-promise, causing the program to block until the site data is delivered

;; This takes about one second instead of three because the site evaluations happen in parallel.

;; By decoupling the requirement for a result from how the result is actually computed,
;; You can perform multiple computations in parallel and save some time



;; !!!!!!!!!!!!!!!!!  Assgin callback in clojure ******************
(let [ferengi-wisdom-promise (promise)]
  (future (println "Here's some Ferengi wisdom:" @ferengi-wisdom-promise))
  (Thread/sleep 100)
  (deliver ferengi-wisdom-promise "whisper your way to success."))


