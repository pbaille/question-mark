
(ns ^:no-doc question-mark.stripped.demo
  (:require [question-mark.stripped.core :refer [?]])
  #?(:cljs (:require-macros [question-mark.stripped.demo :refer [is isnt throws]])))

#?(:clj (do (defmacro is
              "arity 1: assert that given argument is truthy
               arity n : assert that given args are truthy and equals"
              ([x] `(let [x# ~x] `(assert x# (str '~x "\nshould be.")) x#))
              ([x & xs]
               `(let [x# ~x]
                  (assert (= x# ~@xs)
                          (str "\n\nThose expressions should be equals:\n  "
                               (apply str (interpose "\n  " (cons '~x '~xs)))
                               "\n"))
                  x#)))

            (defmacro isnt
              "every argument has to be falsy"
              ([x] `(assert (not ~x) (str "\n" '~x "\nshould not be.")))
              ([x & xs] `(do ~@(map (fn [x] `(isnt ~x)) (cons x xs)))))

            (defmacro throws [x]
              `(assert (= ::catched
                          (try ~x
                               (catch ~(if (:ns &env) 'js/Error 'java.lang.Exception) _# ::catched)))
                       (str '~x "\nshould be an error.")))))



(is :ok (? (pos? 1) :ok))
(isnt (? (pos? 0) :ok))

(is :ok (? (pos? 1) :ok :ko))
(is :ko (? (pos? 0) :ok :ko))

(def m {:a 2 :b 3})

(is 4
    (? [a (get m :a)]
       (+ a a)
       :fail))

(is 5
    (? [a (get m :a)
        b (get m :b)]
       (+ a b)
       :fail))

(is 5
    (? [{:keys [a b]} m]
       (+ a b)))

(isnt (? [{:keys [a c]} m]
         (+ a c)))


(let [f (fn [x]
          (? (pos? x) [:pos x]
             (neg? x) [:neg x]
             :zero))] ;; unlike cond we do not need the :else keyword before the default case

  (is (f -1)
      [:neg -1])
  (is (f 3)
      [:pos 3])
  (is (f 0)
      :zero))

(let [f (fn [m]
          (?
           ;; case 1
           ;; if m contains? a :foo key we bind its value to the symbol 'foo and return it
           [foo (:foo m)] foo
           ;; case 2
           [_ (:baz m) ;; checking that m is containing a :baz key
            bar (:bar m)] ;; if yes we try to bind the :bar value of m to the symbol 'bar
           ;;then return it
           bar
           ;; bottom case
           [:fails m]))]

  (is 1 (f {:foo 1 :bar 2}))
  (is 2 (f {:bar 2 :baz 3}))
  (is [:fails {:some :thing}]
      (f {:some :thing})))

(defn mix-test [x]
  (? ;; the first case do not bind its return value (like if)
   (number? x)
   (? (pos? x) [:pos x]
      (neg? x) [:neg x]
      :zero)
   ;; the second case is like a multi binding if-let, it tries to bind two values
   [a (get x :a)
    b (get x :b)] (+ a b)
   ;; if those two cases have failed we are printing something
   (println "mix-test has failed")
   ))

(? [a 1 b 2]
   (+ a b))