
[![Clojars Project](https://img.shields.io/clojars/v/pbaille/question-mark.svg)](https://clojars.org/pbaille/question-mark)
![cljdoc badge](https://cljdoc.org/badge/pbaille/question-mark)

# Context and Motivation

If you are familiar with clojure, you can [skip](#introducing-the--macro) this section.

## `clojure.core`

In clojure the runtime flow of our programs is mainly handled via boolean logic.

Therefore, `clojure.core` provides a wide variety of:

### predicates

those are functions that return a boolean, often suffixed by a '?' in clojure.core

```clojure
(= true
   (number? 1)
   (string? "io")
   (false? false)
   (true? true)
   (nil? nil))

(= false
   (false? true)
   (true? false)
   (number? "io"))
```

### boolean connectors

```clojure
(= true

   (not nil)
   (not false)

   (or false true)
   (or nil true)

   (and true true)
   (and false true)
   (and true nil))

(= false
   (not true)
   (or nil false)
   (and false true))
```

### Control macros (`if` and friends)

```clojure

(= :ok
   (if (number? 1) :ok :ko)
   (when true :ok)
   (when (not nil) 1 2 3 :ok))

(= :ko
   (if (string? 1) :ok :ko)
   (when true :ko))
```

We also have `if-not` and `when-not`

```clojure
(= :ok (when-not (number? "foo") :ok))
(= :ko (if-not true :ok :ko))
```

We often need to chain `if` forms together

```clojure
(defn ex1
  "an exemple of chaining if forms"
  [x]
  (if (number? x)
    [:number x]
    (if (string? x)
      [:string x]
      [:unknown x])))

(= (ex1 1)
   [:number 1])
(= (ex1 "io")
   [:string "io"])
(= (ex1 ())
   [:unknown ()])
```

We can use `cond` for more readable code

```clojure
(fn [x]
  (cond (number? x) [:number x]
        (string? x) [:string x]
        :else [:unknown x]))
```

### example

For instance we will implement the `plus` function using those predicates and macros.

It will add 2 things together:

- If those are numbers we do an addition
- if they are strings we concat them.
- or return why it failed

```clojure
(defn plus [x y]
  (cond (number? x)
        (if (number? y)
          (+ x y)
          [:fail y :not-a-number])
        (string? x)
        (if (string? y)
          (str x y)
          [:fail y :not-a-string])
        :else
        [:fail :unknown-type x]))
```

It seems to work

```clojure
(assert
  (and
    (= (plus 1 2)
       3)

    (= (plus "io" "p")
       "iop")

    (= (plus 1 "io")
       [:fail "io" :not-a-number])

    (= (plus "io" 1)
       [:fail 1 :not-a-string])

    (= (plus () 1)
       [:fail :unknown-type ()])))
```

### I may return something

In clojure we often deal with functions that may return something or `nil`

One of this function that we often use is `clojure.core/get`

```clojure
(= (get {:a 1} :a)
   1)
(= (get {:a 1} :b)
   nil)
```

With the `if-let` and `when-let` macros we can deal with such functions

```clojure
(let [m {:a 1 :b "io"}] ;; just setting up a map for exemples

  (if-let [a (get m :a)]
    ;; if m contains :a we bind the corresponding value to a
    ;; and increment it
    (inc a)
    ;; if get returns nil
    ;; we simply returns :fail
    :fail)

  ;; if you don't need to handle the false case you can use when-let
  (when-let [x (get m :x)]
    ;; :x is not in m so the following expression is skipped and nil is returned
    (inc x)))
```

`if-let` and `when-let` are limited to only one binding, so if you often have to nest them

```clojure
(let [m {:a 1 :b 2}]

  (when-let [a (get m :a)]
    (when-let [b (get m :b)]
      [:ok (+ a b)]))

  ;; the destructuration does not really help here

  ;; this works
  (when-let [{:keys [a b]} m]
    (+ a b))

  ;; but this is throwing
  '(when-let [{:keys [a c]} m]
     (+ a c))

  ;; because destructured bindings do not determine the behavior of those forms
  ;; so the expression (+ a c) is evaluated despite the fact that c is bound to nil

  ;; so we really have to write it like this
  (when-let [a (get m :a)]
    (when-let [c (get m :c)]
      (+ a c)))

  ;; or
  (when-let [{:keys [a c]} m]
    (if (and a c)
      (+ a c))))
```

Which is quite some effort for such a little thing

In addition to that, `if-let` is handling only two cases.
We could imagine a `cond-let` macro which would be handy.

```clojure
  (let [m {:some :stuff}]
    (cond-let [a (get m :a)] [m :got a]
              [b (get m :b)] [m :got b]
              :else [m :without-a-or-b]))
```

But this macro does not exist in `clojure.core`.

If you try to implement it you may realize that it is not as easy at it seems. Nevertheless there is several libraries that implement it ([A](https://github.com/Engelberg/better-cond), [B](https://github.com/walmartlabs/cond-let)).

Those past years I’ve came up with several version of macros similar to `cond-let`. But only recently I came up with a version that do it correctly in addition to extend it further.

# Introducing the `?` macro.

In fact if one thinks of it, we do not really need a different name for `if`, `if-let` and `cond`.
Those three macros do control flow in only sligthly differently maners.
Their syntaxes do not overlap, so we can safely use only one macro to cover the three behaviors.
Some lisps (like [arc](https://www.wikiwand.com/en/Arc_(programming_language))) do have an if form that can be used like a `cond` for instance.

This macro could be named `?` because we type it so often and because `if` cannot be overriden (being a special form)

With three arguments `?` behave like `if`

```clojure
(is :ok (? (pos? 1) :ok :ko))
(is :ko (? (pos? 0) :ok :ko))
```

It can also bind some value like `if-let` does

```clojure
(def m {:a 2 :b 3}) ; used in following examples

(is 4
    (? [a (get m :a)]
       (+ a a)
       :fail))
```

But the `?` macro can deal with several bindings (`if-let` do not).
I need to check but I'm not sure that existing clojure’s implementations of `cond-let` can do that properly.

```clojure
(is 5
    (? [a (get m :a)
        b (get m :b)]
       (+ a b)
       :fail))
```

We can destructure

```clojure
(is 5
    (? [{:keys [a b]} m]
       (+ a b)))
```

But this time it fails if an inner binding is `nil`

```clojure
(isnt (? [{:keys [a c]} m]
         (+ a c)))
```

The `?` macro can be used like `cond` too

```clojure
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
```

Even better, it can be used like `cond-let`

```clojure
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
```

Those two flavors of `let`/`cond` (`if-let`/`cond-let`) can be mixed together

```clojure
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
```

In fact if you think about it you realize that the `?` macro can behave pretty much like `let` .

All `let` forms that do not bind anything to `nil` can be replaced by the `?` macro

```clojure
(? [a 1 b 2]
   (+ a b))
```

This is fine but sometimes I like to be able to bind things to `nil`!
In fact the `?` macro have a way to do this

```clojure
(? [m {:a 1 :b 2}
    a (get m :a)
    ?c (get m :c)] ;; c is prefixed by ? meaning that is can be falsy
   (? c [:a+c (+ a c)]
      [:only :a a]))
```

In fact those prefixed symbols can be used also in destructuring patterns

```clojure
(? [{:keys [a ?c]} {:a 1 :b 2}]
   (? c [:a+c (+ a c)]
      [:only :a a]))
```

So we cover the whole `let` scope now

There is another thing that can be desirable in our programs.
It is to throw meaningful runtime errors, in clojure we sometimes have to chase `nil` in a complex execution.
Which is not always easy nor pleasant.

The `?` macro is letting you prefix bindings that can never fail with `!`

```clojure
'(? [!a (get {} :a)] :ok)
```

prints

```
strict binding failure:
a
(get {} :a)
```

_

```clojure
(let [f (fn [m]
          (? [!a (get m :a) ;; m has to have an :a key
              b (get m :b)] ;; then we try to find a :b key
             ;; if the :b key exists in m we return a and b
             [:a-and-b a b]
             ;; else we fail
             :fail))]

  (is (f {:a 1 :b 2})
      [:a-and-b 1 2])

  (is (f {:a 1})
      :fail)

  (throws (f {:c 3})))
```

Like the ‘?’ prefix the ‘!’ prefix can be used in destructurations

```clojure
(let [m {:a 1 :b 2}]
  (? [{:keys [!a b ?c]} m]
     (list a b c)
     :fail))
```

# Under the hood

There is several ways to compile the `?` macro.
I will try to expose the different techniques that I tried.

For trivial cases that maps directly to `if`, `when`, `if-let` or `when-let` we can just detect them and just replace the `?` by the corresponding clojure.core's macro.

```clojure
(= (macroexpand-1 
    '(? (pos? x) :ok))
   '(when (pos? x) :ok))

(= (macroexpand-1 
    '(? (pos? x) :ok :ko))
   '(if (pos? x) :ok :ko))

(= (macroexpand-1 
    '(? [xs (seq x)] (count x)))
   '(when-let [xs (seq x)] (count xs)))

(= (macroexpand-1 
    '(? [xs (seq x)] (count x) :not-seq))
   '(if-let [xs (seq x)] (count x) :not-seq))

(= (macroexpand-1 
    '(? (pos? x) :pos (neg? x) :neg :zero))
   '(cond (pos? x) :pos 
          (neg? x) :neg 
          :else :zero))
```

I will not insist on this and go straight to the interesting cases

## multiple bindings

As we've seen, `clojure.core/if-let` do not allow multiple bindings. The following form is not valid clojure code.

```clojure
(if-let [xs (seq x)
         x3 (nth 3 xs nil)] 
    {:seq xs :nth3 x3}
    :invalid)
```

The following is valid:

```clojure
(if-let [xs (seq x)]  
 (if-let [x3 (nth 3 xs nil)]  
   {:seq xs :nth3 x3}  
   :invalid)  
 :invalid)
```

It seems to be a possible expansion but the issue here is that it multiply some parts of the code (here `:invalid`). Here it does not seem to be a problem, but if instead of the simple keyword `:invalid` we have a large piece of code it can become an issue. Imagine that our binding form has more than 2 bindings...

this code:

```clojure
(? [xs (seq x)  
    x3 (nth 3 xs nil)  
    a3 (get x3 :a)\]  
   {:seq xs :a3 a3}  
  :invalid)
```

Will expand to:

```clojure
(if-let [xs (seq x)]  
 (if-let [x3 (nth 3 xs nil)]  
   (if-let [a3 (get x3 :a)]  
     {:seq xs :a3 a3}  
     :invalid)  
   :invalid)  
 :invalid)
```

It is clearly not an option...

One idea that could fix this issue would be to expand to:

```clojure
(or (when-let [xs (seq x)]  
     (when-let [x3 (nth 3 xs nil)]  
      (when-let [a3 (get x3 :a)]  
       {:seq xs :a3 a3})))  
    :invalid)
```

At first glance it seems ok, it removes code duplication but...

Take a look at this

```clojure
(? [xs (seq x)  
    x3 (nth 3 xs nil)]  
  (even? x3)  
  :not-a-seq)
```

which would expand to:

```clojure
(or (when-let [xs (seq x)]  
     (when-let [x3 (nth 3 xs nil)]  
      (even? x3)))  
    :not-a-seq)
```

which will return `:not-a-seq` if `x = (list 0 1 2 3)`...
Because by switching to `when-let` + `or` we have lost the ability to return a falsy value. One could consider this as a feature but this is clearly not what I'm aiming for.

Are we going to accept the duplication of code that the first solution yields ? In the example we've seen we could, but keep in mind that our intention is to support multiple branches (like `cond`) so the code duplication will grow really fast ! (imagine that for each `:invalid` we have to insert all the remaining cases expanded code, this is not an option at all!)

## Destructuration

As mentioned above, `if-let` and `when-let` are not specially helpful in conjonction of destructuration.

Our take is that each binding in a destructuring pattern have to be bound to a truthy value in order for the whole binding to succeed. To ensure so, we have to manually destructure it (the pattern) and check for the truthiness of each individal binding.

For those who are not familiar with `clojure.core/destructure`:

It takes a vector of 2 elements:
- the pattern
- the expression that we want to destructure

And returns a vector of bindings (the kind that `let` takes as first argument)

Here some examples:

```clojure

(destructure '[{:keys [a b]} x])
;; =>
'[map__5052 x 
  map__5052 (if (clojure.core/seq? map__5052) (clojure.lang.PersistentHashMap/create (clojure.core/seq map__5052)) map__5052) 
  a (clojure.core/get map__5052 :a) 
  b (clojure.core/get map__5052 :b)]


(destructure '[[x1 x2] x])
;=>  
'[vec__5057 x 
  x1 (clojure.core/nth vec__5057 0 nil) 
  x2 (clojure.core/nth vec__5057 1 nil)]


(destructure '[[x1 {:keys [a b]}] x])
;=>
'[vec__5064 x 
  x1 (clojure.core/nth vec__5064 0 nil) 
  map__5067 (clojure.core/nth vec__5064 1 nil) 
  map__5067 (if (clojure.core/seq? map__5067) (clojure.lang.PersistentHashMap/create (clojure.core/seq map__5067)) map__5067)
  a (clojure.core/get map__5067 :a)
  b (clojure.core/get map__5067 :b)]
```

When patterns are used in `if-let` or `when-let`'s bindings it check only the truthyness of the first bound symbol. That is not what we want.

But we can use the sequence of bindings that `clojure.core/destructure` returns and transform it into successive `if-let` or `when-let` steps.

For instance this form:

```clojure
(? [[x1 x2] x] 
  {:first x1 :second x2})
```

Could be transformed to:

```clojure
(when-let [vec__5057 x]
  (when-let [x1 (clojure.core/nth vec__5057 0 nil)]
    (when-let [x2 (clojure.core/nth vec__5057 1 nil)]
      {:first x1 :second x2})))
```

Let's now returns to our prior considerations

## Multiple branches

In order to illustrate the duplicated code issue mentioned above, we will take a look at the naïve `if-let` based expansion of the following code.

```clojure
(defn user [x]
  (? (string? x) (user {:full-name x})

     [{:keys [first-name last-name]} x]
     (assoc x :full-name (str first-name " " last-name))

     [n (get x :full-name)
      [first-name last-name] (clojure.string/split n #" ")]
     (assoc x :first-name first-name :last-name last-name)

     [:unvalid-user x]))
  
;; intended to be used like this
(= (user "Pierre Baille")
   (user {:full-name "Pierre Baille"})
   (user {:first-name "Pierre" :last-name "Baille"})
   {:first-name "Pierre", :last-name "Baille", :full-name "Pierre Baille"})


https://www.franceculture.fr/
```

Here it is...
```clojure
(defn user [x]
  (if (string? x)
    (user {:full-name x})
    (let [map__5043 x]
      (let [map__5043
            (if (seq? map__5043)
              (clojure.lang.PersistentHashMap/create
               (seq map__5043))
              map__5043)]
        (if-let [first-name (get map__5043 :first-name)]
          (if-let [last-name (get map__5043 :last-name)]
            (assoc x :full-name (str first-name " " last-name))
			;; A1
            (if-let [n (get x :full-name)]
              (let [vec__5045 (clojure.string/split n #" ")]
                (if-let [first-name (nth vec__5045 0 nil)]
                  (if-let [last-name (nth vec__5045 1 nil)]
                    (assoc x :first-name first-name :last-name last-name)
                    [:unvalid-user x])
                  [:unvalid-user x]))
              [:unvalid-user x]))
		  ;; A2
          (if-let [n (get x :full-name)]
            (let [vec__5045 (clojure.string/split n #" ")]
              (if-let [first-name (nth vec__5045 0 nil)]
                (if-let [last-name (nth vec__5045 1 nil)]
                  (assoc x :first-name first-name :last-name last-name)
                  [:unvalid-user x])
                [:unvalid-user x]))
            [:unvalid-user x]))))))
```

So yes, we definitively have to find something better. You can see that some big block of code are duplicated (`A1` and `A2`), not to mention that `[:unvalid-user x]` is repeated at least 6 times (it could be an arbitrary large expression in practice).

In addition to that, there is another thing to note in the previous expansion. It is that at some place in this code, some bindings are in scope where they should not.

Check at the expression immediatly following the `A1` mark in the previous expansion. You can notice that this code has access to the `first-name` binding (it is in scope). In most cases we do not care about this, but it can yield some really nasty debugging difficulties in my prior experiences.



## Lambda to the rescue

One solution to this problem would be to capture the the potentially duplicated code in lambdas. this way we duplicate only the calling of this lambda and not the code it contains.

So in our case, we need to define a lambda for each case. This way we can call the first lambda, that can call the second in case of failure, that will call the third in case of failure etc...

Each case depends on the following case, so we will have to define them starting form the last.

```clojure

(defn user [x]
  
  (let [case_3
        (fn [] [:unvalid-user x])
        
        case_2
        (fn []
          (if (string? x) (user {:full-name x}) (case_3)))
        
        case_1
        (fn []
          (let [map__5881 x]
            (let [map__5881
                  (if (seq? map__5881)
                    (clojure.lang.PersistentHashMap/create
                     (seq map__5881))
                    map__5881)]
              (if-let [first-name (get map__5881 :first-name)]
                (if-let [last-name (get map__5881 :last-name)]
                  (assoc x :full-name (str first-name " " last-name))
                  (case_2))
                (case_2)))))
        
        case_0
        (fn []
          (if-let [n (get x :full-name)]
            (let [vec__5882 (clojure.string/split n #" ")]
              (if-let [first-name (nth vec__5882 0 nil)]
                (if-let [last-name (nth vec__5882 1 nil)]
                  (assoc x :first-name first-name :last-name last-name)
                  (case_1))
                (case_1)))
            (case_1)))]
    
    (case_0)))
```

With this technique we get rid of all the problems mentioned above, code duplication, falsy return values, wrong scope.

# Further thinking







