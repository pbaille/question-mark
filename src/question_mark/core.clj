(ns question-mark.core
  (:refer-clojure :exclude [compile])
  (:require cljs.core))

(do :impl

    (do :errors

        (defn- errors_strict-binding
          "in strict mode, runtime expression are throwm when we bind a symbol to nil"
          [name code cljs]
          `(throw (new ~(if cljs 'js/Error 'java.lang.Exception)
                       (str "strict binding failure:\n" '~name "\n" '~code)))))

    (do :binding-symbol

        ;; in this section we will take care of handling binding symbols
        ;; within the `?` macro, binding symbols can be prefixed by 3 different characters:
        ;; - ? (:optional mode) meaning that the symbol is allowed to be bound to nil
        ;; - ! (:strict mode) meaning that the we need a runtime error if the symbol binds to nil
        ;; - _ (:ignored mode) meaning that we don't care about the symbol's bound value

        (defn- binding-symbol_generated? [x]
          (re-matches #"^((vec)|(seq)|(first)|(map))__[0-9]+$"
                      (name x)))

        (defn- binding-symbol_pure-prefix? [x]
          (contains? #{"!" "?"} (name x)))

        (defn- binding-symbol_mode [x]
          (case (subs (name x) 0 1)
            "?" :optional
            "!" :strict
            nil))

        (defn- binding-symbol_unprefixed? [x]
          (and (symbol? x)
               (not (binding-symbol_mode x))))

        (defn- binding-symbol_unprefixed [x]
          (cond
            (binding-symbol_pure-prefix? x) nil
            (binding-symbol_mode x) (symbol (subs (name x) 1))
            :else x))

        (defn- binding-symbol_ignored? [x]
          (or (binding-symbol_pure-prefix? x)
              (-> (binding-symbol_unprefixed x)
                  name first (= \_))
              nil))

        (defn- binding-symbol_parse [x]
          {:mode      (binding-symbol_mode x)
           :name      (binding-symbol_unprefixed x)
           :ignored   (binding-symbol_ignored? x)
           :generated (binding-symbol_generated? x)}))

    (do :compilation

        (defn- expand-case

          [{:as   opts
            :keys [bindings expr fail cljs]}]

          (let [[b1 b2 & bs] bindings
                {:keys [generated ignored mode name]} (binding-symbol_parse b1)
                then (if (not bs) expr (expand-case (assoc opts :bindings bs)))
                error (errors_strict-binding name b2 cljs)
                mode (or mode :short)]

            (cond generated
                  `(let [~name ~b2] ~then)

                  ignored
                  (case mode
                    :optional `(do ~b2 ~then)
                    :strict `(if ~b2 ~then ~error)
                    :short `(if ~b2 ~then (~fail)))

                  :else
                  (case mode
                    :optional `(let [~name ~b2] ~then)
                    :strict `(if-let [~name ~b2] ~then ~error)
                    :short `(if-let [~name ~b2] ~then (~fail))))))

        (defn- bindings_normalize-map-pattern
          [{:as pattern :keys [keys]}]
          (if keys
            (->> keys
                 (map #(let [s (binding-symbol_unprefixed %)] [% (keyword (name s))]))
                 (into (dissoc pattern :keys)))
            pattern))

        (defn- bindings_normalize
          "in the map destructuring pattern, prefixed symbols are allowed,
           if those occurs in the :keys vector we have to preprocess it"
          [bindings]
          (->> (partition 2 bindings)
               (mapcat (fn [[pattern expression]]
                         [(if (map? pattern)
                            (bindings_normalize-map-pattern pattern)
                            pattern) expression]))
               (into [])))

        (defn- compile-case
          [{:as opts :keys [bindings expr cljs]}]
          (let [[bindings expr]
                (if (vector? bindings)
                  [((if cljs cljs.core/destructure clojure.core/destructure) (bindings_normalize bindings)) expr]
                  [[(gensym "_check-") bindings] expr])]
            (expand-case (assoc opts :bindings bindings
                                     :expr expr))))

        (defn- compile-bindings
          [{:as opts :keys [cases symbols default]}]
          (->> (mapcat (fn [[sym next-sym] [bindings expr]]
                         (let [opts (assoc opts :bindings bindings :expr expr :fail next-sym)]
                           [sym `(fn [] ~(compile-case opts))]))
                       (reverse (partition 2 1 symbols))
                       cases)
               (into [(last symbols) `(fn [] ~default)])))

        (defn- cases-symbols
          ([] (map #(gensym (str "case_" % "_")) (range)))
          ([n] (take n (cases-symbols))))

        (defn- parse [form]
          (let [[cases default]
                (if (even? (count form))
                  [form nil]
                  [(butlast form) (last form)])]
            {:cases   (partition 2 cases)
             :default default}))

        (defn- compile
          [form cljs?]
          (let [{:as parsed :keys [cases default]} (parse form)
                symbols (cases-symbols (inc (count cases)))
                bindings (compile-bindings (assoc parsed :symbols symbols :cljs cljs?))
                return (list (-> bindings butlast last))]

            `(let ~bindings ~return)))

        (defn- simple-form
          "in simple cases, where we can map directly to regular clojure.core's macros,
          we want don't want to do unescessary things,
          this function emit a normal `if`, `when`, `if-let`, `when-let` or `cond` form if possible."
          [[test then else & others :as body]]
          (let [seq-or-sym?
                #(or (seq? %)
                     (binding-symbol_unprefixed? %))]

            (if others

              ;; simple cond body ?
              (and (every? seq-or-sym?
                           (map first (partition 2 body)))
                   (if (even? (count body))
                     `(cond ~@body)
                     `(cond ~@(butlast body) :else ~(last body))))

              ;; if , when , if-let or when-let ?
              (when-let [binding-form
                         (cond (seq-or-sym? test) (if else `if `when)
                               (and (vector? test)
                                    (= 2 (count test))
                                    (binding-symbol_unprefixed? (first test))) (if else `if-let `when-let))]
                `(~binding-form ~@body)))))))

(do :api

    (defmacro ?
"
With two or three arguments `?` behaves like `if`

```clojure
(is :ok (? (pos? 1) :ok))
(isnt (? (pos? 0) :ok))

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
    (println \"mix-test has failed\")))
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
"

      [& bod]
      (or (simple-form bod)
          (compile bod (boolean (:ns &env))))))


