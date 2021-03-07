
(ns question-mark.core
  (:refer-clojure :exclude [compile]))

(do :impl

    (do :errors

        (defn errors_strict-binding
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

        (defn binding-symbol_generated? [x]
          (re-matches #"^((vec)|(seq)|(first)|(map))__[0-9]+$"
                      (name x)))

        (defn binding-symbol_pure-prefix? [x]
          (contains? #{"!" "?"} (name x)))

        (defn binding-symbol_mode [x]
          (case (subs (name x) 0 1)
            "?" :optional
            "!" :strict
            nil))

        (defn binding-symbol_unprefixed? [x]
          (and (symbol? x)
               (not (binding-symbol_mode x))))

        (defn binding-symbol_unprefixed [x]
          (cond
            (binding-symbol_pure-prefix? x) nil
            (binding-symbol_mode x) (symbol (subs (name x) 1))
            :else x))

        (defn binding-symbol_ignored? [x]
          (or (binding-symbol_pure-prefix? x)
              (-> (binding-symbol_unprefixed x)
                  name first (= \_))
              nil))

        (defn binding-symbol_parse [x]
          {:mode      (binding-symbol_mode x)
           :name      (binding-symbol_unprefixed x)
           :ignored   (binding-symbol_ignored? x)
           :generated (binding-symbol_generated? x)}))

    (do :compilation

        (defn expand-case

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

        (defn bindings_normalize-map-pattern
          [{:as pattern :keys [keys]}]
          (if keys
            (->> keys
                 (map #(let [s (binding-symbol_unprefixed %)] [% (keyword (name s))]))
                 (into (dissoc pattern :keys)))
            pattern))

        (defn bindings_normalize
          "in the map destructuring pattern, prefixed symbols are allowed,
           if those occurs in the :keys vector we have to preprocess it"
          [bindings]
          (->> (partition 2 bindings)
               (mapcat (fn [[pattern expression]]
                         [(if (map? pattern)
                            (bindings_normalize-map-pattern pattern)
                            pattern) expression]))
               (into [])))

        (defn compile-case
          [{:as opts :keys [bindings expr]}]
          (let [[bindings expr]
                (if (vector? bindings)
                  [(destructure (bindings_normalize bindings)) expr]
                  [[(gensym "_check-") bindings] expr])]
            (expand-case (assoc opts :bindings bindings
                                     :expr expr))))

        (defn compile-bindings
          [{:as opts :keys [cases symbols]}]
          (mapcat (fn [[sym next-sym] [bindings expr]]
                    (let [opts (assoc opts :bindings bindings :expr expr :fail next-sym)]
                      [sym `(fn [] ~(compile-case opts))]))
                  (reverse (partition 2 1 symbols))
                  cases))

        (defn cases-symbols
          ([] (map #(gensym (str "case_" % "_")) (range)))
          ([n] (take n (cases-symbols))))

        (defn parse [form]
          (let [[cases default]
                (if (even? (count form))
                  [form nil]
                  [(butlast form) (last form)])]
            {:cases   (partition 2 cases)
             :default default}))

        (defn compile
          [form cljs?]
          (let [{:as parsed :keys [cases default]} (parse form)
                symbols (cases-symbols (inc (count cases)))
                bindings (compile-bindings (assoc parsed :symbols symbols :cljs cljs?))
                bindings (if default
                           (list* (last symbols) `(fn [] ~default) bindings)
                           bindings)]

            `(let ~(vec bindings)
               ~(list (-> bindings butlast last)))))

        (defn simple-form
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
      "the main control flow macro
       kind of a mix of 'if 'if-let 'cond..."
      [& bod]
      (or (simple-form bod)
          (compile bod (boolean (:ns &env))))))



(comment :blog-example

         (? [!a x] (+ a a))

         (defn user [x]
           (? (string? x) (user {:full-name x})

              [{:keys [first-name last-name]} x]
              (assoc x :full-name (str first-name " " last-name))

              [n (get x :full-name)
               [first-name last-name] (clojure.string/split n #" ")]
              (assoc x :first-name first-name :last-name last-name)

              [:unvalid-user x]))

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

         (= (user "Pierre Baille")
            (user {:full-name "Pierre Baille"})
            (user {:first-name "Pierre" :last-name "Baille"})
            {:first-name "Pierre", :last-name "Baille", :full-name "Pierre Baille"}))


