(ns question-mark.stripped.core
  (:require cljs.core))

;; thunk : lambda of zero argument

(defn- thunk-symbols
  "generating symbols to hold case thunks"
  ([] (map #(gensym (str "case_" % "_")) (range))))

(defn- compile-case
  [{:as   case
    :keys [test bindings return next]}]
  (let [cont (when next (list next))]
    (cond
      test `(if ~test ~return ~cont)
      bindings (let [[b1 b2 & bs] bindings]
                 `(if-let [~b1 ~b2]
                    ~(compile-case (assoc case :bindings bs))
                    ~cont))
      :else return)))

(defn- cases->thunks
  [cases]
  (mapv (fn [case]
          (list (:symbol case) [] (compile-case case)))
        cases))

(defn- normalize-body
  [body]
  (if (odd? (count body))
    (concat (butlast body) [::bottom (last body)])
    (concat body [::bottom nil])))

(defn- body->cases
  [body destructure]
  (mapv (fn [[left right] [sym nxt]]
          (let [bindings? (vector? left)
                bottom? (= ::bottom left)]
            {:return   right
             :symbol   sym
             :next     (when-not bottom? nxt)
             :test     (if-not (or bindings? bottom?) left)
             :bindings (when bindings? (destructure left))}))
        (partition 2 (normalize-body body))
        (partition 2 1 (thunk-symbols))))

(defn- emit-form
  [body destructure]
  (let [thunks (-> (body->cases body destructure) cases->thunks)
        return (nth (first thunks) 2)]
    (if-let [bindings (some-> (next thunks) vec)]
      `(letfn ~bindings ~return)
      return)))

(defmacro ?
  [& body]
  (let [destructure
        (if (:ns &env)
          cljs.core/destructure
          clojure.core/destructure)]
    (emit-form body destructure)))

(comment :scratch

         (macroexpand-1 '(? (pos? 1) :ok))

         (macroexpand-1 '(? [a (get m :a)]
                            (+ a a)
                            :fail))

         (macroexpand-1 '(? (pos? x) [:pos x]
                            (neg? x) [:neg x]
                            :zero))

         (defmacro bench [x n]
           `(let [now# (System/currentTimeMillis)]
              (dotimes [_# ~n] ~x)
              (- (System/currentTimeMillis) now#)))

         (letfn [(f [x]
                   (? (pos? x) [:pos x]
                      (neg? x) [:neg x]
                      :zero))
                 (g [x]
                   (cond (pos? x) [:pos x]
                         (neg? x) [:neg x]
                         :else :zero))]

           [(bench (mapv g [-1 0 1]) 10000000)
            (bench (mapv f [-1 0 1]) 10000000)
            ]))