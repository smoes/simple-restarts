(ns simple-restarts.core
  (:require [active.clojure.record :as rec]))


(def ^:dynamic *handlers* {})

(def exception-identifier ::restart-invocation)


(rec/define-record-type Restart
  (restart n invocation-function) restart?
  [n restart-name
   invocation-function restart-invocation-function])


(rec/define-record-type RestartInvocation
  (make-restart-invocation n params) restart-invocation?
  [n restart-invocation-restart-name
   params restart-invocation-params])


(defn invoke-restart [sym & params]
  (make-restart-invocation sym params))


(rec/define-record-type Condition
  (make-condition identifier params) condition?
  [identifier condition-identifier
   params condition-params])


(defmacro defcondition [identifier params]
  `(defn ~identifier ~params
     (make-condition ~identifier ~params)))


(defmacro handler-bind [bindings & body]
  (assert (even? (count bindings)))
  `(binding [*handlers* (apply assoc *handlers* ~bindings)]
     ~@body))



(defn restart-invocation-exception? [e]
  (= (:type (ex-data e)) exception-identifier))

(defn find-restart [restarts name]
  (first (filter (fn [restart] (= (restart-name restart) name)) restarts)))


(defn restart-case-catch [e available-restarts]
  (if (restart-invocation-exception? e)
    (let [handler-result   (:handler-result (ex-data e))
          restart-name     (restart-invocation-restart-name handler-result)
          restart-params   (restart-invocation-params handler-result)]

      (if-let [restart (find-restart available-restarts restart-name)]
        (apply (restart-invocation-function restart) restart-params)
        (throw e)))
    (throw e)))


(defmacro restart-case [body & restarts]
  `(try
    ~body
    (catch Exception e#
      (restart-case-catch e# ~(vec restarts)))))


(defn condition-handler [name]
  (if-let [handler (get *handlers* name)]
    handler
    (throw (ex-info "No handler found" {:condition name}))))



(defn fire-condition [condition]
  (let [handler        (-> condition condition-identifier condition-handler)
        params         (condition-params condition)
        handler-result (apply handler params)]
    (if (restart-invocation? handler-result)
      (throw (ex-info "" {:type           exception-identifier
                          :handler-result handler-result}))
      (throw (ex-info "No restart invocation returned" {:handler handler})))))



