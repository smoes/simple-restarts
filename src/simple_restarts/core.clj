(ns simple-restarts.core
  (:require [active.clojure.record :as rec]))


(def ^:dynamic *handlers* {})


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
  (= (:type e) :restart-invocation))

(defn find-restart [restarts name]
  (first (filter (fn [restart] (= (restart-name restart) name)) restarts)))


(defn restart-case-catch [e restarts]
  (if (restart-invocation-exception? e)
    (let [res     (:restart-invocation (ex-data e))
          name    (restart-invocation-restart-name res)
          params  (restart-invocation-params res)
          restart (find-restart restarts name)]
      (if restart
        (apply (restart-invocation-function restart) params)
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
  (let [condition-identifier (condition-identifier condition)
        handler (condition-handler condition-identifier)
        res (apply handler (condition-params condition))]
    (if (restart-invocation? res)
      (throw (ex-info "" {:type :restart-invocation
                          :restart-invocation res}))
      (throw (ex-info "No restart invocation returned" {:handler handler})))))



