(ns simple-restarts.core
  (:require [active.clojure.record :as rec]))


(def ^:dynamic *handlers* {})


(rec/define-record-type Restart
  (make-restart n foo) restart?
  [n restart-name
   foo restart-foo])


(rec/define-record-type RestartInvokation
  (make-restart-invokation n params) restart-invokation?
  [n restart-invokation-name
   params restart-invokation-params])


(rec/define-record-type Condition
  (make-condition n params) condition?
  [n condition-name
   params condition-params])


(defmacro defcondition [name params]
  `(defn ~name ~params
     (make-condition ~name ~params))) ;; FIXME: Get namespace qualified symbol for name




(defn restart [name fun]
  (make-restart name fun))


(defmacro handler-bind [bindings & body]
  (let [bindings-2 (mapv vec (partition 2 bindings))]
    `(binding [*handlers* (reduce
                            (fn [acc# [sym# foo#]]
                              (assoc acc# sym# foo#))
                            *handlers*
                            ~bindings-2)]
      ~@body)))



(defn restart-invokation-exception? [e]
  (let [data (ex-data e)
        ?restart-invokation (:restart-invokation data)]
    (restart-invokation? ?restart-invokation)))

(defn find-restart [restarts name]
  (first (filter (fn [restart] (= (restart-name restart) name)) restarts)))


(defn restart-case-catch [e restarts]
  (if (restart-invokation-exception? e)
    (let [res     (:restart-invokation (ex-data e))
          name    (restart-invokation-name res)
          params  (restart-invokation-params res)
          restart (find-restart restarts name)]
      (if restart
        (apply (restart-foo restart) params)
        (throw e)))
    (throw e)))



(defn condition-handler [name]
  (if-let [handler (get *handlers* name)]
    handler
    (throw (ex-info "No handler found" {:condition name}))))


(defmacro restart-case [body & restarts]
  `(try
    ~body
    (catch Exception e#
      (restart-case-catch e# ~(vec restarts)))))


(defn invoke-restart [sym & params]
  (make-restart-invokation sym params))


(defn fire-condition [condition]
  (let [condition-name (condition-name condition)
        handler (condition-handler condition-name)
        res (apply handler (condition-params condition))]
    (if (restart-invokation? res)
      (throw (ex-info "" {:restart-invokation res}))
      (throw (ex-info "No restart invokation returned" {:handler handler})))))



