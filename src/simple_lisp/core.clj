(ns simple-lisp.core
  (:refer-clojure :exclude [eval read read-string])
  (:require [clojure.edn :refer [read]])
  (:import (java.util HashMap)))

(defn seq-starts-with? [starts-with form]
  (and (seqable? form) (= (first form) starts-with)))

(def quote? (partial seq-starts-with? 'quote))

(def def? (partial seq-starts-with? 'def))

(def if? (partial seq-starts-with? 'if))

(def closure? (partial seq-starts-with? 'clo))

(def macro? (partial seq-starts-with? 'mac))

(def literal?
  (some-fn string? number? boolean? char? nil? fn? closure? macro?))

(declare eval eval-many)

(defn lookup-symbol [{:keys [globe scope]} sym]
  (let [v (some (fn [m] (when (contains? m sym) [(get m sym)]))
                [{'scope scope} globe scope])]
    (assert v (format "expected value for sym = %s" sym))
    (first v)))

(defn eval-def [env [_ k v]]
  (assert (symbol? k) (format "expected k = %s to be a symbol" k))
  (.put (:globe env) k (eval env v)))

(defn eval-if [env [_ test-form when-true when-false]]
  (let [evaled-test (eval env test-form)]
    (eval env (if evaled-test when-true when-false))))

(defn assign-vars [syms args]
  (assert (= (count syms) (count args))
          (format "syms and args must match syms: %s args: %s"
                  (vec syms) (vec args)))
  (into {} (map vector syms args)))

(defn eval-closure [env [_ scope syms body] args]
  (eval (assoc env :scope (merge scope (assign-vars syms args))) body))

(defn eval-macro [env [_ clo] args]
  (eval env
        (eval env
              (concat [clo] (map (fn [x] (list 'quote x)) args)))))

(defn eval-application [env [f & args]]
  (let [f-evaled (eval env f)]
    (cond
      (fn? f-evaled) (apply f-evaled (eval-many env args))
      (closure? f-evaled) (eval-closure env f-evaled (eval-many env args))
      (macro? f-evaled) (eval-macro env f-evaled args))))

(defn env [] {:globe (HashMap. {'+ + '= = 'list list 'map map 'concat concat
                                'first first 'second second 'not not})
              :scope {}})

(defn eval [env form]
  (cond
    (literal? form) form
    (quote? form) (second form)
    (symbol? form) (lookup-symbol env form)
    (def? form) (eval-def env form)
    (if? form) (eval-if env form)
    :else (eval-application env form)))

(defn eval-many [e forms] (map (partial eval e) forms))

(defn -main [& args]
  (println "Welcome to Simple Lisp!")
  (let [e (env)]
    (loop []
      (println "> ")
      (println (eval e (read)))
      (recur))))

(comment
  (eval (env) "foo")
  (eval (env) 1.2)
  (eval (env) '(clo nil (x) (+ 1 x)))
  (let [e (env)]
    (eval e '(def foo 'bar))
    (eval e 'foo))
  (eval (env) '(if true 'foo 'bar))
  (eval (env) '(+ 1 2))
  (eval (env) '((clo nil (x) (+ x 1)) 4))
  (eval (env) '((mac (clo nil (x) (list 'list nil x))) 1))
  (eval (env) '(map first '((a b))))
  (let [e (env)]
    (eval-many e
               ['(def defmacro
                   (mac (clo nil (n p e)
                             (list 'def n
                                   (list 'mac (list 'clo nil p e))))))
                '(defmacro fn (args body)
                   (list 'list ''clo 'scope
                         (list 'quote args)
                         (list 'quote body)))
                '(defmacro let (pairs body)
                   (concat (list (list 'fn (map first pairs) body))
                           (map second pairs)))
                '(((fn (x) (fn (y) (+ x y))) 1) 2)
                '((fn (z)
                    (let ((x 1) (y 2)) (+ x y z))) 3)])))

