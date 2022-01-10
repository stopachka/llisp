(ns simple-lisp.core
  (:refer-clojure :exclude [eval])
  (:require [clojure.edn :as edn])
  (:import (java.util HashMap)))

;; ------
;; Model

(defn seq-starts-with? [starts-with form]
  (and (seqable? form) (= (first form) starts-with)))

(def lit? (partial seq-starts-with? 'lit))

(def quote? (partial seq-starts-with? 'quote))

(def def? (partial seq-starts-with? 'def))

(def if? (partial seq-starts-with? 'if))

(defn closure? [form] (and (lit? form) (= (second form) 'clo)))
(defn macro? [form] (and (lit? form) (= (second form) 'mac)))

(defn literal? [form]
  ((some-fn string? number? boolean? char? lit? fn? nil?) form))

(declare eval)

;; --------------
;; lookup-symbol

(defn lookup-symbol [{:keys [globe scope]} sym]
  (let [v (some #(get % sym) [globe scope])]
    (assert v (format "expected value for sym = %s" sym))
    v))

;; ---------
;; eval-def

(defn eval-def [env [_ k v]]
  (let [evaled-v (eval env v)]
    (.put (:globe env) k evaled-v)))

;; ------
;; eval-if

(defn eval-if [env [_ test-form when-true when-false]]
  (let [evaled-v (eval env test-form)]
    (if evaled-v
      (eval env when-true)
      (eval env when-false))))

;; -------------
;; eval-closure

(defn assign-vars [scope syms args]
  (merge scope (into {} (map vector syms args))))

(defn eval-closure [env clo args]
  (let [[_ _ scope syms body] clo
        _ (assert (= (count syms) (count args))
                  (format "clo %s got too many arguments: %s" clo args))
        new-scope (assign-vars scope syms args)]
    (eval (assoc env :scope new-scope)
          body)))

;; ------
;; eval-macro

(defn eval-macro [env mac args]
  (let [[_ _ clo] mac
        transformed-args (eval env (concat [clo] args))]
    (eval env transformed-args)))

;; ------
;; eval-application

(defn eval-application [env form]
  (let [[f & args] form
        f-evaled (eval env f)]
    (cond
      (fn? f-evaled)
      (apply f-evaled (map (partial eval env) args))
      (closure? f-evaled)
      (eval-closure env f-evaled (map (partial eval env) args))
      (macro? f-evaled)
      (eval-macro env f-evaled args))))

;; ------
;; Env

(defn env []
  {:globe (HashMap. {'+ + 'list list})
   :scope {}})

;; ------
;; Eval

(defn eval [env form]
  (cond
    (literal? form) form
    (quote? form) (second form)
    (symbol? form) (lookup-symbol env form)
    (def? form) (eval-def env form)
    (if? form) (eval-if env form)
    :else (eval-application env form)))

(comment
  (eval (env) "foo")
  (eval (env) 1.2)
  (eval (env) '(lit clo nil (x) (+ 1 x)))
  (let [e (env)]
    (eval e (quote (def foo 'bar)))
    e)
  (eval (env) '(if true 'foo 'bar))
  (eval (env) '(+ 1 2))
  (eval (env) '((lit clo nil (x) (+ x 1)) 2))
  (eval (env) '((lit mac (lit clo nil (x) (list 'list nil x))) 1)))

;; ------
;; Reader

(def parse edn/read-string)

(comment
  (parse "(defn foo [] (+ 1 2))"))

