(ns evmlisp.reader
  (:gen-class)
  (:require [clojure.string :as string]
            [evmlisp.errors :as errs]))

(def TOKENS-REGEX
  #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:[\\].|[^\\\"])*\"?|;.*|[^\s\[\]{}()'\"`@,;]+)")

;----------------- TYPES -----------------

(def BADSTR-REGEX #"^\"")
(def STR-TYPE #"^\"((?:[\\].|[^\\\"])*)\"$")
(def INT-TYPE #"^-?[0-9]+$")

;; Reader
(defn rdr [tokens]
  {:tokens tokens :pos (atom 0)})

(defn rnext
  "Returns a token in the current position
  and increments a position."
  [rdr]
  (get
   (vec (rdr :tokens))
   (dec (swap! (:pos rdr) inc))))

(defn rpeek
  "Returns the token at the current position"
  [rdr]
  (get
   (vec (rdr :tokens))
   @(:pos rdr)))

;; Basically, this functon is a lexer
(defn tokenize
  "Tokenizes an input string and returns
  a list of tokens. Executes lexical
  analysis step."
  [in]
  (rdr
   (filter (fn [x] (not= \; (first x)))
           (map string/trim
                (map second (re-seq TOKENS-REGEX in))))))

(defn unesc [s]
  (-> s (string/replace "\\\\" "\u029e")
        (string/replace "\\\"" "\"")
        (string/replace "\\n" "\n")
        (string/replace "\u029e" "\\")))

(defn read-atom [rdr]
  (let [token (rnext rdr)]
    (cond
      (re-seq INT-TYPE token) (Integer/parseInt token)
      (re-seq STR-TYPE token) (unesc (second (re-find STR-TYPE token)))
      (re-seq BADSTR-REGEX token) (errs/err-unexpected-tkn token)
      (= token "nil") nil
      (= \: (get token 0)) (keyword (subs token 1))
      (= token "true") true
      (= token "false") false
      :else (symbol token))))

(declare read-form)

(defn read-list [rdr beg end]
  (assert (= beg (rnext rdr)))
  (loop [lst []]
    (let [token (rpeek rdr)]
      (cond
        (= token end) (do (rnext rdr) lst)
        (nil? token) (errs/err-eof-before-paren)
        :else (recur (conj lst (read-form rdr)))))))

(defn read-form
  "Produces AST on tokenized input.
  Executes syntactical analysis step."
  [rdr]
  (let [tkn (rpeek rdr)]
    (cond
      (= tkn "'") (do (rnext rdr) (list 'quote (read-form rdr)))
      (= tkn "`") (do (rnext rdr) (list 'quasiquote (read-form rdr)))
      (= tkn "~") (do (rnext rdr) (list 'unquote (read-form rdr)))
      (= tkn "@") (do (rnext rdr) (list 'deref (read-form rdr)))
      (= tkn "~@") (do (rnext rdr) (list 'splice-unquote (read-form rdr)))
      (= tkn "^") (do (rnext rdr) (let [meta (read-form rdr)
                                       data (read-form rdr)]
                                   (list 'with-meta data meta)))
      (= tkn ")") (errs/err-unbalanced "'( )'")
      (= tkn "(") (apply list (read-list rdr "(" ")"))
      (= tkn "]") (errs/err-unbalanced "'[ ]'")
      (= tkn "[") (vec (read-list rdr "[" "]"))
      (= tkn "}") (errs/err-unbalanced "'{ }'")
      (= tkn "{") (apply hash-map (read-list rdr "{" "}"))
      :else (read-atom rdr))))

(defn read-str [in]  
  (read-form
   (tokenize in)))
