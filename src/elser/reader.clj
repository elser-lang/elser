(ns elser.reader
  (:gen-class)
  (:require [clojure.string :as string]
            [elser.errors :as errs]
            [elser.printer :as printer]))

(def tokens-regex
  #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:[\\].|[^\\\"])*\"?|;.*|[^\s\[\]{}()'\"`@,;]+)")

(def badstr-regex #"^\"")
(def str-regex #"^\"((?:[\\].|[^\\\"])*)\"$")
(def int-regex #"^-?[0-9]+$")
(def newline-regex #"(?:\n\s*)+")

(defrecord Token [char line])

;; Reader
(defn rdr [tokens ctx]
  {:tokens tokens :pos (atom 0) :ctx ctx})

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

(defn rback
  "Returns the token at the previous position"
  [rdr]
  (get
   (vec (rdr :tokens))
   (- @(:pos rdr) 1)))

(defn get-line-ctx
  "Return trimmed context string for line-number of raw-token."
  [rdr raw-token]
  (string/trim (get (:ctx rdr) (:line raw-token))))

(defn tokenize-with-metadata
  "Return a vector with a vector of tokens with line metadata, and context map."
  [in]
  (let [raw-tokens  (filter #(not= \; (first %))
                            (map first (re-seq tokens-regex in)))]
    (loop [line 1
           raw raw-tokens
           tokens '[]
           ctx '{}]
      
      (if (= (count raw) 0) [tokens ctx]

        (let [char (first raw)
              newlines (re-find newline-regex char)
              new-line (if newlines
                         (+ line (count (re-seq #"\n" newlines)))
                         line)]
          
          (recur           
           new-line
           
           (next raw)

           (conj tokens (Token. (string/trim char) new-line))

           (assoc ctx new-line
                  (str (get ctx new-line) char))
           )
          
          ))
      )
    ))

;; Basically, this functon is a lexer
(defn tokenize
  "Tokenizes an input string and returns
  a list of tokens. Executes lexical
  analysis step."
  [in]
  (let [[tokens ctx] (tokenize-with-metadata in)]
    (rdr tokens ctx)))

(defn unesc [s]
  (-> s (string/replace "\\\\" "\u029e")
      (string/replace "\\\"" "\"")
      (string/replace "\\n" "\n")
      (string/replace "\u029e" "\\")))

(defn read-atom [rdr src]
  (let [raw (rnext rdr)
        token (:char raw)]    
    (cond
      (re-seq int-regex token) (Integer/parseInt token)
      (re-seq str-regex token) (unesc (second (re-find str-regex token)))
      (re-seq badstr-regex token) (errs/unexpected-token token)
      (= token "nil") nil
      (= \: (get token 0)) (keyword (subs token 1))
      (= token "true") true
      (= token "false") false
      :else (symbol token))))

(declare read-form)

(defn read-list [rdr beg end src]
  (assert (= beg (:char (rnext rdr))))
  (loop [lst []]
    
    (let [raw (rpeek rdr)
          token (:char raw)]

      (cond
        (= token end) (do (rnext rdr) lst)
        
        (nil? token) (errs/err-throw
                      (printer/err-meta
                       (str "EOF before " "'" end "'")
                       (str beg " ... " end)
                       src
                       (:line (rback rdr))
                       ""))
        
        :else (recur (conj lst (read-form rdr src)))))))

(defn read-form
  "Produces AST on tokenized input.
  Executes syntactical analysis step."
  [rdr src]
  (let [raw (rpeek rdr)
        tkn (:char raw)]
    (cond
      (= tkn "'") (do (rnext rdr) (list 'quote (read-form rdr src)))
      (= tkn "`") (do (rnext rdr) (list 'quasiquote (read-form rdr src)))
      (= tkn "~") (do (rnext rdr) (list 'unquote (read-form rdr src)))
      
      ;; Permissions symbol => jump to the permissions map.
      (= tkn "@") (do (rnext rdr) (list (read-form rdr src)))
      (= tkn "~@") (do (rnext rdr) (list 'splice-unquote (read-form rdr src)))
      (= tkn "^") (do (rnext rdr) (let [meta (read-form rdr src)
                                        data (read-form rdr src)]
                                    (list 'with-meta data meta)))
      
      (= tkn ")") (errs/err-unbalanced tkn)
      (= tkn "(") (apply list (read-list rdr "(" ")" src))

      ;; Ban these brackets.
      (or (= tkn "]")
          (= tkn "[")
          (= tkn "}")
          (= tkn "{")) (errs/err-throw
                        (printer/err-meta (errs/unexpected-token tkn)
                                          (printer/highlight-char-in-ctx
                                           (get-line-ctx rdr raw) tkn)
                                          src (:line raw) ""))
      
      :else (read-atom rdr src))))

(defn read-str [in src] (read-form (tokenize in) src))
