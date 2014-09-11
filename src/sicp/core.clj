(ns sicp.core)

(defn make-rat
  "Create a rational number"
  [n d] 
  (cons n (cons d '())))

(defn gcd
  [a b]
  (if (= b 0)
    a
    (gcd b (rem a b))))

(defn numer
  [x]
  (let
    [g (gcd (first x) (second x))]
    (/ (first x) g)))

(defn denom
  [x]
  (let
    [g (gcd (first x) (second x))]
    (/ (second x) g)))

(defn print-rat
  [x]
  (println (str "\n" (numer x) "/" (denom x))))

(defn add-rat
 [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat
  [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat
  [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat
  [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat?
  [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(def one-half (make-rat 5 10))
(def one-third (make-rat 1 3))

(print-rat one-half)
(print-rat (add-rat one-half one-third))

; What is meant by data?
;
(defn conss
  [x y]
  (fn [m] (m x y)))

(defn car
  [z]
  (z (fn [p q] p)))

(defn cdr
  [z]
  (z (fn [p q] q)))

(print-rat (make-rat (car (conss 1 2))
                     (cdr (conss 1 20))))

(defn zero
  [f]
  (fn [x] x))

(defn add-1
  [n]
  (fn [f] (fn [x] (f ((n f) x)))))


(defn one 
  [f]
  (fn [x] (f x)))

(defn two
  [f]
  (fn [x] (f (f x))))

(defn add-church
  [m n]
  (fn [f]
    (fn [x]
      ((m f) ((n f) x)))))

(println ((one inc) 0))
(println ((one inc) 0))
(println ((two inc) 3))
(println ((two inc) 7))

(def three (add-church one two))
(println ((three inc) 0))


(defn tree-map
  [proc tree]
  (map (fn [sub-tree]
         (if (list? sub-tree)
           (tree-map proc sub-tree)
           (proc sub-tree)))
       tree))

(defn square [n] (* n n))

(defn square-tree
  [tree]
  (tree-map square tree))


(defn accumulate
  [op initial sequen]
  (if (empty? sequen)
    initial
    (op (first sequen)
        (accumulate op initial (rest sequen)))))

(defn enumerate-interval
  [low high]
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))


(defn flatmap
  [proc se]
  (accumulate concat nil (map proc se)))

(defn prime?
  [n]
  (.isProbablePrime (BigInteger/valueOf n) 5))

(defn make-pair-sum
  [pair]
  (let [m (first pair)
        n (second pair)]
    (list m n (+ m n))))

(defn prime-sum?
  [pair]
  (prime? (+ (first pair) (second pair))))

(defn prime-sum-pairs
  [n]
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (fn [i]
                          (map (fn [j] (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

;(def empty-board (list))

;(defn safe?
  ;[k positions]
  ;(println k)
  ;(println positions)
  ;true)


;(defn queens
;  [board-size]
;  (letfn [(place-queen [rank file]
;            (list rank file))
;          (adjoin-position [rank file board]
;            (cons (place-queen rank file)
;                  board))
;          (queen-cols [k] 
;            (if (= k 0)
;              (list empty-board)
;              (filter (fn [positions] (safe? k positions))
;                      (flatmap (fn [rest-of-queens]
;                                 (map (fn [new-row]
;                                        (adjoin-position new-row k rest-of-queens))
;                                      (enumerate-interval 1 board-size)))
;                               (trampoline queen-cols (- k 1))))))]
;  (queen-cols board-size)))

; Symbolic Data


(defn variable?
  [x]
  (symbol? x))

(defn same-variable?
  [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn make-sum
  [a1 a2]
  (list '+ a1 a2))

(defn make-product
  [m1 m2]
  (list '* m1 m2))

(defn sum?
  [x]
  (and (list? x) (= (first x) '+)))

(defn product?
  [x]
  (and (list? x) (= (first x) '*)))

(defn addend
  [s]
  (first (rest s)))

(defn augend
  [s]
  (first (rest (rest s))))


(defn multiplier
  [p]
  (first (rest p)))

(defn multiplicand
  [p]
  (first (rest (rest p))))

(defn deriv
  [exp v]
  (cond 
    (number? exp) 0
    (variable? exp) (if (same-variable? exp v) 1 0)
    (sum? exp) (make-sum (deriv (addend exp) v)
                         (deriv (augend exp) v))
    (product? exp) (make-sum (make-product (multiplier exp)
                                           (deriv (multiplicand exp) v))
                             (make-product (deriv (multiplier exp) v)
                                           (multiplicand exp)))
    :else (println "unknow expression type -- DERIV" (str exp))))

(deriv '(+ x 3) 'x)

