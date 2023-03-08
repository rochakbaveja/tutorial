#lang racket
(require data/heap)

; priority queue and helper functions

(define (comp n1 n2)
  (let ([d1 (cdr n1)]
        [d2 (cdr n2)])
    (<= d1 d2)))

(define queue (make-heap comp))

(define (enqueue n) (heap-add! queue n))

(define (dequeue)
  (let ([n (heap-min queue)])
    (heap-remove-min! queue)
    n))

(define (update-priority s p)
  (let ([q (for/first ([x (in-heap queue)] #:when (equal? s (car x))) x)])
    (heap-remove! queue q)
    (enqueue (cons s p))))

(define (peek-queue) (heap-min queue))

(define (queue->list) (for/list ([n (in-heap queue)]) n))
 
(define (in-queue? s)
  (for/or ([x (in-heap queue)]) (equal? (car x) s)))
            
; ------------------------------------------

(define parent (make-hash))
(define lengths (make-hash))
(define dist (make-hash))

(define (init-graph start-node edges)
  (let* ([INFINITY 9999]
         [swapped (map (lambda (e) (list (second e) (first e) (third e))) edges)] 
         [all-edges (append edges swapped)]
         [nodes (list->set (map (lambda (e) (first e)) all-edges))])
    (hash-clear! lengths)
    (for ([e all-edges]) (hash-set! lengths (cons (first e) (second e)) (third e)))
    (set! queue (make-heap comp))
    (hash-clear! parent)
    (hash-clear! dist)
    (for ([n nodes])
      (hash-set! parent n null)
      (hash-set! dist n INFINITY)
      (if (equal? n start-node)
          (enqueue (cons start-node 0))
          (enqueue (cons n INFINITY))))
    (hash-set! dist start-node 0)
    all-edges))

(define (dijkstra start-node edges)
  (let ([graph (init-graph start-node edges)])
    (define (neighbors n)
      (filter
       (lambda (e) (and (equal? n (first e)) (in-queue? (second e))))
       graph))
    (let loop ()
      (let* ([u (car (dequeue))])
        (for ([n (neighbors u)])
          (let* ([v (second n)]
                 [t (+ (hash-ref dist u) (hash-ref lengths (cons u v)))])
            (when (< t (hash-ref dist v))
              (hash-set! dist v t)
              (hash-set! parent v u)
              (update-priority v t)))))
      (when (> (heap-count queue) 0) (loop)))))

(define (get-path n)
  (define (loop n)
    (if (equal? null n)
        null
        (let ([p (hash-ref parent n)])
          (cons n (loop p)))))
  (reverse (loop n)))

(define (show-paths)
  (for ([n (hash-keys parent)])
    (printf "  ~a: ~a\n" n (get-path n))))

(define (solve start-node edges)
  (dijkstra start-node edges)
  (displayln "Shortest path listing:")
  (show-paths))

(define edge-list
  '((S a 12)
    (S b 8)
    (S c 6)
    (a b 1)
    (b c 9)
    (a e 8)
    (e d 5)
    (b d 10)
    (c d 13)))

(solve 'S edge-list)
