#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2)
  (define (longest-common-prefix-aux w1 w2 prefix)
    (if (or (null? w1) (null? w2) (not (eq? (car w1) (car w2))))
        (list prefix w1 w2)
        (longest-common-prefix-aux (cdr w1) (cdr w2) (append prefix (list (car w1))))))
  (longest-common-prefix-aux w1 w2 '()))


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (if (collection-empty? (collection-rest words)) (collection-first words)    
      (let* ((first-two-prefix (longest-common-prefix (collection-first words) 
                                                      (collection-first (collection-rest words))))
             (new-prefix (car first-two-prefix)))  
        (if (collection-empty? (collection-rest(collection-rest words))) new-prefix
            (longest-common-prefix-of-collection (collection-cons new-prefix (collection-rest(collection-rest words))))))))


(define (match-pattern-with-label st pattern)
  (let* ((branch (get-ch-branch st (car pattern))))
    (if branch
        (let* ((label (get-branch-label branch))
               (subtree (get-branch-subtree branch))
               (prefix-comparison (longest-common-prefix-of-collection (list label pattern))))
          (if (equal? prefix-comparison label)
              (if (equal? label pattern) true
                  (list label (take-right pattern (- (length pattern) (length label) ) ) subtree))
              (if (equal? prefix-comparison pattern) true 
                  (list false prefix-comparison))))
        (list false '()))))

(define (st-has-pattern? st pattern)
  (let* ((result (match-pattern-with-label st pattern)))
    (if (equal? result true)
        #t
        (if (>= (length result) 3)
            (st-has-pattern? (caddr result) (cadr result)) 
            false)))) 

(define (get-suffixes text)
  (if(collection-empty? text)
  collection-stream-empty
  (collection-cons text (get-suffixes(collection-rest text)))))


(define (get-ch-words words ch)
 (collection-filter (lambda (word) 
            (and (not (collection-empty? word)) 
                 (char=? (car word) ch)))  
         words))


(define (ast-func suffixes)
  (let* ((label (list (collection-first (collection-first suffixes))))
         (new-suffixes (collection-map cdr suffixes)))
    (cons label new-suffixes)))


(define (cst-func suffixes)
  (let* ((prefix (longest-common-prefix-of-collection suffixes))
         (prefix-length (length prefix))
         (new-suffixes (collection-map (lambda (suf) (drop suf prefix-length)) suffixes)))
    (cons prefix new-suffixes)))

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (cond ((collection-empty? suffixes) '())
        (else
         (let* ((branches (collection-map 
                            (lambda (ch)
                              (let* ((suffixes-ch (get-ch-words suffixes ch)))
                                (if (not (collection-empty? suffixes-ch))
                                    (let* ((result (labeling-func suffixes-ch))
                                           (label (list (car result)))
                                           (rest (cdr result)))
                                      (append label (suffixes->st labeling-func rest alphabet)))
                                    '())))
                           alphabet)))
           (collection-filter (lambda (branch) (not (null? branch))) branches)))))



; nu uitați să convertiți alfabetul într-un flux
(define (text->st text) 
    (lambda (labeling-func)
      (suffixes->st labeling-func (get-suffixes (append text '(#\$)))(list->stream (sort (remove-duplicates (append text '(#\$))) char<?)
    ))))

(define text->ast
  (lambda (text)
    ((text->st text) ast-func)))

(define text->cst
  (lambda (text)
    ((text->st text) cst-func)))

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (let* ((ast (text->ast text)))
    (st-has-pattern? ast pattern)))

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (define (search-subtree subtree len curr-label)
    (let ((search (helper subtree len)))
      (if (eq? search #f)
          #f
          (append curr-label search))))
  
  (define (helper st len)
    (cond 
      ((st-empty? st) #f)
      ((not (st-empty? (get-branch-subtree (first-branch st))))
       (let* ((curr-branch (first-branch st))
              (curr-label (get-branch-label curr-branch))
              (subtree (get-branch-subtree curr-branch)))
         (cond
           ((>= (length curr-label) len) (take curr-label len))
           (else 
            (let ((search-result (search-subtree subtree (- len (length curr-label)) curr-label)))
              (if search-result
                  search-result
                  (helper (other-branches st) len)))))))
      (else (helper (other-branches st) len))))
  
  (helper (text->cst text) len))
