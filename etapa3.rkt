#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
  (let* ((cst (text->cst text)))
    (st-has-pattern? cst pattern)))



; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).
(define (longest-common-substring text1 text2)
  (let* ([cst (text->cst text1)]
         [suffixes2 (get-suffixes text2)])
    (let iter ((suffixes suffixes2) (lcs '()))
      (if (null? suffixes)
          lcs
          (let* ((curr-suffix (car suffixes))
                 (new-lcs (longer-string lcs (search-in-cst cst curr-suffix))))
            (iter (cdr suffixes) new-lcs))))))

(define (search-in-cst curr-st curr-suf)
  (let ([result (match-pattern-with-label curr-st curr-suf)])
    (cond
      ([equal? result #t] curr-suf)
      ([equal? (car result) #f] (cadr result))
      (else (append (car result) (search-in-cst (caddr result) (cadr result)))))))

(define (longer-string string1 string2)
  (if (< (length string1) (length string2))
      string2
      string1))

; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.
(define (repeated-substring-of-given-length text len)
  (define (search-subtree subtree len curr-label)
    (let ((search (helper subtree len)))
      (if (eq? search #f)
          #f
          (append curr-label search))))
  
  (define (helper st len)
    (cond 
      ((st-empty? st) #f)
      ((> (length (first-branch st)) 1)
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

  




