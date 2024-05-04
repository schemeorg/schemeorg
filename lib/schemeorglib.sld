(define-library (schemeorglib)
  (export
   all-projects
   append-map
   circular-generator
   echo
   filter
   filter-map
   find
   first
   flatten-all
   fold
   generator->list
   get-list
   get-string
   get-string?
   get-symbol-boolean
   get-symbol-boolean?
   list-sort
   pretty-print
   project-groups
   project-group-title
   project-group-projects
   projects-data
   script-directory
   second
   sort-unique
   string-join
   take
   third)
  (import (scheme base)
          (scheme file)
          (scheme read)
          (scheme write)
          (only (srfi 1)
                append-map filter filter-map find first fold
                second take third)
          (only (srfi 193) script-directory))
  (cond-expand
   ((library (srfi 166))
    (import (only (srfi 166) pretty show)))
   (else))
  (cond-expand
   ((library (srfi 166))
    (begin
      (define (pretty-print x)
        (show #t (pretty x)))))
   (else
    (begin
      (define (pretty-print x)
        (write x)
        (newline)))))
  (begin

    (define (circular-generator . elements)  ; Specified in SRFI 158.
      (let ((xs elements))
        (lambda ()
          (let ((x (car xs)))
            (set! xs (if (null? (cdr xs)) elements (cdr xs)))
            x))))

    (define (generator->list g)  ; Subset of the behavior in SRFI 158.
      (let loop ((xs '()))
        (let ((x (g)))
          (if (eof-object? x) (reverse xs) (loop (cons x xs))))))

    (define (flatten-all x)
      (if (list? x)
          (append-map flatten-all x)
          (list x)))

    (define (list-sort < xs)  ; SRFI 132
      (define (insert x xs)
        (if (null? xs) (list x)
            (if (< x (car xs))
                (cons x xs)
                (cons (car xs)
                      (insert x (cdr xs))))))
      (let loop ((xs xs))
        (if (null? xs)
            '()
            (insert (car xs)
                    (loop (cdr xs))))))

    (define (list-delete-neighbor-dups = lis)  ; SRFI 132
      (let loop ((lis lis) (new '()))
        (if (null? lis)
            (reverse new)
            (loop (cdr lis)
                  (if (or (null? new) (not (= (car new) (car lis))))
                      (cons (car lis) new)
                      new)))))

    (define (sort-unique strings)
      (list-delete-neighbor-dups string=? (list-sort string<? strings)))

    ;;

    (define (string-join strings delimiter)
      (if (null? strings)
          ""
          (fold (lambda (s so-far) (string-append so-far delimiter s))
                (car strings)
                (cdr strings))))

    ;;

    (define (echo . xs)
      (for-each display xs)
      (newline))

    ;;

    (define (get-list key alist)
      (let ((entry (assoc key alist)))
        (if entry (cdr entry) '())))

    (define (get-value? key alist valid? not-found)
      (let ((entry (assoc key alist)))
        (cond ((not entry)
               (not-found))
              ((and (= 2 (length entry))
                    (valid? (second entry)))
               (second entry))
              (else
               (error "Not valid" entry)))))

    (define (get-value key alist valid?)
      (get-value? key alist valid? (lambda () (error "Not found" key alist))))

    (define (get-string? key alist)
      (get-value? key alist string? (lambda () #f)))

    (define (get-string key alist)
      (get-value key alist string?))

    (define (boolean-symbol? object)
      (or (eqv? object 'true)
          (eqv? object 'false)))

    (define (get-symbol-boolean? key alist)
      (eqv? 'true (get-value? key alist boolean-symbol? (lambda () 'false))))

    (define (get-symbol-boolean key alist)
      (eqv? 'true (get-value key alist boolean-symbol?)))

    ;;

    (define (projects-file)
      (string-append (script-directory) "../projects.pose"))

    (define (projects-data)
      (call-with-input-file (projects-file) read))

    (define (project-groups)
      (get-list 'projects (projects-data)))

    (define project-group-title car)
    (define project-group-projects cdr)

    (define (all-projects)
      (append-map project-group-projects (project-groups)))))
