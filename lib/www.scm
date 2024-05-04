;; Write HTML files into the `www` subdirectory.
;;
;; Based on the S-expressions in projects.pose, the HTML fragments in
;; the doc subdirectory, and the SXML templates in this file.
;;
;; You need Chicken 5 and
;; `chicken-install http-client openssl r7rs srfi-19 ssax sxpath`.

(cond-expand (chicken (include "schemeorglib.sld")))

(import (scheme base)
        (scheme file)
        (scheme read)
        (scheme write)

        (only (srfi 19) date->string string->date)

        (only (chicken file) create-directory)
        (only (http-client) call-with-input-request)
        (only (ssax) ssax:xml->sxml)
        (only (sxml-transforms) SXML->HTML)
        (only (sxpath-lolevel) sxml:content)

        (schemeorglib))

(define (superscripts s)
  (let ((n (string-length s)))
    (let loop ((a 0) (b 0) (acc '()))
      (cond ((= n b a)
             (reverse acc))
            ((= n b)
             (loop b b (cons (substring s a b) acc)))
            ((char=? #\^ (string-ref s b))
             (loop (+ b 2) (+ b 2)
                   (append `((sup ,(string (string-ref s (+ b 1))))
                             ,(substring s a b))
                           acc)))
            (else (loop a (+ b 1) acc))))))

(define (string->html-id s)  ; "Foo BAR Baz!" -> "foo-bar-baz"
  (let loop ((chars '()) (i 0) (dash? #f))
    (if (= i (string-length s)) (list->string (reverse chars))
        (let ((c (string-ref s i)))
          (if (or (char-alphabetic? c) (char-numeric? c))
              (loop (cons (char-downcase c) (if dash? (cons #\- chars) chars))
                    (+ i 1) #f)
              (loop chars
                    (+ i 1) (not (null? chars))))))))

(define-record-type feed-item
  (make-feed-item date-object description title uri)
  feed-item?
  (date-object feed-item-date-object)
  (description feed-item-description)
  (title feed-item-title)
  (uri feed-item-uri))

(define (feed-item-iso-date item)
  "Convert into \"YYYY-MM-DD\" format."
  (date->string (feed-item-date-object item) "~Y-~m-~d"))

(define (feed-item-friendly-date item)
  (date->string (feed-item-date-object item) "~b ~e"))

(define (matching-subtree? name tree)
  (and (pair? tree)
       (eq? (car tree) name)))

(define ((matches? name) tree) (and (matching-subtree? name tree) tree))

(define (find-one name tree)
  (cond ((find (matches? name) tree) => sxml:content) (else #f)))

(define (find-many name tree) (filter-map (matches? name) (cdr tree)))

(define (skip-attributes tree)
  (cond ((and (pair? (car tree))
              (eq? '@ (caar tree)))
         (cdr tree))
        (else tree)))

(define (parse-rss-date string)
  "For example, \"Sun, 29 Nov 2020 12:34:56 -0800\"."
  (string->date string "~a, ~d ~b ~Y ~H:~M:~S ~z"))

(define (all-strings-in sxml)
  (let outer ((result "") (sxml sxml))
    (cond ((string? sxml) (string-append result sxml))
          ((not (pair? sxml)) result)
          ((symbol? (car sxml))
           (let inner ((result result) (sxml (skip-attributes (cdr sxml))))
             (cond ((null? sxml)
                    result)
                   ((string? (car sxml))
                    (inner (string-append result (car sxml))
                           (cdr sxml)))
                   (else
                    (inner (outer result (car sxml))
                           (cdr sxml))))))
          (else
           (outer (outer result (car sxml))
                  (cdr sxml))))))

(define (rss port)
  (let ((sxml (ssax:xml->sxml port '())))
    (map (lambda (i)
           (make-feed-item
            (parse-rss-date (car (find-one 'pubDate i)))
            (all-strings-in (find-one 'description i))
            (all-strings-in (find-one 'title i))
            (car (find-one 'link i))))
         (find-many 'item (find-one 'channel (cadr sxml))))))

(define (parse-atom-date string)
  "For example, \"2020-11-29T12:34:56Z\"."
  (string->date string "~Y-~m-~dT~H:~M:~SZ"))

(define (atom port)
  (define (find-html-link tree)
    (cond ((find (lambda (subtree)
                   (and (pair? subtree)
                        (eq? 'atom:link (car subtree))
                        (cond ((find-one '@ subtree)
                               => (lambda (attributes)
                                    (cond ((find-one 'type attributes)
                                           => (lambda (t)
                                                (string=? (car t)
                                                          "text/html")))
                                          (else #f))))
                              (else #f))))
                 tree)
           => (lambda (link)
                (let ((attributes (find-one '@ link)))
                  (cond ((find-one 'href attributes) => car)
                        (else #f)))))
          (else #f)))
  (define (parse-date value) (parse-atom-date (car value)))
  (let ((sxml (ssax:xml->sxml port
                              '((atom . "http://www.w3.org/2005/Atom")))))
    (map (lambda (e)
           (make-feed-item
            (cond ((find-one 'atom:published e) => parse-date)
                  ((find-one 'atom:updated e) => parse-date)
                  ((find-one 'atom:source e)
                   => (lambda (s)
                        (cond ((find-one 'atom:published s)
                               => parse-date)
                              (else #f))))
                  (else #f))
            (cond ((find-one 'atom:summary e))
                  (else '("")))
            (all-strings-in (find-one 'atom:title e))
            (find-html-link e)))
         (find-many 'atom:entry (find-one 'atom:feed sxml)))))

(define ((fetch-uri parse) uri)
  (call-with-input-request uri #f parse))

(define fetch-rss (fetch-uri rss))
(define fetch-atom (fetch-uri atom))

;; examples:
;;   (fetch-rss "https://srfi.schemers.org/srfi.rss")
;;   (fetch-atom "https://planet.scheme.org/atom.xml")

(define ((read-file parse) file)
  (call-with-input-file file parse))

(define file-rss (read-file rss))
(define file-atom (read-file atom))

(define (write-html-file www-filename title description body)
  (echo "Writing " www-filename)
  (with-output-to-file www-filename
    (lambda ()
      (write-string "<!DOCTYPE html>")
      (SXML->HTML
       `(html (@ (lang "en"))
              (head
               (meta (@ charset "UTF-8"))
               (title ,title)
               (link (@ (rel "stylesheet") (href "/schemeorg.css")))
               (link (@ (rel "canonical") (href "https://www.scheme.org/")))
               (meta (@ (name "viewport")
                        (content "width=device-width, initial-scale=1")))
               (meta (@ (name "description")
                        (content ,description))))
              (body ,@body))))))

(define (find-subtree head tree)
  (and (pair? tree)
       (if (equal? head (car tree))
           tree
           (or (find-subtree head (car tree))
               (find-subtree head (cdr tree))))))

(define (page-title-from-sxml sxml)
  (let ((h1 (or (find-subtree 'h1 sxml)
                (error "Page has no title" sxml))))
    (apply string-append (cdr h1))))

(define (html-body-as-sxml filename)
  (call-with-port (open-input-file filename)
    (lambda (port)
      (let ((sxml (ssax:xml->sxml port '())))
        (cdr (or (find-subtree 'body sxml)
                 (error "No <body> found in" filename sxml)))))))

(define (write-simple-page www-filename doc-filename description)
  (let ((sxml (html-body-as-sxml doc-filename)))
    (write-html-file www-filename
                     (page-title-from-sxml sxml)
                     description
                     sxml)))

(define redirect-project-id first)
(define redirect-uri second)

(define (redirect-list)
  (list-sort
   (lambda (a b) (string<? (redirect-project-id a)
                           (redirect-project-id b)))
   (filter-map
    (lambda (project)
      (let ((uri (get-string? 'redirect project)))
        (and uri (list (get-string 'project-id project)
                       uri))))
    (all-projects))))

(define (write-redirect-page)
  (write-html-file
   "www/redirect.scheme.org/index.html"
   "redirect.scheme.org"
   "Explains the redirects at Scheme.org."
   `((h1 (@ (id "logo"))
         "Redirects under Scheme.org")
     (table
      (tr (th "Subdomain")
          (th "Destination"))
      ,@(map (lambda (redirect)
               (let ((subdomain (string-append (redirect-project-id redirect)
                                               ".scheme.org")))
                 `(tr (td (a (@ (href ,(string-append "//" subdomain)))
                             (code ,subdomain)))
                      (td (code ,(redirect-uri redirect))))))
             (redirect-list)))
     ,@(html-body-as-sxml "doc/redirect.html"))))

(define (menu items)
  `(header
    (ul (@ (class "menu"))
        ,@(map (lambda (i)
                 (if (eq? 'active (cddr i))
                     `(li (@ (class "active"))
                          ,(car i))
                     `(li (a (@ (href ,(cadr i))) ,(car i)))))
               items))))

(define (whats-new-div atom-feed)
  `(div (@ (class "round-box green-box blog-posts"))
        (h2 "What's new in Scheme")
        ;; We should guard against rogue content, e.g. by filtering
        ;; out entries with foul language or dangerous HTML; and
        ;; include the source.
        (ul ,@(map (lambda (item)
                     `(li (a (@ href ,(feed-item-uri item))
                             ,(feed-item-title item))
                          " "
                          (time (@ (class "date"))
                                ,(feed-item-friendly-date item))))
                   (take atom-feed 5)))
        (p (@ class "more")
           "More on "
           (a (@ href "https://planet.scheme.org/")
              "Planet Scheme")
           ".")))

(define (write-front-page www-filename atom-feed extra-banner)
  (write-html-file
   www-filename
   "The Scheme Programming Language"
   (string-append "Scheme is a minimalist dialect of the Lisp family "
                  "of programming languages. This is the official website "
                  "for the Scheme language.")
   `(,(menu
       `(("Home" "https://www.scheme.org/" . active)
         ("Docs" "https://docs.scheme.org/")
         ("Community" "https://community.scheme.org/")
         ("Standards" "https://standards.scheme.org/")
         ("Implementations" "https://get.scheme.org/")))
     (h1 (@ (id "logo")) "Scheme")
     ,@(if extra-banner `(,extra-banner) '())
     ,@(html-body-as-sxml "doc/front.html")
     ,(whats-new-div atom-feed)
     ,@(append-map
        (let ((next-color (circular-generator "blue" "orange")))
          (lambda (group)
            (let ((trs
                   (map (lambda (project)
                          `(tr (th (a (@ (href
                                          ,(string-append
                                            "//"
                                            (get-string 'project-id project)
                                            ".scheme.org/")))
                                      ,@(superscripts
                                         (get-string 'title project))))
                               (td ,@(superscripts
                                      (get-string 'tagline project)))
                               ,@(map (lambda (note)
                                        `(td (@ (class "sidenote"))
                                             ,@(superscripts note)))
                                      (get-list 'sidenote project))))
                        (filter (lambda (project)
                                  (get-symbol-boolean 'display? project))
                                (project-group-projects group)))))
              (if (null? trs) '()
                  `((div (@ (class ,(string-append
                                     "front-page-group round-box "
                                     (next-color) "-box")))
                         (h2 (@ (id ,(string->html-id
                                      (project-group-title group))))
                             ,(project-group-title group))
                         (table (@ (class "no-border"))
                                ,@trs)))))))
        (project-groups))
     (p (@ (class "center"))
        (a (@ (href "/about/"))
           "About Scheme.org")))))

(define (generate-scheme.org)
  (let ((atom-feed (fetch-atom "https://planet.scheme.org/atom.xml")))
    (create-directory "www/scheme.org")
    (create-directory "www/scheme.org/about")
    (create-directory "www/scheme.org/charter")
    (create-directory "www/scheme.org/schemers")
    (write-front-page "www/scheme.org/index.html"
                      atom-feed
                      #f)
    (write-simple-page "www/scheme.org/about/index.html"
                       "doc/about.html"
                       "description here")
    (write-simple-page "www/scheme.org/charter/index.html"
                       "doc/charter.html"
                       "description here")
    (write-front-page
     "www/scheme.org/schemers/index.html"
     atom-feed
     '(div (@ (class "round-box orange-box"))
           (p "Welcome to "
              (a (@ (href "https://www.scheme.org/"))
                 "Scheme.org")
              ", a new home page for Scheme. We host a "
              (a (@ (href "https://conservatory.scheme.org/schemers/"))
                 "snapshot")
              " of the old Schemers.org. Thanks to "
              (a (@ (href "https://cs.brown.edu/~sk/"))
                 "Prof. Shriram Krishnamurthi")
              " and all the other people who gave Scheme a home"
              " on Schemers.org for nearly twenty-five years.")))
    (write-redirect-page)))

(define (main)
  (generate-scheme.org)
  0)

(main)
