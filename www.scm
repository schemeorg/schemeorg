;; Write HTML files into the `www` subdirectory.
;;
;; Based on S-expressions, markdown, and SXML templates in this file.
;;
;; You need Chicken 5 and
;; `chicken-install html-parser http-client lowdown openssl r7rs ssax`.

(import (scheme base) (scheme file) (scheme read) (scheme write))
(import (chicken file)) ; For create-directory.
(import (sxml-transforms))
(import (lowdown))      ; Markdown-to-SXML parser.
(import (ssax))		; SSAX for parsing RSS
(import (srfi 1))
(import html-parser)
(import http-client)

(define (filter f xs)
  (let loop ((xs xs) (acc '()))
    (if (null? xs) (reverse acc)
        (loop (cdr xs) (if (f (car xs)) (cons (car xs) acc) acc)))))

(define (append-map f xs)
  (let loop ((xs xs) (acc '()))
    (if (null? xs) acc (loop (cdr xs) (append acc (f (car xs)))))))

(define (disp . xs) (for-each display xs) (newline))

(define (string->file string file)
  (call-with-port (open-output-file file)
                  (lambda (out)
                    (write-string string out)
                    (newline out))))

(define (sxml->html expr)
  (cond ((and (pair? expr) (symbol? (car expr)))
         (let* ((elem (symbol->string (car expr)))
                (attr (and (pair? (cdr expr))
                           (pair? (cadr expr))
                           (eqv? '@ (car (cadr expr)))
                           (cdr (cadr expr))))
                (body (if attr (cddr expr) (cdr expr))))
           (string-append
            "<" elem
            (apply string-append
                   (map (lambda (pair)
                          (string-append " " (symbol->string (car pair))
                                         "=\"" (cadr pair) "\""))
                        (or attr '())))
            ">"
            (apply string-append (map sxml->html body))
            "</" elem ">")))
        ((string? expr) expr)
        (else (error "Bad XML expression"))))

(define (get-list property alist)
  (let ((entry (assoc property alist)))
    (if entry (cdr entry) '())))

(define (get-one valid? key alist)
  (let ((tail (cdr (or (assoc key alist) (error "Missing key" key)))))
    (if (and (= 1 (length tail)) (valid? (car tail)))
        (car tail)
        (error "Bad alist entry"))))

(define (get-boolean key alist) (get-one boolean? key alist))
(define (get-string  key alist) (get-one string?  key alist))

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

(define-record-type feed-item (make-feed-item date description title uri)
    feed-item?
    (date fi/date)
    (description fi/description)
    (title fi/title)
    (uri fi/uri))

(define (matching-subtree? name tree)
  (and (pair? tree)
       (eq? (car tree) name)))

(define ((matches? name) tree) (and (matching-subtree? name tree) tree))

(define (find-one name tree)
  (cond ((find (matches? name) tree) => cdr) (else #f)))

(define (find-many name tree) (filter-map (matches? name) (cdr tree)))

(define (skip-attributes tree)
  (cond ((and (pair? (car tree))
	      (eq? '@ (caar tree)))
	 (cdr tree))
	(else tree)))

(define (parse-html string)
  (html->sxml (open-input-string string)))

(define (rss port)
  (let ((sxml (ssax:xml->sxml port '())))
    (map (lambda (i)
	   (make-feed-item
	    (car (find-one 'pubDate i))
	    (cond ((find-one 'description i)
		   => (lambda (d)
			(cdr (parse-html (apply string-append d)))))
		  (else ""))
	    (apply string-append
		   (skip-attributes (find-one 'title i)))
	    (car (find-one 'link i))))
	 (find-many 'item (find-one 'channel (cadr sxml))))))

(define (atom port)
  (define (find-html-link tree)
    (cond ((find (lambda (subtree)
		   (and (pair? subtree)
			(eq? 'atom:link (car subtree))
			(cond ((find-one '@ subtree)
			       => (lambda (attributes)
				    (cond ((find-one 'type attributes)
					   => (lambda (t)
						(string=? (car t) "text/html")))
					  (else #f))))
			      (else #f))))
		 tree)
	   => (lambda (link)
		(let ((attributes (find-one '@ link)))
		  (cond ((find-one 'href attributes) => car)
			(else #f)))))
	  (else #f)))
  (let ((sxml (ssax:xml->sxml port '((atom . "http://www.w3.org/2005/Atom")))))
    (map (lambda (e)
	   (make-feed-item (cond ((find-one 'atom:published e) => car)
				((find-one 'atom:updated e) => car)
				((find-one 'atom:source e)
				 => (lambda (s)
				      (cond ((find-one 'atom:published s)
					     => car)
					    (else #f))))
				(else #f))
			  (cond ((find-one 'atom:summary e))
				(else '("")))
			  (apply string-append
				 (skip-attributes (find-one 'atom:title e)))
			  (find-html-link e)))
	 (find-many 'atom:entry (find-one 'atom:feed sxml)))))

(define ((fetch-uri parse) uri)
  (call-with-input-request uri #f parse))

(define fetch-rss (fetch-uri rss))
(define fetch-atom (fetch-uri atom))

;; examples:
;;   (fetch-rss "https://speechcode.com/blog/rss")
;;   (fetch-atom "http://www.scheme.dk/planet/atom.xml")

(define ((read-file parse) file)
  (call-with-input-file file parse))

(define file-rss (read-file rss))
(define file-atom (read-file atom))

(define projects-scm (call-with-input-file "projects.scm" read))
(define project-groups (get-list 'projects projects-scm))

(define (write-html-file html-filename title body)
  (disp "Writing " html-filename)
  (with-output-to-file html-filename
    (lambda ()
      (write-string "<!DOCTYPE html>")
      (SXML->HTML
       `(html
         (@ lang "en")
         (head
          (meta (@ charset "UTF-8"))
          (title ,title)
          (link (@ (rel "stylesheet") (href "/style.css")))
          (meta (@ (name "viewport")
                   (content "width=device-width, initial-scale=1"))))
         (body ,@body))))))

(define (page-title-from-sxml tags)
  (let rec ((tags tags))
    (cond ((not (pair? tags)) (error "Markdown page has no title"))
          ((eqv? 'h1 (car (car tags)))
           (apply string-append (cadr (car tags))))
          (else (rec (cdr tags))))))

(define (markdown-file->sxml md-filename)
  (call-with-port (open-input-file md-filename) markdown->sxml))

(define (write-simple-page html-filename md-filename)
  (let ((sxml (markdown-file->sxml md-filename)))
    (write-html-file html-filename (page-title-from-sxml sxml) sxml)))

(define (write-menu items)
  `(header
    (ul (@ class "menu")
	,@(map (lambda (i)
		 (if (eq? 'active (cddr i))
		     `(li (@ class "active")
			  ,(car i))
		     `(li (a (@ (href ,(cadr i))) ,(car i)))))
	       items))))

(define (write-front-page html-filename)
  (write-html-file
   html-filename
   "The Scheme Programming Language"
   `(,(write-menu '(("Home" "https://scheme.org/" . active)
		    ("Docs" "https://doc.scheme.org/")
		    ("Email" "https://lists.scheme.org/")
		    ("Files" "https://files.scheme.org/")
		    ("Standards" "https://standards.scheme.org/")))
     (h1 (@ id "logo") "Scheme")
     ,@(markdown-file->sxml "front.md")
     ,@(append-map
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
                                    (get-string 'tagline project)))))
                      (filter (lambda (project)
                                (get-boolean 'display? project))
                              (cdr group)))))
            (if (null? trs) '()
                `((h2 ,(car group))
                  (table ,@trs)))))
        project-groups)
     (h2 "News")

     ;; Insert Planet Scheme feed.  This is just an example of what a feed could
     ;; look like.  It livens up the page, adding frequently updated content.
     ;; We should merge more feeds, e.g. a hypothetical SRFI feed; guard against
     ;; rogue content, e.g. by filtering out entries with foul language or
     ;; dangerous HTML and by limiting the length of the blurb; include the
     ;; source; and format the date in a friendlier way.
     (dl ,@(append-map (lambda (fi)
		  `((dt (a (@ href ,(fi/uri fi))
			   ,(fi/title fi)))
		    (dd (div (@ class "date") ,(fi/date fi))
			,(fi/description fi))))
		(fetch-atom "http://www.scheme.dk/planet/atom.xml")))

     (p (a (@ (href "about/")) "About Scheme.org")))))

(define (main)
  (create-directory "www/about")
  (create-directory "www/charter")
  (write-front-page  "www/index.html")
  (write-simple-page "www/about/index.html"   "about.md")
  (write-simple-page "www/charter/index.html" "charter.md")
  0)

(main)
