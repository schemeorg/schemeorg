(cond-expand (chicken (include "schemeorglib.sld")))

(import (scheme base)
        (schemeorglib))

(define main-domain
  "scheme.org")

(define other-domains
  '("schemers.org"))

(define (compute-cname-map)
  (append
   (append-map
    (lambda (project)
      (let ((project-id (get-string 'project-id project)))
        (filter-map
         (lambda (record)
           (case (length record)
             ((2)
              (and (equal? 'CNAME (list-ref record 0))
                   (cons (string-append project-id "." main-domain)
                         (list-ref record 1))))
             ((3)
              (and (equal? 'CNAME (list-ref record 1))
                   (cons (string-append (list-ref record 0) "."
                                        project-id "." main-domain)
                         (list-ref record 2))))
             (else
              (error "Cannot parse DNS record" project-id record))))
         (get-list 'dns project))))
    (all-projects))
   (append-map (lambda (domain)
                 (list (cons domain "@")
                       (cons (string-append "www." domain) "@")))
               other-domains)))

(define cname-map
  (compute-cname-map))

;; (for-each (lambda (x) (write x) (newline)) cname-map)

(define (root? subdomain)
  (let ((entry (assoc subdomain cname-map)))
    (and entry
         (or (equal? "@" (cdr entry))
             (root? (cdr entry))
             (root? (string-append (cdr entry) "." main-domain))))))

(define subdomains
  (cons main-domain (sort-unique (filter root? (map car cname-map)))))

(echo "sudo certbot renew")
(echo
 (string-join
  (cons (string-append "sudo certbot certonly --nginx --cert-name "
                       main-domain)
        (map (lambda (subdomain) (string-append "  -d " subdomain))
             subdomains))
  " \\\n"))
