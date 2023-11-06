;; Write all DNS records into the file "dns/scheme.org.zone".
;;
;; File format: <https://en.wikipedia.org/wiki/Zone_file>
;;
;; This should run in any R7RS-small implementation.

(import (scheme base) (scheme file) (scheme read) (scheme write))

(define domain-name "scheme.org")
(define domain-ipv4 "8.9.4.141")
(define domain-ipv6 "2001:19f0:5:6000:5400:2ff:fe07:9aa6")
(define domain-time-to-live "10800")

(define (echo . strings) (for-each write-string strings) (newline))

(define (get-list property alist) (cdr (or (assoc property alist) '(#f))))

(define (get-string? property alist)
  (let ((pair (assoc property alist))) (and pair (cadr pair))))

(define (get-string property alist)
  (or (get-string? property alist) (error "Not defined:" property alist)))

(define projects-scm (call-with-input-file "projects.scm" read))
(define project-groups (get-list 'projects projects-scm))

(define (echo-dns-record* name type data)
  (unless (member type '(A AAAA CNAME MX))
    (error "Unupported DNS record type:" type))
  (let ((type (symbol->string type)))
    (echo name " IN " type " " data)))

(define (echo-dns-record project-id record)
  (case (length record)
    ((2)
     (echo-dns-record* project-id
                       (list-ref record 0)
                       (list-ref record 1)))
    ((3)
     (echo-dns-record* (string-append (list-ref record 0) "." project-id)
                       (list-ref record 1)
                       (list-ref record 2)))

    (else
     (error "Cannot parse DNS record" project-id record))))

(define (echo-zone-file)
  (echo "; DNS zone file generated by dns.scm")
  (echo)
  (echo "; $ORIGIN " domain-name ".")
  (echo "$TTL " domain-time-to-live)
  (echo)
  (echo-dns-record* "@" 'A domain-ipv4)
  (echo-dns-record* "@" 'AAAA domain-ipv6)
  (for-each
   (lambda (group)
     (for-each
      (lambda (project)
        (let ((project-id (get-string 'project-id project)))
          (let ((dns (get-list 'dns project)))
            (unless (null? dns)
              (echo)
              (for-each (lambda (record) (echo-dns-record project-id record))
                        dns)))))
      (cdr group)))
   project-groups))

(define (main)
  (echo "Writing dns/scheme.org.zone")
  (with-output-to-file "dns/scheme.org.zone" echo-zone-file)
  0)

(main)
