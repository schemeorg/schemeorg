;; Serve static files from the `scheme.org/www` subdirectory over HTTP.
;;
;; This is so you can browse the site on localhost using the same URL
;; layout that it has on www.scheme.org. We set the bind address to
;; localhost to ensure the server is not visible to other computers.
;; Set the PORT environment variable to serve on a particular port.
;;
;; You need Chicken 5 and `chicken-install spiffy`.

(import (srfi 98))
(import (spiffy))

(define (disp . xs) (for-each display xs) (newline))

(define (main)
  (root-path "scheme.org/www")  ; Serve static files from this directory.
  (server-bind-address "localhost")
  (let ((port (get-environment-variable "PORT")))
    (when port (server-port (string->number port))))
  (disp "Serving contents of scheme.org/www/ directory at "
        "http://" (server-bind-address) ":" (server-port) "/")
  (start-server)
  0)

(main)
