(cond-expand (chicken (include "schemeorglib.sld")))

(import (scheme base)
        (schemeorglib))

(define (letsencrypt-hostname)
  "scheme.org")

(define (letsencrypt-etc)
  "/etc/letsencrypt")

(define (letsencrypt nginx-directive filename)
  (let ((filename (string-append (letsencrypt-etc) "/" filename)))
    (string-append nginx-directive " " filename ";")))

(define (letsencrypt-live nginx-directive filename)
  (letsencrypt
   nginx-directive
   (string-append "live/" (letsencrypt-hostname) "/" filename)))

(define (letsencrypt-directives)
  (list (letsencrypt "ssl_dhparam" "ssl-dhparams.pem")
        (letsencrypt-live "ssl_certificate" "fullchain.pem")
        (letsencrypt-live "ssl_certificate_key" "privkey.pem")))

;;

(define (indent string)
  (string-append (make-string 4 #\space) string))

(define (block head . body)
  (append (list (string-append head " {"))
          (map indent (flatten-all body))
          (list "}")))

(define (write-nginx-block head . body)
  (for-each echo (apply block head body)))

(define (add-header name . params)
  (if (null? params)
      '()
      (list
       (string-join (list "add_header"
                          name
                          (string-append "\"" (string-join params "; ") "\"")
                          "always;")
                    " "))))

;;

(define blocked-features
  '("accelerometer"
    "ambient-light-sensor"
    "autoplay"
    "camera"
    "display-capture"
    "document-domain"
    "encrypted-media"
    "fullscreen"
    "geolocation"
    "gyroscope"
    "layout-animations"
    "magnetometer"
    "microphone"
    "midi"
    "payment"
    "picture-in-picture"
    "speaker"
    "usb"
    "vibrate"
    "vr"))

(define content-security-policy
  (make-parameter
   '(("default-src" "'self'")
     ("style-src"   "'self'" "'unsafe-inline'")
     ("script-src"  "'self'" "'unsafe-inline'")
     ("upgrade-insecure-requests"))))

(define (encode-csp-directive directive)
  (string-join directive " "))

(define (https-security-header-directives)
  (append
   (apply add-header "Content-Security-Policy"
          (map encode-csp-directive (content-security-policy)))
   (apply add-header "Feature-Policy"
          (map (lambda (feature) (string-append feature " 'none'"))
               blocked-features))
   (add-header "Referrer-Policy" "no-referrer")
   (add-header "Strict-Transport-Security"
               "max-age=31536000"
               "includeSubDomains")
   (add-header "X-Content-Type-Options" "nosniff")
   (add-header "X-Frame-Options" "SAMEORIGIN")
   (add-header "X-Permitted-Cross-Domain-Policies" "none")
   (add-header "X-Xss-Protection" "1" "mode=block")))

(define (default-server)
  (block "server"
         "listen [::]:80 default_server;"
         "listen 80 default_server;"
         "listen [::]:443 ssl default_server;"
         "listen 443 ssl default_server;"
         "server_name _;"
         "return 403;"))

(define (http->https-redirect-server primary alias)
  (block "server"
         (string-append "server_name " alias ";")
         "listen [::]:80;"
         "listen 80;"
         (string-append "return 301 https://" primary "$request_uri;")))

(define (https-redirect-server primary alias)
  (block "server"
         (string-append "server_name " alias ";")
         "listen [::]:443 ssl;"
         "listen 443 ssl;"
         (letsencrypt "include" "options-ssl-nginx.conf")
         (https-security-header-directives)
         (string-append "return 301 https://" primary "$request_uri;")))

(define (https-only-server hostname . body)
  (apply block
         "server"
         (string-append "server_name " hostname ";")
         "listen [::]:443 ssl;"
         "listen 443 ssl;"
         (letsencrypt "include" "options-ssl-nginx.conf")
         (https-security-header-directives)
         body))

(define (https-server hostnames . body)
  (let ((primary (car hostnames))
        (aliases (cdr hostnames)))
    (append (apply https-only-server primary body)
            (append-map (lambda (hostname)
                          (https-redirect-server primary hostname))
                        aliases)
            (append-map (lambda (hostname)
                          (http->https-redirect-server primary hostname))
                        hostnames))))

(define (http-redirect-only-server hostname redirect-to)
  (append
   (http->https-redirect-server hostname hostname)
   (block
    "server"
    (string-append "server_name " hostname ";")
    "listen [::]:443 ssl;"
    "listen 443 ssl;"
    (letsencrypt "include" "options-ssl-nginx.conf")
    (https-security-header-directives)
    (block "location = /"
           (string-append "return 301 " redirect-to ";")))))

(define (log-directives hostname)
  (list (string-append "access_log /var/log/nginx/" hostname "_access.log;")
        (string-append "error_log /var/log/nginx/" hostname "_error.log;")))

(define (static-site hostnames . body)
  (let ((primary (car hostnames)))
    (apply
     https-server
     hostnames
     (append
      (log-directives primary)
      (list (string-append "root /var/www/" primary ";"))
      body))))

;;

(write-nginx-block
 "events")

(write-nginx-block
 "http"

 "sendfile on;"
 "server_tokens off;"
 "ssl_stapling on;"
 "ssl_stapling_verify on;"

 (letsencrypt-directives)

 "include /etc/nginx/mime.types;"
 "default_type application/octet-stream;"

 (default-server)

 (static-site '("www.scheme.org" "scheme.org"))

 (static-site '("www.staging.scheme.org" "staging.scheme.org"))

 (static-site '("www.schemers.org" "schemers.org")

              (string-append
               "rewrite"
               " ^/$"
               " https://www.scheme.org/schemers/"
               " permanent;")

              (string-append
               "rewrite"
               " ^/welcome.shtml$"
               " https://conservatory.scheme.org/schemers/Welcome/"
               " permanent;")

              (string-append
               "rewrite"
               " ^/"
               " https://conservatory.scheme.org/schemers$request_uri"
               " permanent;"))

 (static-site '("redirect.scheme.org"))

 (static-site '("lists.scheme.org"))

 (filter-map
  (lambda (project)
    (let ((redirect (get-string? 'redirect project)))
      (and redirect
           (let* ((project-id (get-string 'project-id project))
                  (hostname (string-append project-id ".scheme.org")))
             (http-redirect-only-server hostname redirect)))))
  (all-projects)))
