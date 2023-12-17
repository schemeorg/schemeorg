(import (scheme base)
        (scheme file)
        (schemeorglib))

(define human-users
  '((0 "lassi" "Lassi Kortela")
    (1 "arthur" "Arthur Gleckler")))

(define human-user-ordinal first)
(define human-user-name second)
(define human-user-display-name third)

(define (human-user-id human-user)
  (+ 1000 (human-user-ordinal human-user)))

(define (human-user-groups human-user)
  '("users" "sudo" "docker"))

(define (ssh-key-tasks* user-name human-user-name)
  (map (lambda (key)
         `(task
           (title ,(string-append "add key for " human-user-name))
           (authorized-key
            (user ,user-name)
            (key ,(string-append key " " human-user-name)))))
       (with-input-from-file (string-append (script-directory)
                                            "../server/keys/"
                                            human-user-name)
         (lambda () (generator->list read-line)))))

(define (ssh-key-tasks user-name)
  (ssh-key-tasks* user-name user-name))

(define (human-user-tasks human-user)
  `((task
     (title ,(string-append "make user " (human-user-name human-user)))
     (user
      (uid ,(human-user-id human-user))
      (name ,(human-user-name human-user))
      (comment ,(human-user-display-name human-user))
      (group ,(first (human-user-groups human-user)))
      (groups ,(cdr (human-user-groups human-user)))
      (shell "/bin/bash")))
    ,@(ssh-key-tasks (human-user-name human-user))))

(define (site-tasks subdomain)
  (let ((site-home (string-append "/var/www/" subdomain)))
    `((task
       (title ,(string-append "chown and chmod " site-home))
       (file
        (path ,site-home)
        (state "directory")
        (owner "www-data")
        (group "users")
        (mode "u=rwX,g=rwX,o=rX")
        (follow false)
        (recurse true))))))

(define (write-top-level-expressions . exps)
  (for-each pretty-print exps))

(write-top-level-expressions

 `(options
   (var pipelining true))

 `(groups
   (group
    (name schemeorg)
    (hosts
     (host
      (name scheme)
      (vars
       ;; (var ansible-user "root")
       (var ansible-host "8.9.4.141")
       (var ansible-python-interpreter "/usr/bin/python3"))))))

 `(playbooks

   (playbook
    (name schemeorg)
    (hosts schemeorg)
    (become true)
    (roles
     apt-upgrade
     apt-comfort
     hostname
     sudo
     firewall
     docker
     human-users
     site-www
     site-www-staging
     site-redirect
     site-lists
     nginx
     sshd)))

 `(roles

   (role
    (name apt-upgrade)
    (tasks
     (task
      (title "upgrade all apt packages to latest versions")
      (apt
       (update-cache true)
       (upgrade true)))))

   (role
    (name apt-comfort)
    (tasks
     (task
      (title "text editors")
      (apt
       (name
        ("emacs-nox"
         "mg"
         "nano"
         "vim"))))
     (task
      (title "other tools")
      (apt
       (name
        ("curl"
         "dnsutils"
         "git"
         "htop"
         "httpie"
         "jq"
         "rsync"
         "silversearcher-ag"
         "tmux"
         "tree"
         "unzip"
         "wget"))))))

   (role
    (name hostname)
    (tasks
     (task
      (title "set hostname")
      (hostname (name "scheme.org")))))

   (role
    (name sudo)
    (tasks
     (task
      (title "install sudo")
      (apt (name "sudo")))
     (task
      (title "enable passwordless sudo")
      (lineinfile
       (validate "visudo -cqf %s")
       (path "/etc/sudoers")
       (regexp "%sudo")
       (line "%sudo ALL=(ALL:ALL) NOPASSWD:ALL")))
     (task
      (title "do not create .sudo_as_admin_successful")
      (lineinfile
       (validate "visudo -cqf %s")
       (path "/etc/sudoers")
       (line "Defaults !admin_flag")))))

   (role
    (name sshd)
    (tasks
     (task
      (title "deny ssh password authentication")
      (lineinfile
       (dest "/etc/ssh/sshd_config")
       (regexp "^#?PasswordAuthentication")
       (line "PasswordAuthentication no"))
      (notify "restart ssh")))
    (handlers
     (handler
      (title "restart ssh")
      (service
       (name "sshd")
       (state "restarted")))))

   (role
    (name firewall)
    (tasks
     (task
      (title "install ufw")
      (apt (name "ufw")))
     (task
      (title "allow ssh")
      (ufw
       (rule allow)
       (proto tcp)
       (port ssh)))
     (task
      (title "allow http")
      (ufw
       (rule allow)
       (proto tcp)
       (port http)))
     (task
      (title "allow https")
      (ufw
       (rule allow)
       (proto tcp)
       (port https)))
     (task
      (title "enable firewall with default policy to deny all")
      (ufw
       (state enabled)
       (policy deny)))))

   (role
    (name docker)
    (tasks
     (task
      (title "install docker")
      (apt
       (name
        ("docker.io"
         "docker-compose"))))))

   (role
    (name human-users)
    (tasks
     ,@(append-map human-user-tasks
                   human-users)))

   (role
    (name site-www)
    (tasks ,@(site-tasks "www.scheme.org")))

   (role
    (name site-www-staging)
    (tasks ,@(site-tasks "www.staging.scheme.org")))

   (role
    (name site-redirect)
    (tasks ,@(site-tasks "redirect.scheme.org")))

   (role
    (name site-lists)
    (tasks ,@(site-tasks "lists.scheme.org")))

   (role
    (name nginx)
    (tasks
     (task
      (title "install nginx and certbot")
      (apt
       (name
        ("nginx"
         "certbot"
         "python3-certbot-nginx")))
      (notify "restart nginx"))
     (task
      (title "backup system nginx.conf to nginx.conf.orig")
      (copy
       (dest "/etc/nginx/nginx.conf.orig")
       (src "/etc/nginx/nginx.conf")
       (remote-src true)
       (mode "0444")
       (force false)))
     (task
      (title "install our own nginx.conf")
      (copy
       (validate "nginx -t -c %s")
       (dest "/etc/nginx/nginx.conf")
       (src "nginx.conf")
       (owner "root")
       (group "root")
       (mode "0644"))
      (notify "restart nginx"))
     (task
      (title "start nginx now and at every boot")
      (service
       (name "nginx")
       (enabled true)
       (state "started"))))
    (handlers
     (handler
      (title "restart nginx")
      (service
       (name "nginx")
       (state "restarted")))))))
