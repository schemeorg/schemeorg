((projects

  ("Front page"

   ((project-id "www")
    (comments "The www.scheme.org front page")
    (contacts "arthur" "lassi")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "alpha.servers")))))

  ("Language"

   ((project-id "try")
    (title "Try Scheme")
    (tagline "Type Scheme code and run it in your browser")
    (contacts "lassi")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "faq")
    (title "FAQ")
    (tagline "Frequently asked questions about Scheme")
    (contacts "lassi")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "cookbook")
    (title "Cookbook")
    (tagline "Code snippets that solve common problems")
    (contacts "Jakub T. Jankiewicz" "lassi")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "standards")
    (title "Standards")
    (tagline "Revised^n Report on Scheme and other standards")
    (contacts "arthur" "lassi")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "srfi")
    (title "SRFI")
    (tagline "Scheme Requests for Implementation")
    (comments "Continues under schemers.org until planned how to move.")
    (contacts "arthur")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "srfi.schemers.org."))))

   ((project-id "research")
    (title "Research")
    (tagline "Dive into the academic research behind Scheme")
    (contacts "lassi" "arthur")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers")))))

  ("Community"

   ((project-id "community")
    (title "Community")
    (tagline "Scheme gathering spots around the internet")
    (contacts "lassi" "arthur" "Jakub T. Jankiewicz")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "events")
    (title "Events")
    (tagline "Scheme Workshop, conferences, and other meetups")
    (contacts "arthur" "lassi")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "planet")
    (title "Planet")
    (tagline "Blog posts from every corner of the Scheme community")
    (contacts "lassi" "arthur")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "video")
    (title "Video")
    (tagline "Videos about Scheme")
    (contacts "Jakub T. Jankiewicz")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "lists")
    (title "Lists")
    (tagline "Mailing lists for email discussion of many Scheme topics")
    (contacts "arthur")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "wiki")
    (title "Wiki")
    (tagline "Scheme community wiki")
    (contacts "lassi" "arthur")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "groups")
    (title "Groups")
    (tagline "Work groups")
    (contacts "lassi")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers")))))

  ("Topics"

   ((project-id "learn")
    (title "Education")
    (tagline "Learn Scheme, and learn computer science using Scheme")
    (contacts "arthur" "lassi")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "apps")
    (title "Applications")
    (tagline "Software written or scripted in Scheme")
    (contacts "lassi" "arthur")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "comm")
    (title "Communications")
    (tagline "Comms and networks: TCP/IP, WebSockets, peer-to-peer, radio...")
    (comments "Discussed on the schemecomm@srfi.schemers.org mailing list.")
    (contacts "hga")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "persist")
    (title "Persistence")
    (tagline "Databases, encoding, and logging")
    (comments "Discussed on the schemepersist@srfi.schemers.org mailing list.")
    (contacts "hga")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "test")
    (title "Testing")
    (tagline "Everything having to do with testing in Scheme")
    (comments "Discussed on the schemetest@srfi.schemers.org mailing list.")
    (contacts "hga")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "web")
    (title "Web development")
    (tagline "HTTP, HTML, JavaScript, WebAssembly, and other web standards")
    (comments "Discussed on the schemeweb@srfi.schemers.org mailing list.")
    (contacts "lassi" "arthur")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "alpha.servers")))))

  ("Standards"

   ((project-id "r5rs")
    (title "R^5RS")
    (tagline "Revised^5 Report on the Algorithmic Language Scheme (1998)")
    (contacts "lassi")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "r6rs")
    (title "R^6RS")
    (tagline "Revised^6 Report on the Algorithmic Language Scheme (2007)")
    (contacts "lassi")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "r7rs")
    (title "R^7RS")
    (tagline "Revised^7 Report on the Algorithmic Language Scheme (2013)")
    (contacts "lassi")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "redirect")))))

  ("Implementations"

   ((project-id "get")
    (title "Get Scheme")
    (tagline "Browse and compare all known Scheme systems")
    (sidenote "")
    (contacts "lassi")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "bigloo")
    (title "Bigloo")
    (tagline "Fast Scheme-to-C and Scheme-to-JVM compiler")
    (sidenote "R^5")
    (contacts "Manuel Serrano")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "chez")
    (title "Chez Scheme")
    (tagline "Cross-module optimizing native-code compiler")
    (sidenote "R^6")
    (contacts "Bob Burger"
              "Andy Keep")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "chibi")
    (title "Chibi-Scheme")
    (tagline "Small embeddable interpreter with many optional libraries")
    (sidenote "R^7")
    (contacts "Alex Shinn")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "chicken")
    (title "CHICKEN")
    (tagline "Scheme-to-C compiler with a big, friendly community")
    (sidenote "R^5 R^7")
    (contacts "Mario Domenech Goulart" "chicken-meisters at nongnu.org")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "cyclone")
    (title "Cyclone")
    (tagline "New Scheme-to-C compiler with native threads")
    (sidenote "R^7")
    (contacts "Justin Ethier")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "gambit")
    (title "Gambit")
    (tagline "Fast, concurrent, retargetable optimizing compiler")
    (sidenote "R^5 R^7")
    (contacts "Marc Feeley")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "gambitscheme.org."))))

   ((project-id "gauche")
    (title "Gauche")
    (tagline "Fast script interpreter with many built-in libraries")
    (sidenote "R^5 R^7")
    (contacts "shirok")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "gerbil")
    (title "Gerbil")
    (tagline "Scheme with actors and objects built on Gambit")
    (sidenote "R^5 R^7")
    (contacts "Dimitris Vyzovitis")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "cons.io."))))

   ((project-id "jazz")
    (title "JazzScheme")
    (tagline "Object-oriented GUI and IDE built on Gambit")
    (sidenote "R^5")
    (contacts "Guillaume Cartier")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "kawa")
    (title "Kawa")
    (tagline "JVM compiler with many extensions to Scheme")
    (sidenote "R^7")
    (contacts "Per Bothner")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "loko")
    (title "Loko")
    (tagline "Bare-metal native-code compiler")
    (sidenote "R^6 R^7")
    (contacts "weinholt")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "scheme.fail."))))

   ((project-id "mit")
    (title "MIT/GNU Scheme")
    (tagline "Native-code compiler and development environment")
    (sidenote "R^5 R^7")
    (contacts "arthur")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "mosh")
    (title "Mosh")
    (tagline "Fast and complete R^6RS interpreter")
    (sidenote "R^6")
    (contacts "Taro Minowa")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "s7")
    (title "s7")
    (tagline "Embeddable interpreter for music applications")
    (sidenote "R^7")
    (contacts "bil" "lassi")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "sagittarius")
    (title "Sagittarius")
    (tagline "Script interpreter with many built-in libraries")
    (sidenote "R^6 R^7")
    (contacts "Takashi Kato")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "scm")
    (title "SCM")
    (tagline "Portable C implementation that begat Guile and SLIB")
    (sidenote "R^5")
    (contacts "Aubrey Jaffer" "lassi")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "stklos")
    (title "STklos")
    (tagline "Interpreter with CLOS object-oriented GUI")
    (sidenote "R^7")
    (contacts "Erick Gallesio")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "ypsilon")
    (title "Ypsilon")
    (tagline "Incremental native-code compiler with concurrent GC")
    (sidenote "R^6 R^7")
    (contacts "Göran Weinholt"
              "Yoshikatsu Fujita")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "redirect"))))

   ((project-id "conservatory")
    (title "Conservatory")
    (tagline "Community maintenance of old implementations")
    (sidenote "")
    (contacts "lassi")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers")))))

  ("Tools"

   ((project-id "index")
    (title "Index")
    (tagline "Library search using types, tags, and names")
    (contacts "Arvydas Silanskas")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "ironwolf.servers"))))

   ((project-id "api")
    (title "API")
    (tagline "Programmable queries for all things Scheme")
    (contacts "lassi")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "containers")
    (title "Containers")
    (tagline "Ready-to-run Docker containers")
    (contacts "lassi")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "neptune.servers"))))

   ((project-id "docs")
    (title "Documentation")
    (tagline "Study Scheme implementations and libraries")
    (comments "Discussed on the schemedoc@srfi.schemers.org mailing list.")
    (contacts "lassi" "arthur")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "man")
    (title "Manual pages")
    (tagline "Unix manual pages for tools and libraries")
    (contacts "weinholt" "lassi")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "files")
    (title "Files")
    (tagline "Archive of current and historical files")
    (contacts "lassi")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "gitea")
    (title "Gitea")
    (tagline "Host Git repositories under Scheme.org")
    (contacts "lassi")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "registry")
    (title "Registry")
    (tagline "Catalog of identifiers and other data")
    (contacts "lassi" "hga" "arthur")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))

   ((project-id "servers")
    (title "Servers")
    (tagline "Server pool for Scheme projects")
    (contacts "lassi")
    (display? #t)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))
         (rec (name "alpha")
              (type "CNAME")
              (data "scheme.org."))

         (rec (name "ironwolf")
              (type "A")
              (data "89.40.10.46"))
         (rec (name "ironwolf")
              (type "AAAA")
              (data "2a02:7b40:5928:a2e::1"))

         (rec (name "neptune")
              (type "A")
              (data "5.28.137.182"))))

   ((project-id "staging")
    (title "Staging")
    (tagline "Staging versions of Scheme.org projects")
    (contacts "lassi")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "www.staging"))
         (rec (name "api")
              (type "CNAME")
              (data "alpha.servers"))
         (rec (name "docs")
              (type "CNAME")
              (data "alpha.servers"))
         (rec (name "wiki")
              (type "CNAME")
              (data "alpha.servers"))
         (rec (name "www")
              (type "CNAME")
              (data "alpha.servers")))))

  ("Aliases"

   ((project-id "blog")
    (contacts "lassi" "arthur")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "planet"))))

   ((project-id "play")
    (contacts "lassi" "arthur")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "try"))))

   ((project-id "doc")
    (contacts "lassi" "arthur")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "docs"))))

   ((project-id "list")
    (contacts "arthur" "lassi")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "lists"))))

   ((project-id "redirect")
    (contacts "lassi" "arthur")
    (display? #f)
    (dns (rec (type "CNAME")
              (data "alpha.servers"))))))

 (people

  ((username "arthur")
   (realname "Arthur A. Gleckler"))

  ((username "bil")
   (realname "Bill Schottstaedt"))

  ((username "hga")
   (realname "Harold Ancell"))

  ((username "lassi")
   (realname "Lassi Kortela"))

  ((username "shirok")
   (realname "Shiro Kawai"))

  ((username "weinholt")
   (realname "Göran Weinholt"))))
