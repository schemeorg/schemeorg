((projects

  ("Front page"

   ((project-id "www")
    (comments "The www.scheme.org front page")
    (contacts "Arthur" "Lassi")
    (display? #f)
    (dns (CNAME "@"))))

  ("Language"

   ((project-id "try")
    (title "Try Scheme")
    (tagline "Type Scheme code and run it in your browser")
    (contacts "Lassi")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "faq")
    (title "FAQ")
    (tagline "Frequently asked questions about Scheme")
    (contacts "Lassi")
    (display? #t)
    (redirect "http://community.schemewiki.org/?scheme-faq")
    (dns (CNAME "redirect")))

   ((project-id "books")
    (title "Books")
    (tagline "Published books")
    (contacts "Lassi")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "cookbook")
    (title "Cookbook")
    (tagline "Code snippets that solve common problems")
    (contacts "Jakub T. Jankiewicz" "Lassi")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "standards")
    (title "Standards")
    (tagline "Revised^n Report on Scheme and other standards")
    (contacts "Arthur" "Lassi")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "srfi")
    (title "SRFI")
    (tagline "Scheme Requests for Implementation")
    (comments "Continues under schemers.org until planned how to move.")
    (contacts "Arthur")
    (display? #t)
    (dns (CNAME "srfi.schemers.org.")))

   ((project-id "research")
    (title "Research")
    (tagline "Dive into the academic research behind Scheme")
    (contacts "Lassi" "Arthur")
    (display? #t)
    (dns (CNAME "tuonela"))))

  ("Community"

   ((project-id "community")
    (title "Community")
    (tagline "Scheme gathering spots around the internet")
    (contacts "Lassi" "Arthur" "Jakub T. Jankiewicz")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "workshop")
    (title "Workshop")
    (tagline "The Scheme and Functional Programming Workshop")
    (contacts "Arthur" "Lassi")
    (display? #t)
    (redirect "https://www.schemeworkshop.org/")
    (dns (CNAME "redirect")))

   ((project-id "events")
    (title "Events")
    (tagline "Conferences and other meetups")
    (contacts "Arthur" "Lassi")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "planet")
    (title "Planet")
    (tagline "Blog posts from every corner of the Scheme community")
    (contacts "Lassi" "Arthur")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "video")
    (title "Video")
    (tagline "Videos about Scheme")
    (contacts "Jakub T. Jankiewicz")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "chat")
    (title "Chat")
    (tagline "IRC, Discord, Gitter and other highly informal channels")
    (contacts "Jakub T. Jankiewicz"
              "Wolfgang Corcoran-Mathe"
              "Lassi")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "lists")
    (title "Lists")
    (tagline "Mailing lists for email discussion of many Scheme topics")
    (contacts "Arthur")
    (display? #t)
    (dns (CNAME "@")))

   ((project-id "wiki")
    (title "Wiki")
    (tagline "Scheme community wiki")
    (contacts "Lassi" "Arthur")
    (display? #t)
    (redirect "http://community.schemewiki.org/")
    (dns (CNAME "redirect")))

   ((project-id "groups")
    (title "Groups")
    (tagline "Work groups")
    (contacts "Lassi")
    (display? #t)
    (dns (CNAME "tuonela"))))

  ("Topics"

   ((project-id "apps")
    (title "Applications")
    (tagline "Software written or scripted in Scheme")
    (contacts "Lassi" "Arthur")
    (display? #f)
    (dns (CNAME "tuonela")))

   ((project-id "comm")
    (title "Communications")
    (tagline "Comms and networks: TCP/IP, WebSockets, peer-to-peer, radio...")
    (comments "Discussed on the schemecomm@srfi.schemers.org mailing list.")
    (contacts "Harold Ancell")
    (display? #f)
    (dns (CNAME "tuonela")))

   ((project-id "persist")
    (title "Persistence")
    (tagline "Databases, encoding, and logging")
    (comments "Discussed on the schemepersist@srfi.schemers.org mailing list.")
    (contacts "Harold Ancell")
    (display? #f)
    (dns (CNAME "tuonela")))

   ((project-id "test")
    (title "Testing")
    (tagline "Everything having to do with testing in Scheme")
    (comments "Discussed on the schemetest@srfi.schemers.org mailing list.")
    (contacts "Harold Ancell")
    (display? #f)
    (dns (CNAME "tuonela")))

   ((project-id "web")
    (title "Web development")
    (tagline "HTTP, HTML, JavaScript, WebAssembly, and other web standards")
    (comments "Discussed on the schemeweb@srfi.schemers.org mailing list.")
    (contacts "Lassi" "Arthur")
    (display? #f)
    (dns (CNAME "tuonela"))))

  ("Standards"

   ((project-id "r5rs")
    (title "R^5RS")
    (tagline "Revised^5 Report on the Algorithmic Language Scheme (1998)")
    (contacts "Lassi")
    (display? #f)
    (redirect "http://schemers.org/Documents/Standards/R5RS/")
    (dns (CNAME "redirect")))

   ((project-id "r6rs")
    (title "R^6RS")
    (tagline "Revised^6 Report on the Algorithmic Language Scheme (2007)")
    (contacts "Lassi")
    (display? #f)
    (redirect "http://www.r6rs.org/")
    (dns (CNAME "redirect")))

   ((project-id "r7rs")
    (title "R^7RS")
    (tagline "Revised^7 Report on the Algorithmic Language Scheme (2013)")
    (contacts "Lassi")
    (display? #f)
    (redirect "http://r7rs.org/")
    (dns (CNAME "redirect"))))

  ("Implementations"

   ((project-id "get")
    (title "Get Scheme")
    (tagline "Browse and compare all known Scheme systems")
    (sidenote "")
    (contacts "Lassi")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "bigloo")
    (title "Bigloo")
    (tagline "Scheme-to-C and Scheme-to-JVM compiler")
    (sidenote "")
    (contacts "Manuel Serrano")
    (display? #t)
    (redirect "https://www-sop.inria.fr/indes/fp/Bigloo/")
    (dns (CNAME "redirect")))

   ((project-id "chez")
    (title "Chez Scheme")
    (tagline "Cross-module optimizing native-code compiler")
    (sidenote "R^6")
    (contacts "Bob Burger"
              "Andy Keep")
    (display? #t)
    (redirect "https://cisco.github.io/ChezScheme/")
    (dns (CNAME "redirect")))

   ((project-id "chibi")
    (title "Chibi-Scheme")
    (tagline "Small embeddable interpreter with many optional libraries")
    (sidenote "R^7")
    (contacts "Alex Shinn")
    (display? #t)
    (redirect "https://synthcode.com/scheme/chibi/")
    (dns (CNAME "redirect")))

   ((project-id "chicken")
    (title "CHICKEN")
    (tagline "Scheme-to-C compiler with a big, friendly community")
    (sidenote "R^7")
    (contacts "Mario Domenech Goulart" "chicken-meisters at nongnu.org")
    (display? #t)
    (redirect "https://call-cc.org/")
    (dns (CNAME "redirect")))

   ((project-id "cyclone")
    (title "Cyclone")
    (tagline "New Scheme-to-C compiler with native threads")
    (sidenote "R^7")
    (contacts "Justin Ethier")
    (display? #t)
    (redirect "https://justinethier.github.io/cyclone/")
    (dns (CNAME "redirect")))

   ((project-id "gambit")
    (title "Gambit")
    (tagline "Concurrent, retargetable, optimizing compiler")
    (sidenote "R^7")
    (contacts "Marc Feeley")
    (display? #t)
    (dns (CNAME "gambitscheme.org.")))

   ((project-id "gauche")
    (title "Gauche")
    (tagline "Script interpreter with many built-in libraries")
    (sidenote "R^7")
    (contacts "Shiro Kawai")
    (display? #t)
    (redirect "https://practical-scheme.net/gauche/")
    (dns (CNAME "redirect")))

   ((project-id "gerbil")
    (title "Gerbil")
    (tagline "Scheme with actors and objects built on Gambit")
    (sidenote "R^7")
    (contacts "Dimitris Vyzovitis")
    (display? #t)
    (dns (CNAME "cons.io.")))

   ((project-id "guile")
    (title "Guile")
    (tagline "Main Scheme implementation of the GNU project")
    (sidenote "R^6 R^7")
    (contacts "Ludovic Courtès"
              "Andy Wingo")
    (display? #t)
    (redirect "https://www.gnu.org/software/guile/")
    (dns (CNAME "redirect")))

   ((project-id "jazz")
    (title "JazzScheme")
    (tagline "Object-oriented GUI and IDE built on Gambit")
    (sidenote "")
    (contacts "Guillaume Cartier")
    (display? #t)
    ;; The "www." and "index.htm" are mandatory.
    (redirect "http://www.jazzscheme.org/index.htm")
    (dns (CNAME "redirect")))

   ((project-id "kawa")
    (title "Kawa")
    (tagline "JVM compiler with many extensions to Scheme")
    (sidenote "R^7")
    (contacts "Per Bothner")
    (display? #t)
    (redirect "https://www.gnu.org/software/kawa/")
    (dns (CNAME "redirect")))

   ((project-id "loko")
    (title "Loko")
    (tagline "Bare-metal native-code compiler")
    (sidenote "R^6 R^7")
    (contacts "Göran Weinholt")
    (display? #t)
    (dns (CNAME "scheme.fail.")))

   ((project-id "mit")
    (title "MIT/GNU Scheme")
    (tagline "Native-code compiler and development environment")
    (sidenote "R^7")
    (contacts "Arthur")
    (display? #t)
    (redirect "https://www.gnu.org/software/mit-scheme/")
    (dns (CNAME "redirect")))

   ((project-id "mosh")
    (title "Mosh")
    (tagline "Complete R^6RS interpreter")
    (sidenote "R^6 R^7")
    (contacts "Taro Minowa")
    (display? #t)
    (redirect "https://mosh.monaos.org/")
    (dns (CNAME "redirect")))

   ((project-id "racket")
    (title "Racket")
    (tagline "Native-code compiler")
    (sidenote "R^6 R^7")
    (contacts "John Clements" "Matthias Felleisen" "Robby Findler"
              "Matthew Flatt" "Jay McCarthy" "Sam Tobin-Hochstadt"
              "management at racket-lang.org")
    (display? #t)
    (redirect "https://racket-lang.org/")
    (dns (CNAME "redirect")))

   ((project-id "s7")
    (title "s7")
    (tagline "Embeddable interpreter for music applications")
    (sidenote "R^7")
    (contacts "Bill Schottstaedt" "Lassi")
    (display? #t)
    (redirect "https://ccrma.stanford.edu/software/s7/")
    (dns (CNAME "redirect")))

   ((project-id "sagittarius")
    (title "Sagittarius")
    (tagline "Script interpreter with many built-in libraries")
    (sidenote "R^6 R^7")
    (contacts "Takashi Kato")
    (display? #t)
    (redirect "https://ktakashi.github.io/")
    (dns (CNAME "redirect")))

   ((project-id "scm")
    (title "SCM")
    (tagline "Portable C implementation that begat Guile and SLIB")
    (sidenote "")
    (contacts "Aubrey Jaffer" "Lassi")
    (display? #t)
    (redirect "https://people.csail.mit.edu/jaffer/SCM")
    (dns (CNAME "redirect")))

   ((project-id "stklos")
    (title "STklos")
    (tagline "Interpreter with CLOS object-oriented GUI")
    (sidenote "R^7")
    (contacts "Erick Gallesio")
    (display? #t)
    (redirect "https://stklos.net/")
    (dns (CNAME "redirect")))

   ((project-id "ypsilon")
    (title "Ypsilon")
    (tagline "Incremental native-code compiler with concurrent GC")
    (sidenote "R^6 R^7")
    (contacts "Göran Weinholt"
              "Yoshikatsu Fujita")
    (display? #t)
    (redirect "http://www.littlewingpinball.com/doc/en/ypsilon/")
    (dns (CNAME "redirect"))))

  ("Tools"

   ((project-id "index")
    (title "Index")
    (tagline "Library search using types, tags, and names")
    (contacts "Arvydas Silanskas")
    (display? #t)
    (dns (CNAME "ironwolf")))

   ((project-id "api")
    (title "API")
    (tagline "Programmable queries for all things Scheme")
    (contacts "Lassi")
    (display? #f)
    (dns (CNAME "tuonela")))

   ((project-id "containers")
    (title "Containers")
    (tagline "Ready-to-run Docker containers")
    (contacts "Lassi")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "docs")
    (title "Documentation")
    (tagline "Study Scheme implementations and libraries")
    (comments "Discussed on the schemedoc@srfi.schemers.org mailing list.")
    (contacts "Lassi" "Arthur")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "mailman")
    (title "Mailman")
    (tagline "Host mailing lists under Scheme.org")
    (contacts "Lassi")
    (display? #f)
    (dns (CNAME "@")))

   ((project-id "man")
    (title "Manual pages")
    (tagline "Unix manual pages for tools and libraries")
    (contacts "Göran Weinholt" "Lassi")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "conservatory")
    (title "Conservatory")
    (tagline "Preservation of old software and websites")
    (sidenote "")
    (contacts "Lassi")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "files")
    (title "Files")
    (tagline "Archive of current and historical files")
    (contacts "Lassi")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "gitea")
    (title "Gitea")
    (tagline "Host Git repositories under Scheme.org")
    (contacts "Lassi")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "go")
    (title "Go Scheme")
    (tagline "URL shortening service")
    (contacts "Lassi" "Jakub T. Jankiewicz")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "registry")
    (title "Registry")
    (tagline "Catalog of identifiers and other data")
    (contacts "Lassi" "Harold Ancell" "Arthur")
    (display? #t)
    (dns (CNAME "tuonela")))

   ((project-id "staging")
    (title "Staging")
    (tagline "Staging versions of Scheme.org projects")
    (contacts "Lassi")
    (display? #f)
    (dns (CNAME "www.staging")
         ("www" CNAME "@")
         ("api" CNAME "tuonela")
         ("docs" CNAME "tuonela")
         ("wiki" CNAME "tuonela"))))

  ("Servers"

   ((project-id "ironwolf")
    (contacts "Arvydas Silanskas")
    (display? #f)
    (dns (A "89.40.10.46")
         (AAAA "2a02:7b40:5928:a2e::1")))

   ((project-id "tuonela")
    (contacts "Lassi")
    (display? #f)
    (dns (A "192.210.181.186"))))

  ("Redirect"

   ((project-id "redirect")
    (contacts "Lassi" "Arthur")
    (display? #f)
    (dns (CNAME "@"))))

  ("Aliases"

   ((project-id "blog")
    (display? #f)
    (dns (CNAME "planet")))

   ((project-id "doc")
    (display? #f)
    (dns (CNAME "docs")))

   ((project-id "list")
    (display? #f)
    (dns (CNAME "lists")))

   ((project-id "play")
    (display? #f)
    (dns (CNAME "try"))))))
