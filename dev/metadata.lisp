;;;-*- Mode: Lisp; Package: regenerate-websites -*-

(in-package #:regenerate-websites)

(setf *metabang-common-lisp-systems*
      (sort
       (mapcar (lambda (data)
		 (apply #'make-instance 'metabang-system data))
	       '((:key :asdf :name "ASDF"
		  :short-description "Another System Definition Facility"
		  :build-documentation? nil
		  :homepage "http://common-lisp.net/project/asdf"
;;		  :home-directory "user-home:repository;asdf;"
		  :home-directory "/repository/git/asdf/"
		  :load-system? nil
		  :vcs git)
                 
		 (:key :asdf-binary-locations :name "ASDF-Binary-Locations"
		  :short-description "Put Lisp binaries in their places"
		  :build-documentation? nil
		  :homepage "http://common-lisp.net/project/asdf-binary-locations")
                   
		 (:key :asdf-install-tester
		  :name "ASDF-Install-Tester"
		  :root "http://common-lisp.net/project/ait/"
		  :short-description "Test ASDF Installable systems automagically"
		  :build-documentation? t
		  :home-directory "ait"
		  :folder ""
		  :homepage "http://common-lisp.net/project/tinaa/darcs/ait")
                   
		 (:key :asdf-status :name "ASDF-Status"
		  :sub-folder "cl-containers"
		  :short-description "Display ASDF-Install-testers results nicely"
		  :homepage "http://common-lisp.net/project/cl-containers/asdf-status/darcs/asdf-status")
                   
		 (:key :asdf-system-connections :name "ASDF-System-Connections"
		  :sub-folder "cl-containers"
		  :short-description "Link ASDF systems together declaratively"
		  :build-documentation? t
		  :homepage "http://common-lisp.net/project/cl-containers/asdf-system-connections/darcs/asdf-system-connections")
                   
		 (:key :bundler :name "Bundler"
		  :sub-folder "bundler"
		  :short-description "Bundle many ASDF systems into one"
		  :build-documentation? nil
		  :metabang-software? t
		  :homepage "http://common-lisp.net/project/cl-containers/bundler/darcs/bundler")
                   
		 (:key :cl-containers :name "CL-Containers"
		  :vcs git
		  :short-description "Common-Lisp's answer to STL and Smalltalk"
		  :build-documentation? t
		  :home-directory "user-home:repository;cl-containers;"
		  :homepage "http://common-lisp.net/project/cl-containers")
                   
		 (:key :cl-graph :name "CL-Graph"
		  :vcs git
		  :short-description "Utilities and algorithms for Graph manipulation"
		  :build-documentation? t
		  :home-directory "user-home:repository;cl-graph;"
		  :homepage "http://common-lisp.net/project/cl-graph")
                   
		 (:key :cl-markdown :name "CL-Markdown"
		  :vcs git
		  :short-description "Common Lisp version of the Markdown text processing langauge"
		  :build-documentation? t
		  :home-directory "user-home:repository;cl-markdown;"
		  :homepage "http://common-lisp.net/project/cl-markdown")
                   
		 (:key :cl-mathstats :name "CL-MathStats"
		  :vcs git
		  :short-description "Miscellaneous math and statistics utilities"
		  :build-documentation? t
		  :home-directory "user-home:repository;cl-mathstats;"
		  :homepage "http://common-lisp.net/project/cl-mathstats")
                   
		 (:key :cl-variates :name "CL-Variates"
		  :short-description "Portable Random Number Generators and tools."
		  :vcs git
		  :build-documentation? t
		  :home-directory "user-home:repository;cl-variates;"
		  :homepage "http://common-lisp.net/project/cl-variates")
                   
		 (:key :clnuplot :name "CLNUPlot"
		  :short-description "Common Lisp interface for GNUPlot"
		  :build-documentation? t
		  :metabang-software? nil
		  :vcs git
		  :home-directory "user-home:repository;clnuplot;"
		  :homepage "http://common-lisp.net/project/cl-containers/clnuplot/darcs/clnuplot")
                   
		 (:key :defsystem-compatibility :name "defsystem-compatibility"
		  :sub-folder "cl-containers"
		  :short-description "Help different system definers to live together."
		  :build-documentation? t
		  :homepage "http://common-lisp.net/project/cl-containers/defsystem-compatibility/darcs/defsystem-compatibility")
                   
		 (:key :e8el :name "Enterprise Lisp"
		  :root "http://www.enterpriselisp.com/"
		  :home-directory "enterpriselisp"
		  :metabang-software? nil
		  :build-documentation? nil
		  )
                   
		 (:key :lift :name "LIFT"
		  :short-description "the LIsp Framework for Testing"
		  :build-documentation? t
		  :documentation-package lift
		  :homepage "http://common-lisp.net/project/lift/"
		  :home-directory "user-home:repository;lift;"
		  :vcs git)
                   
		 (:key :log5 :name "log5"
		  :short-description "Common Lisp Logging - It's one more"
		  :build-documentation? t
		  :homepage "http://common-lisp.net/project/log5/darcs/log5"
		  :home-directory "user-home:repository;log5;"
		  :vcs git)


		 (:key :metacopy :name "metacopy"
		  :short-description "Shallow and deep copy toolkit"
		  :build-documentation? t
		  :homepage "http://common-lisp.net/project/metacopy/darcs/metacopy")
                   
		 (:key :metatilities :name "Metatilities" 
		  :short-description "Various useful utilities"
		  :build-documentation? t
		  :vcs git
		  :home-directory "user-home:repository;metatilities;"
		  :homepage "http://common-lisp.net/project/metatilities")
                   
		 (:key :moptilities :name "Moptilities"
		  :short-description "Implementation independent MOP utilities"
		  :build-documentation? t
		  :vcs git
		  :home-directory "user-home:repository;moptilities;"
		  :homepage "http://common-lisp.net/project/moptilities")
                   
		 (:key :metabang-bind :name "metabang-bind" 
		  :cliki "bind"
		  :short-description "Handle destructuring, multiple-values and let simultaneously"
		  :build-documentation? t
		  ;:home-directory "metabang-bind"
		  :homepage "http://common-lisp.net/project/metabang-bind"
		  :home-directory "user-home:repository;metabang-bind;"
		  :vcs git)
                   
		 (:key :simple-advice :name "simple-advice"
		  :short-description "Portable Common Lisp advice facility"
		  :build-documentation? nil
		  :homepage "http://common-lisp.net/project/simple-advice/")

		 (:key :simple-http :name "Simple HTTP" 
		  :short-description "Not as trivial as trivial HTTP, but still simple..."
		  :build-documentation? t
		  :homepage "http://common-lisp.net/project/simple-http"
		  :e8el? t)

		 (:key :trivial-http :name "Trivial HTTP" 
		  :short-description "You say simple, I say trivial"
		  :build-documentation? t
		  :vcs git
		  :home-directory "user-home:repository;trivial-http;"
		  :homepage "http://common-lisp.net/project/trivial-http"
		  :e8el? t)

		 (:key :geohash :name "Geohash in Common Lisp" 
		  :short-description "Implements the geohash.org algorithm"
		  :build-documentation? t
		  :homepage "http://common-lisp.net/project/geohash"
		  :e8el? t)

		 (:key :system-check :name "System-check" 
		  :short-description "Keeping your systems up to date"
		  :root "http://metabang.gotdns.com/software/" 
		  :build-documentation? t
		  :homepage "http://metabang.gotdns.com/software/system-check/darcs/system-check"
		  :e8el? t)
                   
		 (:key :tinaa :name "TINAA"
		  :short-description "Common-Lisp documentation tool"
		  :build-documentation? t
		  :vcs git
		  :home-directory "user-home:repository;tinaa;"
		  :homepage "http://common-lisp.net/project/tinaa/")
                   
		 (:key :trivial-shell :name "trivial-shell"
		  :short-description "One shell to run them all"
		  :build-documentation? t
		  :homepage "http://common-lisp.net/project/trivial-shell/"
		  :home-directory "user-home:repository;trivial-shell;"
		  :vcs git)

		 (:key :trivial-timeout :name "trivial-timeout"
		  :short-description "Because sometimes you can't wait"
		  :build-documentation? t
		  :homepage "http://common-lisp.net/project/trivial-timeout/")     
		                
		 (:key :metasite :name "metabang.com" 
		  :root "http://www.metabang.com/"
		  :folder ""
		  :metabang-software? nil)
                   
		 (:key :metabang-local :name "metabang.com" 
		  :root "http://metabang.gotdns.com/"
		  :folder ""
		  :home-directory "metasite;metabang-local"
		  :metabang-software? nil)             
                   
		 (:key :cl-html-parse :name "Common Lisp HTML Parser" 
		  :short-description "A portable version of Franz's Opensource HTML Parser"
		  :metabang-software? nil
		  :asdf-packaging? t
		  :build-website? nil
		  :build-documentation? t
		  :homepage "http://metabang.gotdns.com/software/system-check/darcs/system-check"
		  :e8el? t)
                   
		 (:key :portable-threads :name "Portable Threads"
		  :root "http://gbbopen.org/hyperdoc/ref-portable-thread-entities.html"
		  :short-description "A Portable Thread Library originally developed for GBBOpen."
		  :folder ""
		  :metabang-software? nil
		  :asdf-packaging? t
		  :build-website? nil)
                   
		 (:key :asdf-install :name "ASDF-Install"
		  :short-description "A tool for downloading and installing lisp libraries and packages."
		  :metabang-software? nil
		  :asdf-packaging? t
		  :build-website? t
		  :homepage "http://common-lisp.net/project/asdf-install/"
		  :home-directory "user-home:repository;asdf-install;"
		  :vcs git)

		 (:key :dynamic-classes :name "dynamic-classes"
		  :short-description "Classes the way you want them"
		  :build-documentation? t
		  :vcs git
		  :home-directory "user-home:repository;dynamic-classes;"
		  :homepage "http://common-lisp.net/project/dynamic-classes")
		   
		 (:key :metatilities-base :name "metatilities-base"
		  :short-description "something to stand on"
		  :build-documentation? t
		  :vcs git
		  :home-directory "user-home:repository;metatilities-base;"
		  :homepage "http://common-lisp.net/project/metatilities-base/")

		 (:key :docudown :name "docudown"
		  :short-description "CL-Markdown does documentation, details at 11"
		  :build-documentation? t
		  :homepage "http://common-lisp.net/project/docudown/")

		 (:key :trivial-backtrace :name "trivial-backtrace"
		  :short-description "a backrace for the rest of us."
		  :build-documentation? t
		  :home-directory "user-home:repository;trivial-backtrace;"
		  :homepage "http://common-lisp.net/project/trivial-backtrace/"
		  :vcs git)))
       #'string-lessp
       :key #'key))
