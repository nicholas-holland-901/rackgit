#! /usr/bin/env racket
#lang racket


(require file/sha1)
(require file/gzip)
(require net/http-client)
(require net/base64)
(require racket/system)


(define username "nicholas-holland-901")
(define url void)
(define repo-name "rackgit")

(define author-name "Nicholas Holland")
(define committer-name "Nicholas Holland")
(define email "the.nicholas.holland@gmail.com")
(define token (getenv "REPO_TOKEN"))

;; Get input from command line
(define args (current-command-line-arguments))


;; Convert arguments to list
(define list-args (vector->list args))


;; Object can have type "blob ", "commit ", or "tree "
(define obj-blob "blob ")
(define obj-commit "commit ")
(define obj-tree "tree ")


;;add-to-path: path string -> path
;;purpose: add to file path
(define add-to-path
  (lambda (p s)
    (string->path
     (string-append (path->string p)
                    s))))


;;combine-objects: (listof hash) -> string
;;(define (combine-objects lst)
;;  ...)


;;try-get-second: (listof string) -> string
;;purpose: return second item in list if exists, otherwise return empty string
(define try-get-second
  (lambda (lst)
    (if ( >= (length lst) 2)
        (second lst)
        "")))


;;read-file-bytestr: path -> byte-string
;;purpose: read contents from file as byte-string
(define read-file-bytestr
  (lambda (p)
    (with-input-from-file p
      (lambda ()
        (read-bytes (file-size p))))))


;;find-hash-object: string -> path
;;purpose: find file in .git folder given sha-1 hash prefix
(define (find-hash-object hash)
  (local [(define possible-path (add-to-path (current-directory) (string-append 
                                                                  ".git/objects/" 
                                                                  (substring hash 0 2) 
                                                                  "/" 
                                                                  (substring hash 2))))]
    (if (file-exists? possible-path)
        possible-path
        (printf (string-append "File '" 
                               hash 
                               "' not found.")))))


;;hash-object: string path -> hex-string
;;purpose: returns sha-1 hash of the given object
(define (hash-object p-o p)
  (bytes->hex-string (sha1-bytes (string->bytes/utf-8 (string-append p-o
                                                                     (number->string (file-size p))
                                                                     "\0"
                                                                     (bytes->string/utf-8 (read-file-bytestr p)))))))


;;store-object: string path -> void
;;purpose: stores an object in the git object database given its type and path
(define (store-object p-o p)
  (local [(define hash (hash-object p-o p))]
    (define new-dir (make-directory* (add-to-path (current-directory)
                                                  (string-append ".git/objects/"
                                                                 (substring (hash-object p-o p)
                                                                            0
                                                                            2)))))
    (with-output-to-file (add-to-path (current-directory)
                                      (string-append ".git/objects/"
                                                     (substring (hash-object p-o p)
                                                                0
                                                                2)
                                                     "/"
                                                     (substring (hash-object p-o p)
                                                                2)))
      (lambda () (write-bytes (string->bytes/utf-8 (string-append p-o
                                                                  (number->string (file-size p))
                                                                  "\0"
                                                                  (bytes->string/utf-8 (read-file-bytestr p))))))
      #:mode 'binary
      #:exists 'replace)))


;;add-to-index: string path -> void
;;purpose: adds file to git index given its type and path
(define (add-to-index p-o p) 
  (local [(define line-to-add (string-append (path->string p) 
                                             " " 
                                             (hash-object p-o 
                                                          p)
                                             "\n"))]
    (with-output-to-file (add-to-path (current-directory) 
                                      ".git/index")
      (lambda () 
        (display line-to-add))
      #:exists 'append)))


;;add: string -> void
;;purpose: adds given file to staging area by storing as blob and adding to index
(define (add p)
  (local [(define path (string->path (first p)))]
    (store-object obj-blob 
                  path)
    (add-to-index obj-blob 
                  path)))


;;help: string -> void
;;purpose: prints the explanation of inputted procedure
(define (help lst-args)
  (local [(define sarg (try-get-second lst-args))]
    (cond [(equal? "help" sarg) (printf "Prints the explanation of inputted procedure.\n")]
          [(equal? "clone" sarg) (printf "Creates version of repository specified by link on local computer.")]
          [(equal? "init" sarg) (printf "Initializes the repository by setting up '.git' folder with necessary components for managing the repo.\n")]
          [(equal? "push" sarg) (printf "Pushes local changes to a remote repository.\n")]
          [(equal? "remote" sarg) (printf "Manages set of tracked repositories.\n")]
          [(equal? "add" sarg) (printf "Adds given file to staging area.\n")]
          [(equal? "" sarg) (printf "Command argument missing\n")]
          [else (printf "Command not found")])))


;;init: repo -> void
;;purpose: initializes the repository with the files git needs to manage the repo
(define init
  (local [(define folders (list "objects" "refs" "refs/heads"))]
    (lambda() (make-directory (add-to-path (current-directory) ".git"))
      (with-output-to-file (add-to-path (current-directory) ".git/HEAD") (lambda () (display "ref: refs/heads/master")))
      (with-output-to-file (add-to-path (current-directory) ".git/index") (lambda () (display "")))
      (map (lambda (n)
             (make-directory* (add-to-path (current-directory)
                                           (string-append ".git/"
                                                          n)))) folders))))


;;cat-file: string string -> void
;;purpose: display information about a file
(define (cat-file type sha1-hash)
  (local [(define file-path (find-hash-object sha1-hash))
          (define file-contents-decompressed
            (bytes->string/utf-8 (read-bytes (file-size file-path)(open-input-file
                                                                   file-path
                                                                   #:mode 'binary))))
          (define list-of-file-contents (string-split (first (string-split file-contents-decompressed
                                                                           "\0"))
                                                      " "))]
    (if (file-exists? file-path)
        (cond [(equal? type "-t") (printf (string-append "Object type: "
                                                         (first list-of-file-contents)))] ;; display object type
              [(equal? type "-s") (printf (string-append "Object size: "
                                                         (second list-of-file-contents)
                                                         " bytes"))] ;; display object size
              [(equal? type "-e")] ;; exit of object exists, otherwise error (already does this)
              [(equal? type "-p") (printf (string-append "Object type: "
                                                         (first list-of-file-contents)
                                                         "\n"
                                                         "Object size: "
                                                         (second list-of-file-contents)
                                                         " bytes"))] ;; pretty-print object contents
              [else (printf "Invalid type.")])
        (printf "File not found."))))


;;store-easy: hash string -> void
;;purpose: stores an object in the git object database given its hash and contents
(define (store-easy tree-hash contents)
  (define new-dir (make-directory* (add-to-path (current-directory)
                                                (string-append ".git/objects/"
                                                               (substring tree-hash
                                                                          0
                                                                          2)))))
  (with-output-to-file (add-to-path (current-directory)
                                    (string-append ".git/objects/"
                                                   (substring tree-hash
                                                              0
                                                              2)
                                                   "/"
                                                   (substring tree-hash
                                                              2)))
    (lambda () (write-bytes (string->bytes/utf-8 contents)))
    #:mode 'binary
    #:exists 'replace))


;;create-tree: void -> hash
;;purpose: create a tree to represent file structure of project based on index
(define (create-tree-from-index)
  (local [(define contents "")]
    (with-input-from-file (add-to-path (current-directory) "/.git/index")
      (lambda () 
        (for ([line (in-lines)])
          (local [(define split-line (string-split line " "))
                  (define file-path (first split-line))
                  (define file-name (last (string-split file-path "/")))
                  (define file-hash (second split-line))
                  (define t-line (string-append "100664 " obj-blob file-hash " " file-name "\0"))]
            (set! contents (string-append contents t-line))))))
    (local [(define contents-size (bytes-length (string->bytes/utf-8 contents)))
            (define seq (string-append obj-tree " " (number->string contents-size) "\0" contents))
            (define tree-hash (bytes->hex-string (sha1-bytes (string->bytes/utf-8 seq))))]
      (store-easy tree-hash contents)
      tree-hash)))


;;commit: msg -> void
;;purpose: create commit object and point to newly created tree
(define (commit msg)
  (local [(define tree-hash-data (create-tree-from-index))]

    ;;make root tree to point to data tree (the one just created)
    (local [(define contents (string-append "040000 " obj-tree tree-hash-data))
            (define contents-size (bytes-length (string->bytes/utf-8 contents)))
            (define seq (string-append obj-tree " " (number->string contents-size) "\0" contents))
            (define tree-hash-root (bytes->hex-string (sha1-bytes (string->bytes/utf-8 seq))))
            (define tree-root-hash (store-easy tree-hash-root contents))
            (define current-time (current-seconds))
            (define commit-object-contents (string-append obj-tree 
                                                          tree-root-hash 
                                                          "\n" 
                                                          "author " 
                                                          author-name 
                                                          "<"email">" 
                                                          current-time 
                                                          "-0500" 
                                                          "\n" 
                                                          "committer" 
                                                          committer-name 
                                                          "<"email">" 
                                                          current-time 
                                                          "-0500\n\n" 
                                                          msg))
            (define commit-hash (bytes->hex-string (sha1-bytes (string->bytes/utf-8 commit-object-contents))))]

      (store-easy commit-hash commit-object-contents)

      ;;store hash of commit object to header??

      )))


;;curl-receive: void -> string
;; purpose: get refs from github repo
(define (curl-receive)
  (define command
    (string-append
     "curl.exe -v -H \"Git-Protocol: version=2\" \"https://github.com/"
     username
     "/"
     repo-name
     ".git/info/refs?service=git-upload-pack\""))
  (define output
    (with-output-to-string
      (lambda () (system command))))
  output)  


;;remote: (listof String) -> void
;;purpose: set or receive remote url
(define (remote lst-args)
  (cond [(and (equal? (length lst-args)
                      1)
              (equal? (first
                       lst-args)
                      "-v")) (if (void? url)
                                 (printf (string-append url "\n"))
                                 (printf "No url set"))]
        [(and (equal? (length lst-args)
                      3)
              (equal? (list (first lst-args)
                            (second lst-args))
                      (list "set-url" "origin")))
         (set! url (third lst-args))]
        [else (printf lst-args)]))


;; Check command against available options
(define (main lst-args)
  (print token)
  (if (empty? lst-args)
      (help '("rackgit"))
      (local [(define farg (first lst-args))]
        (cond [(equal? "help" farg) (help lst-args)]
              [(equal? "init" farg) (init)]
              [(equal? "pull" farg) (curl-receive)]
              [(equal? "remote" farg) (remote (rest lst-args))]
              [(equal? "add" farg) (add (rest lst-args))]
              [(equal? "commit" farg) (commit (rest lst-args))]
              [else (printf "Command not found\n")]))))


(main list-args)