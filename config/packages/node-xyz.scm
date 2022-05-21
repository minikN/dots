(define-module (config packages node-xyz)
  #:use-module (gnu packages node)
  #:use-module (gnu packages node-xyz)
  #:use-module (guix build-system node)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public node-commander-9.2.0
  (package
   (name "node-commander")
   (version "9.2.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/commander/-/commander-9.2.0.tgz")
     (sha256
      (base32
       "0ksgh8v1rzljkmpnhrkghw75n2qx4m4rdvips0dpi6n7h65pskka"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/tj/commander.js#readme")
   (synopsis
    "the complete solution for node.js command-line programs")
   (description
    "the complete solution for node.js command-line programs")
   (license license:expat)))

(define-public node-jsonfile-6.1.0
  (package
   (name "node-jsonfile")
   (version "6.1.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/jsonfile/-/jsonfile-6.1.0.tgz")
     (sha256
      (base32
       "1csrz2dy4chva2qzjxpx6jxjbxqqm6jr64vb2zc3y4cj7b9yxn0b"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-graceful-fs" ,node-graceful-fs-4.2.10)
      ("node-universalify" ,node-universalify-2.0.0)))
   (home-page
    "https://github.com/jprichardson/node-jsonfile#readme")
   (synopsis "Easily read/write JSON files.")
   (description "Easily read/write JSON files.")
   (license license:expat)))

(define-public node-universalify-2.0.0
  (package
   (name "node-universalify")
   (version "2.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/universalify/-/universalify-2.0.0.tgz")
     (sha256
      (base32
       "10a8wqni1k8rgcwxsb7x8ryjz5w33mwg856yb4qnf7ld52jgf527"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/RyanZim/universalify#readme")
   (synopsis
    "Make a callback- or promise-based function support both promises and callbacks.")
   (description
    "Make a callback- or promise-based function support both promises and callbacks.")
   (license license:expat)))

(define-public node-fs-extra-10.1.0
  (package
   (name "node-fs-extra")
   (version "10.1.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/fs-extra/-/fs-extra-10.1.0.tgz")
     (sha256
      (base32
       "1shzjwi0jj6haqwji5mc45nb08gw0zyq6gy26mmv0sw65s2ngajh"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-universalify" ,node-universalify-2.0.0)
      ("node-jsonfile" ,node-jsonfile-6.1.0)
      ("node-graceful-fs" ,node-graceful-fs-4.2.10)))
   (home-page
    "https://github.com/jprichardson/node-fs-extra")
   (synopsis
    "fs-extra contains methods that aren't included in the vanilla Node.js fs package. Such as recursive mkdir, copy, and remove.")
   (description
    "fs-extra contains methods that aren't included in the vanilla Node.js fs package. Such as recursive mkdir, copy, and remove.")
   (license license:expat)))

(define-public node-p-debounce-2.1.0
  (package
   (name "node-p-debounce")
   (version "2.1.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/p-debounce/-/p-debounce-2.1.0.tgz")
     (sha256
      (base32
       "0wb1sa0xd7hzp318mymdas72lmk7mk7ff3vxm5k77r41s21nhz8n"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/sindresorhus/p-debounce#readme")
   (synopsis
    "Debounce promise-returning & async functions")
   (description
    "Debounce promise-returning & async functions")
   (license license:expat)))

(define-public node-p-try-2.2.0
  (package
   (name "node-p-try")
   (version "2.2.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/p-try/-/p-try-2.2.0.tgz")
     (sha256
      (base32
       "141pf5z1f3xmm5c0fdrfddsf7xfigjxfl103zh59bpwrk2wb5453"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/sindresorhus/p-try#readme")
   (synopsis "`Start a promise chain")
   (description "`Start a promise chain")
   (license license:expat)))

(define-public node-p-limit-2.3.0
  (package
   (name "node-p-limit")
   (version "2.3.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/p-limit/-/p-limit-2.3.0.tgz")
     (sha256
      (base32
       "15djin88kfxjdvzd7f2gnwblgclqljzqxiidm1pmrsyg14j4ajrq"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs `(("node-p-try" ,node-p-try-2.2.0)))
   (home-page
    "https://github.com/sindresorhus/p-limit#readme")
   (synopsis
    "Run multiple promise-returning & async functions with limited concurrency")
   (description
    "Run multiple promise-returning & async functions with limited concurrency")
   (license license:expat)))

(define-public node-p-locate-3.0.0
  (package
   (name "node-p-locate")
   (version "3.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/p-locate/-/p-locate-3.0.0.tgz")
     (sha256
      (base32
       "1fbvw7ka1lgrhr7kynsjv7iqw1sdqqrh088py2r4kyjhbl8xzq70"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs `(("node-p-limit" ,node-p-limit-2.3.0)))
   (home-page
    "https://github.com/sindresorhus/p-locate#readme")
   (synopsis
    "Get the first fulfilled promise that satisfies the provided testing function")
   (description
    "Get the first fulfilled promise that satisfies the provided testing function")
   (license license:expat)))

(define-public node-path-exists-3.0.0
  (package
   (name "node-path-exists")
   (version "3.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/path-exists/-/path-exists-3.0.0.tgz")
     (sha256
      (base32
       "0b9j0s6mvbf7js1fsga1jx4k6c4k17yn9c1jlaiziqkmvi98gxyp"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/sindresorhus/path-exists#readme")
   (synopsis "Check if a path exists")
   (description "Check if a path exists")
   (license license:expat)))

(define-public node-locate-path-3.0.0
  (package
   (name "node-locate-path")
   (version "3.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/locate-path/-/locate-path-3.0.0.tgz")
     (sha256
      (base32
       "1ghmaifkp6r47h6ygdgkf7srvxhc5qwhgjwq00ia250kpxww5xdm"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-path-exists" ,node-path-exists-3.0.0)
      ("node-p-locate" ,node-p-locate-3.0.0)))
   (home-page
    "https://github.com/sindresorhus/locate-path#readme")
   (synopsis
    "Get the first path that exists on disk of multiple paths")
   (description
    "Get the first path that exists on disk of multiple paths")
   (license license:expat)))

(define-public node-find-up-3.0.0
  (package
   (name "node-find-up")
   (version "3.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/find-up/-/find-up-3.0.0.tgz")
     (sha256
      (base32
       "0ln7mwc9b465l646xvjd0692yy6izi27xb4y7g8ffpvjl7s9fx4r"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-locate-path" ,node-locate-path-3.0.0)))
   (home-page
    "https://github.com/sindresorhus/find-up#readme")
   (synopsis
    "Find a file or directory by walking up parent directories")
   (description
    "Find a file or directory by walking up parent directories")
   (license license:expat)))

(define-public node-pkg-up-3.1.0
  (package
   (name "node-pkg-up")
   (version "3.1.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/pkg-up/-/pkg-up-3.1.0.tgz")
     (sha256
      (base32
       "033ncy995cs401ywvrdjdcdi53c9hn2i0065aggyhlcssinbzmxl"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs `(("node-find-up" ,node-find-up-3.0.0)))
   (home-page
    "https://github.com/sindresorhus/pkg-up#readme")
   (synopsis "Find the closest package.json file")
   (description
    "Find the closest package.json file")
   (license license:expat)))

(define-public node-yallist-4.0.0
  (package
   (name "node-yallist")
   (version "4.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/yallist/-/yallist-4.0.0.tgz")
     (sha256
      (base32
       "0jadz9mh1lzfk19bvqqlrg40ggfk2yyfyrpgj5c62dk54ym7h358"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/isaacs/yallist#readme")
   (synopsis "Yet Another Linked List")
   (description "Yet Another Linked List")
   (license license:isc)))

(define-public node-lru-cache-6.0.0
  (package
   (name "node-lru-cache")
   (version "6.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/lru-cache/-/lru-cache-6.0.0.tgz")
     (sha256
      (base32
       "0pnziizgv8jpg708ykywcjby0syjz1l2ll1j727rdxhw0gmhvr2w"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs `(("node-yallist" ,node-yallist-4.0.0)))
   (home-page
    "https://github.com/isaacs/node-lru-cache#readme")
   (synopsis
    "A cache object that deletes the least-recently-used items.")
   (description
    "A cache object that deletes the least-recently-used items.")
   (license license:isc)))

(define-public node-semver-7.3.7
  (package
   (name "node-semver")
   (version "7.3.7")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/semver/-/semver-7.3.7.tgz")
     (sha256
      (base32
       "0x5pd50bcnim1inm4wgdnc7829ra14ss7qcdn80mdf39a3d85qlg"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-lru-cache" ,node-lru-cache-6.0.0)))
   (home-page
    "https://github.com/npm/node-semver#readme")
   (synopsis
    "The semantic version parser used by npm.")
   (description
    "The semantic version parser used by npm.")
   (license license:isc)))

(define-public node-array-union-2.1.0
  (package
   (name "node-array-union")
   (version "2.1.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/array-union/-/array-union-2.1.0.tgz")
     (sha256
      (base32
       "1ih8b5i4b06l71652xm8r89h89vdj8vp648s2a5bgr4kbzh4kmx8"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/sindresorhus/array-union#readme")
   (synopsis
    "Create an array of unique values, in order, from the input arrays")
   (description
    "Create an array of unique values, in order, from the input arrays")
   (license license:expat)))

(define-public node-path-type-4.0.0
  (package
   (name "node-path-type")
   (version "4.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/path-type/-/path-type-4.0.0.tgz")
     (sha256
      (base32
       "15wvcgwg053hr2h11ja5swvdz3vvxciqq5aad0ara9qmzgwfh9f0"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/sindresorhus/path-type#readme")
   (synopsis
    "Check if a path is a file, directory, or symlink")
   (description
    "Check if a path is a file, directory, or symlink")
   (license license:expat)))

(define-public node-dir-glob-3.0.1
  (package
   (name "node-dir-glob")
   (version "3.0.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/dir-glob/-/dir-glob-3.0.1.tgz")
     (sha256
      (base32
       "0wj53iqp275dlsg7v36kxv97fid5pan2girgnhbdqw0vj8qplmkp"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-path-type" ,node-path-type-4.0.0)))
   (home-page
    "https://github.com/kevva/dir-glob#readme")
   (synopsis
    "Convert directories to glob compatible strings")
   (description
    "Convert directories to glob compatible strings")
   (license license:expat)))

(define-public node-nodelib-fs-stat-2.0.5
  (package
   (name "node-nodelib-fs-stat")
   (version "2.0.5")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/@nodelib/fs.stat/-/fs.stat-2.0.5.tgz")
     (sha256
      (base32
       "0sqkaapvl86zldyw00j920hv4yncwb14nbgwnf3wl6pja4sm7y6q"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://www.npmjs.com/package/node-nodelib-fs-stat")
   (synopsis
    "Get the status of a file with some features")
   (description
    "Get the status of a file with some features")
   (license license:expat)))

(define-public node-queue-microtask-1.2.3
  (package
   (name "node-queue-microtask")
   (version "1.2.3")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/queue-microtask/-/queue-microtask-1.2.3.tgz")
     (sha256
      (base32
       "1kcyybqa9jqb339mr9i9fxa7b6pn7fl5fzm278a5x1h1l8h0mbzz"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/feross/queue-microtask")
   (synopsis
    "fast, tiny `queueMicrotask` shim for modern engines")
   (description
    "fast, tiny `queueMicrotask` shim for modern engines")
   (license license:expat)))

(define-public node-run-parallel-1.2.0
  (package
   (name "node-run-parallel")
   (version "1.2.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/run-parallel/-/run-parallel-1.2.0.tgz")
     (sha256
      (base32
       "1j3syw7nnhr98sr9jngzmgqj33khjnl5rimhgbpih2vy6zsk38kb"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-queue-microtask"
       ,node-queue-microtask-1.2.3)))
   (home-page
    "https://github.com/feross/run-parallel")
   (synopsis
    "Run an array of functions in parallel")
   (description
    "Run an array of functions in parallel")
   (license license:expat)))

(define-public node-nodelib-fs-scandir-2.1.5
  (package
   (name "node-nodelib-fs-scandir")
   (version "2.1.5")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/@nodelib/fs.scandir/-/fs.scandir-2.1.5.tgz")
     (sha256
      (base32
       "0k7r1kjscdfbm2ckdgvq13zgycd4mci1admxn3dqp3n72yivy959"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-run-parallel" ,node-run-parallel-1.2.0)
      ("node-nodelib-fs-stat"
       ,node-nodelib-fs-stat-2.0.5)))
   (home-page
    "https://www.npmjs.com/package/node-nodelib-fs-scandir")
   (synopsis
    "List files and directories inside the specified directory")
   (description
    "List files and directories inside the specified directory")
   (license license:expat)))

(define-public node-reusify-1.0.4
  (package
   (name "node-reusify")
   (version "1.0.4")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/reusify/-/reusify-1.0.4.tgz")
     (sha256
      (base32
       "1i1kl423618nfp3rjalyl810v7sxz2x04smrmfpafbzs2zahql5a"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/mcollina/reusify#readme")
   (synopsis
    "Reuse objects and functions with style")
   (description
    "Reuse objects and functions with style")
   (license license:expat)))

(define-public node-fastq-1.13.0
  (package
   (name "node-fastq")
   (version "1.13.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/fastq/-/fastq-1.13.0.tgz")
     (sha256
      (base32
       "1qy2wl0x9iakx3fd6sydj6c21lwvamm8v58632rznp2d41fm8bqa"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs `(("node-reusify" ,node-reusify-1.0.4)))
   (home-page
    "https://github.com/mcollina/fastq#readme")
   (synopsis "Fast, in memory work queue")
   (description "Fast, in memory work queue")
   (license license:isc)))

(define-public node-nodelib-fs-walk-1.2.8
  (package
   (name "node-nodelib-fs-walk")
   (version "1.2.8")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/@nodelib/fs.walk/-/fs.walk-1.2.8.tgz")
     (sha256
      (base32
       "0gbxfa920a6ykrl8a4phhvlwgybvivm2z10yyybww8mqd4gn5yfb"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-fastq" ,node-fastq-1.13.0)
      ("node-nodelib-fs-scandir"
       ,node-nodelib-fs-scandir-2.1.5)))
   (home-page
    "https://www.npmjs.com/package/node-nodelib-fs-walk")
   (synopsis
    "A library for efficiently walking a directory recursively")
   (description
    "A library for efficiently walking a directory recursively")
   (license license:expat)))

(define-public node-glob-parent-5.1.2
  (package
   (name "node-glob-parent")
   (version "5.1.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/glob-parent/-/glob-parent-5.1.2.tgz")
     (sha256
      (base32
       "1mfna9lpp82lapng0qq5x4x5j10nhimcx36lg4m5k4wbs7msy5ln"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs `(("node-is-glob" ,node-is-glob-4.0.3)))
   (home-page
    "https://github.com/gulpjs/glob-parent#readme")
   (synopsis
    "Extract the non-magic parent path from a glob string.")
   (description
    "Extract the non-magic parent path from a glob string.")
   (license license:isc)))

(define-public node-is-number-7.0.0
  (package
   (name "node-is-number")
   (version "7.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/is-number/-/is-number-7.0.0.tgz")
     (sha256
      (base32
       "07nmmpplsj1gxzng6fxhnnyfkif9fvhvxa89d5lrgkwqf42w2xbv"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/jonschlinkert/is-number")
   (synopsis
    "Returns true if a number or string value is a finite number. Useful for regex matches, parsing, user input, etc.")
   (description
    "Returns true if a number or string value is a finite number. Useful for regex matches, parsing, user input, etc.")
   (license license:expat)))

(define-public node-to-regex-range-5.0.1
  (package
   (name "node-to-regex-range")
   (version "5.0.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/to-regex-range/-/to-regex-range-5.0.1.tgz")
     (sha256
      (base32
       "1ms2bgz2paqfpjv1xpwx67i3dns5j9gn99il6cx5r4qaq9g2afm6"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-is-number" ,node-is-number-7.0.0)))
   (home-page
    "https://github.com/micromatch/to-regex-range")
   (synopsis
    "Pass two numbers, get a regex-compatible source string for matching ranges. Validated against more than 2.78 million test assertions.")
   (description
    "Pass two numbers, get a regex-compatible source string for matching ranges. Validated against more than 2.78 million test assertions.")
   (license license:expat)))

(define-public node-fill-range-7.0.1
  (package
   (name "node-fill-range")
   (version "7.0.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/fill-range/-/fill-range-7.0.1.tgz")
     (sha256
      (base32
       "0wp93mwfgzcddi6ii62qx7gb082jgh0rfq6pgvv2xndjyaygvk98"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-to-regex-range"
       ,node-to-regex-range-5.0.1)))
   (home-page
    "https://github.com/jonschlinkert/fill-range")
   (synopsis
    "Fill in a range of numbers or letters, optionally passing an increment or `step` to use, or create a regex-compatible range with `options.toRegex`")
   (description
    "Fill in a range of numbers or letters, optionally passing an increment or `step` to use, or create a regex-compatible range with `options.toRegex`")
   (license license:expat)))

(define-public node-braces-3.0.2
  (package
   (name "node-braces")
   (version "3.0.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/braces/-/braces-3.0.2.tgz")
     (sha256
      (base32
       "1kpaa113m54qc1n2zvs0p1ika4s9dzvcczlw8q66xkyliy982n3k"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-fill-range" ,node-fill-range-7.0.1)))
   (home-page
    "https://github.com/micromatch/braces")
   (synopsis
    "Bash-like brace expansion, implemented in JavaScript. Safer than other brace expansion libs, with complete support for the Bash 4.3 braces specification, without sacrificing speed.")
   (description
    "Bash-like brace expansion, implemented in JavaScript. Safer than other brace expansion libs, with complete support for the Bash 4.3 braces specification, without sacrificing speed.")
   (license license:expat)))

(define-public node-picomatch-2.3.1
  (package
   (name "node-picomatch")
   (version "2.3.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/picomatch/-/picomatch-2.3.1.tgz")
     (sha256
      (base32
       "07y1h9gbbdyjdpwb461x7dai0yg7hcyijxrcnpbr1h37r2gfw50v"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/micromatch/picomatch")
   (synopsis
    "Blazing fast and accurate glob matcher written in JavaScript, with no dependencies and full support for standard and extended Bash glob features, including braces, extglobs, POSIX brackets, and regular expressions.")
   (description
    "Blazing fast and accurate glob matcher written in JavaScript, with no dependencies and full support for standard and extended Bash glob features, including braces, extglobs, POSIX brackets, and regular expressions.")
   (license license:expat)))

(define-public node-micromatch-4.0.5
  (package
   (name "node-micromatch")
   (version "4.0.5")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/micromatch/-/micromatch-4.0.5.tgz")
     (sha256
      (base32
       "1axxwnl1i0mibpbir72nvz76dzi43cv8lhyzpxw95056mddnlmi0"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-picomatch" ,node-picomatch-2.3.1)
      ("node-braces" ,node-braces-3.0.2)))
   (home-page
    "https://github.com/micromatch/micromatch")
   (synopsis
    "Glob matching for javascript/node.js. A replacement and faster alternative to minimatch and multimatch.")
   (description
    "Glob matching for javascript/node.js. A replacement and faster alternative to minimatch and multimatch.")
   (license license:expat)))

(define-public node-fast-glob-3.2.11
  (package
   (name "node-fast-glob")
   (version "3.2.11")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/fast-glob/-/fast-glob-3.2.11.tgz")
     (sha256
      (base32
       "1p1sxg1vnnqc45m0xd8fliscv5vchd3m56yxkjz4zfxb6cz8k3v0"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-micromatch" ,node-micromatch-4.0.5)
      ("node-merge2" ,node-merge2-1.4.1)
      ("node-glob-parent" ,node-glob-parent-5.1.2)
      ("node-nodelib-fs-walk"
       ,node-nodelib-fs-walk-1.2.8)
      ("node-nodelib-fs-stat"
       ,node-nodelib-fs-stat-2.0.5)))
   (home-page
    "https://github.com/mrmlnc/fast-glob#readme")
   (synopsis
    "It's a very fast and efficient glob library for Node.js")
   (description
    "It's a very fast and efficient glob library for Node.js")
   (license license:expat)))

(define-public node-ignore-5.2.0
  (package
   (name "node-ignore")
   (version "5.2.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/ignore/-/ignore-5.2.0.tgz")
     (sha256
      (base32
       "1kaia5s9yhayx63kgqdrnk38l2cyvyp6al31g4qkhhcplmqsfkfn"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/kaelzhang/node-ignore#readme")
   (synopsis
    "Ignore is a manager and filter for .gitignore rules, the one used by eslint, gitbook and many others.")
   (description
    "Ignore is a manager and filter for .gitignore rules, the one used by eslint, gitbook and many others.")
   (license license:expat)))

(define-public node-merge2-1.4.1
  (package
   (name "node-merge2")
   (version "1.4.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/merge2/-/merge2-1.4.1.tgz")
     (sha256
      (base32
       "10bq7m23r366fk3r6j058i1l4jz6vn5xlxcfnp2mkj5kr68i4f5q"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/teambition/merge2")
   (synopsis
    "Merge multiple streams into one stream in sequence or parallel.")
   (description
    "Merge multiple streams into one stream in sequence or parallel.")
   (license license:expat)))

(define-public node-globby-11.1.0
  (package
   (name "node-globby")
   (version "11.1.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/globby/-/globby-11.1.0.tgz")
     (sha256
      (base32
       "1x7bjpz2s2j4y7hxc19vfj9rh3ck0693c9xa2z47qxa7b09xhx46"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-slash" ,node-slash-3.0.0)
      ("node-merge2" ,node-merge2-1.4.1)
      ("node-ignore" ,node-ignore-5.2.0)
      ("node-fast-glob" ,node-fast-glob-3.2.11)
      ("node-dir-glob" ,node-dir-glob-3.0.1)
      ("node-array-union" ,node-array-union-2.1.0)))
   (home-page
    "https://github.com/sindresorhus/globby#readme")
   (synopsis "User-friendly glob matching")
   (description "User-friendly glob matching")
   (license license:expat)))

(define-public node-graceful-fs-4.2.10
  (package
   (name "node-graceful-fs")
   (version "4.2.10")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/graceful-fs/-/graceful-fs-4.2.10.tgz")
     (sha256
      (base32
       "0vacs46qbczsrp1zgavd6rvyjm9riyzv45va5saqyrnncji5vl5r"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/isaacs/node-graceful-fs#readme")
   (synopsis
    "A drop-in replacement for fs, making various improvements.")
   (description
    "A drop-in replacement for fs, making various improvements.")
   (license license:isc)))

(define-public node-is-extglob-2.1.1
  (package
   (name "node-is-extglob")
   (version "2.1.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/is-extglob/-/is-extglob-2.1.1.tgz")
     (sha256
      (base32
       "06dwa2xzjx6az40wlvwj11vican2w46710b9170jzmka2j344pcc"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/jonschlinkert/is-extglob")
   (synopsis
    "Returns true if a string has an extglob.")
   (description
    "Returns true if a string has an extglob.")
   (license license:expat)))

(define-public node-is-glob-4.0.3
  (package
   (name "node-is-glob")
   (version "4.0.3")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/is-glob/-/is-glob-4.0.3.tgz")
     (sha256
      (base32
       "1imyq6pjl716cjc1ypmmnn0574rh28av3pq50mpqzd9v37xm7r1z"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-is-extglob" ,node-is-extglob-2.1.1)))
   (home-page
    "https://github.com/micromatch/is-glob")
   (synopsis
    "Returns `true` if the given string looks like a glob pattern or an extglob pattern. This makes it easy to create code that only uses external modules like node-glob when necessary, resulting in much faster code execution and initialization time, and a bet")
   (description
    "Returns `true` if the given string looks like a glob pattern or an extglob pattern. This makes it easy to create code that only uses external modules like node-glob when necessary, resulting in much faster code execution and initialization time, and a bet")
   (license license:expat)))

(define-public node-is-path-cwd-2.2.0
  (package
   (name "node-is-path-cwd")
   (version "2.2.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/is-path-cwd/-/is-path-cwd-2.2.0.tgz")
     (sha256
      (base32
       "0bs3dmrz6ifsyy8ivd43jy30cibxnpl1rx02dmrsnqyn2gcrqjw3"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/sindresorhus/is-path-cwd#readme")
   (synopsis
    "Check if a path is the current working directory")
   (description
    "Check if a path is the current working directory")
   (license license:expat)))

(define-public node-is-path-inside-3.0.3
  (package
   (name "node-is-path-inside")
   (version "3.0.3")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/is-path-inside/-/is-path-inside-3.0.3.tgz")
     (sha256
      (base32
       "0dipvy02ypbyz43gyvsp3hjgaqmxs4lpjzww2xlyj3x2wrgnb4gn"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/sindresorhus/is-path-inside#readme")
   (synopsis
    "Check if a path is inside another path")
   (description
    "Check if a path is inside another path")
   (license license:expat)))

(define-public node-clean-stack-2.2.0
  (package
   (name "node-clean-stack")
   (version "2.2.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/clean-stack/-/clean-stack-2.2.0.tgz")
     (sha256
      (base32
       "0manylf8kgqm9knb26lwxs7lfdf384r8hnxjwmhgzimq19k3fv05"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/sindresorhus/clean-stack#readme")
   (synopsis "Clean up error stack traces")
   (description "Clean up error stack traces")
   (license license:expat)))

(define-public node-indent-string-4.0.0
  (package
   (name "node-indent-string")
   (version "4.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/indent-string/-/indent-string-4.0.0.tgz")
     (sha256
      (base32
       "1822k378f65ipx6v9i132bywcnfjzk3rgilhnp443csfsz9p8sxw"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/sindresorhus/indent-string#readme")
   (synopsis "Indent each line in a string")
   (description "Indent each line in a string")
   (license license:expat)))

(define-public node-aggregate-error-3.1.0
  (package
   (name "node-aggregate-error")
   (version "3.1.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/aggregate-error/-/aggregate-error-3.1.0.tgz")
     (sha256
      (base32
       "0nr0ig4k5d5n019cjj4h4027316ppdjy8wnykv32b95bnnw0qdh3"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-indent-string" ,node-indent-string-4.0.0)
      ("node-clean-stack" ,node-clean-stack-2.2.0)))
   (home-page
    "https://github.com/sindresorhus/aggregate-error#readme")
   (synopsis "Create an error from multiple errors")
   (description
    "Create an error from multiple errors")
   (license license:expat)))

(define-public node-p-map-4.0.0
  (package
   (name "node-p-map")
   (version "4.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/p-map/-/p-map-4.0.0.tgz")
     (sha256
      (base32
       "147z64sp0ifrix961cr3f6sw2fs2wqx225d24mdbgvn3c5sj6c51"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-aggregate-error"
       ,node-aggregate-error-3.1.0)))
   (home-page
    "https://github.com/sindresorhus/p-map#readme")
   (synopsis "Map over promises concurrently")
   (description "Map over promises concurrently")
   (license license:expat)))

(define-public node-fs-realpath-1.0.0
  (package
   (name "node-fs-realpath")
   (version "1.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/fs.realpath/-/fs.realpath-1.0.0.tgz")
     (sha256
      (base32
       "174g5vay9jnd7h5q8hfdw6dnmwl1gdpn4a8sz0ysanhj2f3wp04y"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/isaacs/fs.realpath#readme")
   (synopsis
    "Use node's fs.realpath, but fall back to the JS implementation if the native one fails")
   (description
    "Use node's fs.realpath, but fall back to the JS implementation if the native one fails")
   (license license:isc)))

(define-public node-inflight-1.0.6
  (package
   (name "node-inflight")
   (version "1.0.6")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/inflight/-/inflight-1.0.6.tgz")
     (sha256
      (base32
       "16w864087xsh3q7f5gm3754s7bpsb9fq3dhknk9nmbvlk3sxr7ss"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-wrappy" ,node-wrappy-1.0.2)
      ("node-once" ,node-once-1.4.0)))
   (home-page "https://github.com/isaacs/inflight")
   (synopsis
    "Add callbacks to requests in flight to avoid async duplication")
   (description
    "Add callbacks to requests in flight to avoid async duplication")
   (license license:isc)))

(define-public node-inherits-2.0.4
  (package
   (name "node-inherits")
   (version "2.0.4")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/inherits/-/inherits-2.0.4.tgz")
     (sha256
      (base32
       "1bxg4igfni2hymabg8bkw86wd3qhhzhsswran47sridk3dnbqkfr"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/isaacs/inherits#readme")
   (synopsis
    "Browser-friendly inheritance fully compatible with standard node.js inherits()")
   (description
    "Browser-friendly inheritance fully compatible with standard node.js inherits()")
   (license license:isc)))

(define-public node-balanced-match-1.0.2
  (package
   (name "node-balanced-match")
   (version "1.0.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/balanced-match/-/balanced-match-1.0.2.tgz")
     (sha256
      (base32
       "1hdwrr7qqb37plj7962xbwjx1jvjz7ahl7iqrwh82yhcvnmzfm6q"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/juliangruber/balanced-match")
   (synopsis
    "Match balanced character pairs, like \"{\" and \"}\"")
   (description
    "Match balanced character pairs, like \"{\" and \"}\"")
   (license license:expat)))

(define-public node-concat-map-0.0.1
  (package
   (name "node-concat-map")
   (version "0.0.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/concat-map/-/concat-map-0.0.1.tgz")
     (sha256
      (base32
       "0qa2zqn9rrr2fqdki44s4s2dk2d8307i4556kv25h06g43b2v41m"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/substack/node-concat-map")
   (synopsis "concatenative mapdashery")
   (description "concatenative mapdashery")
   (license license:expat)))

(define-public node-brace-expansion-1.1.11
  (package
   (name "node-brace-expansion")
   (version "1.1.11")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/brace-expansion/-/brace-expansion-1.1.11.tgz")
     (sha256
      (base32
       "1nlmjvlwlp88knblnayns0brr7a9m2fynrlwq425lrpb4mcn9gc4"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-concat-map" ,node-concat-map-0.0.1)
      ("node-balanced-match"
       ,node-balanced-match-1.0.2)))
   (home-page
    "https://github.com/juliangruber/brace-expansion")
   (synopsis
    "Brace expansion as known from sh/bash")
   (description
    "Brace expansion as known from sh/bash")
   (license license:expat)))

(define-public node-minimatch-3.1.2
  (package
   (name "node-minimatch")
   (version "3.1.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/minimatch/-/minimatch-3.1.2.tgz")
     (sha256
      (base32
       "0kd3h6q90kvmzzw1v7cc3dr911gjkb9s547cdvfncfqanq84p5hk"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-brace-expansion"
       ,node-brace-expansion-1.1.11)))
   (home-page
    "https://github.com/isaacs/minimatch#readme")
   (synopsis "a glob matcher in javascript")
   (description "a glob matcher in javascript")
   (license license:isc)))

(define-public node-path-is-absolute-1.0.1
  (package
   (name "node-path-is-absolute")
   (version "1.0.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/path-is-absolute/-/path-is-absolute-1.0.1.tgz")
     (sha256
      (base32
       "0p7p04xxd8q495qhxmxydyjgzcf762dp1hp2wha2b52n3agp0vbf"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/sindresorhus/path-is-absolute#readme")
   (synopsis
    "Node.js 0.12 path.isAbsolute() ponyfill")
   (description
    "Node.js 0.12 path.isAbsolute() ponyfill")
   (license license:expat)))

(define-public node-glob-7.2.3
  (package
   (name "node-glob")
   (version "7.2.3")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/glob/-/glob-7.2.3.tgz")
     (sha256
      (base32
       "10a336nxv867xkjs3ipgbharwdzp5lnz7wr8viawn1lc66qqx8zh"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-path-is-absolute"
       ,node-path-is-absolute-1.0.1)
      ("node-once" ,node-once-1.4.0)
      ("node-minimatch" ,node-minimatch-3.1.2)
      ("node-inherits" ,node-inherits-2.0.4)
      ("node-inflight" ,node-inflight-1.0.6)
      ("node-fs-realpath" ,node-fs-realpath-1.0.0)))
   (home-page
    "https://github.com/isaacs/node-glob#readme")
   (synopsis "a little globber")
   (description "a little globber")
   (license license:isc)))

(define-public node-rimraf-3.0.2
  (package
   (name "node-rimraf")
   (version "3.0.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/rimraf/-/rimraf-3.0.2.tgz")
     (sha256
      (base32
       "0lkzjyxjij6ssh5h2l3ncp0zx00ylzhww766dq2vf1s7v07w4xjq"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs `(("node-glob" ,node-glob-7.2.3)))
   (home-page
    "https://github.com/isaacs/rimraf#readme")
   (synopsis
    "A deep deletion module for node (like `rm -rf`)")
   (description
    "A deep deletion module for node (like `rm -rf`)")
   (license license:isc)))

(define-public node-slash-3.0.0
  (package
   (name "node-slash")
   (version "3.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/slash/-/slash-3.0.0.tgz")
     (sha256
      (base32
       "01sxm2s0cvya4m52cb8w578a345nas5gwaw6dz7i2i9k9g8ig6pj"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/sindresorhus/slash#readme")
   (synopsis
    "Convert Windows backslash paths to slash paths")
   (description
    "Convert Windows backslash paths to slash paths")
   (license license:expat)))

(define-public node-del-6.1.0
  (package
   (name "node-del")
   (version "6.1.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/del/-/del-6.1.0.tgz")
     (sha256
      (base32
       "183a1wq6xs0jl5nkknkdp18530zjvh9py779wyr6n1bq7zvckmk2"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-slash" ,node-slash-3.0.0)
      ("node-rimraf" ,node-rimraf-3.0.2)
      ("node-p-map" ,node-p-map-4.0.0)
      ("node-is-path-inside"
       ,node-is-path-inside-3.0.3)
      ("node-is-path-cwd" ,node-is-path-cwd-2.2.0)
      ("node-is-glob" ,node-is-glob-4.0.3)
      ("node-graceful-fs" ,node-graceful-fs-4.2.10)
      ("node-globby" ,node-globby-11.1.0)))
   (home-page
    "https://github.com/sindresorhus/del#readme")
   (synopsis "Delete files and directories")
   (description "Delete files and directories")
   (license license:expat)))

(define-public node-is-stream-2.0.1
  (package
   (name "node-is-stream")
   (version "2.0.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/is-stream/-/is-stream-2.0.1.tgz")
     (sha256
      (base32
       "1mxnc5nlh73zg34vwdd9k5mx2wcs5pc84j8a2yig2y0bl9rgy09m"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/sindresorhus/is-stream#readme")
   (synopsis
    "Check if something is a Node.js stream")
   (description
    "Check if something is a Node.js stream")
   (license license:expat)))

(define-public node-temp-dir-2.0.0
  (package
   (name "node-temp-dir")
   (version "2.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/temp-dir/-/temp-dir-2.0.0.tgz")
     (sha256
      (base32
       "1hn8gx9dwlqhyxic2xka317anflf4dmqlq5frakrzv1l17fbyxsy"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/sindresorhus/temp-dir#readme")
   (synopsis
    "Get the real path of the system temp directory")
   (description
    "Get the real path of the system temp directory")
   (license license:expat)))

(define-public node-type-fest-0.16.0
  (package
   (name "node-type-fest")
   (version "0.16.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/type-fest/-/type-fest-0.16.0.tgz")
     (sha256
      (base32
       "1mj0c1ijia8q655dh1swhg4700lqy4j3f1v8an63fyx8bmi1wmm7"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/sindresorhus/type-fest#readme")
   (synopsis
    "A collection of essential TypeScript types")
   (description
    "A collection of essential TypeScript types")
   (license #f)))

(define-public node-crypto-random-string-2.0.0
  (package
   (name "node-crypto-random-string")
   (version "2.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/crypto-random-string/-/crypto-random-string-2.0.0.tgz")
     (sha256
      (base32
       "1angxzwxc9qiin2lbi5axp14aw2k3jdgbhc08fxp7h7hmnfk6s2m"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/sindresorhus/crypto-random-string#readme")
   (synopsis
    "Generate a cryptographically strong random string")
   (description
    "Generate a cryptographically strong random string")
   (license license:expat)))

(define-public node-unique-string-2.0.0
  (package
   (name "node-unique-string")
   (version "2.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/unique-string/-/unique-string-2.0.0.tgz")
     (sha256
      (base32
       "1dc7vv5pvkdn20jk4acimb432iias4jzy62acpk35nahxgksxglf"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-crypto-random-string"
       ,node-crypto-random-string-2.0.0)))
   (home-page
    "https://github.com/sindresorhus/unique-string#readme")
   (synopsis "Generate a unique random string")
   (description "Generate a unique random string")
   (license license:expat)))

(define-public node-tempy-1.0.1
  (package
   (name "node-tempy")
   (version "1.0.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/tempy/-/tempy-1.0.1.tgz")
     (sha256
      (base32
       "0gkh0l99c2278q4wngfxgv05fdihyylz5jnf7nkl6dfkma85s94z"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-unique-string" ,node-unique-string-2.0.0)
      ("node-type-fest" ,node-type-fest-0.16.0)
      ("node-temp-dir" ,node-temp-dir-2.0.0)
      ("node-is-stream" ,node-is-stream-2.0.1)
      ("node-del" ,node-del-6.1.0)))
   (home-page
    "https://github.com/sindresorhus/tempy#readme")
   (synopsis
    "Get a random temporary file or directory path")
   (description
    "Get a random temporary file or directory path")
   (license license:expat)))

(define-public node-vscode-jsonrpc-6.0.0
  (package
   (name "node-vscode-jsonrpc")
   (version "6.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/vscode-jsonrpc/-/vscode-jsonrpc-6.0.0.tgz")
     (sha256
      (base32
       "0g1v62c9dqzp9mdl9nz56ily9fj6b1n8k83xi28zd31xj4xqjiji"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/Microsoft/vscode-languageserver-node#readme")
   (synopsis
    "A json rpc implementation over streams")
   (description
    "A json rpc implementation over streams")
   (license license:expat)))

(define-public node-vscode-languageserver-types-3.16.0
  (package
   (name "node-vscode-languageserver-types")
   (version "3.16.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/vscode-languageserver-types/-/vscode-languageserver-types-3.16.0.tgz")
     (sha256
      (base32
       "0b3ajk37d3lr0w72cl03qc0ra5z15f90d1xf45fnpzd6xnjlx4zb"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/Microsoft/vscode-languageserver-node#readme")
   (synopsis
    "Types used by the Language server for node")
   (description
    "Types used by the Language server for node")
   (license license:expat)))

(define-public node-vscode-languageserver-protocol-3.16.0
  (package
   (name "node-vscode-languageserver-protocol")
   (version "3.16.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/vscode-languageserver-protocol/-/vscode-languageserver-protocol-3.16.0.tgz")
     (sha256
      (base32
       "19d3j2v3khl93l4ffgjgs6yb1hb3d698g056mwkh86bd2far67bf"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-vscode-languageserver-types"
       ,node-vscode-languageserver-types-3.16.0)
      ("node-vscode-jsonrpc"
       ,node-vscode-jsonrpc-6.0.0)))
   (home-page
    "https://github.com/Microsoft/vscode-languageserver-node#readme")
   (synopsis
    "VSCode Language Server Protocol implementation")
   (description
    "VSCode Language Server Protocol implementation")
   (license license:expat)))

(define-public node-vscode-languageserver-7.0.0
  (package
   (name "node-vscode-languageserver")
   (version "7.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/vscode-languageserver/-/vscode-languageserver-7.0.0.tgz")
     (sha256
      (base32
       "12bp2f43ljs7idk37ssbxp6gijnq5hb1mkccyln1mnhxj8w2zr9p"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-vscode-languageserver-protocol"
       ,node-vscode-languageserver-protocol-3.16.0)))
   (home-page
    "https://github.com/Microsoft/vscode-languageserver-node#readme")
   (synopsis
    "Language server implementation for node")
   (description
    "Language server implementation for node")
   (license license:expat)))

(define-public node-vscode-jsonrpc-8.0.1
  (package
   (name "node-vscode-jsonrpc")
   (version "8.0.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/vscode-jsonrpc/-/vscode-jsonrpc-8.0.1.tgz")
     (sha256
      (base32
       "1qzz0wr7fayf4253vdg0baqq3vxhxhfjdm00lxy17lcm6i1zcs4d"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/Microsoft/vscode-languageserver-node#readme")
   (synopsis
    "A json rpc implementation over streams")
   (description
    "A json rpc implementation over streams")
   (license license:expat)))

(define-public node-vscode-languageserver-types-3.17.1
  (package
   (name "node-vscode-languageserver-types")
   (version "3.17.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/vscode-languageserver-types/-/vscode-languageserver-types-3.17.1.tgz")
     (sha256
      (base32
       "1m9zk900px4dyz6vpi843aryw3n9sp0zbg85lpp6a828pv2ki8vp"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/Microsoft/vscode-languageserver-node#readme")
   (synopsis
    "Types used by the Language server for node")
   (description
    "Types used by the Language server for node")
   (license license:expat)))

(define-public node-vscode-languageserver-protocol-3.17.1
  (package
   (name "node-vscode-languageserver-protocol")
   (version "3.17.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/vscode-languageserver-protocol/-/vscode-languageserver-protocol-3.17.1.tgz")
     (sha256
      (base32
       "136wqfq25g3ljvml8s4lz65svi5w8cfxrn00xl4p60wqbhscvycd"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs
    `(("node-vscode-languageserver-types"
       ,node-vscode-languageserver-types-3.17.1)
      ("node-vscode-jsonrpc"
       ,node-vscode-jsonrpc-8.0.1)))
   (home-page
    "https://github.com/Microsoft/vscode-languageserver-node#readme")
   (synopsis
    "VSCode Language Server Protocol implementation")
   (description
    "VSCode Language Server Protocol implementation")
   (license license:expat)))

(define-public node-vscode-languageserver-textdocument-1.0.4
  (package
   (name "node-vscode-languageserver-textdocument")
   (version "1.0.4")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/vscode-languageserver-textdocument/-/vscode-languageserver-textdocument-1.0.4.tgz")
     (sha256
      (base32
       "08im7izycg36qyhbhz7i4jvrzh5ickxd07kqw1m348q3j6b16h4n"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/Microsoft/vscode-languageserver-node#readme")
   (synopsis
    "A simple text document implementation for Node LSP servers")
   (description
    "A simple text document implementation for Node LSP servers")
   (license license:expat)))

(define-public node-vscode-uri-3.0.3
  (package
   (name "node-vscode-uri")
   (version "3.0.3")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/vscode-uri/-/vscode-uri-3.0.3.tgz")
     (sha256
      (base32
       "0xh3qkg5c6p4dwi0y3yjw3vzxr1nvnz6c1q5z1qyd42f1j2daqr6"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/microsoft/vscode-uri#readme")
   (synopsis
    "The URI implementation that is used by VS Code and its extensions")
   (description
    "The URI implementation that is used by VS Code and its extensions")
   (license license:expat)))

(define-public node-isexe-2.0.0
  (package
   (name "node-isexe")
   (version "2.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/isexe/-/isexe-2.0.0.tgz")
     (sha256
      (base32
       "0nc3rcqjgyb9yyqajwlzzhfcqmsb682z7zinnx9qrql8w1rfiks7"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (home-page
    "https://github.com/isaacs/isexe#readme")
   (synopsis
    "Minimal module to check if a file is executable.")
   (description
    "Minimal module to check if a file is executable.")
   (license license:isc)))

(define-public node-which-2.0.2
  (package
   (name "node-which")
   (version "2.0.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/which/-/which-2.0.2.tgz")
     (sha256
      (base32
       "1p2fkm4lr36s85gdjxmyr6wh86dizf0iwmffxmarcxpbvmgxyfm1"))))
   (build-system node-build-system)
   (arguments
    `(#:tests?
      #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'build))))
   (inputs `(("node-isexe" ,node-isexe-2.0.0)))
   (home-page
    "https://github.com/isaacs/node-which#readme")
   (synopsis
    "Like which(1) unix command. Find the first instance of an executable in the PATH.")
   (description
    "Like which(1) unix command. Find the first instance of an executable in the PATH.")
   (license license:isc)))

(define-public node-typescript-language-server-0.10.0
  (package
   (name "node-typescript-language-server")
   (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/typescript-language-server/-/typescript-language-server-0.10.0.tgz")
        (sha256
          (base32
            "00bm5ls35dgvq9yqynm1pz5bwsx42li998s1px7s0598979mzy9y"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-which" ,node-which-2.0.2)
        ("node-vscode-uri" ,node-vscode-uri-3.0.3)
        ("node-vscode-languageserver-textdocument"
         ,node-vscode-languageserver-textdocument-1.0.4)
        ("node-vscode-languageserver-protocol"
         ,node-vscode-languageserver-protocol-3.17.1)
        ("node-vscode-languageserver"
         ,node-vscode-languageserver-7.0.0)
        ("node-tempy" ,node-tempy-1.0.1)
        ("node-semver" ,node-semver-7.3.7)
        ("node-pkg-up" ,node-pkg-up-3.1.0)
        ("node-p-debounce" ,node-p-debounce-2.1.0)
        ("node-fs-extra" ,node-fs-extra-10.1.0)
        ("node-commander" ,node-commander-9.2.0)))
    (propagated-inputs
     `(("node-typescript" ,node-typescript-4.6.4)))
    (home-page
      "https://www.npmjs.com/package/node-typescript-language-server")
    (synopsis
      "Language Server Protocol (LSP) implementation for TypeScript using tsserver")
    (description
      "Language Server Protocol (LSP) implementation for TypeScript using tsserver")
    (license license:asl2.0)))

(define-public node-once-1.4.0
  (package
    (name "node-once")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/once/-/once-1.4.0.tgz")
        (sha256
          (base32
            "1kygzk36kdcfiqz01dhql2dk75rl256m2vlpigv9iikhlc5lclfg"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-wrappy" ,node-wrappy-1.0.2)))
    (home-page
      "https://github.com/isaacs/once#readme")
    (synopsis "Run a function exactly one time")
    (description "Run a function exactly one time")
    (license license:isc)))

(define-public node-wrappy-1.0.2
  (package
    (name "node-wrappy")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/wrappy/-/wrappy-1.0.2.tgz")
        (sha256
          (base32
            "1yzx63jf27yz0bk0m78vy4y1cqzm113d2mi9h91y3cdpj46p7wxg"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/npm/wrappy")
    (synopsis "Callback wrapping utility")
    (description "Callback wrapping utility")
    (license license:isc)))

(define-public node-typescript-4.6.4
  (package
    (name "node-typescript")
    (version "4.6.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/typescript/-/typescript-4.6.4.tgz")
        (sha256
          (base32
            "10q35cf9bi627d04c91sxcqrncs3w3pw6zmj4hhqv43zhg6pycpy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://www.typescriptlang.org/")
    (synopsis
      "TypeScript is a language for application scale JavaScript development")
    (description
      "TypeScript is a language for application scale JavaScript development")
    (license license:asl2.0)))

(define-public node-typescript-language-server
  node-typescript-language-server-0.10.0)

(define-public node-typescript
  node-typescript-4.6.4)
