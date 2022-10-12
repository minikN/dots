(list (channel
        (name 'emacs)
        (url "https://github.com/babariviere/guix-emacs")
        (branch "master")
        (commit
          "bb4152432328ae9ffddf6e69ae023653682dc3fd")
        (introduction
          (make-channel-introduction
            "72ca4ef5b572fea10a4589c37264fa35d4564783"
            (openpgp-fingerprint
              "261C A284 3452 FB01 F6DF  6CF4 F9B7 864F 2AB4 6F18"))))
      (channel
        (name 'flat)
        (url "https://github.com/flatwhatson/guix-channel.git")
        (branch "master")
        (commit
          "e57424b680e1724105e2598b68c30084b180cf58")
        (introduction
          (make-channel-introduction
            "33f86a4b48205c0dc19d7c036c85393f0766f806"
            (openpgp-fingerprint
              "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "12024f8855983bba21f4f11c33faf24c73f4d855")
        (introduction
          (make-channel-introduction
            "46c1d8bcca674d3a71cd077c52dde9552a89873d"
            (openpgp-fingerprint
              "8141 6036 E81A 5CF7 8F80  1071 ECFC 8398 8B4E 4B9F"))))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
          "86ec52f66735b122b9035eba56516fd16f3be958")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
