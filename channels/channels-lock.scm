(use-modules (guix channels))

(list (channel
        (name 'emacs)
        (url "https://github.com/babariviere/guix-emacs")
        (branch "master")
        (commit
          "fe10f46bf6b5bb6f8fb18d63198305deff131340")
        (introduction
          (make-channel-introduction
            "72ca4ef5b572fea10a4589c37264fa35d4564783"
            (openpgp-fingerprint
              "261C A284 3452 FB01 F6DF  6CF4 F9B7 864F 2AB4 6F18"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "f6121e161d3c5414e3829560b4517c7ba6af89bc")
        (introduction
          (make-channel-introduction
            "46c1d8bcca674d3a71cd077c52dde9552a89873d"
            (openpgp-fingerprint
              "8141 6036 E81A 5CF7 8F80  1071 ECFC 8398 8B4E 4B9F"))))
      (channel
        (name 'rde)
        (url "https://git.sr.ht/~abcdw/rde")
        (branch "master")
        (commit
          "00f986aa616cc1e55a9392d4e4d227643fedd486")
        (introduction
          (make-channel-introduction
            "257cebd587b66e4d865b3537a9a88cccd7107c95"
            (openpgp-fingerprint
              "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
          "76a3414a1bc500626a9feca013673f994eb51a34")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
