(use-modules (guix ci)
             (guix channels))

(list
 %default-guix-channel
 (channel
  (name 'rde)
  (url "https://git.sr.ht/~abcdw/rde")
  (introduction
   (make-channel-introduction
    "257cebd587b66e4d865b3537a9a88cccd7107c95"
    (openpgp-fingerprint
     "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (introduction
   (make-channel-introduction
    "46c1d8bcca674d3a71cd077c52dde9552a89873d"
    (openpgp-fingerprint
     "8141 6036 E81A 5CF7 8F80  1071 ECFC 8398 8B4E 4B9F"))))
(channel
  (name 'emacs)
  (url "https://github.com/babariviere/guix-emacs")
  (introduction
   (make-channel-introduction
    "72ca4ef5b572fea10a4589c37264fa35d4564783"
    (openpgp-fingerprint
     "261C A284 3452 FB01 F6DF  6CF4 F9B7 864F 2AB4 6F18")))))
