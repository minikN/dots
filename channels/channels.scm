(list
 (channel
  (name 'guix)
  (url "https://git.savannah.gnu.org/git/guix.git")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
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
