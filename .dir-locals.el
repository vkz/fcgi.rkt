;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((racket-mode
  (eval . (progn
            (put 'bit-string-case 'racket-indent-function 1)
            (put 'when? 'racket-indent-function 1)
            (put 'unless? 'racket-indent-function 1)))))
