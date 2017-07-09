(load "tora/prelude.scm")

(define random-init
  "lua () math.randomseed(os.time()) end")

(define random-between
  "lua (lo, hi) return math.random(lo, hi) end")

(define read-line
  "lua () return tora_string(io.read('*line')) end")

(define string->number
  "lua (s) return tonumber(lua_string(s)) end")

(begin
  (print "~ Guess the number between 1 and 100 ~")
  (random-init)
  (let ((play #t)
        (answer (random-between 1 100)))
    (while play
      (letrec ((guess (read-line))
               (number (string->number guess)))
        (if (or (= guess "q") (= guess "quit"))
          (begin
            (print (string-append "The number was " answer))
            (set! play #f))
          (if (number? number)
            (cond ((= number answer)
                   (begin
                     (print "Correct!")
                     (set! play #f)))
                  ((< number answer)
                   (print "Too low, try again"))
                  (else
                    (print "Too high, try again")))
            (print "Please enter a number")))))))
