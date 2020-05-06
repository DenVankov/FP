(defun numb (n)
(cond
        ((= 0 (floor n 10)) 1)
        (t (+ 1 (numb (floor n 10))))))

(defun remove-extra-spaces (w)
  (loop for c across w
        unless (and n (char= c (car (last n)) #\space))
        collect c into n
        finally (return (string-trim " " (concatenate 'string n)))))

(defun number-sentence (n)
(let ((basis (make-array '(11 10) :initial-contents
        '((""   ""       ""         ""         ""          ""          ""           ""          ""            "")
            ("" "один"   "два"      "три"      "четыре"    "пять"      "шесть"      "семь"      "восемь"      "девять")
            ("" "десять" "двадцать" "тридцать" "сорок"     "пятьдесят" "шестьдесят" "семьдесят" "восемьдесят" "девяносто")
            ("" "сто"    "двести"   "триста"   "четыреста" "пятьсот"   "шестьсот"   "семьсот"   "восемьсот"   "девятьсот")
            ("" "одна"   "две"      "три"      "четыре"    "пять"      "шесть"      "семь"      "восемь"      "девять")
            ("" "десять" "двадцать" "тридцать" "сорок"     "пятьдесят" "шестьдесят" "семьдесят" "восемьдесят" "девяносто")
            ("" "сто"    "двести"   "триста"   "четыреста" "пятьсот"   "шестьсот"   "семьсот"   "восемьсот"   "девятьсот")
            ("" "один"   "два"      "три"      "четыре"    "пять"      "шесть"      "семь"      "восемь"      "девять")
            ("" "десять" "двадцать" "тридцать" "сорок"     "пятьдесят" "шестьдесят" "семьдесят" "восемьдесят" "девяносто")
            ("" "сто"    "двести"   "триста"   "четыреста" "пятьсот"   "шестьсот"   "семьсот"   "восемьсот"   "девятьсот")
            ("" "один"   "два"      "три "     "четыре"    "пять"      "шесть"      "семь"      "восемь"      "девять"))))

    (osn (make-array '(8) :initial-contents
        '(0 1 10 100 1000 10000 100000 1000000)))

    (termination (make-array '(10 4) :initial-contents
        '(("" "" "" "тысяч")
            ("" "" "" "тысяча")
            ("" "" "" "тысячи")
            ("" "" "" "тысячи")
            ("" "" "" "тысячи")
            ("" "" "" "тысяч")
            ("" "" "" "тысяч")
            ("" "" "" "тысяч")
            ("" "" "" "тысяч")
            ("" "" "" "тысяч"))))

    (dec (make-array '(10) :initial-contents
        '("десять" "одиннадцать" "двенадцать" "тринадцать" "четырнадцать" "пятнадцать" "шестнадцать" "семнадцать" "восемнадцать" "девятнадцать")
        ))
    (result "")
    (cnt (numb n))
    (celoe 0))
    ;End of initializing

    (when (= n 0)
        (setf result "ноль")
        result)

    (loop while (> n 0)
    do
        (progn
        (when (= (mod (+ cnt 1) 3) 0)
            (when (= (floor n (aref osn cnt)) 1)
                (setf n (mod n (aref osn cnt)))
                (setf cnt (- cnt 1))
                (setf celoe (floor n (aref osn cnt)))
                (setf n (mod n (aref osn cnt)))
                (setf result (concatenate 'string result (aref dec celoe)))
                (setf cnt (- cnt 1))
                (when (= (mod cnt 3) 0)
                    (setf result (concatenate 'string result " " (aref termination 0 cnt) " "))))
            (when (= cnt 0)
                (return)))
        (setf celoe (floor n (aref osn cnt)))
        (setf n (mod n (aref osn cnt)))
        (setf result (concatenate 'string result (aref basis cnt celoe) " "))
        (setf cnt (- cnt 1))
        (when (= (mod cnt 3) 0)
            (setf result (concatenate 'string result (aref termination celoe cnt) " ")))
    ))
    (setf result (remove-extra-spaces result))
    result
))

; (print (number-sentence 3967))
; (format t "~%")
; (print (number-sentence 0))
; (format t "~%")
; (print (number-sentence 1))
; (format t "~%")
; (print (number-sentence 11))
; (format t "~%")
; (print (number-sentence 15))
; (format t "~%")
; (print (number-sentence 9367))
; (format t "~%")
; (print (number-sentence 3517))
; (format t "~%")
; (print (number-sentence 120004))
