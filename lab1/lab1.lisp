(defun power (arg)
    ( * arg arg)
)

(defun obtuse-angled (a b c)
    (if
        (or
            (>= a (+ b c))
            (>= b (+ a c))
            (>= c (+ a b))) nil (if
                (not
                    (and
                        (> (+ (power a) (power b)) (power c))
                        (> (+ (power a) (power c)) (power b))
                        (> (+ (power c) (power b)) (power a)))) T)))
