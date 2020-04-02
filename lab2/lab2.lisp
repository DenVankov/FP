(defun to-list (tree)
    (if (null tree)
        nil
        (if (atom (first tree))
            (cons (first tree) (to-list (rest tree)))
            (append (to-list (first tree)) (to-list (rest tree)))
        )
    )
)

(defun tree-similar-p (tree1 tree2)
    (equal (to-list tree1) (to-list tree2))
)

;(tree-similar-p '(1 (2 (3 4)) 5) '((1 2) 3 (4 5)))

;(tree-similar-p '(1 (2 (3))) '(((3) 2 ) 1))

;(tree-similar-p '(1 (2 (3 4 5) 6 (7))) '((1 2) (3 4) (5 6) 7))
