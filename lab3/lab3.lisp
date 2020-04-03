(defun mprint (m)
  (loop for i below (car (array-dimensions m)) do
        (loop for j below (cadr (array-dimensions m)) do
          (let ((cell (aref m i j)))
            (format t "~a " cell)))
        (format t "~%")))

(defun matrix-tl-bl (n)
    (PROG
        ((i 0) (j 0) (value 1) (last (- n 1)))
        (setf matrix (make-array (list n n)))
        (loop for diag-length from 1 to n do
    		(if (oddp diag-length)
    			(let ((end (- diag-length 1)))
    				(loop for l from 0 to end do
    					(setf (aref matrix i j) value)
    					(incf value)
    					(if (/= l end)
                            (do () ((decf i) (incf j)))
    					)
    				)
    				(if (= j last)
    					(incf i)
    					; else
    					(incf j)
    				)
    			)
    			; else
    			(let ((end (- diag-length 1)))
    				(loop for l from 0 to end do
    					(setf (aref matrix i j) value)
    					(incf value)
    					(if (/= l end)
                            (do () ((incf i)(decf j)))
    					)
    				)
    				(if (= i last)
    					(incf j)
    					; else
    					(incf i)
    				)
    			)
    		)
    	)
        (loop for diag-length from last downto 1 do
    		(if (oddp diag-length)
    			(let ((end (- diag-length 1)))
    				(loop for l from 0 to end do
    					(setf (aref matrix i j) value)
    					(incf value)
    					(if (/= l end)
    						(do () ((decf i) (incf j)))
    					)
    				)
    				(incf i)
    			)
    			; else
    			(let ((end (- diag-length 1)))
    				(loop for l from 0 to end do
    					(setf (aref matrix i j) value)
    					(incf value)
    					(if (/= l end)
    						(do () ((incf i) (decf j)))
    					)
    				)
    				(incf j)
    			)
    		)
    	)
    )
    matrix
)
