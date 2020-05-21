(defclass cart ()                ; имя класса и надклассы
	((x :initarg :x :reader cart-x)   ; дескриптор слота x
	 (y :initarg :y :reader cart-y))) ; дескриптор слота y

(defmethod print-object ((c cart) stream)
	(format stream "[CART x ~d y ~d]"
		(cart-x c) (cart-y c)))

(defclass line ()
	((start :initarg :start :accessor line-start)
	 (end   :initarg :end   :accessor line-end)))

(defmethod print-object ((lin line) stream)
 	(format stream "[ОТРЕЗОК ~s ~s]"
		(line-start lin) (line-end lin)))

(defclass triangle ()
	((vertex1 :initarg :1 :reader vertex1)  ; селектор может совпадать
	 (vertex2 :initarg :2 :reader vertex2)  ; с именем слота
	 (vertex3 :initarg :3 :reader vertex3)))

(defmethod print-object ((tri triangle) stream)
	(format stream "[ТРЕУГ ~s ~s ~s]"
    	(vertex1 tri) (vertex2 tri) (vertex3 tri)))
 
(setq tri (make-instance 'triangle
       	   :1 (make-instance 'cart :x 4 :y 3)
           :2 (make-instance 'cart :x 7 :y 5)
           :3 (make-instance 'cart :x 5 :y -1)))

(defmethod get-vector ((a cart) (b cart))
	(make-instance 'cart :x (- (cart-x b) (cart-x a)) :y (- (cart-y b) (cart-y a))))

(defmethod get-length ((a cart))
	(sqrt (+ (* (cart-x a) (cart-x a)) (* (cart-y a) (cart-y a)) )))

(defmethod sum-cart ((a cart) (b cart))
	(make-instance 'cart :x (+ (cart-x a) (cart-x b)) :y (+ (cart-y a) (cart-y b))))

(defun биссектриса ((tri triangle))
	(let* ((AB (get-vector (vertex1 tri) (vertex2 tri)))
	(BC (get-vector (vertex2 tri) (vertex3 tri)))
	(AC (get-vector (vertex1 tri) (vertex3 tri)))
	(len-AB (get-length AB))
	(len-BC (get-length BC))
	(len-AC (get-length AC))
	(a (make-instance 'cart :x (/ (cart-x AB) len-AB) :y (/ (cart-y AB) len-AB)))
	(b (make-instance 'cart :x (/ (cart-x AC) len-AC) :y (/ (cart-y AC) len-AC)))
	(AK (sum-cart a b))
	(lambd (/ len-AB len-AC))
	(x-k (/ (+ (cart-x (vertex2 tri)) (* (cart-x (vertex3 tri)) lambd)) (+ 1 lambd)))
	(y-k (/ (+ (cart-y (vertex2 tri)) (* (cart-y (vertex3 tri)) lambd)) (+ 1 lambd)))
	(len-AK (get-length (get-vector (vertex1 tri) (make-instance 'cart :x x-k :y y-k))))
	(lin (make-instance 'line
		:start (make-instance 'cart :x (cart-x (vertex1 tri)) :y (cart-y (vertex1 tri)))
		:end (make-instance 'cart :x x-k :y y-k))))
	lin
	))

; (setq tri (make-instance 'triangle
;            :1 (make-instance 'cart :x 4 :y 3)
;            :2 (make-instance 'cart :x 7 :y 5)
;            :3 (make-instance 'cart :x 5 :y -1)))