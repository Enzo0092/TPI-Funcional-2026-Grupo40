;Utilizando la librerria local-time, modifique el requerimiento 3 para poder dejar como la consigna de la fase 2 pedia:

;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura (siempre devuelve el mismo resultado para los mismos parametros)
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun trancision (color-actual cambiar-a) (let ((r 'rojo) (y 'amarillo) (g 'verde)) 
						(cond 
						((and (eq color-actual r) (eq cambiar-a 'verde)) (list color-actual cambiar-a));Uso cond para abarcar los 3 casos de cambio de color del semaforo
						((and (eq color-actual y) (eq cambiar-a 'rojo)) (list color-actual cambiar-a))
						((and (eq color-actual g) (eq cambiar-a 'amarillo)) (list color-actual cambiar-a)))));Utilizo el requerimiento 1 para hacer este requerimiento


;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (depende unicamente del parametro time)
;; ESTRATEGIA: Expresion aritmetica y condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun timer (time) (let ((t-color (mod time 216))) (cond ((< t-color 90) (trancision 'rojo 'verde)) 
			((< t-color 210) (trancision 'verde 'amarillo)) (t (trancision 'amarillo 'rojo)))));Tambien, utilizo el requerimiento 2


;; ========================================================
;; FUNCIÓN: logging
;; NATURALEZA: Pura (depende unicamente del parametro tiempo)
;; ESTRATEGIA: Expresion aritmetica y condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun logging (tiempo) (let ((fecha (local-time:format-timestring
            nil (local-time:unix-to-timestamp tiempo) :format '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2))))

(color-anterior (car (timer tiempo)))
(color-actual (cadr (timer tiempo)))) 
(format nil "~A: la luz cambio de ~A a ~A" fecha color-anterior color-actual)));Una vez que recibe la lista, guarda el CAR y el CADR de la lista y arma ya el resultado que se espera
(format nil "~A: la luz cambio de ~A a ~A" fecha color-anterior color-actual)));En esta funcion, guardo lo que devuelve local-time en una variable local, en este caso lo va a guardar entre comillas ("2026-05-16 14:30")
