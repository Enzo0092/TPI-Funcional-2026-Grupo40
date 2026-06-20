
;===========
;Iteración 1
;===========

;Intermitencia de Seguridad

(defun transicion (color-actual cambiar-a) 
	(let ((r 'en-rojo) (y 'en-amarillo) (g 'en-verde))
	 (cond 
	 	;Transición: de rojo a rojo intermitente, y de rojo intermitente a verde  
		((and (eq color-actual r) (eq cambiar-a 'rojo-intermitente))(list color-actual (format nil "cambiar a ~A" cambiar-a)))
		((and(eq color-actual 'rojo-intermitente)(eq cambiar-a g))(list color-actual (format nil "cambiar a ~A" cambiar-a)))
        ;Transicion: de verde a verde intermitente, y de verde intermitente a amarillo
		((and (eq color-actual g) (eq cambiar-a 'verde-intermitente))(list color-actual (format nil "cambiar a ~A" cambiar-a)))
		((and (eq color-actual 'verde-intermitente) (eq cambiar-a y))(list color-actual (format nil "cambiar a ~A" cambiar-a)))
		;Transicion:de amarillo a amarillo intermitente, y de amarillo intermitente a rojo
		((and (eq color-actual y) (eq cambiar-a 'amarillo-intermitente))(list color-actual (format nil "cambiar a ~A" cambiar-a)))
		((and (eq color-actual 'amarillo-intermitente)(eq cambiar-a r))(list color-actual (format nil "cambiar a ~A" cambiar-a)))
		;Caso por defecto ante una transicion inválida o fuera de secuencia
		(t NIL))))



(defun timer (time) 
	(let ((t-color (mod time 225))) ;el ciclo total ahora dura 225 segundos
		(cond 
			((< t-color 90) (transicion 'en-rojo 'rojo-intermitente)) ; de 0 a 89sg: Cambia a rojo intermitente
			((< t-color 93) (transicion 'rojo-intermitente 'en-verde)) ;de 90 a 92sg: Cambia a verde
			((< t-color 213) (transicion 'en-verde 'verde-intermitente)) ;de 93 a 212sg: Cambia a verde intermitente
			((< t-color 216) (transicion 'verde-intermitente 'en-amarillo)) ;de 213 a 215: Cambia a amarillo
			((< t-color 222) (transicion 'en-amarillo 'amarillo-intermitente)) ;de 216 a 221: Cambia a amarillo intermitente
			(t (transicion 'amarillo-intermitente 'en-rojo))))) ;de 222 a 224: Cambia a Rojo

(defun logging (tiempo)
	(let ((color-anterior (car (timer tiempo)))
			(color-actual (cadr (timer tiempo))))
	(format nil "Tiempo ~D: la luz ha cambiado de ~A a ~A" tiempo color-anterior color-actual)
))

(defun duracion-ciclo (t-rojo t-amarillo t-verde)
;Se le suma 3 segundos de intermitencia por cada una de las 3 fases (en total 9 segundos extras)
(+ t-rojo t-amarillo t-verde 3 3 3))

(defun recomendacion-ciclo (duracion-ciclo)
	(cond
		((< duracion-ciclo 35) "El ciclo es muy corto")
		((> duracion-ciclo 150) "El ciclo es muy largo")
		(T "El ciclo es perfecto")
	)

)

(defun ciclos-por-tiempo (minutos)
;Como usamos la funcion duracion-ciclo pasándole los parámetros base (90 6 120)
;La función internamente sumará la intermitencia dando 225
	(floor (/(* minutos 60) (duracion-ciclo 90 6 120)))
)


(defun distribucion-hora-n (t-rojo t-amarillo t-verde n)
  (let* ((t-int 3) ; Tiempo fijo de intermitencia solicitado
         ; El ciclo ahora incluye dinámicamente las tres intermitencias (total 225)
         (ciclo (duracion-ciclo t-rojo t-amarillo t-verde))
         (inicio (* (1- n) 3600))   ; segundo inicial de la hora N
         (offset (mod inicio ciclo))    ; aca se determina en qué punto del ciclo arranca la hora 
         (resto-inicial (- ciclo offset)))
    
    ; aca se calculan los segundos de la hora N
    (let* ((primera (cond
                      ((<= 3600 resto-inicial)
                       ; toda la hora entra en el ciclo actual
                       (list (min 3600 t-rojo)
                             (min (max 0 (- 3600 t-rojo)) t-int)
                             (min (max 0 (- 3600 (+ t-rojo t-int))) t-verde)
                             (min (max 0 (- 3600 (+ t-rojo t-int t-verde))) t-int)
                             (min (max 0 (- 3600 (+ t-rojo t-int t-verde t-int))) t-amarillo)
                             (max 0 (- 3600 (+ t-rojo t-int t-verde t-int t-amarillo)))))
                      (t
                       ; aca se completa el ciclo en curso
                       (list (min resto-inicial t-rojo)
                             (min (max 0 (- resto-inicial t-rojo)) t-int)
                             (min (max 0 (- resto-inicial (+ t-rojo t-int))) t-verde)
                             (min (max 0 (- resto-inicial (+ t-rojo t-int t-verde))) t-int)
                             (min (max 0 (- resto-inicial (+ t-rojo t-int t-verde t-int))) t-amarillo)
                             (max 0 (- resto-inicial (+ t-rojo t-int t-verde t-int t-amarillo)))))))
           
           (restante (- 3600 (reduce #'+ primera)))   ; estos son los segundos que quedan de la hora después de cerrar ese ciclo
           (ciclos-completos (floor (/ restante ciclo)))
           (tiempo-ciclos (* ciclos-completos ciclo))
           
           ; tiempos acumulados por color e intermitencia en ciclos completos
           (rojo-base (+ (nth 0 primera) (* ciclos-completos t-rojo)))
           (int-r-base (+ (nth 1 primera) (* ciclos-completos t-int)))
           (verde-base (+ (nth 2 primera) (* ciclos-completos t-verde)))
           (int-v-base (+ (nth 3 primera) (* ciclos-completos t-int)))
           (amarillo-base (+ (nth 4 primera) (* ciclos-completos t-amarillo)))
           (int-y-base (+ (nth 5 primera) (* ciclos-completos t-int)))
           
           (sobrante (- restante tiempo-ciclos))
           
           ; aca se reparten los segundos sobrantes del último ciclo parcial
           (final (list (min sobrante t-rojo)   
                        (min (max 0 (- sobrante t-rojo)) t-int)
                        (min (max 0 (- sobrante (+ t-rojo t-int))) t-verde)
                        (min (max 0 (- sobrante (+ t-rojo t-int t-verde))) t-int)
                        (min (max 0 (- sobrante (+ t-rojo t-int t-verde t-int))) t-amarillo)
                        (max 0 (- sobrante (+ t-rojo t-int t-verde t-int t-amarillo))))))
      
      ; aca se devuelven los porcentajes de las 6 fases combinadas
      (mapcar (lambda (x) (* 100 (/ x 3600.0)))
              (list (+ rojo-base (nth 0 final))
                    (+ int-r-base (nth 1 final))
                    (+ verde-base (nth 2 final))
                    (+ int-v-base (nth 3 final))
                    (+ amarillo-base (nth 4 final))
                    (+ int-y-base (nth 5 final)))))))

;=============
;Iteracion 2
;=============

;Persistencia de datos

(defun informe (tiempo) (with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output) (format stream "Informe de Ejecución del Sistema Semafórico~%") (format stream "=========================================~%") (let ((fecha (local-time:format-timestring nil (local-time:unix-to-timestamp tiempo) :format '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2)))) (color-anterior (car (timer2 tiempo))) (color-actual (cadr (timer2 tiempo)))) (format stream "~A: la luz cambio de ~A a ~A~%" fecha color-anterior color-actual)) (format stream "~% --- Fin del Informe ---")))

(defun transicion2 (color-actual cambiar-a) (let ((r 'rojo) (y 'amarillo) (g 'verde)) (cond ((and (eq color-actual r) (eq cambiar-a 'verde)) (list color-actual cambiar-a)) ((and (eq color-actual y) (eq cambiar-a 'rojo)) (list color-actual cambiar-a)) ((and (eq color-actual g) (eq cambiar-a 'amarillo)) (list color-actual cambiar-a)))))

(defun timer2 (time) (let ((t-color (mod time 216))) (cond ((< t-color 90) (transicion2 'rojo 'verde)) ((< t-color 210) (transicion2 'verde 'amarillo)) (t (transicion2 'amarillo 'rojo)))))
