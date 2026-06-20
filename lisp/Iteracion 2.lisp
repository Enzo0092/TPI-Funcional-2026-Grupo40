
;===========
;Extensión 1
;===========

;Intermitencia de Seguridad

;; ========================================================
;; REQUERIMIENTO 1 : transicion (Versión 2.1)
;; NATURALEZA: Pura (Siempre devuelve el mismo resultado para los mismos parámetros)
;; ESTRATEGIA: Condicional / Evaluación de Predicados
;; IMPACTO: No destructiva
;; ========================================================

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


;; ========================================================
;; REQUERIMIENTO 2 : timer (Versión 2.1)
;; NATURALEZA: Pura (Depende únicamente del parámetro time)
;; ESTRATEGIA: Expresión Aritmética Modular y Condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun timer (time) 
	(let ((t-color (mod time 225))) ;el ciclo total ahora dura 225 segundos
		(cond 
			((< t-color 90) (transicion 'en-rojo 'rojo-intermitente)) ; de 0 a 89sg: Cambia a rojo intermitente
			((< t-color 93) (transicion 'rojo-intermitente 'en-verde)) ;de 90 a 92sg: Cambia a verde
			((< t-color 213) (transicion 'en-verde 'verde-intermitente)) ;de 93 a 212sg: Cambia a verde intermitente
			((< t-color 216) (transicion 'verde-intermitente 'en-amarillo)) ;de 213 a 215: Cambia a amarillo
			((< t-color 222) (transicion 'en-amarillo 'amarillo-intermitente)) ;de 216 a 221: Cambia a amarillo intermitente
			(t (transicion 'amarillo-intermitente 'en-rojo))))) ;de 222 a 224: Cambia a Rojo

;; ========================================================
;; REQUERIMIENTO 3: logging (Versión 2.1)
;; NATURALEZA: Pura (Devuelve una cadena estructurada con format nil)
;; ESTRATEGIA: Formateo de cadenas y composición de funciones
;; IMPACTO: No destructiva
;; ========================================================

(defun logging (tiempo)
	(let ((color-anterior (car (timer tiempo)))
			(color-actual (cadr (timer tiempo))))
	(format nil "Tiempo ~D: la luz ha cambiado de ~A a ~A" tiempo color-anterior color-actual)
))

;; ========================================================
;; REQUERIMIENTO 4a: duracion-ciclo (Versión 2.1)
;; NATURALEZA: Pura (Calcula la duración total sumando fases e intermitencias fijas de 3s)
;; ESTRATEGIA: Expresión Aritmética
;; IMPACTO: No destructiva
;; ========================================================

(defun duracion-ciclo (t-rojo t-amarillo t-verde)
;Se le suma 3 segundos de intermitencia por cada una de las 3 fases (en total 9 segundos extras)
(+ t-rojo t-amarillo t-verde 3 3 3))

;; ========================================================
;; REQUERIMIENTO 4b: recomendacion-ciclo (Versión 2.1)
;; NATURALEZA: Pura (Evalúa de forma determinista basándose en criterios de tráfico)
;; ESTRATEGIA: Composición Condicional y evaluación de predicados
;; IMPACTO: No destructiva
;; ========================================================

(defun recomendacion-ciclo (duracion-ciclo)
	(cond
		((< duracion-ciclo 35) "El ciclo es muy corto")
		((> duracion-ciclo 150) "El ciclo es muy largo")
		(T "El ciclo es perfecto")
	)

)

;; ========================================================
;; REQUERIMIENTO 5: ciclos-por-tiempo (Versión 2.1)
;; NATURALEZA: Pura (Calcula los ciclos completos basándose en la nueva duración con intermitencia)
;; ESTRATEGIA: Composición Aritmética / Truncamiento
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo (minutos)
;Como usamos la funcion duracion-ciclo pasándole los parámetros base (90 6 120)
;La función internamente sumará la intermitencia dando 225
	(floor (/(* minutos 60) (duracion-ciclo 90 6 120)))
)

;; ========================================================
;; REQUERIMIENTO 6: distribucion-por-hora (Versión 2.1)
;; NATURALEZA: Pura (Depende solo de parámetros de tiempo)
;; ESTRATEGIA: Composición aritmética avanzada y condicionales anidados
;; IMPACTO: No destructiva
;; ========================================================

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
;Extensión 2
;=============

;Persistencia de datos

;; ========================================================
;; FUNCIÓN: informe
;; NATURALEZA: Pura (siempre devuelve el mismo resultado para los mismos parametros)
;; ESTRATEGIA: Guardado de datos devueltos a un archivo de texto plano
;; IMPACTO: Destructiva (Destruye el archivo de texto plano anterior)
;; ========================================================

(defun informe (tiempo) (with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output) (format stream "Informe de Ejecución del Sistema Semafórico~%") (format stream "=========================================~%") (let ((fecha (local-time:format-timestring nil (local-time:unix-to-timestamp tiempo) :format '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2)))) (color-anterior (car (contador tiempo))) (color-actual (cadr (contador tiempo)))) (format stream "~A: la luz cambio de ~A a ~A~%" fecha color-anterior color-actual)) (format stream "~% --- Fin del Informe ---")))

;; ========================================================
;; FUNCIÓN: obtener-transicion
;; NATURALEZA: Pura (siempre devuelve el mismo resultado para los mismos parametros)
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun obtener-transicion (color-actual cambiar-a) (let ((r 'rojo) (y 'amarillo) (g 'verde)) (cond ((and (eq color-actual r) (eq cambiar-a 'verde)) (list color-actual cambiar-a)) ((and (eq color-actual y) (eq cambiar-a 'rojo)) (list color-actual cambiar-a)) ((and (eq color-actual g) (eq cambiar-a 'amarillo)) (list color-actual cambiar-a)))))

;; ========================================================
;; FUNCIÓN: contador
;; NATURALEZA: Pura (depende unicamente del parametro time)
;; ESTRATEGIA: Expresion aritmetica y condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun contador (time) (let ((t-color (mod time 216))) (cond ((< t-color 90) (obtener-transicion 'rojo 'verde)) ((< t-color 210) (obtener-transicion 'verde 'amarillo)) (t (obtener-transicion 'amarillo 'rojo)))))
