Requerimiento 1:

;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura (siempre devuelve el mismo resultado para los mismos parametros)
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun transicion (color-actual cambiar-a) 
	(let ((r 'en-rojo) (y 'en-amarillo) (g 'en-verde))
	 (cond 
		  ((and (eq color-actual r) (eq cambiar-a 'verde)) (list color-actual (format nil "cambiar a ~A" cambiar-a)));Uso un Cond para abarcar los 3 casos de cambio de color del semaforo
		  ((and (eq color-actual y) (eq cambiar-a 'rojo)) (list color-actual (format nil "cambiar a ~A" cambiar-a)))
		  ((and (eq color-actual g) (eq cambiar-a 'amarillo)) (list color-actual (format nil "cambiar a ~A" cambiar-a)))
		  (t Nil)
					)))



;Requerimiento 2:

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (depende unicamente del parametro time)
;; ESTRATEGIA: Expresion aritmetica y condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun timer (time) 
(let ((t-color (mod time 216)))
	  (cond 
			((< t-color 90) 'rojo); De 0 a 89, rojo
			((< t-color 210) 'verde); De 90 a 209, verde
			(t 'amarillo); De 210 a 215, amarillo
				)))


Requerimiento 3:
;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura (siempre devuelve el mismo resultado para los mismos parametros)
;; ESTRATEGIA: Condicional (por su uso del cond)
;; IMPACTO: No destructiva
;; ========================================================

(defun transicion (color-actual cambiar-a) (let ((r 'rojo) (y 'amarillo) (g 'verde)) 
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

(defun timer (time) (let ((t-color (mod time 216))) (cond ((< t-color 90) (transicion 'rojo 'verde)) 
			((< t-color 210) (transicion 'verde 'amarillo)) (t (transicion 'amarillo 'rojo)))));Tambien, utilizo el requerimiento 2

;; ========================================================
;; FUNCIÓN: logging
;; NATURALEZA: Pura (unicamente devuelve un string con format nil dependiendo de lo que reciba por parametro)
;; ESTRATEGIA: Formateo de cadenas y composición de funciones
;; IMPACTO: No destructiva
;; ========================================================

(defun logging (tiempo) (let ((color-anterior (car (timer tiempo))) (color-actual (cadr (timer tiempo))));Una vez que recibe la lista, guarda el CAR y el CADR de la lista y arma ya el resultado que se espera
(format nil "Tiempo ~D: la luz cambio de ~A a ~A" tiempo color-anterior color-actual)))



;Requerimiento 4:
;4a. Función duracion-ciclo:
;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura (Debido al tiempo de los cambios, calcula los ciclos matematicamente)
;; ESTRATEGIA: Expresion Aritmética
;; IMPACTO: No destructiva
;; ========================================================

(defun duracion-ciclo (t-rojo t-verde t-amarillo)
  ;Suma los tiempos de las tres fases para obtener el ciclo completo (rojo -> verde -> amarillo )
  (+ t-rojo t-verde t-amarillo))

;4b. Función recomendacion-ciclo:
;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura (En base a la duración del ciclo, calcula y evalua el ciclo sin efectos secundarios)
;; ESTRATEGIA: Composición Condicional y evaluación de predicados
;; IMPACTO: No destructiva
;; ========================================================

(defun recomendacion-ciclo (duracion-ciclo)
 ;Evalúa los predicados de arriba hacia abajo y frena en la primera que sea verdadera.
    (cond
    ; 1. Compara si la duración es menor al límite psicológico de 35 segundos
    ((< duracion-ciclo 35) "El ciclo es muy corto")
    ; 2. Si no es menor a 35, compara si supera el límite máximo de 150 segundos
    ((> duracion-ciclo 150) "El ciclo es muy largo")
    ; Si llegó hasta acá, significa que el tiempo está en el rango óptimo (entre 35 y 150 segundos)
    (T "El ciclo es perfecto")
))



;Requerimiento 5:

;opcion 1 sin sacar el resto

;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura (Dado un tiempo en minutos, calcula de forma determinista los ciclos completos sin efectos secundarios)
;; ESTRATEGIA: Composición Aritmética y Truncamiento
;; IMPACTO: No destructiva (No modifica estructuras; devuelve un número entero nuevo)
;; ========================================================

(defun ciclos-por-tiempo (minutos)
  ; 1. Convertimos los minutos recibidos por parámetro a segundos multiplicándolos por 60.
  ; 2. Llamamos a 'duracion-ciclo' pasándole los tiempos puros de la Fase 1 (90, 6, 120), que suman 216s.
  ; 3. Dividimos el tiempo total en segundos por la duración del ciclo básico.
  ; 4. La función 'floor' trunca el resultado hacia abajo (descarta los decimales),
  ;    asegurando que solo se devuelva la cantidad de ciclos COMPLETOS.
  (floor (/ (* minutos 60) (duracion-ciclo 90 6 120)))
)

;opcion 2 sacando el resto

;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura (Calcula la cantidad de ciclos completos basándose únicamente en el parámetro de entrada)
;; ESTRATEGIA: Composición Aritmética / Extracción de Valores Múltiples (nth-value)
;; IMPACTO: No destructiva (Retorna un valor numérico atómico sin alterar el entorno)
;; ========================================================

(defun ciclos-por-tiempo (minutos)
  ; 1. Convertimos los minutos de entrada a segundos (* minutos 60).
  ; 2. Dividimos por la duración calculada del ciclo puro de Fase 1 (216 segundos).
  ; 3. La función primitiva 'floor' realiza la división y genera dos salidas en paralelo: (cociente-entero resto-en-segundos).
  ; 4. 'nth-value 0' interviene para capturar únicamente el valor de la posición cero (el cociente),
  ;    descartando el residuo para cumplir estrictamente con la especificación de ciclos completos.
  (nth-value 0 (floor (/ (* minutos 60) (duracion-ciclo 90 6 120))))
)


;Requerimiento 6
;; ========================================================
;; FUNCIÓN: distribución-hora-n
;; NATURALEZA: Pura
;; ESTRATEGIA: Composición aritmética y condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun distribucion-hora-n (t-rojo t-amarillo t-verde n)
  (let* ((ciclo (+ t-rojo t-amarillo t-verde))
         (inicio (* (1- n) 3600))		;; segundo inicial de la hora N
         (offset (mod inicio ciclo))		;; aca se determina en qué punto del ciclo arranca la hora 
         (resto-inicial (- ciclo offset)))
    ;; aca se calculan los segundos de la hora N
    (let* ((primera (cond
                      ((<= 3600 resto-inicial)
                       ;; toda la hora entra en el ciclo actual
                       (list (min 3600 t-rojo)
                             (min (max 0 (- 3600 t-rojo)) t-amarillo)
                             (max 0 (- 3600 (+ t-rojo t-amarillo)))))
                      (t
                       ;; aca se completa el ciclo en curso
                       (list (min resto-inicial t-rojo)
                             (min (max 0 (- resto-inicial t-rojo)) t-amarillo)
                             (max 0 (- resto-inicial (+ t-rojo t-amarillo)))))))
           (restante (- 3600 (reduce #'+ primera)))		;; estos son los segundos que quedan de la hora después de cerrar ese ciclo
           (ciclos-completos (floor (/ restante ciclo)))
           (tiempo-ciclos (* ciclos-completos ciclo))
           (rojo-base (+ (first primera) (* ciclos-completos t-rojo)))
           (amarillo-base (+ (second primera) (* ciclos-completos t-amarillo)))
           (verde-base (+ (third primera) (* ciclos-completos t-verde)))
           (sobrante (- restante tiempo-ciclos))
           (final (list (min sobrante t-rojo)		;; aca se reparten los segundos sobrantes del último ciclo parcial
                        (min (max 0 (- sobrante t-rojo)) t-amarillo)
                        (max 0 (- sobrante (+ t-rojo t-amarillo))))))
      ;; aca se devuelven los porcentajes
      (mapcar (lambda (x) (* 100 (/ x 3600.0)))
              (list (+ rojo-base (first final))
                    (+ amarillo-base (second final))
                    (+ verde-base (third final)))))))
	;; los porcentajes de la primera, segunda y tercer hora varian y luego se repiten cadda tres horas.