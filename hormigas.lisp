;; Iniciamos las variables globales usadas para los datos de entrada
;; además de asignar los valores escogidos a los parametros del 
;; algoritmo

(setf *nObjetos* 0)
(setf *pesoMaximo* 0)
(setf *tablaObjetos* ())

(setf *alfa* 1)
(setf *beta* 1)
(setf *gamma* 1)

(setf *noIteraciones* 10)
(setf *noHormigas* 30)

;; Para ejecutar el ejercicio 1 basta con poner a NIL esta variable.
(setf *mejoraIterativa* nil)

(setf *fichero* "pruebaMEDIA.txt")

;; ESTRUCTURA MOCHILA
;; Definimos la estructura Mochila para utilizarla en la solución final
;; y en las iteraciones e iniciamos la instrancia de la solución final.

(defstruct (mochila (:print-function imprimeMochila) ) (objetos ()) (valor 0) (peso 0))
(setf *solucionMejor* (make-mochila :objetos () :valor 0 :peso 0))

;;
;; IMPRIME MOCHILA
;; Función para mostrar las mochilas usando print

(defun imprimeMochila (moch &optional (canal t) profundidad) (
	format canal "Solucion ~A.
El peso de los objetos es ~A.
El valor de la mochila es ~A."
	(mapcar (lambda (x) (+ x 1)) (mochila-objetos moch)) (mochila-peso moch) (mochila-valor moch)
	)
)

;;
;; CAPTURA ENTRADA
;; Parseo del fichero de entrada.

(defun capturaEntrada () (with-open-file (stream *fichero*)
    (loop for line = (read-line stream nil 'foo)
          until (eq line 'foo)
			do
			(if (and (eq 'Numero (read-from-string line nil t :start 0)) (eq 'Items (read-from-string line nil t :start 7))) 
			    (setf *nObjetos* (read-from-string line nil t :start 13))
			    (if (and (eq 'Capacidad (read-from-string line nil t :start 0)) (eq 'Mochila (read-from-string line nil t :start 10))) 
			        (setf *pesoMaximo* (read-from-string line nil t :start 18))
			        (if (and (eq 'numero (read-from-string line nil t :start 0)) (eq 'peso (read-from-string line nil t :start 7))) 
			            nil
			            (rellenaTabla line)
			        ) 
			    ) 
			 ) 
    )
  )
)

;;
;; RELLENA TABLA
;; Función auxiliar para capturar los pesos y valores de cada objeto.

(defun rellenaTabla (linea) (let* ((numero (read-from-string linea nil t :start 0)) (atributos ()) (puntero (length (write-to-string numero))))
								
								(setf numero (read-from-string linea nil t :start puntero))
								(setf atributos (list numero))
								(setf puntero (+ puntero (length (write-to-string numero))))
								(setf numero (read-from-string linea nil t :start (+ 1 puntero)))
								(setf atributos (append atributos (list numero)))
								(setf *tablaObjetos* (append *tablaObjetos* (list atributos)))
							)
)

;;
;; INICIALIZAR FEROMONAS
;; Inicializa las feromonas a 1 en todos los objetos por decisión de diseño.


(defun inicializarFeromonas () (loop for x from 1 to *nObjetos* collect 1) )

;;
;; ACTUALIZAR Fi
;; Actualiza la lista de Fi utilizando la formula proporcionada


(defun actualizarFi (tFer) (
	loop for x from 0 to (- *nObjetos* 1) 
	collect (* (expt (nth x tFer) *alfa*) 
			(*(expt (float (/ 1 (first (nth x *tablaObjetos*)))) *beta*) 
			(expt (- 1 (float (/ 1 (second (nth x *tablaObjetos*))))) *gamma*)))
	)
)

;;
;; SUMATORIO Fi
;; Calcula el sumatorio de la lista Fi.


(defun sumatorioFi (tFi) (loop for Fi in tFi summing Fi))

;;
;; BUSCA SOLUCION
;; Busca una solución al problema de la mochila. Cada hormiga lo ejecuta
;; una vez y genera una solución propia.

(defun buscaSolucion (tFi sumFi) 
	(let ((objeto 0) (solucionHormiga (make-mochila :objetos () :valor 0 :peso 0))) 
	(loop while (> sumFi 0) do 
		(let* ((tProb (calculaProbAcum (calculaProb tFi sumFi) ))) 
			(setf objeto (seleccionaObjeto tProb))
			(setf solucionHormiga (introduceObjeto objeto solucionHormiga))
			(setf tFi (iteraFi tFi objeto solucionHormiga))
			(setf sumFi (sumatorioFi tFi))
			)
		)
		solucionHormiga
	)
)


;;
;; CALCULA PROBABILIDAD
;; Calcula la lista de probabilidades asociadas a cada objeto.


(defun calculaProb (tFi sumFi) (
	loop for x from 0 to (- *nObjetos* 1) 
	collect (/ (nth x tFi) sumFi)
	)
)

;;
;; PROBABILIDAD ACUMULADA
;; Lista de apoyo para seleccionar el objeto siguiente usando la probabilidad.

(defun calculaProbAcum (tPi) 
	(let ((acumulada 0))
		(loop for x from 0 to (- *nObjetos* 1) 
		collect 
				(setf acumulada (+ acumulada (nth x tPi)))	
		)
	)
)

;;
;; SELECCIONA OBJETO
;; Selecciona un objeto teniendo una probabilidad proporcionada.

(defun seleccionaObjeto (tablaProb) 

	(let* ((numero (generaAleatorio)) (contador 0) (objeto MOST-POSITIVE-DOUBLE-FLOAT) (enc t))
		(loop for x from 0 to (- *nObjetos* 1)
			do
			(if (and (> (nth x tablaProb) numero) (>= objeto MOST-POSITIVE-DOUBLE-FLOAT))
				(setf objeto x)
			)
		)
		objeto
	)
)

;;
;; GENERA ALEATORIO
;; Generador de un numero aleatorio.

(defun generaAleatorio ()
	(setf *random-state* (make-random-state t))
	(random 1.0)
)

;;
;; INTRODUCE OBJETO
;; Añade objeto a la solución proporcionada.

(defun introduceObjeto (objeto solucionHormiga)

	(setf (mochila-objetos solucionHormiga) (append (mochila-objetos solucionHormiga) (list objeto)))
	(setf (mochila-peso solucionHormiga) (+ (mochila-peso solucionHormiga) (first (nth objeto *tablaObjetos*))))
	(setf (mochila-valor solucionHormiga) (+ (mochila-valor solucionHormiga) (second (nth objeto *tablaObjetos*))))
	solucionHormiga

)

;;
;; ITERA Fi
;; Elimina la probabilidad de escoger los objetos ya escogidos o los que
;; no quepan en la solución porcionada.

(defun iteraFi (tablaFi objeto solucionHormiga)
	(setf (nth objeto tablaFi) 0)
	(loop for x from 0 to (- *nObjetos* 1) do
		(if (> (nth x tablaFi) 0) 
			(if (< (- *pesoMaximo* (mochila-peso solucionHormiga)) (first (nth x *tablaObjetos*)))
				(setf (nth x tablaFi) 0)
			)
		)
	)
	tablaFi
)

;;
;; ACTUALIZA FEROMONAS
;; Implementa la disipación y aumento de las feromonas.

(defun actualizaFeromonas (solucionHormiga tFer) 
		(loop for x from 0 to (- *nObjetos* 1)
			do
			(if (member x (mochila-objetos solucionHormiga))
				(setf (nth x tFer) (* (nth x tFer) 1.2))
				(setf (nth x tFer) (* (nth x tFer) 0.8))	
			)
		)		
)

;;
;; COMPARA SOLUCION
;; Actualización de la solucion global(*solucionMejor*) en caso de que
;; la solución proporcionada sea mejor.

(defun comparaSolucion (moch)
		(if (>= (mochila-valor moch) (mochila-valor *solucionMejor*))
			(if (> (mochila-valor moch) (mochila-valor *solucionMejor*))
				(setf *solucionMejor* moch)
				(if (< (mochila-peso moch) (mochila-peso *solucionMejor*))
					(setf *solucionMejor* moch)
				)
			)
		)
)

;;
;; FUNCIONES DEL EJERCICIO 2
;;

;;
;; MEJORA ITERATIVA
;; Implementación del algoritmo de mejora iterativa.

(defun mejoraIterativa (sol)
	(let* ((objetoEliminado 0) (candidatos ()) (objetoNuevo 1))
		(loop until (= objetoEliminado objetoNuevo)
			do
			(setf objetoEliminado (escogeObjeto sol))
			(setf candidatos (calculaCandidatos sol objetoEliminado))
			(setf objetoNuevo (seleccionaCandidato candidatos objetoEliminado))
			(if (NOT(= objetoNuevo objetoEliminado))
				(setf sol (make-mochila :objetos (append (list objetoNuevo) (remove objetoEliminado (mochila-objetos sol)))
						:valor (+ (mochila-valor sol) (- (second (nth objetoNuevo *tablaObjetos*)) (second (nth objetoEliminado *tablaObjetos*))))
						:peso (+ (mochila-peso sol) (- (first (nth objetoNuevo *tablaObjetos*)) (first (nth objetoEliminado *tablaObjetos*))))))
			)
		)
	)
	sol
)

;;
;; ESCOGE OBJETO
;; Escoge el peor objeto, según la heurística elegida(relación valor/peso).

(defun escogeObjeto (moch)
	(let ((objeto 0) (valorPeso MOST-POSITIVE-DOUBLE-FLOAT))
		(loop for x in (mochila-objetos moch) 
			do
				(if (> (/ (second (nth x *tablaObjetos*)) (first (nth x *tablaObjetos*))) valorPeso)
					(setf objeto x)
				)
		)
		objeto
	)

)

;;
;; CALCULA CANDIDATOS
;; Genera una lista de posibles objetos candidatos a ser seleccionados
;; para reemplazar al objeto entregado como parametro en la solución dada.
;; Elimina el objeto dado y los objetos cuyo peso sea mayor al peso restante
;; de la mochila después de eliminado el objeto proporcionado.

(defun calculaCandidatos (moch objeto)  
	(let ((candidatos ()) (pesoRestante (- *pesoMaximo* (- (mochila-peso moch) (first (nth objeto *tablaObjetos*))))))
		(loop for x from 0 to (- *nObjetos* 1)
			do 
				(if (AND (< (first (nth x *tablaObjetos*)) pesoRestante) (NOT (member x (mochila-objetos moch))))
					(setf candidatos (append (list x) candidatos))
				)
		)
		candidatos
	)
)

;;
;; SELECCIONA CANDIDATO
;; Toma una lista de candidatos y elige el mejor, según la heurística
;; valor/peso.

(defun seleccionaCandidato (candidatos objeto)
	(loop for x in candidatos 
			do		
				(if (OR (> (second (nth x *tablaObjetos*)) (second (nth objeto *tablaObjetos*))) 
						(AND (= (second (nth x *tablaObjetos*)) (second (nth objeto *tablaObjetos*))) 
							(< (first(nth x *tablaObjetos*)) (first (nth objeto *tablaObjetos*)))))
					(setf objeto x)
				)
	)
	objeto
)

;;
;; FIN FUNCIONES EJERCICIO 2
;;

;;
;; ALGORITMO GENERAL
;; Resolución completa del problema.

(defun mochila-hormigas ()
	(capturaEntrada)
	(loop for x from 1 to *noIteraciones* do
		(let* ((tablaFeromonas (inicializarFeromonas)) (tablaFi (actualizarFi tablaFeromonas)) (totalFi (sumatorioFi tablaFi)) (solucion ())) 
			(print "Itracion")
			(print x)
			(loop for y from 1 to *noHormigas* do
				(let* ( (solucionHormiga (make-mochila :objetos () :valor 0 :peso 0)))
					(setf solucionHormiga (buscaSolucion tablaFi totalFi))
					(setf solucion solucionHormiga)
					(if *mejoraIterativa*
						(setf solucionHormiga (mejoraIterativa solucionHormiga))
					)
					(actualizaFeromonas solucionHormiga tablaFeromonas)
					(setf tablaFi (actualizarFi tablaFeromonas))
					(setf totalFi (sumatorioFi tablaFi))
					(comparaSolucion solucionHormiga)
					(print y)
					(print solucionHormiga)
				)
			)
		)	
	)
	(print *solucionMejor*)
)

(mochila-hormigas)

