#lang racket
;; Alumno: Cristhian Gonzalo Marecos Sanchez
;;
;; Basado en: http://www-cs-faculty.stanford.edu/~zelenski/rsg/handouts/107_hw1c.pdf



;; La funcion GOA es el punto de entrada para ejecutar el programa.
;; 
;; Ejemplo de uso:
;;        (goa 'Gramatica-No-termine-mi-proyecto-porque)
;;
;; El parametro corresponde a un nombre de una gramatica guardada en la 
;; variable global *gramaticas*
;;
;;
(define (goa nombre-de-gramatica) 
   (if(hay-gramatica nombre-de-gramatica)
     (escribir (retornar-gramatica nombre-de-gramatica)
            )
     #f)
  )

;; Esta funcion tal vez te sea util
;;
;; Ejemplo de uso:
;;
;;  (es-expandible '<inicio>)   ==> T
;;
;;  (es-expandible 'palabra)    ==> nil
;;
;; Esta funcion nos sirve para distinguir entre simbolos
;; que son terminales o no. 
;;

(define (es-expandible simbolo)
    (if (symbol? simbolo) 
        [let* ((palabra (~a simbolo))
               (longitud (string-length palabra))
               (inicio (- longitud 1)))
          [cond ((not (equal? "<" (substring palabra 0 1))) #f)
                ((not (equal? ">" (substring palabra inicio longitud))) #f)
                (else #t)]]
        #f))


;; Incluye mas funciones tuyas segun tu necesidad


;Busca si existe la gramatica 
(define (hay-gramatica nombre)
  (ormap (lambda (x) (eq?(car x)nombre))*gramaticas*)
  )

;Retorna la gramatica buscada
(define (retornar-gramatica nombre)
  (car(filter (lambda (x) (eq? (car x) nombre))*gramaticas*))
  ) 

;Retorna la lista del expandible
(define (retorna-lista nombre gramatica)
  (cdr(car(filter(lambda(x) (eq? (car x) nombre))gramatica)))
  )

;Retorna una palabra aleatoria
;;nombre = nombre del expandible a buscar
;;gramatica = lista de expandibles de la gramatica
(define (retornar-expandibles nombre gramatica)
  (list-ref(retorna-lista  nombre  gramatica)
           (random (length (retorna-lista  nombre gramatica))
                   )
           )
  )

;Funcion recursiva que genera oracion
;;lista = oracion inicio
;;gramatica = lista de los expandibles de la gramatica especificada
(define (oracion lista gramatica)
  (string-join (map(lambda(x)(if(es-expandible x)(oracion(retornar-expandibles x gramatica) gramatica) (~a x)))lista))
  )

;Funcion para elaborar la oracion
(define(escribir gramatica)
  (oracion(car(cdr(car(cdr gramatica)))) (cdr gramatica))
  )


;; Definicion de gramaticas
;; 
;; Es importante que tus nuevas gramaticas sigan el mismo formato que las
;; incluidas aqui
;;
(define *gramaticas* '(

   ;; Gramatica para generar poemas
   ;;
   (Gramatica-para-poemas
       (<inicio>
              (Las <objeto> <verbo> esta noche.))
       
       (<objeto>
              (olas)
              (flores amarillas grandes)
              (sanguijuelas))
       
       (<verbo>
              (suspiran <adverbio>)
              (presagian como <objeto>)
              (mueren <adverbio>))
       
       (<adverbio>
              (cautelosamente)
              (cascarrabiosamente)))


   
 ;; Gramatica para generar excusas para pedir prorrogas para tu proyecto
 ;;
 (Gramatica-No-termine-mi-proyecto-porque
  
   (<inicio>
      (Necesito una prórroga porque <suplica> #\.))
   
   (<suplica>
      (<excusa-dudosa>)
      (<excusa-dudosa>)
      (<excusa-dudosa>)
      (<excusa-dudosa>)
      (<excusa-dudosa>)
      (<excusa-dudosa> #\, y luego <suplica> )
      (<excusa-dudosa> #\, y encima de eso <suplica> )
      (<excusa-dudosa> #\, y como si eso no fuera suficiente <suplica> )
      (<excusa-dudosa> #\, y escucha esto #\, <suplica> )
      (<excusa-dudosa> #\, y justo entonces <suplica> )
      (<excusa-dudosa> #\, y #\, bueno estoy un poco avergonzado por esto #\, pero <suplica> )
      (<excusa-dudosa> #\, y estoy seguro que has escuchado esto antes #\, pero <suplica> )
      (<excusa-dudosa> #\, o #\, y luego <suplica> )
      (<excusa-dudosa> #\, y y si recuerdo correctamente <suplica> ))
   
   (<excusa-dudosa>
      (mi disco duro se borro)
      (el perro comió mi <algo>)
      (la persona con la que vivo comió mi <algo>)
      (no sabía que yo estaba en esta materia)
      (pensé que ya me había graduado)
      (mi casa se quemó)
      (pasé todo el fin de semana con una resaca) 
      (tuve <mucho-trabajo>)
      (tuve <mucho-trabajo>)
      (bueno #\, ya no recuerdo mucho de lo que pasó)
      (tuve que irme a <evento-atletico>)
      (tuve que practicar para <evento-atletico>)
      (tuve que preocuparme de <evento-atletico>)
      (perdí plata apostando en <evento-atletico>)
      (me olvidé de cómo escribir)
      (todos mis lápices se rompieron)
      (la librería ya no tenía borradores)
      (se me terminé todo mi papel)
      (tuve que irme a un evento muy importante)
      (me quedé atrapado en una tormenta)
      (mi karma no estaba bueno)
      (no tenía ganas de trabajar)
      (estaba demasiado lindo afuera)
      (el lenguaje de programación no era suficientemente abstracto)
      (tuve que lavar mi ropa)
      (perdí mi <algo>)
      (mi <algo> tuve un problema privado)
      (mi <algo> fue confiscado por aduana)
      (mi <algo> fue envuelto en una bruma misteriosa por tres dias y luego desapareció)
      (tuve sueños recurrentes sobre <algo>))
   
   (<mucho-trabajo>
      (<numero-impresionante> parciales)
      (<numero-impresionante> parciales y <numero-impresionante> tareas)
      (que terminar mi propuesta de tesis)
      (<numero-impresionante> programas en <numero-impresionante> lenguajes distintos))
   
   (<evento-atletico>
      (las unimpiadas)
      (una lucha libre de yacares)
      (un partido de futbol)
      (las semi-finales de danza paraguaya))
   
   (<numero-impresionante>
      (4)
      (7)
      (como #\, un millón de)
      (toneladas de)
      (mega)
      (como #\, muchisimos #\,))
   
   (<algo>
      (disco duro)
      (cd)
      (mochila)
      (mente)
      (sentido de propósito)
      (libro)
      (anotaciones)
      (mi compu)
      (mi notebook)
      (especificacion del módulo)
      (código fuente)
      (sueños)
      (motivación)))
 ))




