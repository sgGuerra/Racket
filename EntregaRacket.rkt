#lang racket
; Lista Coordinadores en formato "código-contraseña"
(define coordinadores
  '("4-741"
    "8-258"))


; Lista de profesores en formato "codigo-contraseña-materia"
(define profesores
  '("1-123-Programación"
    "2-456-Bases de Datos"
    "3-789-#f")) ;; #f indica que no tiene materia asignada


; Lista de Materias con sus estudiantes y notas (nota inicial = 0)
(define materias
  (list
   (list "Programación"
         (list (list "Ana" 0)
               (list "Luis" 0)))
   (list "Bases de Datos"
         (list (list "María" 0)
               (list "Pedro" 0)))))


; Función para separar los datos de un profesor o coordinador
(define (split-registros datos)
  (string-split datos "-"))

; Buscar profesor con código y clave
(define (buscar-profesor codigo clave)
  (define encontrado
    (filter (lambda (p)
              (let ((parts (split-registros p)))
                (and (= (string->number (first parts)) codigo)
                     (string=? (second parts) clave))))
            profesores))
  (if (null? encontrado)
      #f
      (split-registros (first encontrado))))


; Buscar coordinador con código y clave
(define (buscar-coordinador codigo clave)
  (define encontrado
    (filter (lambda (p)
              (let ((parts (split-registros p)))
                (and (= (string->number (first parts)) codigo)
                     (string=? (second parts) clave))))
            coordinadores))
  (if (null? encontrado)
      #f
      (split-registros (first encontrado))))


; Buscar materia por nombre
(define (buscar-materia nombre)
  (define encontrado
    (filter (lambda (m) (string=? (first m) nombre)) materias))
  (if (null? encontrado)
      #f
      (first encontrado)))

; Eliminar estudiante
(define (eliminar-estudiante materia nombre-estudiante)
  (define m (buscar-materia materia))
  (if m
      (set! materias
            (map (lambda (mat)
                   (if (string=? (first mat) materia)
                       (list (first mat)
                             (filter (lambda (e)
                                       (not (string=? (first e) nombre-estudiante)))
                                     (second mat)))
                       mat))
                 materias))
      (printf "La materia no existe\n")))

; Agregar nota a un estudiante existente
(define (agregar-nota materia nombre-estudiante nota)
  (define m (buscar-materia materia))
  (if (not m)
      (printf "La materia no existe\n")
      (if (and (>= nota 0) (<= nota 5))
          (let ((estudiantes (second m)))
            (if (assoc nombre-estudiante estudiantes) ; ya está inscrito?
                (set! materias
                      (map (lambda (mat)
                             (if (string=? (first mat) materia)
                                 (list (first mat)
                                       (map (lambda (e)
                                              (if (string=? (first e) nombre-estudiante)
                                                  (begin
                                                    (printf "Nota agregada para ~a\n" (first e))
                                                    (list (first e) nota))
                                                  e))
                                            (second mat)))
                                 mat))
                           materias))
                (printf "Error: El estudiante ~a no está inscrito en ~a\n"
                        nombre-estudiante materia)))
          (printf "Error: La nota debe estar entre 0 y 5\n"))))

; Modificar nota de un estudiante existente
(define (modificar-nota materia nombre-estudiante nota)
  (if (and (>= nota 0) (<= nota 5))
      (set! materias
            (map (lambda (mat)
                   (if (string=? (first mat) materia)
                       (let ((estudiantes (second mat)))
                         (if (assoc nombre-estudiante estudiantes)
                             (list (first mat)
                                   (map (lambda (e)
                                          (if (string=? (first e) nombre-estudiante)
                                              (begin
                                                (printf "Nota modificada para ~a\n" (first e))
                                                (list (first e) nota))
                                              e))
                                        estudiantes))
                             (begin
                               (printf "Error: El estudiante ~a no está inscrito en ~a\n"
                                       nombre-estudiante materia)
                               mat)))
                       mat))
                 materias))
      (printf "Error: La nota debe estar entre 0 y 5\n")))

; Mostrar estudiantes
(define (mostrar-estudiantes materia)
  (define m (buscar-materia materia))
  (if m
      (begin
        (printf "Estudiantes en ~a:\n" materia)
        (for-each (lambda (e)
                    (printf "- ~a: ~a\n" (first e) (second e)))
                  (second m)))
      (printf "La materia no existe\n")))

; Calcular promedio
(define (promedio-materia materia)
  (define m (buscar-materia materia))
  (if (and m (not (null? (second m))))
      (let* ((notas (map second (second m)))
             (suma (apply + notas))
             (prom (/ suma (length notas))))
        (printf "El promedio de ~a es: ~a\n" materia prom))
      (printf "No hay estudiantes en la materia o no existe\n")))

; Agregar estudiante a una materia
(define (agregar-estudiante materia nombre-estudiante)
  (define m (buscar-materia materia))
  (if (not m)
      (printf "La materia no existe\n")
      (let ((estudiantes (second m)))
        (if (assoc nombre-estudiante estudiantes)
            (printf "Error: El estudiante ~a ya está inscrito en ~a\n" nombre-estudiante materia)
            (begin
              (set! materias
                    (map (lambda (mat)
                           (if (string=? (first mat) materia)
                               (list (first mat)
                                     (append estudiantes (list (list nombre-estudiante 0))))
                               mat))
                         materias))
              (printf "Estudiante ~a agregado a ~a con nota inicial 0\n" nombre-estudiante materia))))))

(define (agregar-profesor codigo clave materia)
  ; Validación 1: la materia NO debe existir ya
  (if (buscar-materia materia)
      (printf "Error: La materia '~a' ya existe. No se puede asignar un nuevo profesor.\n" materia)
      ; Validación 2: verificar si el código ya existe
      (let ([prof-existente 
             (findf (lambda (p)
                      (string=? (first (split-registros p)) codigo))
                    profesores)])
        (cond
          ;Profesor existe y tiene "#f" → permitir reasignación
          [(and prof-existente 
                (string=? (third (split-registros prof-existente)) "#f"))
           (printf "El profesor ~a no tiene materia asignada. Asignando '~a'...\n" codigo materia)
           ; Actualizar el registro del profesor
           (set! profesores
                 (map (lambda (p)
                        (if (string=? (first (split-registros p)) codigo)
                            (string-append codigo "-" clave "-" materia)
                            p))
                      profesores))
           ; Crear la materia vacía
           (set! materias (append materias (list (list materia '()))))
           (printf "Materia '~a' asignada al profesor ~a.\n" materia codigo)]

          ; Caso B: Profesor existe y ya tiene una materia (no es "#f")
          [prof-existente
           (printf "Error: Ya existe un profesor con el código ~a, asignado a la materia '~a'.\n"
                   codigo (third (split-registros prof-existente)))]

          ; Caso C: Profesor no existe → crear nuevo
          [else
           (set! profesores 
                 (append profesores 
                         (list (string-append codigo "-" clave "-" materia))))
           (set! materias (append materias (list (list materia '()))))
           (printf "Profesor agregado con éxito:\n")
           (printf "Código: ~a\n" codigo)
           (printf "Materia: ~a\n" materia)
           (printf "La materia '~a' ha sido creada (sin estudiantes).\n" materia)]))))


; Menú de materia
(define (menu-materia materia)
  (let loop ()
    (printf "\n--- Menú de ~a ---\n" materia)
    (printf "1. Agregar nota\n")
    (printf "2. Modificar nota\n")
    (printf "3. Mostrar estudiantes\n")
    (printf "4. Mostrar promedio\n")
    (printf "5. Volver al menú principal\n")
    (printf "Seleccione una opción: ")
    (define opcion (string->number (read-line)))
    (cond
      [(= opcion 1)
       (printf "Nombre del estudiante: ")
       (define nombre (read-line))
       (printf "Nota (0 a 5): ")
       (define nota (string->number (read-line)))
       (agregar-nota materia nombre nota)
       (loop)]
      [(= opcion 2)
       (printf "Nombre del estudiante: ")
       (define nombre (read-line))
       (printf "Nueva nota (0 a 5): ")
       (define nota (string->number (read-line)))
       (modificar-nota materia nombre nota)
       (loop)]
      [(= opcion 3)
       (mostrar-estudiantes materia)
       (loop)]
      [(= opcion 4)
       (promedio-materia materia)
       (loop)]
      [(= opcion 5)
       (printf "Volviendo al menú principal...\n")
       (menu-principal)]
      [else
       (printf "Opción inválida\n")
       (loop)])))

; Menú profesor
(define (menu-profesor)
  (printf "Ingrese su código de profesor: ")
  (define codigo (string->number (read-line)))
  (printf "Ingrese su contraseña: ")
  (define clave (read-line))
  (define profesor (buscar-profesor codigo clave))
  (if profesor
      (let ((materia (third profesor)))
        (if (not (string=? materia "#f"))
            (menu-materia materia)
            (printf "Error: No tiene materia asignada\n")))
      (printf "Credenciales inválidas\n")))

; Login coordinador
(define (login-coordinador)
  (printf "Ingrese su código de coordinador: ")
  (define codigo (string->number (read-line)))
  (printf "Ingrese su contraseña: ")
  (define clave (read-line))
  (define coordinador (buscar-coordinador codigo clave))
  (if coordinador
      (menu-coordinador)    
      (printf "Credenciales inválidas\n")))


; Menu coordinador
(define (menu-coordinador)
  (let loop ()
    (printf "\n--- Menú de coordinador ---\n" )
    (printf "1. Agregar profesor\n")
    (printf "2. Agregar estudiante\n")
    (printf "3. Volver al menú principal\n")
    (printf "Seleccione una opción: ")
    (define opcion (string->number (read-line)))
    (cond
      [(= opcion 1)
       (printf "Crear código del profesor: ")
       (define codigo (read-line))
       (printf "Crear clave del profesor: ")
       (define clave (read-line))
       (printf "Nueva materia a ingresar: ")
       (define materia (read-line))
       (agregar-profesor codigo clave materia)
       (loop)]
      [(= opcion 2)
       (printf "Nombre de la materia: ")
       (define materia (read-line))
       (printf "Nombre del estudiante: ")
       (define nombre-estudiante (read-line))
       (agregar-estudiante materia nombre-estudiante)    
       (loop)]
      [(= opcion 3)
       (printf "Volviendo al menú principal...\n")
       (menu-principal)]
      [else
       (printf "Opción inválida\n")
       (loop)]
  )))

; Menú principal

(define (menu-principal)
  (printf "-------- MÉNU PRINCIPAL --------\n")
  (printf "¡Hola de nuevo!\n")
  (let loop ()
    (printf "1. Ingresar como coordinar\n")
    (printf "2. Ingresar como profesor\n")
    (printf "3. Salir\n")
    (printf "Seleccione una opción: ")
    (define opcion (string->number (read-line)))
    (cond
      [(= opcion 1)
       (printf "Redirigiendo al menú de coordinadores...\n")
       (login-coordinador)]
      [(= opcion 2)
       (printf "Redirigiendo al menú de profesores...\n")
       (menu-profesor)]
      [(= opcion 3)
       (printf "Cerrando el programa...\n")
       ]
      [else
       (printf "Opción inválida\n")
       (loop)]
      )
  ))

; Inicio del programa
(menu-principal)
