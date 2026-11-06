;===============================================================================
; CUADRO DE DERROTEROS - VERSION LIMPIA
;===============================================================================

(defun c:CuadroDerroterosCAD ( / ss ent vertices tabla-pos x y num-vertices)
  (princ "\nCUADRO DE DERROTEROS - Selecciona poligono...")
  
  ; Seleccionar polígono
  (setq ss (ssget ":S" '((0 . "LWPOLYLINE,POLYLINE"))))
  
  (if ss
    (progn
      (setq ent (ssname ss 0))
      
      ; Obtener vértices
      (setq vertices (obtener-vertices-simple ent))
      (setq num-vertices (length vertices))
      
      (princ (strcat "\nVertices encontrados: " (itoa num-vertices)))
      
      (if (> num-vertices 2)
        (progn
          ; Obtener punto de inserción
          (setq tabla-pos (getpoint "\nPunto de insercion de la tabla: "))
          
          (if tabla-pos
            (progn
              (setq x (car tabla-pos))
              (setq y (cadr tabla-pos))
              
              ; DIBUJAR CUADRO
              (dibujar-cuadro-simple x y vertices num-vertices)
              
              (princ "\n*** CUADRO DE DERROTEROS GENERADO ***")
            )
          )
        )
        (princ "\nError: El poligono debe tener al menos 3 vertices")
      )
    )
    (princ "\nNo se selecciono poligono")
  )
  (princ)
)

; Función principal
(defun dibujar-cuadro-simple (x y vertices num-vertices / 
                              ancho-total altura-total
                              ancho1 ancho2 ancho3 ancho4 ancho5 ancho6 ancho7
                              alto1 alto2 alto3 alto4
                              x1 x2 x3 x4 x5 x6 x7 x8
                              y1 y2 y3 y4 y-final i)
  
  ; DEFINIR ANCHOS DE COLUMNAS
  (setq ancho1 0.85)
  (setq ancho2 0.85)
  (setq ancho3 3.13)
  (setq ancho4 1.98)
  (setq ancho5 0.80)
  (setq ancho6 2.188)
  (setq ancho7 2.188)
  
  ; DEFINIR ALTURAS DE FILAS
  (setq alto1 0.485)
  (setq alto2 0.485)
  (setq alto3 0.389)
  (setq alto4 0.45)
  
  ; CALCULAR DIMENSIONES TOTALES
  (setq ancho-total (+ ancho1 ancho2 ancho3 ancho4 ancho5 ancho6 ancho7))
  (setq altura-total (+ alto1 alto2 alto3 (* alto4 num-vertices)))
  
  ; CALCULAR POSICIONES X
  (setq x1 x)
  (setq x2 (+ x1 ancho1))
  (setq x3 (+ x2 ancho2))
  (setq x4 (+ x3 ancho3))
  (setq x5 (+ x4 ancho4))
  (setq x6 (+ x5 ancho5))
  (setq x7 (+ x6 ancho6))
  (setq x8 (+ x7 ancho7))
  
  ; CALCULAR POSICIONES Y
  (setq y1 y)
  (setq y2 (- y1 alto1))
  (setq y3 (- y2 alto2))
  (setq y4 (- y3 alto3))
  (setq y-final (- y altura-total))
  
  ; DIBUJAR RECTANGULO EXTERIOR
  (command "_.rectangle" (list x1 y1) (list x8 y-final))
  
  ; DIBUJAR LINEAS HORIZONTALES INTERNAS
  (command "_.pline" (list x1 y2) (list x8 y2) "")
  (command "_.pline" (list x1 y3) (list x8 y3) "")
  (command "_.pline" (list x1 y4) (list x8 y4) "")
  
  ; Líneas de datos
  (setq i 1)
  (repeat (- num-vertices 1)
    (command "_.pline" 
             (list x1 (- y4 (* alto4 i)))
             (list x8 (- y4 (* alto4 i)))
             "")
    (setq i (+ i 1))
  )
  
  ; DIBUJAR LINEAS VERTICALES INTERNAS
  (command "_.pline" (list x2 y2) (list x2 y-final) "")
  (command "_.pline" (list x3 y1) (list x3 y-final) "")
  (command "_.pline" (list x4 y1) (list x4 y-final) "")
  (command "_.pline" (list x5 y1) (list x5 y-final) "")
  (command "_.pline" (list x6 y1) (list x6 y-final) "")
  (command "_.pline" (list x7 y2) (list x7 y-final) "")
  
  ; DIBUJAR TEXTOS
  ; Título principal
  (command "_.text" (list (+ x1 (/ ancho-total 2)) (- y1 (/ alto1 2))) 0.25 0 "CUADRO DE DERROTEROS")
  
  ; Encabezados principales
  (command "_.text" (list (+ x1 (/ (- x3 x1) 2)) (- y2 (/ alto2 2))) 0.25 0 "LADO")
  (command "_.text" (list (+ x3 (/ ancho3 2)) (- y2 (/ alto2 2))) 0.25 0 "RUMBO")
  (command "_.text" (list (+ x4 (/ ancho4 2)) (- y2 (/ alto2 2))) 0.25 0 "DISTANCIA")
  (command "_.text" (list (+ x5 (/ ancho5 2)) (- y2 (/ alto2 2))) 0.25 0 "V")
  (command "_.text" (list (+ x6 (/ (- x8 x6) 2)) (- y2 (/ alto2 2))) 0.25 0 "COORDENADAS")
  
  ; Subencabezados
  (command "_.text" (list (+ x1 (/ ancho1 2)) (- y3 (/ alto3 2))) 0.25 0 "EST")
  (command "_.text" (list (+ x2 (/ ancho2 2)) (- y3 (/ alto3 2))) 0.25 0 "PV")
  (command "_.text" (list (+ x6 (/ ancho6 2)) (- y3 (/ alto3 2))) 0.25 0 "X")
  (command "_.text" (list (+ x7 (/ ancho7 2)) (- y3 (/ alto3 2))) 0.25 0 "Y")
  
  ; DIBUJAR DATOS
  (dibujar-datos-simple x1 x2 x3 x4 x5 x6 x7 y4 alto4 vertices num-vertices ancho1 ancho2 ancho3 ancho4 ancho5 ancho6 ancho7)
)

; Función para dibujar datos
(defun dibujar-datos-simple (x1 x2 x3 x4 x5 x6 x7 y-inicio alto-datos vertices num-vertices 
                             ancho1 ancho2 ancho3 ancho4 ancho5 ancho6 ancho7 /
                             i p1 p2 rumbo dist coord-x coord-y fila-y siguiente-vertice)
  (setq i 0)
  (repeat num-vertices
    (setq p1 (nth i vertices))
    (setq p2 (nth (if (= i (- num-vertices 1)) 0 (+ i 1)) vertices))
    
    (if (and p1 p2 (listp p1) (listp p2) (>= (length p1) 2) (>= (length p2) 2))
      (progn
        ; Calcular rumbo y distancia
        (setq rumbo (calcular-rumbo-simple p1 p2))
        (setq dist (distance p1 p2))
        (setq coord-x (car p1))
        (setq coord-y (cadr p1))
        (setq siguiente-vertice (if (= i (- num-vertices 1)) 1 (+ i 2)))
        
        ; Posición Y de la fila actual
        (setq fila-y (- y-inicio (* alto-datos i) (/ alto-datos 2)))
        
        ; DIBUJAR DATOS CENTRADOS
        (command "_.text" (list (+ x1 (/ ancho1 2)) fila-y) 0.15 0 (itoa (+ i 1)))
        (command "_.text" (list (+ x2 (/ ancho2 2)) fila-y) 0.15 0 (itoa siguiente-vertice))
        (command "_.text" (list (+ x3 (/ ancho3 2)) fila-y) 0.15 0 rumbo)
        (command "_.text" (list (+ x4 (/ ancho4 2)) fila-y) 0.15 0 (rtos dist 2 3))
        (command "_.text" (list (+ x5 (/ ancho5 2)) fila-y) 0.15 0 (itoa (+ i 1)))
        (command "_.text" (list (+ x6 (/ ancho6 2)) fila-y) 0.15 0 (rtos coord-x 2 3))
        (command "_.text" (list (+ x7 (/ ancho7 2)) fila-y) 0.15 0 (rtos coord-y 2 3))
      )
    )
    (setq i (+ i 1))
  )
)

; Función para obtener vértices
(defun obtener-vertices-simple (ent / entdata vertices)
  (setq entdata (entget ent))
  (setq vertices '())
  
  (foreach item entdata
    (if (= (car item) 10)
      (setq vertices (cons (cdr item) vertices))
    )
  )
  (reverse vertices)
)

; Función para calcular rumbo
(defun calcular-rumbo-simple (p1 p2 / dx dy azimut grados minutos segundos)
  (setq dx (- (car p2) (car p1)))
  (setq dy (- (cadr p2) (cadr p1)))
  
  (if (and (/= dx 0.0) (/= dy 0.0))
    (progn
      (setq azimut (* (atan dy dx) (/ 180.0 pi)))
      (if (< azimut 0) (setq azimut (+ azimut 360)))
      
      (cond
        ((<= azimut 90)
         (setq grados (fix azimut))
         (setq minutos (fix (* (- azimut grados) 60)))
         (setq segundos (fix (* (- (* (- azimut grados) 60) minutos) 60)))
         (strcat "N " (itoa grados) "d" (itoa minutos) "'" (itoa segundos) "E")
        )
        ((<= azimut 180)
         (setq azimut (- 180 azimut))
         (setq grados (fix azimut))
         (setq minutos (fix (* (- azimut grados) 60)))
         (setq segundos (fix (* (- (* (- azimut grados) 60) minutos) 60)))
         (strcat "S " (itoa grados) "d" (itoa minutos) "'" (itoa segundos) "E")
        )
        ((<= azimut 270)
         (setq azimut (- azimut 180))
         (setq grados (fix azimut))
         (setq minutos (fix (* (- azimut grados) 60)))
         (setq segundos (fix (* (- (* (- azimut grados) 60) minutos) 60)))
         (strcat "S " (itoa grados) "d" (itoa minutos) "'" (itoa segundos) "W")
        )
        (t
         (setq azimut (- 360 azimut))
         (setq grados (fix azimut))
         (setq minutos (fix (* (- azimut grados) 60)))
         (setq segundos (fix (* (- (* (- azimut grados) 60) minutos) 60)))
         (strcat "N " (itoa grados) "d" (itoa minutos) "'" (itoa segundos) "W")
        )
      )
    )
    "N 0d0'0E"
  )
)

;=================================
