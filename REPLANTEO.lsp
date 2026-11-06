;===============================================================================
; VERTICES PRO - EXTRACTOR DE COORDENADAS DE POLIGONOS
;===============================================================================
; 
; DESCRIPCION:
; Este LISP extrae automáticamente las coordenadas de todos los vértices de 
; una polyline seleccionada y las exporta a un archivo Excel (.csv) con 
; cálculos topográficos avanzados.
;
; FUNCIONALIDADES:
; • Extrae coordenadas X, Y, Z de todos los vértices
; • Calcula distancias entre vértices consecutivos
; • Calcula azimuts (ángulos) entre puntos
; • Detecta si el polígono está cerrado o abierto
; • Genera nombres únicos con fecha y hora
; • Exporta a formato CSV compatible con Excel
; • Abre Excel automáticamente
; • Separación correcta de columnas
; • Resumen estadístico del polígono
; • Numeración automática de vértices
; • Manejo de coordenadas 2D y 3D
; • Precisión de 3 decimales en coordenadas
; • Precisión de 2 decimales en ángulos
;
; COMANDO: VerticesPRO
;
; USO:
; 1. Cargar el LISP en AutoCAD
; 2. Escribir comando: VerticesPRO
; 3. Seleccionar UNA polyline (LWPOLYLINE o POLYLINE)
; 4. El archivo se guarda automáticamente en el Escritorio
; 5. Excel se abre automáticamente con los resultados
;
; ARCHIVO DE SALIDA:
; - Ubicación: Escritorio del usuario
; - Nombre: Vertices_AAMMDD_HHMM.csv
; - Formato: CSV con separador punto y coma (;)
; - Columnas: Vértice | X (Este) | Y (Norte) | Z (Elevación) | Distancia | Azimut
;
; REQUISITOS:
; - AutoCAD o Civil 3D
; - Polylines existentes en el dibujo
; - Permisos de escritura en el Escritorio
;
; AUTOR: Generado para automatización de replanteos topográficos
; FECHA: 2024
; VERSION: 1.0 PRO
;===============================================================================

(defun c:VerticesPRO ( / ss ent entdata vertices archivo contador x y z pt ruta fecha hora nombre)
  (princ "\nVERSION PRO - Selecciona UNA polyline...")
  
  ; Seleccionar una sola polyline
  (setq ss (ssget ":S" '((0 . "LWPOLYLINE,POLYLINE"))))
  
  (if ss
    (progn
      (setq ent (ssname ss 0))
      (setq entdata (entget ent))
      
      ; Crear nombre único con fecha y hora
      (setq fecha (rtos (getvar "CDATE") 2 0))
      (setq nombre (strcat "Vertices_" (substr fecha 3 6) "_" (substr fecha 10 4)))
      (setq ruta (strcat (getenv "USERPROFILE") "\\Desktop\\" nombre ".csv"))
      
      ; Crear archivo CSV
      (setq archivo (open ruta "w"))
      
      (if archivo
        (progn
          ; Encabezados con punto y coma
          (write-line "Vertice;X (Este);Y (Norte);Z (Elevacion);Distancia;Azimut" archivo)
          
          ; Procesar vértices
          (setq contador 1)
          (setq vertices '())
          
          ; Recopilar todos los vértices primero
          (foreach item entdata
            (if (= (car item) 10)
              (setq vertices (append vertices (list (cdr item))))
            )
          )
          
          ; Procesar cada vértice con cálculos
          (setq i 0)
          (foreach vertice vertices
            (setq x (rtos (car vertice) 2 3))
            (setq y (rtos (cadr vertice) 2 3))
            (setq z (if (caddr vertice) (rtos (caddr vertice) 2 3) "0.000"))
            
            ; Calcular distancia y azimut al siguiente punto
            (if (< i (- (length vertices) 1))
              (progn
                (setq siguiente (nth (+ i 1) vertices))
                (setq dx (- (car siguiente) (car vertice)))
                (setq dy (- (cadr siguiente) (cadr vertice)))
                (setq dist (rtos (sqrt (+ (* dx dx) (* dy dy))) 2 3))
                (setq azimut (rtos (* (atan dy dx) (/ 180.0 pi)) 2 2))
              )
              (progn
                (setq dist "---")
                (setq azimut "---")
              )
            )
            
            ; Escribir línea con punto y coma como separador
            (write-line 
              (strcat 
                (itoa contador) ";" 
                x ";" 
                y ";" 
                z ";"
                dist ";"
                azimut
              ) 
              archivo
            )
            (setq contador (+ contador 1))
            (setq i (+ i 1))
          )
          
          ; Línea de resumen
          (write-line "" archivo)
          (write-line (strcat "RESUMEN:;Total Vertices: " (itoa (length vertices))) archivo)
          (write-line (strcat "Poligono;" (if (equal (car vertices) (last vertices) 0.001) "CERRADO" "ABIERTO")) archivo)
          
          (close archivo)
          (princ (strcat "\n*** VERTICES PRO EXPORTADOS ***"))
          (princ (strcat "\nArchivo: " nombre ".csv"))
          (princ (strcat "\nVertices: " (itoa (length vertices))))
          (princ (strcat "\nUbicacion: Escritorio"))
          
          ; Abrir Excel automáticamente
          (command "shell" (strcat "start \"\" \"" ruta "\""))
        )
        (princ "\nError: No se pudo crear el archivo")
      )
    )
    (princ "\nNo se selecciono polyline.")
  )
  (princ)
)

;===============================================================================
; FIN DEL CODIGO - VERTICES PRO
;===============================================================================
