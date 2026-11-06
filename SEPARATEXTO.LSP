(defun c:SeparaTexto ( / keyword ss i ent txt data count )
  ; Pedir la palabra clave al usuario en la línea de comandos
  (setq keyword (getstring T "\nEscribe la palabra clave a buscar en los textos: "))
  
  (if (= keyword "")
    (progn
      (princ "\nNo ingresaste ninguna palabra clave.")
      (exit)
    )
  )

  ; Buscar todos los textos y MTexts en el modelo/plano
  (setq ss (ssget "X" '((0 . "TEXT,MTEXT"))))
  
  (if ss
    (progn
      (setq count 0)
      ; Crear un nuevo conjunto de selección vacío para guardar los coincidentes
      (setq newss (ssadd))
      
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq data (entget ent))
        (setq txt (cdr (assoc 1 data)))
        
        ; Comprobar si el texto contiene la palabra clave (ignorando mayúsculas/minúsculas)
        (if (vl-string-search (strcase keyword) (strcase txt))
          (progn
            (ssadd ent newss)
            (setq count (1+ count))
          )
        )
      )
      
      ; Si encontró coincidencias, los selecciona en la pantalla
      (if (> count 0)
        (progn
          (sssetfirst nil newss) ; Selecciona los objetos visualmente en AutoCAD
          (princ (strcat "\n¡Listo! Se seleccionaron " (itoa count) " textos que contienen la palabra \"" keyword "\"."))
        )
        (princ (strcat "\nNo se encontraron textos con la palabra \"" keyword "\"."))
      )
    )
    (princ "\nNo se encontraron textos en el dibujo.")
  )
  (princ)
)