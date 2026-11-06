(defun c:ALINEARTEXTO ( / linea texto ang angfinal pt1 pt2 mid off dx dy resp)

  ; Seleccionar la linea
  (setq linea (car (entsel "\nSelecciona la LINEA: ")))
  (if (null linea) (exit))

  ; Verificar que sea una linea
  (if (not (eq (cdr (assoc 0 (entget linea))) "LINE"))
    (progn (alert "El objeto seleccionado no es una linea.") (exit))
  )

  ; Seleccionar el texto
  (setq texto (car (entsel "\nSelecciona el TEXTO o MTEXT: ")))
  (if (null texto) (exit))

  ; Pedir separacion
  (initget 6) ; no permite cero ni negativo
  (setq off (getreal "\nSeparacion del texto a la linea <0.5>: "))
  (if (null off) (setq off 0.5))

  ; Obtener datos de la linea
  (setq pt1 (cdr (assoc 10 (entget linea))))
  (setq pt2 (cdr (assoc 11 (entget linea))))

  ; Calcular angulo de la linea
  (setq ang (angle pt1 pt2))

  ; Calcular punto medio de la linea
  (setq mid (list
    (/ (+ (car pt1) (car pt2)) 2.0)
    (/ (+ (cadr pt1) (cadr pt2)) 2.0)
    0.0
  ))

  ; Preguntar direccion
  (initget "Normal Voltear")
  (setq resp (getkword "\nDireccion del texto? [Normal/Voltear] <Normal>: "))
  (if (null resp) (setq resp "Normal"))

  ; Aplicar volteo si el usuario lo pide
  (if (eq resp "Voltear")
    (setq angfinal (+ ang pi))
    (setq angfinal ang)
  )

  ; Calcular offset perpendicular
  (setq dx (* off (- (sin angfinal))))
  (setq dy (* off (cos angfinal)))

  ; Mover el texto al punto medio + offset
  (entmod (subst (cons 10 (list
                            (+ (car mid) dx)
                            (+ (cadr mid) dy)
                            0.0))
                 (assoc 10 (entget texto))
                 (entget texto)))

  ; Rotar el texto
  (entmod (subst (cons 50 angfinal)
                 (assoc 50 (entget texto))
                 (entget texto)))

  (entupd texto)
  (princ "\nTexto alineado correctamente.")
  (princ)
)
