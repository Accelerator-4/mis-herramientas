;;; CORTAFIN.LSP
;;; Elimina el texto que va despues del primer espacio en TEXT y MTEXT
;;; Ejemplo: "PVS-04  166.27"  ->  "PVS-04"
;;; Comando: CORTAFIN

(defun c:CORTAFIN ( / ss i n ent ed txt st p1 p2 pos new)
  (prompt "\nSeleccione los TEXT / MTEXT a recortar: ")
  (if (setq ss (ssget '((0 . "TEXT,MTEXT"))))
    (progn
      (setq i 0 n 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              ed  (entget ent)
              txt (cdr (assoc 1 ed))
              st  0)
        ;; saltar espacios iniciales
        (while (= (substr txt (1+ st) 1) " ")
          (setq st (1+ st)))
        ;; buscar primer espacio o salto de linea de MTEXT (\P)
        (setq p1 (vl-string-search " " txt st)
              p2 (vl-string-search "\\P" txt st)
              pos (cond
                    ((and p1 p2) (min p1 p2))
                    (p1 p1)
                    (p2 p2)
                    (T nil)))
        (if pos
          (progn
            (setq new (substr txt 1 pos))
            ;; cerrar llave si el MTEXT tenia formato {\f...;texto}
            (if (and (wcmatch txt "{*}") (not (wcmatch new "*}")))
              (setq new (strcat new "}")))
            (setq ed (subst (cons 1 new) (assoc 1 ed) ed))
            (entmod ed)
            (entupd ent)
            (setq n (1+ n))))
        (setq i (1+ i)))
      (prompt (strcat "\nSe modificaron " (itoa n) " texto(s)."))))
  (princ))

(princ "\nComando CORTAFIN cargado.")
(princ)