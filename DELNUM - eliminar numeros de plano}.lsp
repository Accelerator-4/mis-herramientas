;; BORRA3.LSP (v3)
;; Elimina en todos los layouts los MText formados SOLO por digitos
;; con una cantidad exacta de cifras (por defecto 3: 201, 202, ...).
;; NO toca el espacio modelo ni textos con letras o simbolos.
;; Comando: BORRA3

(defun LimpiaMT (s / i n c r nx)
  (setq i 1 n (strlen s) r "")
  (while (<= i n)
    (setq c (substr s i 1))
    (cond
      ((= c "\\")
       (setq nx (substr s (1+ i) 1))
       (cond
         ((= nx "P")
          (setq r (strcat r " ") i (+ i 2)))
         ((= nx "~")
          (setq r (strcat r " ") i (+ i 2)))
         ((member nx '("\\" "{" "}"))
          (setq r (strcat r nx) i (+ i 2)))
         ((member nx '("L" "l" "O" "o" "K" "k"))
          (setq i (+ i 2)))
         (T
          (setq i (1+ i))
          (while (and (<= i n) (/= (substr s i 1) ";"))
            (setq i (1+ i)))
          (setq i (1+ i)))))
      ((member c '("{" "}")) (setq i (1+ i)))
      (T (setq r (strcat r c) i (1+ i)))))
  r)

(defun c:BORRA3 ( / doc lay blk obj txt cont lista err cifras patron k)
  (vl-load-com)
  (setq cifras (getint "\nCantidad de cifras a buscar <3>: "))
  (if (not cifras) (setq cifras 3))
  ;; Patron de comodines: "###" para 3 digitos
  (setq patron "" k 0)
  (repeat cifras (setq patron (strcat patron "#")))
  (setq doc (vla-get-activedocument (vlax-get-acad-object))
        cont 0)
  (vlax-for lay (vla-get-layouts doc)
    (if (/= (strcase (vla-get-name lay)) "MODEL")
      (progn
        (setq blk (vla-get-block lay)
              lista nil)
        (vlax-for obj blk
          (if (= (vla-get-objectname obj) "AcDbMText")
            (progn
              (setq txt (vl-string-trim " " (LimpiaMT (vla-get-textstring obj))))
              (if (wcmatch txt patron)
                (setq lista (cons obj lista))))))
        (foreach obj lista
          (setq err (vl-catch-all-apply 'vla-delete (list obj)))
          (if (not (vl-catch-all-error-p err))
            (setq cont (1+ cont)))))))
  (vla-regen doc acallviewports)
  (princ (strcat "\nMText numericos de " (itoa cifras) " cifras eliminados en layouts: " (itoa cont)))
  (princ))

(princ "\nBORRA3 cargado. Escribe BORRA3 para ejecutar.")
(princ)
