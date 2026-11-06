;; Busca el patron LETRAS-NUMERO dentro del texto, desde la posicion "desde".
;; Devuelve (inicio prefijo fin) o nil
(defun rn:parse (txt desde / len s a b res)
  (setq len (strlen txt) s desde)
  (while (and (null res) (<= s len))
    (if (and (wcmatch (substr txt s 1) "@")
             (or (= s 1) (not (wcmatch (substr txt (1- s) 1) "@"))))
      (progn
        (setq a s)
        (while (and (<= a len) (wcmatch (substr txt a 1) "@"))
          (setq a (1+ a)))
        (if (and (< a len)
                 (= (substr txt a 1) "-")
                 (wcmatch (substr txt (1+ a) 1) "#"))
          (progn
            (setq b (1+ a))
            (while (and (<= b len) (wcmatch (substr txt b 1) "#"))
              (setq b (1+ b)))
            (setq res (list s (substr txt s (- a s)) b))))))
    (setq s (1+ s)))
  res)

;; Convierte "PVS, PVP" en ("PVS" "PVP")
(defun rn:split (str / lst cur i ch)
  (setq lst nil cur "" i 1)
  (while (<= i (strlen str))
    (setq ch (substr str i 1))
    (if (or (= ch ",") (= ch " ") (= ch ";"))
      (progn (if (/= cur "") (setq lst (append lst (list cur))))
             (setq cur ""))
      (setq cur (strcat cur ch)))
    (setq i (1+ i)))
  (if (/= cur "") (setq lst (append lst (list cur))))
  lst)

(defun c:RENUM (/ ini filtros ss i ent dat txt p key cnt tot lista par num out pos chg)
  (setq ini (getint "\nNúmero inicial <1>: "))
  (if (null ini) (setq ini 1))
  (setq filtros (rn:split (strcase (getstring "\nPrefijo(s) a renumerar (ej. PVS  o  PVS,PVP) o Enter para TODOS: "))))
  (prompt "\nSelecciona los textos EN ORDEN: ")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq i 0 tot 0 lista nil)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              dat (entget ent)
              txt (cdr (assoc 1 dat))
              out "" pos 1 chg nil)
        (if txt
          (progn
            (while (setq p (rn:parse txt pos))
              (setq key (strcase (cadr p)))
              (if (or (null filtros) (member key filtros))
                (progn
                  (setq par (assoc key lista))
                  (if (null par)
                    (setq lista (cons (cons key ini) lista)
                          par   (assoc key lista)))
                  (setq cnt (cdr par))
                  (setq num (strcat (if (< cnt 10) "0" "") (itoa cnt)))
                  (setq out (strcat out
                                    (substr txt pos (- (car p) pos))
                                    (cadr p) "-" num))
                  (setq lista (subst (cons key (1+ cnt)) par lista))
                  (setq tot (1+ tot) chg T))
                ;; no esta en el filtro: se copia tal cual
                (setq out (strcat out (substr txt pos (- (caddr p) pos)))))
              (setq pos (caddr p)))
            (if chg
              (progn
                (setq out (strcat out (substr txt pos)))
                (entmod (subst (cons 1 out) (assoc 1 dat) dat))
                (entupd ent)))))
        (setq i (1+ i)))
      (prompt (strcat "\nSe renumeraron " (itoa tot) " numeraciones."))
      (foreach x lista
        (prompt (strcat "\n  " (car x) ": del " (itoa ini)
                        " al " (itoa (1- (cdr x)))))))
    (prompt "\nNo se seleccionó nada."))
  (princ))