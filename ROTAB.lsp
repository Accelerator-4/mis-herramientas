;;; ===========================================================================
;;; ROTAB.lsp  -  Comando: ROTAB
;;; Rota / escala tablas y ENSANCHA las columnas cuyo texto no cabe
;;; ===========================================================================
;;;
;;; DESCRIPCION
;;;   Rota y escala tablas dibujadas con polilineas/lineas y textos (por
;;;   ejemplo las tablas de datos de pozos PVP). Tambien centra cada texto en
;;;   su celda y colorea la tabla.
;;;
;;; QUE HACE
;;;   1. Rota la tabla completa alrededor de un punto base hasta el angulo
;;;      indicado (angulo absoluto, no relativo al actual).
;;;   2. Escala la TABLA COMPLETA (lineas + textos) con un factor
;;;      (1 = tamano original).
;;;   3. ENSANCHA las columnas cuyo texto no cabe: la columna completa crece
;;;      lo necesario (texto + margen) y lo que esta a su derecha se desplaza.
;;;      Los textos NO se comprimen ni cambian de tamano.
;;;   4. Centra cada TEXT en su celda (justificacion Medio-Centro) y lo
;;;      alinea con el angulo de la tabla.
;;;   5. Colorea: borde exterior de la tabla en CYAN (4) y lineas internas
;;;      en ROJO (1). Si una polilinea mezcla borde e interior, la divide en
;;;      segmentos para poder colorear cada parte.
;;;   DIFERENCIA CON ROTAJ: ROTAB solo ensancha columnas; nunca las achica.
;;;
;;; COMO USARLO
;;;   1. Cargar el archivo con APPLOAD (o arrastrarlo a la ventana de CAD).
;;;   2. Escribir ROTAB.
;;;   3. Factor de escala: Enter para usar el valor guardado o escribir uno
;;;      nuevo (ej. 0.5 = mitad, 2 = doble).
;;;   4. Angulo de rotacion: Enter para usar el valor guardado, escribir un
;;;      angulo en grados, o indicar 2 puntos para alinear la tabla.
;;;   5. Seleccionar UNA tabla (sus polilineas/lineas y textos) con una
;;;      ventana de seleccion.
;;;   6. Indicar el punto base de rotacion/escala (ej. una esquina de la
;;;      tabla; ese punto no se mueve).
;;;   7. Repetir con otras tablas (se usan los mismos factor y angulo).
;;;      Enter sin seleccionar nada para terminar.
;;;
;;; VALORES GUARDADOS
;;;   El factor y el angulo se guardan en el registro de Windows
;;;   (variables ROTAB_FACTOR y ROTAB_ANGULO) y se reutilizan como valor
;;;   por defecto hasta que se escriban otros. Se comparten entre ROTAB y
;;;   ROTAJ y se conservan al cerrar el CAD.
;;;
;;; NOTAS Y LIMITACIONES
;;;   - Seleccionar una sola tabla por vez.
;;;   - Funciona con LINE, LWPOLYLINE (sin arcos) y TEXT. Los MTEXT solo se
;;;     rotan/escalan/mueven, no se centran ni se miden.
;;;   - El factor se aplica cada vez que se procesa una tabla: volver a
;;;     ejecutar el comando sobre una tabla ya escalada la escala otra vez
;;;     (usar factor 1 si solo se quiere rotar o reajustar).
;;;   - Se asume UCS = WCS. Todo el proceso se puede deshacer con UNDO.
;;; ===========================================================================

(vl-load-com)

;; Rota punto P alrededor de C un angulo A (radianes)
(defun rb:rot (p c a / dx dy)
  (setq dx (- (car p) (car c))
        dy (- (cadr p) (cadr c)))
  (list (+ (car c) (- (* dx (cos a)) (* dy (sin a))))
        (+ (cadr c) (* dx (sin a)) (* dy (cos a)))))

;; Segmentos ((p1 p2) ...) de un LINE o LWPOLYLINE
(defun rb:segs (ed / typ vs v1 res)
  (setq typ (cdr (assoc 0 ed)))
  (cond
    ((= typ "LINE")
     (list (list (cdr (assoc 10 ed)) (cdr (assoc 11 ed)))))
    ((= typ "LWPOLYLINE")
     (setq vs (mapcar 'cdr
                      (vl-remove-if-not '(lambda (x) (= (car x) 10)) ed)))
     (setq v1 (car vs))
     (while (cdr vs)
       (setq res (cons (list (car vs) (cadr vs)) res)
             vs  (cdr vs)))
     (if (and (= 1 (logand 1 (cdr (assoc 70 ed)))) (not (equal (car vs) v1 1e-9)))
       (setq res (cons (list (car vs) v1) res)))
     (reverse res))
    (T nil)))

;; Es segmento de borde? (seg en coordenadas locales)
(defun rb:isborder (s bb tol / ax ay bx by)
  (setq ax (car (car s))  ay (cadr (car s))
        bx (car (cadr s)) by (cadr (cadr s)))
  (cond
    ((and (< (abs (- ax bx)) tol)
          (or (< (abs (- ax (nth 0 bb))) tol) (< (abs (- ax (nth 1 bb))) tol))) T)
    ((and (< (abs (- ay by)) tol)
          (or (< (abs (- ay (nth 2 bb))) tol) (< (abs (- ay (nth 3 bb))) tol))) T)
    (T nil)))

;; --- Las siguientes funciones usan variables locales de rb:proc
;; --- (alcance dinamico): lst base ang1 lsegs allpts bb tol

;; Recalcula segmentos locales, bounding box y tolerancia
(defun rb:geom ( / e ed s p1 p2 minx maxx miny maxy)
  (setq lsegs nil allpts nil)
  (foreach e lst
    (setq ed (entget e))
    (foreach s (rb:segs ed)
      (setq p1 (rb:rot (car s)  base (- ang1))
            p2 (rb:rot (cadr s) base (- ang1)))
      (setq lsegs  (cons (list p1 p2) lsegs)
            allpts (cons p1 (cons p2 allpts)))))
  (if (null lsegs)
    (progn (princ "\nNo hay lineas/polilineas en la seleccion.") (exit)))
  (setq minx (apply 'min (mapcar 'car allpts))
        maxx (apply 'max (mapcar 'car allpts))
        miny (apply 'min (mapcar 'cadr allpts))
        maxy (apply 'max (mapcar 'cadr allpts))
        bb   (list minx maxx miny maxy)
        tol  (max (* 0.002 (max (- maxx minx) (- maxy miny))) 1e-6)))

;; Celda (l r b t) que contiene el punto local (px py), o nil
(defun rb:cell (px py / l r b tp s x0 y0 x1 y1)
  (foreach s lsegs
    (setq x0 (car (car s))  y0 (cadr (car s))
          x1 (car (cadr s)) y1 (cadr (cadr s)))
    (cond
      ((and (< (abs (- x0 x1)) tol) (> (abs (- y0 y1)) tol))
       (if (and (>= py (- (min y0 y1) tol)) (<= py (+ (max y0 y1) tol)))
         (cond ((< x0 px) (if (or (null l) (> x0 l)) (setq l x0)))
               ((> x0 px) (if (or (null r) (< x0 r)) (setq r x0))))))
      ((and (< (abs (- y0 y1)) tol) (> (abs (- x0 x1)) tol))
       (if (and (>= px (- (min x0 x1) tol)) (<= px (+ (max x0 x1) tol)))
         (cond ((< y0 py) (if (or (null b) (> y0 b)) (setq b y0)))
               ((> y0 py) (if (or (null tp) (< y0 tp)) (setq tp y0))))))))
  (if (and l r b tp) (list l r b tp)))

;; Info de un TEXT: (centro-mundo ancho alto)
(defun rb:tinfo (ed / h wf tb w j72 j73 rot anc ox oy)
  (setq h   (cdr (assoc 40 ed))
        tb  (textbox ed)
        w   (abs (- (car (cadr tb)) (car (car tb))))
        j72 (if (assoc 72 ed) (cdr (assoc 72 ed)) 0)
        j73 (if (assoc 73 ed) (cdr (assoc 73 ed)) 0)
        rot (cdr (assoc 50 ed))
        anc (if (and (= j72 0) (= j73 0))
              (cdr (assoc 10 ed)) (cdr (assoc 11 ed))))
  (cond
    ((= j72 4) (setq ox 0.0 oy 0.0))
    (T
     (setq ox (cond ((= j72 0) (/ w 2.0))
                    ((= j72 2) (/ w -2.0))
                    (T 0.0)))
     (setq oy (cond ((<= j73 1) (/ h 2.0))
                    ((= j73 3) (/ h -2.0))
                    (T 0.0)))))
  (list (list (+ (car anc) (- (* ox (cos rot)) (* oy (sin rot))))
              (+ (cadr anc) (* ox (sin rot)) (* oy (cos rot)))
              (if (caddr anc) (caddr anc) 0.0))
        w h))

;; Procesa una tabla
(defun rb:proc (ss base ang1 fac / lst i e ed ang0 delta obj segs lsegs allpts
                bb tol cls col s p1 p2 lay elev
                ti cen w h lc cell l r b tp cx cy zz nfit nskip
                cols c key need d shifts pad)
  ;; lista de entidades
  (setq i 0)
  (repeat (sslength ss)
    (setq lst (cons (ssname ss i) lst) i (1+ i)))

  ;; angulo actual de la tabla = rotacion del primer texto
  (setq ang0 0.0)
  (foreach e lst
    (setq ed (entget e))
    (if (and (member (cdr (assoc 0 ed)) '("TEXT" "MTEXT")) (zerop ang0))
      (setq ang0 (cdr (assoc 50 ed)))))
  (setq delta (- ang1 ang0))

  ;; rotar todo
  (foreach e lst
    (vla-rotate (vlax-ename->vla-object e) (vlax-3d-point base) delta))

  ;; escalar TODA la tabla (lineas + textos) desde el punto base
  (if (not (equal fac 1.0 1e-9))
    (foreach e lst
      (vla-ScaleEntity (vlax-ename->vla-object e) (vlax-3d-point base) fac)))

  (rb:geom)

  ;; ---------- ENSANCHAR COLUMNAS DONDE EL TEXTO NO CABE ----------
  ;; cols = lista de (l r ancho-necesario) por columna
  (foreach e lst
    (setq ed (entget e))
    (if (= "TEXT" (cdr (assoc 0 ed)))
      (progn
        (setq ti   (rb:tinfo ed)
              cen  (car ti) w (cadr ti) h (caddr ti)
              lc   (rb:rot cen base (- ang1))
              cell (rb:cell (car lc) (cadr lc)))
        (if cell
          (progn
            (setq need (+ w (* 0.8 h))   ; texto + margen a cada lado
                  key  nil)
            (foreach c cols
              (if (and (equal (car c) (car cell) tol)
                       (equal (cadr c) (cadr cell) tol))
                (setq key c)))
            (if key
              (if (> need (caddr key))
                (setq cols (subst (list (car key) (cadr key) need) key cols)))
              (setq cols (cons (list (car cell) (cadr cell) need) cols))))))))

  ;; desplazamientos: (x-derecha-de-columna  incremento)
  (setq shifts nil)
  (foreach c cols
    (setq d (- (caddr c) (- (cadr c) (car c))))
    (if (> d tol)
      (setq shifts (cons (list (cadr c) d) shifts))))

  (if shifts
    (progn
      ;; mover vertices: cada X local >= borde derecho de una columna
      ;; ensanchada se desplaza el incremento correspondiente
      (foreach e lst
        (setq ed (entget e))
        (if (member (cdr (assoc 0 ed)) '("LINE" "LWPOLYLINE"))
          (progn
            (setq ed
              (mapcar
                '(lambda (g / p lp dx z)
                   (if (member (car g) '(10 11))
                     (progn
                       (setq p  (cdr g)
                             z  (caddr p)
                             lp (rb:rot p base (- ang1))
                             dx 0.0)
                       (foreach s shifts
                         (if (>= (car lp) (- (car s) tol))
                           (setq dx (+ dx (cadr s)))))
                       (setq lp (rb:rot (list (+ (car lp) dx) (cadr lp)) base ang1))
                       (cons (car g)
                             (if z (list (car lp) (cadr lp) z) (list (car lp) (cadr lp)))))
                     g))
                ed))
            (entmod ed))))
      ;; mover tambien los textos que quedan a la derecha de cada columna ensanchada
      (foreach e lst
        (setq ed (entget e))
        (if (member (cdr (assoc 0 ed)) '("TEXT" "MTEXT"))
          (progn
            (setq cen (if (= "TEXT" (cdr (assoc 0 ed)))
                        (car (rb:tinfo ed))
                        (cdr (assoc 10 ed)))
                  lc  (rb:rot cen base (- ang1))
                  d   0.0)
            (foreach s shifts
              (if (>= (car lc) (- (car s) tol))
                (setq d (+ d (cadr s)))))
            (if (> d 0.0)
              (vla-move (vlax-ename->vla-object e)
                        (vlax-3d-point '(0.0 0.0 0.0))
                        (vlax-3d-point (list (* d (cos ang1)) (* d (sin ang1)) 0.0)))))))
      (rb:geom)))

  ;; ---------- TEXTOS: centrar en la celda ----------
  (setq nfit 0 nskip 0)
  (foreach e lst
    (setq ed (entget e))
    (if (= "TEXT" (cdr (assoc 0 ed)))
      (progn
        (setq ti   (rb:tinfo ed)
              cen  (car ti)
              zz   (caddr cen)
              lc   (rb:rot cen base (- ang1))
              cell (rb:cell (car lc) (cadr lc)))
        (if cell
          (progn
            (setq cx (/ (+ (car cell) (cadr cell)) 2.0)
                  cy (/ (+ (nth 2 cell) (nth 3 cell)) 2.0))
            (setq obj (vlax-ename->vla-object e))
            (vla-put-Rotation obj ang1)
            (vla-put-Alignment obj 10) ; acAlignmentMiddleCenter
            (setq cen (rb:rot (list cx cy) base ang1))
            (vla-put-TextAlignmentPoint
              obj (vlax-3d-point (list (car cen) (cadr cen) zz)))
            (setq nfit (1+ nfit)))
          (setq nskip (1+ nskip))))))

  ;; ---------- COLORES ----------
  (foreach e lst
    (setq ed (entget e))
    (if (member (cdr (assoc 0 ed)) '("LINE" "LWPOLYLINE"))
      (progn
        (setq segs (rb:segs ed) cls nil)
        (foreach s segs
          (setq cls (cons (rb:isborder
                            (list (rb:rot (car s) base (- ang1))
                                  (rb:rot (cadr s) base (- ang1)))
                            bb tol)
                          cls)))
        (cond
          ((not (member nil cls))
           (vla-put-color (vlax-ename->vla-object e) 4))
          ((not (member T cls))
           (vla-put-color (vlax-ename->vla-object e) 1))
          (T
           (setq lay  (cdr (assoc 8 ed))
                 elev (if (assoc 38 ed) (cdr (assoc 38 ed)) 0.0))
           (foreach s segs
             (setq col (if (rb:isborder
                             (list (rb:rot (car s) base (- ang1))
                                   (rb:rot (cadr s) base (- ang1)))
                             bb tol) 4 1))
             (entmake
               (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                     (cons 8 lay) (cons 62 col)
                     '(100 . "AcDbPolyline") '(90 . 2) '(70 . 0)
                     (cons 38 elev)
                     (cons 10 (list (car (car s))  (cadr (car s))))
                     (cons 10 (list (car (cadr s)) (cadr (cadr s)))))))
           (entdel e))))))

  (princ (strcat "\nTabla lista. Textos centrados: " (itoa nfit)
                 (if (> nskip 0)
                   (strcat "  |  Textos fuera de celda (no ajustados): " (itoa nskip))
                   "")))
  (princ))

;; ---------------- COMANDO ----------------
(defun c:ROTAB ( / *error* doc ss base ang1 fac fin saved sang)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg)))
    (princ))

  (princ "\n--- ROTAB: rotar/escalar tablas (seleccione UNA tabla a la vez) ---")
  ;; Factor de escala: se guarda (registro de Windows) hasta que lo cambie
  (setq saved (getenv "ROTAB_FACTOR"))
  (setq fac (if (and saved (> (atof saved) 0)) (atof saved) 1.0))
  (initget 6)
  (setq fin (getreal (strcat "\nFactor de escala de la TABLA completa <"
                             (rtos fac 2 2) ">: ")))
  (if fin (setq fac fin))
  (setenv "ROTAB_FACTOR" (rtos fac 2 4))
  (princ (strcat "\nFactor guardado: " (rtos fac 2 2)))

  ;; Angulo de rotacion: tambien se guarda hasta que lo cambie
  (setq saved (getenv "ROTAB_ANGULO"))
  (setq ang1 (if saved (atof saved) 0.0))          ; grados
  (setq fin (getangle (strcat "\nAngulo de rotacion de la tabla en grados (o indique 2 puntos) <"
                             (rtos ang1 2 2) ">: ")))
  (if fin (setq ang1 (* fin (/ 180.0 pi))))
  (setenv "ROTAB_ANGULO" (rtos ang1 2 6))
  (princ (strcat "\nAngulo guardado: " (rtos ang1 2 2) " grados"))
  (setq ang1 (* ang1 (/ pi 180.0)))                ; a radianes

  (while (setq ss (progn
                    (princ "\nSeleccione la tabla (polilineas y textos), Enter para salir: ")
                    (ssget '((0 . "LWPOLYLINE,LINE,TEXT,MTEXT")))))
    (setq base (getpoint "\nPunto base de rotacion (ej. esquina de la tabla): "))
    (if base
      (progn
        (setq base (trans base 1 0))
        (vla-StartUndoMark doc)
        (rb:proc ss base ang1 fac)
        (vla-EndUndoMark doc))))
  (princ))

(princ "\nROTAB cargado. Escriba ROTAB para iniciar.")
(princ)
