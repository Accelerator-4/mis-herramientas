;;; ===========================================================================
;;; ROTAJ.lsp  -  Comando: ROTAJ
;;; Igual que ROTAB, pero la tabla se AJUSTA AL TEXTO (ancho y alto)
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
;;;   3. AJUSTA el ancho de cada columna al texto mas ancho que contiene: si
;;;      no cabe la columna se ENSANCHA y si sobra espacio se ACHICA
;;;      (texto + margen); lo que esta a su derecha se desplaza.
;;;      Igual con la ALTURA de cada fila (texto + margen arriba y abajo):
;;;      se ensancha o achica hacia abajo y lo que esta debajo se desplaza.
;;;      Los textos NO se comprimen ni cambian de tamano.
;;;   4. Centra cada TEXT en su celda (justificacion Medio-Centro) y lo
;;;      alinea con el angulo de la tabla.
;;;   5. Colorea: borde exterior de la tabla en CYAN (4) y lineas internas
;;;      en ROJO (1). Si una polilinea mezcla borde e interior, la divide en
;;;      segmentos para poder colorear cada parte.
;;;   DIFERENCIA CON ROTAB: ROTAJ ademas achica las columnas que tienen
;;;   espacio de sobra, y tambien ajusta la altura de las filas.
;;;
;;; COMO USARLO
;;;   1. Cargar el archivo con APPLOAD (o arrastrarlo a la ventana de CAD).
;;;   2. Escribir ROTAJ.
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
(defun rj:rot (p c a / dx dy)
  (setq dx (- (car p) (car c))
        dy (- (cadr p) (cadr c)))
  (list (+ (car c) (- (* dx (cos a)) (* dy (sin a))))
        (+ (cadr c) (* dx (sin a)) (* dy (cos a)))))

;; Segmentos ((p1 p2) ...) de un LINE o LWPOLYLINE
(defun rj:segs (ed / typ vs v1 res)
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
(defun rj:isborder (s bb tol / ax ay bx by)
  (setq ax (car (car s))  ay (cadr (car s))
        bx (car (cadr s)) by (cadr (cadr s)))
  (cond
    ((and (< (abs (- ax bx)) tol)
          (or (< (abs (- ax (nth 0 bb))) tol) (< (abs (- ax (nth 1 bb))) tol))) T)
    ((and (< (abs (- ay by)) tol)
          (or (< (abs (- ay (nth 2 bb))) tol) (< (abs (- ay (nth 3 bb))) tol))) T)
    (T nil)))

;; --- Las siguientes funciones usan variables locales de rj:proc
;; --- (alcance dinamico): lst base ang1 lsegs allpts bb tol

;; Recalcula segmentos locales, bounding box y tolerancia
(defun rj:geom ( / e ed s p1 p2 minx maxx miny maxy)
  (setq lsegs nil allpts nil)
  (foreach e lst
    (setq ed (entget e))
    (foreach s (rj:segs ed)
      (setq p1 (rj:rot (car s)  base (- ang1))
            p2 (rj:rot (cadr s) base (- ang1)))
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
(defun rj:cell (px py / l r b tp s x0 y0 x1 y1)
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
(defun rj:tinfo (ed / h wf tb w j72 j73 rot anc ox oy)
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

;; Nueva posicion X (local) tras ensanchar/achicar columnas
;; usa las variables dinamicas shifts y tol de rj:proc
(defun rj:shx (x / dx s)
  (setq dx 0.0)
  (foreach s shifts
    (if (>= x (- (car s) tol))
      (setq dx (+ dx (cadr s)))))
  (+ x dx))

;; Nueva posicion Y (local) tras ajustar la altura de las filas.
;; Cada fila ajustada mantiene fijo su borde superior y crece/encoge hacia
;; abajo: lo que esta a la altura del borde inferior de la fila o mas abajo
;; se desplaza. Usa las variables dinamicas yshifts y tol de rj:proc
(defun rj:shy (y / dy s)
  (setq dy 0.0)
  (foreach s yshifts
    (if (<= y (+ (car s) tol))
      (setq dy (+ dy (cadr s)))))
  (- y dy))

;; Procesa una tabla
(defun rj:proc (ss base ang1 fac / lst i e ed ang0 delta obj segs lsegs allpts
                bb tol cls col s p1 p2 lay elev
                ti cen w h lc cell l r b tp cx cy zz nfit nskip
                cols c key need d shifts pad tlist item rows rkey yshifts dxm dym)
  ;; lista de entidades
  (setq i 0)
  (repeat (sslength ss)
    (setq lst (cons (ssname ss i) lst) i (1+ i)))
  (setq tlist nil rows nil yshifts nil)

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

  (rj:geom)

  ;; ---------- ENSANCHAR COLUMNAS DONDE EL TEXTO NO CABE ----------
  ;; cols = lista de (l r ancho-necesario) por columna
  (foreach e lst
    (setq ed (entget e))
    (if (= "TEXT" (cdr (assoc 0 ed)))
      (progn
        (setq ti   (rj:tinfo ed)
              cen  (car ti) w (cadr ti) h (caddr ti)
              lc   (rj:rot cen base (- ang1))
              cell (rj:cell (car lc) (cadr lc)))
        (if cell
          (progn
            (setq tlist (cons (list e cell) tlist))
            (setq need (+ w (* 0.8 h))   ; texto + margen a cada lado
                  key  nil)
            (foreach c cols
              (if (and (equal (car c) (car cell) tol)
                       (equal (cadr c) (cadr cell) tol))
                (setq key c)))
            (if key
              (if (> need (caddr key))
                (setq cols (subst (list (car key) (cadr key) need) key cols)))
              (setq cols (cons (list (car cell) (cadr cell) need) cols)))
            ;; altura necesaria de la fila (texto + margen arriba y abajo)
            (setq need (* 1.8 h)
                  rkey nil)
            (foreach c rows
              (if (and (equal (car c) (nth 2 cell) tol)
                       (equal (cadr c) (nth 3 cell) tol))
                (setq rkey c)))
            (if rkey
              (if (> need (caddr rkey))
                (setq rows (subst (list (car rkey) (cadr rkey) need) rkey rows)))
              (setq rows (cons (list (nth 2 cell) (nth 3 cell) need) rows))))))))

  ;; desplazamientos: (x-derecha-de-columna  incremento)
  (setq shifts nil)
  (foreach c cols
    (setq d (- (caddr c) (- (cadr c) (car c))))
    (if (> (abs d) tol)
      (setq shifts (cons (list (cadr c) d) shifts))))

  ;; desplazamientos verticales: (y-borde-inferior-de-fila  incremento)
  (foreach c rows
    (setq d (- (caddr c) (- (cadr c) (car c))))
    (if (> (abs d) tol)
      (setq yshifts (cons (list (car c) d) yshifts))))

  (if (or shifts yshifts)
    (progn
      ;; mover vertices: cada X local >= borde derecho de una columna
      ;; ensanchada se desplaza el incremento correspondiente
      (foreach e lst
        (setq ed (entget e))
        (if (member (cdr (assoc 0 ed)) '("LINE" "LWPOLYLINE"))
          (progn
            (setq ed
              (mapcar
                '(lambda (g / p lp z)
                   (if (member (car g) '(10 11))
                     (progn
                       (setq p  (cdr g)
                             z  (caddr p)
                             lp (rj:rot p base (- ang1)))
                       (setq lp (rj:rot (list (rj:shx (car lp)) (rj:shy (cadr lp)))
                                        base ang1))
                       (cons (car g)
                             (if z (list (car lp) (cadr lp) z) (list (car lp) (cadr lp)))))
                     g))
                ed))
            (entmod ed))))
      ;; mover tambien los textos que quedan a la derecha de cada columna ensanchada
      (foreach e lst
        (setq ed (entget e))
        (if (and (member (cdr (assoc 0 ed)) '("TEXT" "MTEXT"))
                 (not (assoc e tlist)))
          (progn
            (setq cen (if (= "TEXT" (cdr (assoc 0 ed)))
                        (car (rj:tinfo ed))
                        (cdr (assoc 10 ed)))
                  lc  (rj:rot cen base (- ang1))
                  dxm (- (rj:shx (car lc)) (car lc))
                  dym (- (rj:shy (cadr lc)) (cadr lc)))
            (if (or (/= dxm 0.0) (/= dym 0.0))
              (vla-move (vlax-ename->vla-object e)
                        (vlax-3d-point '(0.0 0.0 0.0))
                        (vlax-3d-point
                          (list (- (* dxm (cos ang1)) (* dym (sin ang1)))
                                (+ (* dxm (sin ang1)) (* dym (cos ang1)))
                                0.0)))))))
      (rj:geom)))

  ;; ---------- TEXTOS: centrar en su celda (columna ya ajustada) ----------
  ;; Cada texto conserva la celda en la que estaba ANTES de ajustar; su nueva
  ;; celda se obtiene desplazando los bordes izquierdo y derecho con rj:shx.
  (setq nfit 0 nskip (- (length (vl-remove-if-not
                                  '(lambda (x) (= "TEXT" (cdr (assoc 0 (entget x)))))
                                  lst))
                        (length tlist)))
  (foreach item tlist
    (setq e    (car item)
          cell (cadr item)
          ed   (entget e)
          zz   (if (caddr (cdr (assoc 10 ed))) (caddr (cdr (assoc 10 ed))) 0.0)
          cx   (/ (+ (rj:shx (nth 0 cell)) (rj:shx (nth 1 cell))) 2.0)
          cy   (/ (+ (rj:shy (nth 2 cell)) (rj:shy (nth 3 cell))) 2.0)
          obj  (vlax-ename->vla-object e))
    (vla-put-Rotation obj ang1)
    (vla-put-Alignment obj 10) ; acAlignmentMiddleCenter
    (setq cen (rj:rot (list cx cy) base ang1))
    (vla-put-TextAlignmentPoint
      obj (vlax-3d-point (list (car cen) (cadr cen) zz)))
    (setq nfit (1+ nfit)))

  ;; ---------- COLORES ----------
  (foreach e lst
    (setq ed (entget e))
    (if (member (cdr (assoc 0 ed)) '("LINE" "LWPOLYLINE"))
      (progn
        (setq segs (rj:segs ed) cls nil)
        (foreach s segs
          (setq cls (cons (rj:isborder
                            (list (rj:rot (car s) base (- ang1))
                                  (rj:rot (cadr s) base (- ang1)))
                            bb tol)
                          cls)))
        (cond
          ((not (member nil cls))
           (vla-put-color (vlax-ename->vla-object e) 2))
          ((not (member T cls))
           (vla-put-color (vlax-ename->vla-object e) 1))
          (T
           (setq lay  (cdr (assoc 8 ed))
                 elev (if (assoc 38 ed) (cdr (assoc 38 ed)) 0.0))
           (foreach s segs
             (setq col (if (rj:isborder
                             (list (rj:rot (car s) base (- ang1))
                                   (rj:rot (cadr s) base (- ang1)))
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
(defun c:ROTAJ ( / *error* doc ss base ang1 fac fin saved sang)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg)))
    (princ))

  (princ "\n--- ROTAJ (tabla ajustada al texto): rotar/escalar tablas (seleccione UNA tabla a la vez) ---")
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
        (rj:proc ss base ang1 fac)
        (vla-EndUndoMark doc))))
  (princ))

(princ "\nROTAJ cargado. Escriba ROTAJ para iniciar.")
(princ)
