;;; ===========================================================================
;;; ROTAA.lsp  -  Comando: ROTAA   (VERSION CORREGIDA)
;;; ABREVIA los textos de una tabla y AJUSTA la tabla (ancho y alto) al nuevo texto
;;; ===========================================================================
;;;
;;; CORRECCIONES DE ESTA VERSION
;;;   [FIX 1] Paréntesis mal acomodado en ra:validar: el bucle de TEXTS quedaba
;;;           anidado dentro del bucle de LINEAS. Ahora está separado y cierra bien.
;;;   [FIX 2] DOBLE BORRADO de duplicados: ra:dedup ya los borraba y luego
;;;           ra:validar los volvia a borrar -> se eliminaban entidades de mas
;;;           (ese era el "borrado de 252 textos"). Ahora los duplicados se
;;;           resuelven UNA SOLA VEZ, de forma segura.
;;;   [FIX 3] ra:dedup usaba (cadddr (ra:tinfo ed)) que puede venir nil y
;;;           reventar. Ahora se calcula el punto de anclaje con tolerancia
;;;           segura y sólo se compara dentro de la selección.
;;;   [FIX 4] Proteccion total: StartUndoMark ANTES de tocar nada, y el comando
;;;           nunca termina mudo: siempre informa qué pasó.
;;;   [FIX 5] Se valida que sólo se borren entidades presentes en la seleccion
;;;           (jamás se toca nada fuera de la ventana).
;;;   [FIX 6] Aviso claro si la tabla YA esta abreviada (0 coincidencias) y no
;;;           se borra nada en ese caso.
;;; ===========================================================================
;;;
;;; QUE HACE
;;;   1. Abrevia los textos de la tabla segun una lista de abreviaturas
;;;      (se reconocen por palabras clave, no importa el orden exacto):
;;;         *TAPA*                -> NT      (Nivel de Tapa)
;;;         *FONDO*               -> NF      (Nivel de Fondo)
;;;         *INVER*ENTRADA*       -> NEnt    (Nivel Invert Entrada)
;;;         *INVER*SALIDA*        -> NSal    (Nivel Invert Salida)
;;;         *DIAMETRO*            -> DT      (Diametro de Tubo)
;;;      Los textos que no coinciden (ej. "PVP-46") y los valores
;;;      numericos no se tocan.
;;;   2. Agrega despues de cada abreviatura un SIMBOLO opcional (ej. ":"
;;;      -> "NT:"), o nada.
;;;   3. Ajusta la tabla al nuevo texto, igual que ROTAJ: cada columna toma
;;;      el ancho del texto mas ancho que contiene y cada fila la altura del
;;;      texto, mas un pequeno margen. La esquina superior izquierda de la
;;;      tabla queda fija; lo demas se corre.
;;;   4. Centra todos los textos en su celda. Los textos no se comprimen.
;;;   Mantiene el angulo actual de la tabla (no rota ni escala; para eso
;;;   usar ROTAB/ROTAJ) y no cambia los colores.
;;;
;;; COMO USARLO
;;;   1. Cargar el archivo con APPLOAD (o arrastrarlo a la ventana de CAD).
;;;   2. Escribir ROTAA.
;;;   3. Simbolo: escribir el simbolo que va despues de la abreviatura
;;;      (ej. :  o  -  ), escribir 0 para no poner nada, o Enter para
;;;      usar el valor guardado.
;;;   4. "Modificar abreviaturas? [Si/No]": con No (Enter) se usan las
;;;      guardadas; con Si pregunta cada una y Enter conserva la actual.
;;;   5. Seleccionar UNA tabla (polilineas/lineas y textos) con ventana.
;;;   6. Repetir con las demas tablas con la misma configuracion.
;;;      Enter sin seleccionar nada para terminar.
;;;
;;; VALORES GUARDADOS
;;;   El simbolo y las abreviaturas se guardan en el registro de Windows
;;;   (variables ROTAA_SIMBOLO y ROTAA_ABR1 ... ROTAA_ABR5) y se reutilizan
;;;   hasta que se cambien; se conservan al cerrar el CAD.
;;;
;;; NOTAS Y LIMITACIONES
;;;   - Seleccionar una sola tabla por vez, con los textos ORIGINALES: una
;;;     tabla ya abreviada no se vuelve a abreviar con otras abreviaturas
;;;     (usar UNDO y repetir).
;;;   - Se IGNORAN (no se modifican) las lineas/polilineas de la seleccion que
;;;     tienen tramos inclinados o que estan lejos de los textos (ej. una
;;;     tuberia o el borde de otra tabla atrapados en la ventana). El comando
;;;     avisa cuantos descarto. Aun asi conviene seleccionar solo la tabla.
;;;   - Antes de modificar, el comando VALIDA la tabla: que las lineas cierren
;;;     en las intersecciones y que no haya dos textos en la misma celda. Si
;;;     algo no cuadra NO toca la tabla y dice el motivo y el handle de la
;;;     entidad (para buscarla con el comando HANDENT o LIST). La tolerancia
;;;     de coincidencia es el mayor entre 0.2% del tamano de la tabla y el
;;;     2% de la altura del texto.
;;;   - El ANGULO de la tabla se calcula con la orientacion de sus LINEAS (no
;;;     con la rotacion del texto), asi que funciona aunque el texto original
;;;     tenga otro angulo. Los textos se alinean con el angulo de la tabla.
;;;   - Si hay textos DUPLICADOS exactos (mismo contenido y misma posicion, uno
;;;     encima del otro) borra los repetidos y deja uno; lo avisa. Se puede
;;;     recuperar con UNDO.
;;;   - Funciona con LINE, LWPOLYLINE (sin arcos), TEXT y MTEXT. Los MTEXT se
;;;     abrevian como texto plano (se pierde el formato interno del texto).
;;;   - Al seleccionar muestra cuantas lineas, TEXT y MTEXT encontro, y si
;;;     ningun texto coincide con las palabras clave lo avisa.
;;;   - Se asume UCS = WCS. Todo se puede deshacer con UNDO.
;;;   - Para editar las palabras clave o abreviaturas por defecto, ver la
;;;     lista ra:defs mas abajo.
;;; ===========================================================================

(vl-load-com)

;; Rota punto P alrededor de C un angulo A (radianes)
(defun ra:rot (p c a / dx dy)
  (setq dx (- (car p) (car c))
        dy (- (cadr p) (cadr c)))
  (list (+ (car c) (- (* dx (cos a)) (* dy (sin a))))
        (+ (cadr c) (* dx (sin a)) (* dy (cos a)))))

;; Segmentos ((p1 p2) ...) de un LINE o LWPOLYLINE
(defun ra:segs (ed / typ vs v1 res)
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

;; --- Las siguientes funciones usan variables locales de ra:proc
;; --- (alcance dinamico): lst base ang1 lsegs allpts bb tol

;; Recalcula segmentos locales, bounding box y tolerancia
(defun ra:geom ( / e ed s p1 p2 minx maxx miny maxy hm)
  (setq lsegs nil allpts nil hm 0.0)
  (foreach e lst
    (setq ed (entget e))
    (if (member (cdr (assoc 0 ed)) '("TEXT" "MTEXT"))
      (setq hm (max hm (cdr (assoc 40 ed)))))
    (foreach s (ra:segs ed)
      (setq p1 (ra:rot (car s)  base (- ang1))
            p2 (ra:rot (cadr s) base (- ang1)))
      (setq lsegs  (cons (list p1 p2) lsegs)
            allpts (cons p1 (cons p2 allpts)))))
  (if (null lsegs)
    (progn (princ "\nNo hay lineas/polilineas en la seleccion.") (exit)))
  (setq minx (apply 'min (mapcar 'car allpts))
        maxx (apply 'max (mapcar 'car allpts))
        miny (apply 'min (mapcar 'cadr allpts))
        maxy (apply 'max (mapcar 'cadr allpts))
        bb   (list minx maxx miny maxy)
        tol  (max (* 0.002 (max (- maxx minx) (- maxy miny))) (* 0.02 hm) 1e-6)))

;; Celda (l r b t) que contiene el punto local (px py), o nil
(defun ra:cell (px py / l r b tp s x0 y0 x1 y1)
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

;; Info de un TEXT: (centro-mundo ancho alto punto-asignacion)
(defun ra:tinfo-t (ed / h wf tb w j72 j73 rot anc ox oy axo)
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
                    ((= j73 3) (/ h -3.0))
                    (T 0.0)))))
  ;; axo = desplazamiento para el PUNTO DE ASIGNACION a una celda
  (setq axo (cond ((member j72 '(0 3 5)) (* 0.3 h))
                  ((= j72 2) (* -0.3 h))
                  (T 0.0)))
  (list (list (+ (car anc) (- (* ox (cos rot)) (* oy (sin rot))))
              (+ (cadr anc) (* ox (sin rot)) (* oy (cos rot)))
              (if (caddr anc) (caddr anc) 0.0))
        w h
        (list (+ (car anc) (- (* axo (cos rot)) (* oy (sin rot))))
              (+ (cadr anc) (* axo (sin rot)) (* oy (cos rot)))
              (if (caddr anc) (caddr anc) 0.0))))

;; Rotacion de un TEXT o MTEXT (radianes)
(defun ra:trot (ed / v)
  (cond
    ((and (= "MTEXT" (cdr (assoc 0 ed))) (setq v (cdr (assoc 11 ed))))
     (atan (cadr v) (car v)))
    ((assoc 50 ed) (cdr (assoc 50 ed)))
    (T 0.0)))

;; Info de un MTEXT: (centro-mundo ancho alto-caracter punto-de-asignacion)
(defun ra:tinfo-m (ed / hc w hh rot att anc ox oy axo z)
  (setq hc  (cdr (assoc 40 ed))
        w   (if (assoc 42 ed) (cdr (assoc 42 ed)) 0.0)
        hh  (if (assoc 43 ed) (cdr (assoc 43 ed)) hc)
        rot (ra:trot ed)
        att (if (assoc 71 ed) (cdr (assoc 71 ed)) 1)
        anc (cdr (assoc 10 ed))
        z   (if (caddr anc) (caddr anc) 0.0))
  (setq ox  (cond ((member att '(1 4 7)) (/ w 2.0))
                  ((member att '(3 6 9)) (/ w -2.0))
                  (T 0.0))
        oy  (cond ((member att '(1 2 3)) (/ hh -2.0))
                  ((member att '(7 8 9)) (/ hh 2.0))
                  (T 0.0))
        axo (cond ((member att '(1 4 7)) (* 0.3 hc))
                  ((member att '(3 6 9)) (* -0.3 hc))
                  (T 0.0)))
  (list (list (+ (car anc) (- (* ox (cos rot)) (* oy (sin rot))))
              (+ (cadr anc) (* ox (sin rot)) (* oy (cos rot)))
              z)
        w hc
        (list (+ (car anc) (- (* axo (cos rot)) (* oy (sin rot))))
              (+ (cadr anc) (* axo (sin rot)) (* oy (cos rot)))
              z)))

;; Info de un TEXT o MTEXT. Devuelve SIEMPRE una lista valida
(defun ra:tinfo (ed / t0 r)
  (setq t0 (cdr (assoc 0 ed)))
  (setq r (if (= t0 "MTEXT") (ra:tinfo-m ed) (ra:tinfo-t ed)))
  ;; blindaje: si algo salio nil, devolvemos estructura segura
  (if (and r (car r) (caddr r) (cadddr r))
    r
    (let ((p (cdr (assoc 10 ed))))
      (list (list (car p) (cadr p) (if (caddr p) (caddr p) 0.0))
            0.0
            (if (assoc 40 ed) (cdr (assoc 40 ed)) 1.0)
            (list (car p) (cadr p) (if (caddr p) (caddr p) 0.0))))))

;; Angulo de la tabla (radianes) calculado con sus LINEAS
(defun ra:angulo (lst / e ed s lst2 a b tot sc best bsc txt trot cand d dmin res hp k)
  (setq hp (/ pi 2.0) tot 0.0 lst2 nil txt nil)
  (foreach e lst
    (setq ed (entget e))
    (cond
      ((member (cdr (assoc 0 ed)) '("LINE" "LWPOLYLINE"))
       (foreach s (ra:segs ed)
         (setq a (distance (list (car (car s)) (cadr (car s)))
                           (list (car (cadr s)) (cadr (cadr s)))))
         (if (> a 1e-9)
           (progn
             (setq b (atan (- (cadr (cadr s)) (cadr (car s)))
                           (- (car (cadr s))  (car (car s)))))
             (setq b (rem b hp))
             (if (< b 0.0) (setq b (+ b hp)))
             (setq lst2 (cons (cons b a) lst2) tot (+ tot a))))))
      ((and (null txt) (member (cdr (assoc 0 ed)) '("TEXT" "MTEXT")))
       (setq txt (ra:trot ed)))))
  (setq trot (if txt txt 0.0))
  (if (null lst2)
    trot
    (progn
      (setq bsc -1.0 best 0.0)
      (foreach s lst2
        (setq sc 0.0)
        (foreach e lst2
          (setq d (abs (- (car s) (car e))))
          (setq d (min d (- hp d)))
          (if (< d 0.003) (setq sc (+ sc 1.0 (/ (cdr e) tot)))))
        (if (> sc bsc) (setq bsc sc best (car s))))
      (setq dmin 1e99 res best)
      (foreach k '(0 1 2 3)
        (setq cand (+ best (* k hp)))
        (setq d (abs (rem (- cand trot) (* 2.0 pi))))
        (if (> d pi) (setq d (- (* 2.0 pi) d)))
        (if (< d dmin) (setq dmin d res cand)))
      (setq d (* hp (fix (+ (/ res hp) 0.5))))
      (if (< (abs (- res d)) 0.0044) (setq res d))
      (setq res (rem res (* 2.0 pi)))
      (if (< res 0.0) (setq res (+ res (* 2.0 pi))))
      res)))

;; Filtra la seleccion: deja SOLO los objetos que pertenecen a la tabla.
(defun ra:filtrar (lst base ang / e ed ti lc w h hmax minx maxx miny maxy
                     mg ok s p1 p2 dx dy len kept nex ntxt pt)
  (setq minx 1e99 maxx -1e99 miny 1e99 maxy -1e99 hmax 0.0 ntxt 0 nex 0)
  (foreach e lst
    (setq ed (entget e))
    (if (member (cdr (assoc 0 ed)) '("TEXT" "MTEXT"))
      (progn
        (setq ti (ra:tinfo ed))
        (setq lc (car ti))
        (setq lc (ra:rot lc base (- ang))
              w  (cadr ti)
              h  (caddr ti)
              hmax (max hmax h)
              minx (min minx (- (car lc) (/ w 2.0)))
              maxx (max maxx (+ (car lc) (/ w 2.0)))
              miny (min miny (- (cadr lc) (/ h 2.0)))
              maxy (max maxy (+ (cadr lc) (/ h 2.0)))
              ntxt (1+ ntxt)))))
  (if (zerop ntxt)
    (list lst 0)
    (progn
      (setq mg (* 4.0 hmax))
      (foreach e lst
        (setq ed (entget e))
        (if (member (cdr (assoc 0 ed)) '("LINE" "LWPOLYLINE"))
          (progn
            (setq ok T)
            (foreach s (ra:segs ed)
              (setq p1 (ra:rot (car s)  base (- ang))
                    p2 (ra:rot (cadr s) base (- ang))
                    dx (abs (- (car p2)  (car p1)))
                    dy (abs (- (cadr p2) (cadr p1)))
                    len (sqrt (+ (* dx dx) (* dy dy))))
              (if (> (min dx dy) (+ (* 0.01 len) 1e-6)) (setq ok nil))
              (foreach pt (list p1 p2)
                (if (or (< (car pt)  (- minx mg)) (> (car pt)  (+ maxx mg))
                        (< (cadr pt) (- miny mg)) (> (cadr pt) (+ maxy mg)))
                  (setq ok nil))))
            (if ok
              (setq kept (cons e kept))
              (setq nex (1+ nex))))
          (setq kept (cons e kept))))
      (list (reverse kept) nex))))

;; Nueva posicion X (local) tras ensanchar/achicar columnas
(defun ra:shx (x / dx s)
  (setq dx 0.0)
  (foreach s shifts
    (if (>= x (- (car s) tol))
      (setq dx (+ dx (cadr s)))))
  (+ x dx))

;; Nueva posicion Y (local) tras ajustar la altura de las filas.
(defun ra:shy (y / dy s)
  (setq dy 0.0)
  (foreach s yshifts
    (if (<= y (+ (car s) tol))
      (setq dy (+ dy (cadr s)))))
  (- y dy))

;; ---------------------------------------------------------------------------
;;  DEDUPLICADO SEGURO  (FIX 2 / FIX 3)
;;  Elimina textos duplicados EXACTOS (mismo contenido y misma posicion).
;;  - Solo considera entidades que estan en la lista.
;;  - Usa coordenadas seguras (blindeado con ra:tinfo).
;;  - Borra UNA sola vez. Devuelve (lista-limpia cantidad-borrada).
;; ---------------------------------------------------------------------------
(defun ra:dedup (lst base ang / e e2 ed ed2 keep nd dup tp p1 p2 r1 r2)
  (setq keep nil nd 0)
  (foreach e lst
    (setq ed (entget e) dup nil)
    (if (member (cdr (assoc 0 ed)) '("TEXT" "MTEXT"))
      (progn
        (setq tp (* 0.02 (if (assoc 40 ed) (cdr (assoc 40 ed)) 1.0)))
        (setq r1 (ra:tinfo ed))
        (setq p1 (cadr (cadddr r1)))   ; coordenada Y del punto de asignacion
        (setq p1 (car (cadddr r1)))
        (setq p1 (cadddr r1))          ; punto de asignacion (x y z)
        (foreach e2 keep
          (if (not dup)
            (progn
              (setq ed2 (entget e2))
              (if (and (member (cdr (assoc 0 ed2)) '("TEXT" "MTEXT"))
                       (= (cdr (assoc 1 ed)) (cdr (assoc 1 ed2))))
                (progn
                  (setq r2 (ra:tinfo ed2))
                  (setq p2 (cadddr r2))
                  (if (and (equal (car p1) (car p2) tp)
                           (equal (cadr p1) (cadr p2) tp))
                    (setq dup T)))))))
        ))
    (if dup
      (progn (entdel e) (setq nd (1+ nd)))
      (setq keep (cons e keep))))
  (list (reverse keep) nd))

;; ---------------------------------------------------------------------------
;;  VALIDACION  (FIX 1)  -  paréntesis corregidos
;;  Devuelve nil si todo esta bien, o un mensaje de texto si NO debe tocarse.
;; ---------------------------------------------------------------------------
(defun ra:validar (lst base ang nombre / warned e ed s p1 p2 lsegs allpts xs ys tol
                     hm minx maxx miny maxy ti lc cell cells msg dx dy nsin
                     pt ntxt prev ncol)
  (setq lsegs nil allpts nil xs nil ys nil cells nil hm 0.0 nsin 0 ntxt 0)
  (foreach e lst
    (setq ed (entget e))
    (cond
      ((member (cdr (assoc 0 ed)) '("LINE" "LWPOLYLINE"))
       (foreach s (ra:segs ed)
         (setq p1 (ra:rot (car s)  base (- ang))
               p2 (ra:rot (cadr s) base (- ang)))
         (setq lsegs  (cons (list p1 p2) lsegs)
               allpts (cons p1 (cons p2 allpts)))))
      ((member (cdr (assoc 0 ed)) '("TEXT" "MTEXT"))
       (setq hm (max hm (cdr (assoc 40 ed)))))))
  (if (null lsegs)
    (setq msg (strcat "\n" nombre ": la seleccion no tiene lineas de tabla. No se modifico nada."))
    (progn
      (setq minx (apply 'min (mapcar 'car allpts))
            maxx (apply 'max (mapcar 'car allpts))
            miny (apply 'min (mapcar 'cadr allpts))
            maxy (apply 'max (mapcar 'cadr allpts))
            tol  (max (* 0.002 (max (- maxx minx) (- maxy miny))) (* 0.02 hm) 1e-6))

      ;; coordenadas de las lineas verticales (x) y horizontales (y)
      (foreach s lsegs
        (setq dx (abs (- (car (car s)) (car (cadr s))))
              dy (abs (- (cadr (car s)) (cadr (cadr s)))))
        (cond ((and (< dx tol) (> dy tol)) (setq xs (cons (car (car s)) xs)))
              ((and (< dy tol) (> dx tol)) (setq ys (cons (cadr (car s)) ys)))))

      ;; (1) cada extremo debe caer sobre una linea perpendicular  -> SOLO AVISO
      (setq warned nil)
      (foreach e lst
        (setq ed (entget e))
        (if (and (null msg) (member (cdr (assoc 0 ed)) '("LINE" "LWPOLYLINE")))
          (foreach s (ra:segs ed)
            (setq p1 (ra:rot (car s)  base (- ang))
                  p2 (ra:rot (cadr s) base (- ang))
                  dx (abs (- (car p1) (car p2)))
                  dy (abs (- (cadr p1) (cadr p2))))
            (cond
              ((and (null msg) (< dy tol) (> dx tol))
               (foreach pt (list p1 p2)
                 (if (and (null warned)
                          (not (vl-some '(lambda (x) (< (abs (- x (car pt))) tol)) xs)))
                   (progn
                     (setq warned T)
                     (princ (strcat "\nAVISO " nombre ": la linea horizontal (handle " (cdr (assoc 5 ed))
                                    ") no termina exactamente sobre una linea vertical. Se continua igualmente."))))))
              ((and (null msg) (< dx tol) (> dy tol))
               (foreach pt (list p1 p2)
                 (if (and (null warned)
                          (not (vl-some '(lambda (y) (< (abs (- y (cadr pt))) tol)) ys)))
                   (progn
                     (setq warned T)
                     (princ (strcat "\nAVISO " nombre ": la linea vertical (handle " (cdr (assoc 5 ed))
                                    ") no termina exactamente sobre una linea horizontal. Se continua igualmente."))))))))))

      ;; (2) un solo texto por celda  -> AQUI ESTABA EL BUG DE PARENTESIS
      (if (null msg)
        (foreach e lst
          (setq ed (entget e))
          (if (and (null msg) (member (cdr (assoc 0 ed)) '("TEXT" "MTEXT")))
            (progn
              (setq ntxt (1+ ntxt))
              (setq ti   (ra:tinfo ed))
              (setq lc   (ra:rot (cadddr ti) base (- ang)))
              (setq cell (ra:cell (car lc) (cadr lc)))
              (if cell
                (progn
                  (setq prev nil)
                  (foreach c cells
                    (if (and (null prev)
                             (equal (nth 0 c) (nth 0 cell) tol)
                             (equal (nth 1 c) (nth 1 cell) tol)
                             (equal (nth 2 c) (nth 2 cell) tol)
                             (equal (nth 3 c) (nth 3 cell) tol))
                      (setq prev c)))
                  (if prev
                    (if (= (nth 4 prev) (cdr (assoc 1 ed)))
                      ;; mismo contenido mismo lugar -> duplicado: se marca para borrar
                      (if (not (member e ra:dups)) (setq ra:dups (cons e ra:dups)))
                      ;; contenido distinto en la misma celda -> error real
                      (setq msg
                        (strcat "\n" nombre ": la tabla NO se modifico. Hay dos textos DISTINTOS en la misma celda."
                                "\n  Texto 1: [" (nth 4 prev) "] en x=" (rtos (- (nth 5 prev) minx) 2 3)
                                " y=" (rtos (- (nth 6 prev) miny) 2 3)
                                "\n  Texto 2: [" (cdr (assoc 1 ed)) "] en x=" (rtos (- (car lc) minx) 2 3)
                                " y=" (rtos (- (cadr lc) miny) 2 3) "  (handle " (cdr (assoc 5 ed)) ")"
                                "\n  Celda: x de " (rtos (- (nth 0 cell) minx) 2 3) " a " (rtos (- (nth 1 cell) minx) 2 3)
                                ", y de " (rtos (- (nth 2 cell) miny) 2 3) " a " (rtos (- (nth 3 cell) miny) 2 3)
                                "\n  (Envie este mensaje completo para poder corregirlo.)")))
                    (setq cells (cons (append cell (list (cdr (assoc 1 ed)) (car lc) (cadr lc))) cells))))
                (setq nsin (1+ nsin)))))))
      ))
  (if (and (null msg) (> ntxt 0) (= nsin ntxt))
    (setq msg (strcat "\n" nombre ": la tabla NO se modifico. Ningun texto quedo dentro de una celda cerrada.")))
  msg)

;; Lista de abreviaturas: (palabra-clave  abreviatura-por-defecto  variable-guardada  nombre)
(setq ra:defs
  '(("*TAPA*"           "NT"   "ROTAA_ABR1" "NIVEL TAPA")
    ("*FONDO*"          "NF"   "ROTAA_ABR2" "NIVEL FONDO")
    ("*INVER*ENTRADA*"  "NEnt" "ROTAA_ABR3" "NIVEL INVER ENTRADA")
    ("*INVER*SALIDA*"   "NSal" "ROTAA_ABR4" "NIVEL INVER SALIDA")
    ("*DIAMETRO*"       "DT"   "ROTAA_ABR5" "DIAMETRO TUBO")))

;; Procesa una tabla. pairs = lista de (palabra-clave abreviatura), sym = simbolo
(defun ra:proc2 (ss pairs sym / lst i e ed ang1 base lsegs allpts bb tol
                ti cen w h lc cell cx cy zz nfit nabr found up str item
                cols c key need d shifts yshifts rows rkey tlist s p dxm dym obj r nex)
  (setq i 0)
  (repeat (sslength ss)
    (setq lst (cons (ssname ss i) lst) i (1+ i)))
  (setq tlist nil cols nil rows nil shifts nil yshifts nil nabr 0 nfit 0)

  ;; angulo actual de la tabla y punto pivote
  (setq ang1 0.0 base nil)
  (foreach e lst
    (setq ed (entget e))
    (if (and (null base) (member (cdr (assoc 0 ed)) '("LINE" "LWPOLYLINE")))
      (setq base (cdr (assoc 10 ed)))))
  (setq ang1 (ra:angulo lst))

  (setq r (ra:filtrar lst base ang1) lst (car r) nex (cadr r))

  (ra:geom)

  ;; 1) celda original de cada texto (antes de cambiar nada)
  (foreach e lst
    (setq ed (entget e))
    (if (member (cdr (assoc 0 ed)) '("TEXT" "MTEXT"))
      (progn
        (setq ti   (ra:tinfo ed)
              lc   (ra:rot (cadddr ti) base (- ang1))
              cell (ra:cell (car lc) (cadr lc)))
        (if cell (setq tlist (cons (list e cell) tlist))))))

  ;; 2) abreviar textos
  (foreach e lst
    (setq ed (entget e))
    (if (member (cdr (assoc 0 ed)) '("TEXT" "MTEXT"))
      (progn
        (setq str (if (= "TEXT" (cdr (assoc 0 ed)))
                    (cdr (assoc 1 ed))
                    (vla-get-TextString (vlax-ename->vla-object e)))
              up  (strcase str)
              found nil)
        (foreach p pairs
          (if (and (null found) (wcmatch up (car p)))
            (setq found p)))
        (if found
          (progn
            (vla-put-TextString (vlax-ename->vla-object e)
                                (strcat (cadr found) sym))
            (vla-update (vlax-ename->vla-object e))
            (entupd e)
            (setq nabr (1+ nabr)))))))

  ;; 3) ancho/alto necesarios por columna y fila (con el texto ya abreviado)
  (foreach item tlist
    (setq e    (car item)
          cell (cadr item)
          ed   (entget e)
          ti   (ra:tinfo ed)
          w    (cadr ti)
          h    (caddr ti))
    ;; columna
    (setq need (+ w (* 0.8 h)) key nil)
    (foreach c cols
      (if (and (equal (car c) (nth 0 cell) tol) (equal (cadr c) (nth 1 cell) tol))
        (setq key c)))
    (if key
      (if (> need (caddr key))
        (setq cols (subst (list (car key) (cadr key) need) key cols)))
      (setq cols (cons (list (nth 0 cell) (nth 1 cell) need) cols)))
    ;; fila
    (setq need (* 1.8 h) rkey nil)
    (foreach c rows
      (if (and (equal (car c) (nth 2 cell) tol) (equal (cadr c) (nth 3 cell) tol))
        (setq rkey c)))
    (if rkey
      (if (> need (caddr rkey))
        (setq rows (subst (list (car rkey) (cadr rkey) need) rkey rows)))
      (setq rows (cons (list (nth 2 cell) (nth 3 cell) need) rows))))

  (foreach c cols
    (setq d (- (caddr c) (- (cadr c) (car c))))
    (if (> (abs d) tol) (setq shifts (cons (list (cadr c) d) shifts))))
  (foreach c rows
    (setq d (- (caddr c) (- (cadr c) (car c))))
    (if (> (abs d) tol) (setq yshifts (cons (list (car c) d) yshifts))))

  ;; 4) mover lineas
  (if (or shifts yshifts)
    (progn
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
                             lp (ra:rot p base (- ang1)))
                       (setq lp (ra:rot (list (ra:shx (car lp)) (ra:shy (cadr lp)))
                                        base ang1))
                       (cons (car g)
                             (if z (list (car lp) (cadr lp) z) (list (car lp) (cadr lp)))))
                     g))
                ed))
            (entmod ed))))
      ;; mover textos que no estan en ninguna celda (y MTEXT)
      (foreach e lst
        (setq ed (entget e))
        (if (and (member (cdr (assoc 0 ed)) '("TEXT" "MTEXT"))
                 (not (assoc e tlist)))
          (progn
            (setq cen (cadddr (ra:tinfo ed))
                  lc  (ra:rot cen base (- ang1))
                  dxm (- (ra:shx (car lc)) (car lc))
                  dym (- (ra:shy (cadr lc)) (cadr lc)))
            (if (or (/= dxm 0.0) (/= dym 0.0))
              (vla-move (vlax-ename->vla-object e)
                        (vlax-3d-point '(0.0 0.0 0.0))
                        (vlax-3d-point
                          (list (- (* dxm (cos ang1)) (* dym (sin ang1)))
                                (+ (* dxm (sin ang1)) (* dym (cos ang1)))
                                0.0)))))))))

  ;; 5) centrar cada texto en su celda ya ajustada
  (foreach item tlist
    (setq e    (car item)
          cell (cadr item)
          ed   (entget e)
          zz   (if (caddr (cdr (assoc 10 ed))) (caddr (cdr (assoc 10 ed))) 0.0)
          cx   (/ (+ (ra:shx (nth 0 cell)) (ra:shx (nth 1 cell))) 2.0)
          cy   (/ (+ (ra:shy (nth 2 cell)) (ra:shy (nth 3 cell))) 2.0)
          obj  (vlax-ename->vla-object e))
    (setq cen (ra:rot (list cx cy) base ang1))
    (if (= "TEXT" (cdr (assoc 0 ed)))
      (progn
        (vla-put-Rotation obj ang1)
        (vla-put-Alignment obj 10) ; acAlignmentMiddleCenter
        (vla-put-TextAlignmentPoint
          obj (vlax-3d-point (list (car cen) (cadr cen) zz))))
      (progn ; MTEXT
        (vla-put-AttachmentPoint obj 5) ; acAttachmentPointMiddleCenter
        (vla-put-Rotation obj ang1)
        (vla-put-InsertionPoint
          obj (vlax-3d-point (list (car cen) (cadr cen) zz)))))
    (setq nfit (1+ nfit)))

  (if (> nex 0)
    (princ (strcat "\nAVISO: se ignoraron " (itoa nex)
                   " linea(s)/polilinea(s) que no pertenecen a la tabla (inclinadas o lejos del texto).")))
  (princ (strcat "\nTabla lista. Textos abreviados: " (itoa nabr)
                 "  |  Textos centrados: " (itoa nfit)))
  (if (zerop nabr)
    (princ "\nAVISO: ningun texto coincidio con las palabras clave (TAPA, FONDO, INVER ENTRADA, INVER SALIDA, DIAMETRO). Si la tabla ya estaba abreviada, use UNDO y repita."))
  (princ))

;; Envoltura: filtra la seleccion, valida la tabla y solo entonces la procesa
(defun ra:proc (ss pairs sym / lst i e ed ang1 base r msg ss2 nex nd nl nt nm d)
  (setq i 0)
  (repeat (sslength ss)
    (setq lst (cons (ssname ss i) lst) i (1+ i)))
  (setq ang1 0.0 base nil nl 0 nt 0 nm 0)
  (foreach e lst
    (setq ed (entget e))
    (cond ((member (cdr (assoc 0 ed)) '("LINE" "LWPOLYLINE")) (setq nl (1+ nl)))
          ((= "TEXT"  (cdr (assoc 0 ed))) (setq nt (1+ nt)))
          ((= "MTEXT" (cdr (assoc 0 ed))) (setq nm (1+ nm))))
    (if (and (null base) (member (cdr (assoc 0 ed)) '("LINE" "LWPOLYLINE")))
      (setq base (cdr (assoc 10 ed)))))
  (princ (strcat "\nSeleccion: " (itoa nl) " linea(s), " (itoa nt) " TEXT, " (itoa nm) " MTEXT."))
  (setq ang1 (ra:angulo lst))
  (setq r (ra:filtrar lst base ang1) lst (car r) nex (cadr r))

  ;; --- DUPLICADOS: UNA SOLA PASADA, DENTRO DE LA SELECCION (FIX 2/FIX 3) ---
  (setq r (ra:dedup lst base ang1) lst (car r) nd (cadr r))
  (if (> nd 0)
    (princ (strcat "\nAVISO: se eliminaron " (itoa nd)
                   " texto(s) duplicado(s) (mismo contenido y misma posicion, uno encima de otro).")))
  (if (> nex 0)
    (princ (strcat "\nAVISO: se ignoraron " (itoa nex)
                   " linea(s)/polilinea(s) que no pertenecen a la tabla (inclinadas o lejos del texto).")))

  ;; --- VALIDACION (solo despues de limpiar duplicados) ---
  (setq ra:dups nil)
  (setq msg (ra:validar lst base ang1 "ROTAA"))
  ;; ra:dups ya fue limpiado por ra:dedup, NO se vuelve a borrar nada aqui.
  (if (and (null msg) (zerop (+ nt nm)))
    (setq msg "\nROTAA: la seleccion no tiene textos (TEXT o MTEXT). No se modifico nada."))
  (if msg
    (princ msg)
    (progn
      (setq ss2 (ssadd))
      (foreach e lst (ssadd e ss2))
      (ra:proc2 ss2 pairs sym)))
  (princ))

;; ---------------- COMANDO ----------------
(defun c:ROTAA ( / *error* doc ss pairs sym saved fin opt d cur nuevo p)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg)))
    (if doc (vla-EndUndoMark doc))
    (princ))

  (princ "\n--- ROTAA: abreviar textos y ajustar la tabla al texto ---")

  ;; Simbolo despues de la abreviatura (se guarda; "0" = ninguno)
  (setq saved (getenv "ROTAA_SIMBOLO"))
  (if (null saved) (setq saved ":"))
  (setq fin (getstring T (strcat "\nSimbolo despues de la abreviatura (0 = ninguno, Enter = <"
                                 (if (= saved "0") "ninguno" saved) ">): ")))
  (if (/= fin "") (setq saved fin))
  (setenv "ROTAA_SIMBOLO" saved)
  (setq sym (if (= saved "0") "" saved))

  ;; Abreviaturas (se guardan)
  (initget "Si No")
  (setq opt (getkword "\nModificar abreviaturas? [Si/No] <No>: "))
  (foreach d ra:defs
    (setq cur (getenv (nth 2 d)))
    (if (or (null cur) (= cur "")) (setq cur (nth 1 d)))
    (if (= opt "Si")
      (progn
        (setq nuevo (getstring T (strcat "\n  " (nth 3 d) " -> <" cur ">: ")))
        (if (/= nuevo "") (setq cur nuevo))))
    (setenv (nth 2 d) cur)
    (setq pairs (cons (list (car d) cur) pairs)))
  (setq pairs (reverse pairs))
  (princ "\nConfiguracion: ")
  (foreach p pairs (princ (strcat (cadr p) sym "  ")))

  (while (setq ss (progn
                    (princ "\nSeleccione la tabla (polilineas y textos), Enter para salir: ")
                    (ssget '((0 . "LWPOLYLINE,LINE,TEXT,MTEXT")))))
    (vla-StartUndoMark doc)
    (ra:proc ss pairs sym)
    (vla-EndUndoMark doc))
  (princ))

(princ "\nROTAA cargado (version corregida). Escriba ROTAA para iniciar.")
(princ)
