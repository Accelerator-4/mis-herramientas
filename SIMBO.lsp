;;; ------------------------------------------------------------------
;;; SIMBO.lsp  -  Genera un cuadro de simbologia / nomenclatura
;;; Comando: SIMBO
;;;  - Toma los BLOQUES (sin repetir) y las LINEAS (capa+color+tipo
;;;    de linea+grosor, sin repetir) de los objetos seleccionados.
;;;  - Si presionas Enter en la seleccion, usa todo el dibujo
;;;    (solo el espacio actual: Model o la presentacion activa).
;;;  - Dibuja: simbolo | descripcion  (como tu cuadro NOMENCLATURA)
;;; Nota: usar en SCU Universal (WCS).
;;; ------------------------------------------------------------------

(vl-load-com)

(defun sm:line (p1 p2 lay)
  (entmake (list '(0 . "LINE") (cons 8 lay) (cons 10 p1) (cons 11 p2)))
)

(defun sm:rect (x1 y1 x2 y2 lay)
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay)
                 '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1)
                 (list 10 x1 y1) (list 10 x2 y1) (list 10 x2 y2) (list 10 x1 y2)))
)

(defun sm:text (str p h just lay)
  ;; just: 0 = izquierda-medio, 1 = centro-medio
  (entmake (list '(0 . "TEXT") (cons 8 lay) (cons 10 p) (cons 40 h)
                 (cons 1 str) (cons 7 (getvar "TEXTSTYLE"))
                 (cons 72 just) (cons 11 p) (cons 73 2)))
)

;; Ancho real de un texto con el estilo actual y altura h
(defun sm:tw (str h / tb)
  (setq tb (textbox (list (cons 1 str) (cons 40 h) (cons 7 (getvar "TEXTSTYLE")))))
  (if tb
    (- (car (cadr tb)) (car (car tb)))
    (* h (strlen str) 0.9)
  )
)

;; Inserta un bloque, lo escala para que quepa en la celda y lo centra
(defun sm:blk (name lay cx cy rowh / en obj mn mx d f)
  (entmake (list '(0 . "INSERT") (cons 8 lay) (cons 2 name)
                 (list 10 cx cy 0.0) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) '(50 . 0.0)))
  (setq en  (entlast)
        obj (vlax-ename->vla-object en))
  (vla-getboundingbox obj 'mn 'mx)
  (setq mn (vlax-safearray->list mn)
        mx (vlax-safearray->list mx)
        d  (max (- (car mx) (car mn)) (- (cadr mx) (cadr mn))))
  (if (> d 1e-9)
    (progn
      (setq f (/ (* rowh 0.6) d))
      (vla-scaleentity obj (vlax-3d-point cx cy 0.0) f)
      (vla-getboundingbox obj 'mn 'mx)
      (setq mn (vlax-safearray->list mn)
            mx (vlax-safearray->list mx))
    )
  )
  (vla-move obj
            (vlax-3d-point (list (/ (+ (car mn) (car mx)) 2.0)
                                 (/ (+ (cadr mn) (cadr mx)) 2.0) 0.0))
            (vlax-3d-point cx cy 0.0))
)

;; Muestra de linea con las mismas propiedades que el original
(defun sm:sample (lay col lt lw x1 x2 y / ent)
  (setq ent (list '(0 . "LINE") (cons 8 lay)))
  (if col (setq ent (append ent (list (cons 62 col)))))
  (if lt  (setq ent (append ent (list (cons 6 lt)))))
  (if lw  (setq ent (append ent (list (cons 370 lw)))))
  (entmake (append ent (list (list 10 x1 y 0.0) (list 11 x2 y 0.0))))
)

(defun c:SIMBO (/ flt ss i n e ed typ lay name col lt lw key blks lins items
                  titulo rowh symw descw th p0 x0 y0 nrows k ymid cx desc edit
                  lyrT it descs maxw pad)
  (setq lyrT "SIMBOLOGIA")
  (setq flt "INSERT,LINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,SPLINE,ELLIPSE")

  (prompt "\nSeleccione los objetos a incluir (Enter = todo el dibujo): ")
  (setq ss (ssget (list (cons 0 flt))))
  (if (null ss)
    (setq ss (ssget "_X" (list (cons 0 flt) (cons 410 (getvar "CTAB")))))
  )

  (if (null ss)
    (prompt "\nNo se encontraron objetos.")
    (progn
      ;; ---- recolectar bloques y lineas unicos ----
      (setq n (sslength ss) i 0 blks nil lins nil)
      (while (< i n)
        (setq e   (ssname ss i)
              ed  (entget e)
              typ (cdr (assoc 0 ed))
              lay (cdr (assoc 8 ed)))
        (if (/= (strcase lay) lyrT)
          (if (= typ "INSERT")
            (progn
              (setq name (vl-catch-all-apply
                           'vla-get-effectivename
                           (list (vlax-ename->vla-object e))))
              (if (vl-catch-all-error-p name) (setq name (cdr (assoc 2 ed))))
              (if (and (/= (substr name 1 1) "*") (not (assoc name blks)))
                (setq blks (cons (cons name lay) blks))
              )
            )
            (progn
              (setq col (cdr (assoc 62 ed))
                    lt  (cdr (assoc 6 ed))
                    lw  (cdr (assoc 370 ed)))
              (if (/= (strcase (if lt lt "")) "BYBLOCK")
                (progn
                  (setq key (vl-princ-to-string (list lay col lt lw)))
                  (if (not (assoc key lins))
                    (setq lins (cons (list key lay col lt lw) lins))
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (setq blks (reverse blks) lins (reverse lins))

      (if (and (null blks) (null lins))
        (prompt "\nNo hay bloques ni lineas validas.")
        (progn
          ;; ---- parametros ----
          (setq titulo (getstring T "\nTitulo del cuadro <NOMENCLATURA>: "))
          (if (= titulo "") (setq titulo "NOMENCLATURA"))
          (setq rowh (getreal "\nAltura de fila <8.0>: "))
          (if (null rowh) (setq rowh 8.0))
          (setq symw  (* rowh 3.0)
                descw (* rowh 8.0)
                th    (* rowh 0.4))
          (initget "Si No")
          (setq edit (getkword "\n¿Editar cada descripcion? [Si/No] <No>: "))
          (setq p0 (getpoint "\nPunto de insercion (esquina superior izquierda): "))

          (if p0
            (progn
              ;; capa del cuadro
              (if (not (tblsearch "LAYER" lyrT))
                (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                               '(100 . "AcDbLayerTableRecord") (cons 2 lyrT)
                               '(70 . 0) '(62 . 7) '(6 . "Continuous")))
              )

              ;; items: (tipo nombre capa col lt lw)
              (setq items nil)
              (foreach b blks
                (setq items (cons (list "B" (car b) (cdr b) nil nil nil) items)))
              (foreach l lins
                (setq items (cons (list "L" (cadr l) (cadr l) (caddr l)
                                        (cadddr l) (nth 4 l)) items)))
              (setq items (reverse items))

              ;; descripciones finales
              (setq descs nil)
              (foreach it items
                (setq desc (strcase (cadr it)))
                (if (= edit "Si")
                  (progn
                    (setq name (getstring T (strcat "\nDescripcion para <" desc ">: ")))
                    (if (/= name "") (setq desc (strcase name)))
                  )
                )
                (setq descs (cons desc descs))
              )
              (setq descs (reverse descs))

              ;; ajustar ancho de la columna al texto mas largo
              (setq pad  (* rowh 0.6)
                    maxw 0.0)
              (foreach d descs
                (setq maxw (max maxw (sm:tw d th))))
              (setq descw (max (+ maxw pad (* rowh 0.3))
                               (- (+ (sm:tw titulo th) pad) symw)
                               (* rowh 3.0)))

              (setq x0 (car p0) y0 (cadr p0)
                    nrows (length items))

              ;; marco, titulo y divisiones
              (sm:rect x0 (- y0 (* rowh (1+ nrows))) (+ x0 symw descw) y0 lyrT)
              (sm:text titulo (list (+ x0 (/ (+ symw descw) 2.0)) (- y0 (/ rowh 2.0)) 0.0)
                       th 1 lyrT)
              (sm:line (list x0 (- y0 rowh) 0.0)
                       (list (+ x0 symw descw) (- y0 rowh) 0.0) lyrT)
              (sm:line (list (+ x0 symw) (- y0 rowh) 0.0)
                       (list (+ x0 symw) (- y0 (* rowh (1+ nrows))) 0.0) lyrT)

              ;; filas
              (setq k 1)
              (foreach it items
                (setq ymid (- y0 (* rowh (+ k 0.5)))
                      cx   (+ x0 (/ symw 2.0))
                      desc (nth (1- k) descs))
                ;; separador de fila
                (if (< k nrows)
                  (sm:line (list x0 (- y0 (* rowh (1+ k))) 0.0)
                           (list (+ x0 symw descw) (- y0 (* rowh (1+ k))) 0.0) lyrT)
                )
                ;; simbolo
                (if (= (car it) "B")
                  (sm:blk (cadr it) (caddr it) cx ymid rowh)
                  (sm:sample (caddr it) (nth 3 it) (nth 4 it) (nth 5 it)
                             (+ x0 (* symw 0.12)) (+ x0 (* symw 0.88)) ymid)
                )
                ;; descripcion
                (sm:text desc (list (+ x0 symw (* rowh 0.3)) ymid 0.0) th 0 lyrT)
                (setq k (1+ k))
              )
              (prompt (strcat "\nCuadro creado: " (itoa (length blks)) " bloque(s) y "
                              (itoa (length lins)) " tipo(s) de linea."))
            )
          )
        )
      )
    )
  )
  (princ)
)

(prompt "\nSIMBO cargado. Escribe SIMBO para crear el cuadro de simbologia.")
(princ)
