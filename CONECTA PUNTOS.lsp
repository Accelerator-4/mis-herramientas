;;; ============================================================
;;; CONECTAR-PUNTOS-POR-DESCRIPCION.LSP
;;; Versión con opción de cerrar polígono
;;; ============================================================

(defun c:CONECTAPUNTOS ( / *error* pts-all codigos codigos-elegidos
                           modo-3d capa-nombre input-codigos
                           metodo cod-lista cod-trim pos cant
                           ss i ent obj code x y z num pt
                           grupo restantes pt-actual
                           dist-min idx-sig distancia
                           pts-sorted last-ent
                           sel-metodo ss-manual
                           rango-desde rango-hasta
                           cerrar-poligono)

  (vl-load-com)

  ;; Manejo de errores
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; ── Función: ordenar por proximidad ────────────────────────
  (defun ordena-proximidad (grupo / sorted restantes pt-actual
                                    dist-min idx-sig distancia)
    (setq sorted    (list (car grupo)))
    (setq restantes (cdr grupo))
    (while (> (length restantes) 0)
      (setq pt-actual (last sorted))
      (setq dist-min  1e99)
      (setq idx-sig   nil)
      (foreach candidato restantes
        (setq distancia
          (sqrt
            (+
              (* (- (cadr candidato)  (cadr pt-actual))
                 (- (cadr candidato)  (cadr pt-actual)))
              (* (- (caddr candidato) (caddr pt-actual))
                 (- (caddr candidato) (caddr pt-actual)))
            )
          )
        )
        (if (< distancia dist-min)
          (progn
            (setq dist-min distancia)
            (setq idx-sig  candidato)
          )
        )
      )
      (setq sorted (append sorted (list idx-sig)))
      (setq restantes
        (vl-remove-if
          '(lambda (p)
             (and (= (cadr p)   (cadr idx-sig))
                  (= (caddr p)  (caddr idx-sig))
                  (= (cadddr p) (cadddr idx-sig))))
          restantes
        )
      )
    )
    sorted
  )

  ;; ── Función: ordenar por número de punto ───────────────────
  (defun ordena-por-numero (grupo)
    (vl-sort grupo '(lambda (a b) (< (nth 4 a) (nth 4 b))))
  )

  ;; ── Función: ordenar por X ─────────────────────────────────
  (defun ordena-por-x (grupo)
    (vl-sort grupo '(lambda (a b) (< (cadr a) (cadr b))))
  )

  ;; ── Función: ordenar por Y ─────────────────────────────────
  (defun ordena-por-y (grupo)
    (vl-sort grupo '(lambda (a b) (< (caddr a) (caddr b))))
  )

  ;; ── Función: leer un punto COGO desde ename ────────────────
  (defun lee-punto-cogo (ent / obj code x y z num)
    (setq obj (vlax-ename->vla-object ent))
    (if obj
      (progn
        (setq code
          (vl-catch-all-apply
            'vlax-get-property (list obj 'RawDescription))
        )
        (setq x
          (vl-catch-all-apply
            'vlax-get-property (list obj 'Easting))
        )
        (setq y
          (vl-catch-all-apply
            'vlax-get-property (list obj 'Northing))
        )
        (setq z
          (vl-catch-all-apply
            'vlax-get-property (list obj 'Elevation))
        )
        (setq num
          (vl-catch-all-apply
            'vlax-get-property (list obj 'Number))
        )
        (if (and code x y z num
                 (not (vl-catch-all-error-p code))
                 (not (vl-catch-all-error-p x))
                 (not (vl-catch-all-error-p y))
                 (not (vl-catch-all-error-p z))
                 (not (vl-catch-all-error-p num))
            )
          (progn
            (setq code (strcase (vl-string-trim " " code)))
            (if (= code "") (setq code "SIN-CODIGO"))
            (list code x y z num)
          )
          nil
        )
      )
      nil
    )
  )

  ;; ── 1. Elegir qué puntos tomar en cuenta ───────────────────
  (princ "\n\n══════════════════════════════════════")
  (princ "\n¿Qué puntos deseas procesar?")
  (princ "\n  T  →  Todos los puntos del dibujo")
  (princ "\n  G  →  Selección gráfica (clic en el dibujo)")
  (princ "\n  R  →  Rango de número de punto")
  (princ "\n══════════════════════════════════════")
  (initget "T G R")
  (setq sel-metodo (getkword "\nOpción [T/G/R] <T>: "))
  (if (null sel-metodo) (setq sel-metodo "T"))

  ;; ── 2. Recolectar puntos según selección ───────────────────
  (cond

    ;; TODOS
    ((= sel-metodo "T")
      (setq ss (ssget "_X" '((0 . "AECC_COGO_POINT"))))
      (if (null ss)
        (progn (princ "\nNo se encontraron puntos COGO.") (exit))
      )
      (princ (strcat "\nLeyendo " (itoa (sslength ss)) " puntos..."))
      (setq pts-all '())
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq pt (lee-punto-cogo ent))
        (if pt (setq pts-all (append pts-all (list pt))))
        (setq i (1+ i))
      )
    )

    ;; GRÁFICA
    ((= sel-metodo "G")
      (princ "\nSelecciona los puntos en el dibujo y presiona Enter cuando termines.")
      (setq ss-manual
        (ssget '((0 . "AECC_COGO_POINT")))
      )
      (if (null ss-manual)
        (progn (princ "\nNo seleccionaste ningún punto.") (exit))
      )
      (princ (strcat "\n" (itoa (sslength ss-manual)) " puntos seleccionados."))
      (setq pts-all '())
      (setq i 0)
      (repeat (sslength ss-manual)
        (setq ent (ssname ss-manual i))
        (setq pt (lee-punto-cogo ent))
        (if pt (setq pts-all (append pts-all (list pt))))
        (setq i (1+ i))
      )
    )

    ;; RANGO
    ((= sel-metodo "R")
      (setq rango-desde (getint "\nNúmero de punto inicial: "))
      (setq rango-hasta (getint "\nNúmero de punto final: "))
      (if (or (null rango-desde) (null rango-hasta)
              (> rango-desde rango-hasta))
        (progn (princ "\nRango inválido. Operación cancelada.") (exit))
      )
      (princ (strcat "\nBuscando puntos del " (itoa rango-desde)
                     " al " (itoa rango-hasta) "..."))
      (setq ss (ssget "_X" '((0 . "AECC_COGO_POINT"))))
      (if (null ss)
        (progn (princ "\nNo se encontraron puntos COGO.") (exit))
      )
      (setq pts-all '())
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq pt (lee-punto-cogo ent))
        (if (and pt
                 (>= (nth 4 pt) rango-desde)
                 (<= (nth 4 pt) rango-hasta))
          (setq pts-all (append pts-all (list pt)))
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n" (itoa (length pts-all))
                     " puntos encontrados en ese rango."))
    )
  )

  (if (null pts-all)
    (progn
      (princ "\nNo se pudieron leer datos de los puntos.")
      (exit)
    )
  )

  (princ (strcat "\n" (itoa (length pts-all)) " puntos leídos correctamente."))

  ;; ── 3. Códigos únicos ──────────────────────────────────────
  (setq codigos '())
  (foreach pt pts-all
    (setq code (car pt))
    (if (not (member code codigos))
      (setq codigos (append codigos (list code)))
    )
  )

  (princ "\n\n══════════════════════════════════════")
  (princ "\nCódigos disponibles en la selección:")
  (foreach c codigos
    (setq cant
      (length
        (vl-remove-if-not
          '(lambda (p) (= (car p) c))
          pts-all
        )
      )
    )
    (princ (strcat "\n  → " c "  (" (itoa cant) " puntos)"))
  )
  (princ "\n══════════════════════════════════════")

  ;; ── 4. Elegir códigos a procesar ───────────────────────────
  (princ "\n\nEscribe los códigos a unir separados por coma.")
  (princ "\nEjemplo:  EP,CL,3,4")
  (princ "\nO escribe TODOS para procesar todos.")
  (setq input-codigos (getstring T "\nCódigos: "))

  (if (= (strcase input-codigos) "TODOS")
    (setq codigos-elegidos codigos)
    (progn
      (setq codigos-elegidos '())
      (setq cod-lista (vl-string-trim " " input-codigos))
      (while (> (strlen cod-lista) 0)
        (setq pos (vl-string-search "," cod-lista))
        (if pos
          (progn
            (setq cod-trim
              (strcase (vl-string-trim " " (substr cod-lista 1 pos)))
            )
            (setq cod-lista
              (vl-string-trim " " (substr cod-lista (+ pos 2)))
            )
          )
          (progn
            (setq cod-trim
              (strcase (vl-string-trim " " cod-lista))
            )
            (setq cod-lista "")
          )
        )
        (if (and (> (strlen cod-trim) 0)
                 (not (member cod-trim codigos-elegidos)))
          (setq codigos-elegidos
            (append codigos-elegidos (list cod-trim))
          )
        )
      )
    )
  )

  (setq codigos-elegidos
    (vl-remove-if-not
      '(lambda (c) (member c codigos))
      codigos-elegidos
    )
  )

  (if (null codigos-elegidos)
    (progn
      (princ "\nNingún código válido. Operación cancelada.")
      (exit)
    )
  )

  (princ "\n\nCódigos a procesar: ")
  (foreach c codigos-elegidos (princ (strcat c "  ")))

  ;; ── 5. Método de ordenamiento ──────────────────────────────
  (princ "\n\n══════════════════════════════════════")
  (princ "\nMétodos de ordenamiento:")
  (princ "\n  P  →  Proximidad (nearest neighbor)")
  (princ "\n  N  →  Número de punto (ascendente)")
  (princ "\n  X  →  Coordenada X (izquierda a derecha)")
  (princ "\n  Y  →  Coordenada Y (abajo hacia arriba)")
  (princ "\n══════════════════════════════════════")
  (initget "P N X Y")
  (setq metodo (getkword "\nMétodo [P/N/X/Y] <P>: "))
  (if (null metodo) (setq metodo "P"))

  ;; ── 6. Modo 2D o 3D ────────────────────────────────────────
  (initget "2D 3D")
  (setq modo-3d (getkword "\n¿Polilíneas en [2D/3D]? <2D>: "))
  (if (null modo-3d) (setq modo-3d "2D"))

  ;; ── 7. ¿Cerrar polígono? ───────────────────────────────────
  (initget "S N")
  (setq cerrar-poligono (getkword "\n¿Cerrar polígono (unir último punto con el primero)? [S/N] <N>: "))
  (if (null cerrar-poligono) (setq cerrar-poligono "N"))
  (if (= cerrar-poligono "S")
    (princ "\nPolígono: CERRADO")
    (princ "\nPolígono: ABIERTO")
  )

  ;; ── 8. Capa ────────────────────────────────────────────────
  (setq capa-nombre
    (getstring T "\nNombre de capa (Enter = capa actual): ")
  )
  (if (= capa-nombre "")
    (setq capa-nombre (getvar "CLAYER"))
  )
  (if (null (tblsearch "LAYER" capa-nombre))
    (progn
      (command "._-LAYER" "_Make" capa-nombre "")
      (princ (strcat "\nCapa creada: " capa-nombre))
    )
    (princ (strcat "\nUsando capa: " capa-nombre))
  )

  ;; ── 9. Procesar cada código ────────────────────────────────
  (foreach code codigos-elegidos
    (setq grupo
      (vl-remove-if-not
        '(lambda (p) (= (car p) code))
        pts-all
      )
    )

    (if (>= (length grupo) 2)
      (progn
        (princ (strcat "\n\nProcesando: " code
                       " (" (itoa (length grupo)) " puntos)"))

        (cond
          ((= metodo "P") (setq pts-sorted (ordena-proximidad grupo)))
          ((= metodo "N") (setq pts-sorted (ordena-por-numero grupo)))
          ((= metodo "X") (setq pts-sorted (ordena-por-x grupo)))
          ((= metodo "Y") (setq pts-sorted (ordena-por-y grupo)))
        )

        ;; Dibujar polilínea
        (if (= modo-3d "3D")
          (progn
            (command "._3DPOLY")
            (foreach p pts-sorted
              (command (list (cadr p) (caddr p) (cadddr p)))
            )
            ;; Cerrar si se eligió S
            (if (= cerrar-poligono "S")
              (command "C")
              (command "")
            )
          )
          (progn
            (command "._PLINE")
            (foreach p pts-sorted
              (command (list (cadr p) (caddr p)))
            )
            ;; Cerrar si se eligió S
            (if (= cerrar-poligono "S")
              (command "C")
              (command "")
            )
          )
        )

        ;; Asignar capa
        (setq last-ent (entlast))
        (entmod
          (subst (cons 8 capa-nombre)
                 (assoc 8 (entget last-ent))
                 (entget last-ent))
        )
        (entupd last-ent)
        (princ (strcat " → OK en capa [" capa-nombre "]"))
      )
      (princ (strcat "\nCódigo " code ": solo 1 punto, se omite."))
    )
  )

  (princ "\n\n✓ Proceso completado.")
  (princ)
)
