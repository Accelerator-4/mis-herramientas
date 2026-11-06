;;; ======================================================================
;;;  FLUJO.lsp  -  Inserta el bloque de flecha de flujo en el extremo
;;;                de una tubería (PVP / PVS), alineado a la línea.
;;; ======================================================================
;;;
;;;  QUÉ HACE
;;;  --------
;;;  - Inserta TU bloque de flecha de flujo en el extremo de la línea,
;;;    polilínea o arco donde hagas clic (el extremo más cercano al clic).
;;;  - El bloque queda alineado con la dirección de la tubería.
;;;  - Puedes ponerlo a una distancia (Offset) del extremo, sobre la
;;;    misma línea.
;;;  - Puedes cambiar la escala, invertir el sentido y corregir el giro.
;;;  - Al cambiar Escala, Invertir, Offset o Giro, el comando BORRA el
;;;    último bloque puesto y lo vuelve a poner con el nuevo valor.
;;;  - Los ajustes se recuerdan para las siguientes flechas.
;;;
;;;  CÓMO CARGARLO
;;;  -------------
;;;  1. Escribe APPLOAD, selecciona FLUJO.lsp y pulsa Cargar.
;;;     (Para que cargue siempre: botón "Contenido" > Agregar.)
;;;  2. Escribe FLUJO y pulsa Enter.
;;;
;;;  CÓMO SE USA
;;;  -----------
;;;  1. Si tu bloque NO se llama FLUJO, escribe B y el nombre del bloque.
;;;  2. Escribe O para indicar la distancia desde el extremo
;;;     (0 = pegado al extremo).
;;;  3. Escribe E para la escala (2 = el doble de grande, 0.5 = la mitad).
;;;  4. Haz clic en la tubería CERCA del extremo donde quieres la flecha.
;;;     El bloque se inserta alineado a esa línea.
;;;  5. Si quieres el sentido contrario, escribe I: borra la flecha que
;;;     acabas de poner y coloca la invertida.
;;;  6. Sigue haciendo clic en otras tuberías. Enter o Esc para terminar.
;;;
;;;  OPCIONES (se escriben en la línea de comandos)
;;;  ----------------------------------------------
;;;  E = Escala     Factor de escala del bloque. Rehace el último bloque.
;;;  I = Invertir   Cambia el sentido de la flecha. Rehace el último.
;;;  O = Offset     Distancia desde el extremo, medida sobre la línea.
;;;                 Puedes teclear el valor o marcar dos puntos.
;;;  B = Bloque     Nombre del bloque a usar (debe existir en el dibujo).
;;;  G = Giro       Corrección de giro en grados (por defecto -90).
;;;
;;;  CONSEJOS
;;;  --------
;;;  - Haz clic cerca del extremo correcto: el comando elige el extremo
;;;    más cercano al punto donde pulsas.
;;;  - El bloque debe estar ya en el dibujo. Si lo tienes en otro archivo,
;;;    insértalo una vez con INSERT y bórralo, la definición queda.
;;;  - El valor por defecto de Giro es -90 porque el bloque se dibujó con
;;;    la flecha apuntando hacia arriba (+Y). Si tu bloque apunta a la
;;;    derecha (+X) usa G y escribe 0. Si sale girado, prueba 90, 180 o
;;;    -90 hasta que quede bien; el valor se conserva.
;;;  - El punto de inserción es el punto base del bloque. Si quieres que
;;;    otro punto de la flecha toque el extremo, edita el bloque con
;;;    BEDIT y mueve el punto base.
;;;  - Para cambiar el diseño de TODAS las flechas, edita el bloque con
;;;    BEDIT: se actualizan todas las insertadas.
;;;  - El bloque toma la capa actual si fue dibujado en capa 0. Pon la
;;;    capa deseada como actual antes de usar el comando.
;;;  - Cada flecha es un bloque normal: SCALE, MOVE, ROTATE y ERASE
;;;    funcionan sobre ella.
;;;  - Solo se rehace la ÚLTIMA flecha. Para corregir una anterior,
;;;    bórrala con ERASE y vuelve a ponerla.
;;;  - Funciona con LINE, LWPOLYLINE, POLYLINE y ARC.
;;;
;;; ======================================================================

(vl-load-com)

;; ---------- Inserta el bloque con los valores guardados ----------
(defun flj-place ( / rot)
  (setq rot (+ *flj-ang* (* *flj-giro* (/ pi 180.0))))
  (if *flj-inv* (setq rot (+ rot pi)))
  (setq *flj-last*
        (entmakex (list '(0 . "INSERT") (cons 2 *flj-blk*)
                        (cons 10 *flj-pt*)
                        (cons 41 *flj-esc*) (cons 42 *flj-esc*) (cons 43 *flj-esc*)
                        (cons 50 rot)))))

;; ---------- Borra el último bloque y lo vuelve a poner ----------
(defun flj-redo ()
  (if (and *flj-last* *flj-pt*)
    (progn
      (if (entget *flj-last*) (entdel *flj-last*))
      (flj-place))
    (princ "\nTodavía no hay bloque para modificar.")))

;; ---------- Calcula punto y ángulo sobre la tubería ----------
(defun flj-calc ( / len dist prm der a)
  (setq len  (vlax-curve-getDistAtParam *flj-ent* (vlax-curve-getEndParam *flj-ent*))
        dist (min *flj-off* len))
  (if *flj-atstart*
    (setq prm (vlax-curve-getParamAtDist *flj-ent* dist))
    (setq prm (vlax-curve-getParamAtDist *flj-ent* (- len dist))))
  (setq *flj-pt* (vlax-curve-getPointAtParam *flj-ent* prm)
        der (vlax-curve-getFirstDeriv *flj-ent* prm)
        a   (angle '(0 0 0) (list (car der) (cadr der))))
  ;; ángulo = dirección del extremo hacia adentro de la tubería
  (setq *flj-ang* (if *flj-atstart* a (+ a pi))))

;; ---------- Comando principal ----------
(defun c:FLUJO ( / sel ent clk sp ep loop k v)
  (if (not *flj-blk*)  (setq *flj-blk* "FLUJO"))
  (if (not *flj-esc*)  (setq *flj-esc* 1.0))
  (if (not *flj-off*)  (setq *flj-off* 0.0))
  (if (not *flj-giro*) (setq *flj-giro* -90.0)) ; tu bloque apunta hacia arriba (+Y)
  (if (not *flj-inv*)  (setq *flj-inv* nil))
  (setq *flj-last* nil *flj-pt* nil *flj-ang* nil *flj-ent* nil)

  (if (not (tblsearch "BLOCK" *flj-blk*))
    (princ (strcat "\nAviso: el bloque \"" *flj-blk* "\" no existe. Use la opción Bloque.")))

  (setq loop T)
  (while loop
    (initget "Escala Invertir Offset Bloque Giro")
    (setq sel (entsel (strcat "\nClic en tubería cerca del extremo [Escala/Invertir/Offset/Bloque/Giro] <Esc="
                              (rtos *flj-esc* 2 2) " Off=" (rtos *flj-off* 2 2)
                              " Giro=" (rtos *flj-giro* 2 0)
                              (if *flj-inv* " INVERTIDO" "") ">: ")))
    (cond
      ;; ---- opciones ----
      ((= (type sel) 'STR)
       (setq k (strcase sel))
       (cond
         ((wcmatch k "E*")
          (setq v (getreal (strcat "\nEscala del bloque <" (rtos *flj-esc* 2 2) ">: ")))
          (if (and v (> v 0)) (progn (setq *flj-esc* v) (flj-redo))))
         ((wcmatch k "I*")
          (setq *flj-inv* (not *flj-inv*))
          (flj-redo))
         ((wcmatch k "O*")
          (setq v (getdist (strcat "\nDistancia desde el extremo <" (rtos *flj-off* 2 2) ">: ")))
          (if (and v (>= v 0))
            (progn (setq *flj-off* v)
                   (if *flj-ent* (flj-calc))
                   (flj-redo))))
         ((wcmatch k "B*")
          (setq v (getstring (strcat "\nNombre del bloque <" *flj-blk* ">: ")))
          (if (and v (/= v ""))
            (if (tblsearch "BLOCK" v)
              (setq *flj-blk* v)
              (princ "\nEse bloque no existe en el dibujo."))))
         ((wcmatch k "G*")
          (setq v (getreal (strcat "\nGiro de corrección en grados <" (rtos *flj-giro* 2 0) ">: ")))
          (if v (progn (setq *flj-giro* v) (flj-redo))))))

      ;; ---- clic sobre una tubería ----
      ((and sel (= (type sel) 'LIST))
       (setq ent (car sel)
             clk (trans (cadr sel) 1 0))
       (if (vl-catch-all-error-p
             (vl-catch-all-apply 'vlax-curve-getStartPoint (list ent)))
         (princ "\nSeleccione una línea, polilínea o arco.")
         (if (not (tblsearch "BLOCK" *flj-blk*))
           (princ "\nEl bloque no existe. Use la opción Bloque.")
           (progn
             (setq sp (vlax-curve-getStartPoint ent)
                   ep (vlax-curve-getEndPoint ent)
                   *flj-ent* ent
                   *flj-atstart* (< (distance clk sp) (distance clk ep)))
             (flj-calc)
             (flj-place)))))

      ;; ---- Enter o Esc ----
      (T (setq loop nil))))
  (setq *flj-ent* nil)
  (princ))

;; Reinicia el giro al cargar (tu bloque apunta hacia arriba)
(setq *flj-giro* -90.0)
(princ "\nComando FLUJO cargado. Escriba FLUJO para usarlo.")
(princ)
