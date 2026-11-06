;;; ------------------------------------------------------------------
;;; AutoRangoCivil3D v4
;;; Lee los textos de estacion (ej. 1+050.00) que quedan dentro de cada
;;; viewport de cada layout y escribe "DE EST. <menor> A EST. <mayor>"
;;; en el texto descriptivo del layout.
;;; v3: prueba dos formas de ubicar el viewport en el Model y muestra
;;;     informacion por viewport para poder diagnosticar.
;;; Comandos:  AutoRangoCivil3D   (principal)
;;;            SECDIAG            (que objetos hay en un viewport)
;;; ------------------------------------------------------------------

(defun sec:digitp (ch)
  (and (= (strlen ch) 1) (wcmatch ch "#"))
)

;; busca una estacion tipo 1+050.00 -> (valor "texto") o nil
(defun sec:find-sta (s / pos res i j a b len)
  (setq len (strlen s)
        pos (vl-string-search "+" s))
  (while (and pos (not res))
    (setq i (1- pos))
    (while (and (>= i 0) (sec:digitp (substr s (1+ i) 1)))
      (setq i (1- i)))
    (setq a (substr s (+ i 2) (- pos i 1)))
    (setq j (1+ pos))
    (while (and (< j len)
                (or (sec:digitp (substr s (1+ j) 1))
                    (= (substr s (1+ j) 1) ".")))
      (setq j (1+ j)))
    (setq b (substr s (+ pos 2) (- j pos 1)))
    (if (and (> (strlen a) 0) (>= (strlen b) 3))
      (setq res (list (+ (* 1000.0 (atof a)) (atof b))
                      (strcat a "+" b)))
      (setq pos (vl-string-search "+" s (1+ pos)))
    )
  )
  res
)

;; limites del viewport en el Model -> (minX minY maxX maxY)
;; usetg = T  : centro = grupo 12 + grupo 17 (target)
;; usetg = nil: centro = grupo 12 solo
(defun sec:vp-bounds (vd tol usetg / c12 tg vh pw ph cx cy vw)
  (setq c12 (cdr (assoc 12 vd))
        tg  (cdr (assoc 17 vd))
        vh  (cdr (assoc 45 vd))
        pw  (cdr (assoc 40 vd))
        ph  (cdr (assoc 41 vd)))
  (if (and c12 vh pw ph (> ph 0.0) (> vh 0.0))
    (progn
      (setq vw (* vh (/ pw ph))
            cx (+ (car c12)  (if (and usetg tg) (car tg)  0.0))
            cy (+ (cadr c12) (if (and usetg tg) (cadr tg) 0.0)))
      (list (- cx (/ vw 2.0) tol) (- cy (/ vh 2.0) tol)
            (+ cx (/ vw 2.0) tol) (+ cy (/ vh 2.0) tol))
    )
  )
)

(defun c:AutoRangoCivil3D ( / acDoc lays lay layName ssM i d ip st mtList
                              ssVp k vd bnd lo hi loS hiS ssL j obj s up p e
                              newSub tol nUpd hits m mode nvp origLay)
  (vl-load-com)
  (setq tol 1.0)
  (setq acDoc (vla-get-activedocument (vlax-get-acad-object)))
  (setq lays  (vla-get-layouts acDoc))
  (setq origLay (vla-get-activelayout acDoc))

  ;; 1. Textos de estacion del Model
  (setq mtList '())
  (setq ssM (ssget "X" '((0 . "TEXT,MTEXT") (410 . "Model"))))
  (if ssM
    (repeat (setq i (sslength ssM))
      (setq i (1- i))
      (setq d  (entget (ssname ssM i)))
      (setq ip (cdr (assoc 10 d)))
      (if (and (= (cdr (assoc 0 d)) "TEXT") (assoc 11 d)
               (or (and (assoc 72 d) (/= 0 (cdr (assoc 72 d))))
                   (and (assoc 74 d) (/= 0 (cdr (assoc 74 d))))))
        (setq ip (cdr (assoc 11 d))))
      (setq st (sec:find-sta (cdr (assoc 1 d))))
      (if (and st ip)
        (setq mtList (cons (list (car ip) (cadr ip) (car st) (cadr st)) mtList)))
    )
  )
  (princ (strcat "\nTextos de estacion en Model: " (itoa (length mtList))))

  ;; 2. Layouts
  (vlax-for lay lays
    (setq layName (vla-get-name lay))
    (if (/= (strcase layName) "MODEL")
      (progn
        (setq lo nil hi nil loS nil hiS nil nvp 0)
        (princ (strcat "\n=== Layout: " layName))
        ;; activar el layout: en los inactivos el estado de los viewports no es confiable
        (vla-put-activelayout acDoc lay)
        (setq ssVp (ssget "X" (list '(0 . "VIEWPORT") (cons 410 layName))))
        (if ssVp
          (repeat (setq k (sslength ssVp))
            (setq k (1- k))
            (setq vd (entget (ssname ssVp k)))
            (princ (strcat "\n   [viewport id=" (itoa (cdr (assoc 69 vd)))
                           " estado=" (itoa (cdr (assoc 68 vd))) "]"))
            (if (and (/= (cdr (assoc 69 vd)) 1)
                     (/= (cdr (assoc 68 vd)) 0))
              (progn
                (setq nvp (1+ nvp) hits 0)
                ;; prueba con target y, si no encuentra nada, sin target
                (foreach mode '(T nil)
                  (if (= hits 0)
                    (if (setq bnd (sec:vp-bounds vd tol mode))
                      (progn
                        (foreach m mtList
                          (if (and (>= (car m) (car bnd))  (<= (car m) (caddr bnd))
                                   (>= (cadr m) (cadr bnd)) (<= (cadr m) (cadddr bnd)))
                            (progn
                              (setq hits (1+ hits))
                              (if (or (not lo) (< (caddr m) lo))
                                (setq lo (caddr m) loS (cadddr m)))
                              (if (or (not hi) (> (caddr m) hi))
                                (setq hi (caddr m) hiS (cadddr m)))
                            )
                          )
                        )
                        (princ (strcat "\n   VP " (itoa nvp)
                                       " X:" (rtos (car bnd) 2 1) "-" (rtos (caddr bnd) 2 1)
                                       " Y:" (rtos (cadr bnd) 2 1) "-" (rtos (cadddr bnd) 2 1)
                                       (if mode " [con target]" " [sin target]")
                                       " -> estaciones: " (itoa hits)))
                      )
                    )
                  )
                )
              )
            )
          )
          (princ "\n   (sin viewports)")
        )

        ;; 3. Escribir en el texto del layout
        (if lo
          (progn
            (setq newSub (strcat "DE EST. " loS " A EST. " hiS))
            (setq nUpd 0)
            (setq ssL (ssget "X" (list '(0 . "TEXT,MTEXT") (cons 410 layName))))
            (if ssL
              (repeat (setq j (sslength ssL))
                (setq j (1- j))
                (setq obj (vlax-ename->vla-object (ssname ssL j)))
                (setq s   (vla-get-textstring obj))
                (setq up  (strcase s))
                (if (setq p (vl-string-search "DE EST" up))
                  (progn
                    (setq e (vl-string-search "\\P" s p))
                    (if (not e) (setq e (strlen s)))
                    (vla-put-textstring obj
                      (strcat (substr s 1 p) newSub (substr s (1+ e))))
                    (setq nUpd (1+ nUpd))
                  )
                )
              )
            )
            (princ (strcat "\n[OK] " newSub
                           (if (> nUpd 0) ""
                             "  (no encontre el texto DE EST. A EST. en este layout)")))
          )
          (princ "\n[AVISO] no encontre estaciones en los viewports de este layout")
        )
      )
    )
  )
  (vl-catch-all-apply (quote vla-put-activelayout) (list acDoc origLay))
  (princ "\n--- Listo (F2 para ver el detalle) ---")
  (princ)
)

;; Diagnostico: lista los objetos del Model que caen dentro de un viewport
(defun c:SECDIAG ( / e vd bnd ss i obj mn mx p1 p2 cx cy types ent nm r)
  (vl-load-com)
  (setq e (car (entsel "\nSelecciona el borde de un viewport: ")))
  (if (and e (= (cdr (assoc 0 (entget e))) "VIEWPORT"))
    (progn
      (setq vd  (entget e))
      (setq bnd (sec:vp-bounds vd 0.0 T))
      (setq ss  (ssget "X" '((410 . "Model"))))
      (setq types '())
      (if (and bnd ss)
        (repeat (setq i (sslength ss))
          (setq i (1- i))
          (setq ent (ssname ss i))
          (setq obj (vlax-ename->vla-object ent))
          (setq mn nil mx nil)
          (setq r (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx)))
          (if (and mn mx (not (vl-catch-all-error-p r)))
            (progn
              (setq p1 (vlax-safearray->list mn)
                    p2 (vlax-safearray->list mx))
              (setq cx (/ (+ (car p1) (car p2)) 2.0)
                    cy (/ (+ (cadr p1) (cadr p2)) 2.0))
              (if (and (>= cx (car bnd)) (<= cx (caddr bnd))
                       (>= cy (cadr bnd)) (<= cy (cadddr bnd)))
                (progn
                  (setq nm (cdr (assoc 0 (entget ent))))
                  (if (assoc nm types)
                    (setq types (subst (cons nm (1+ (cdr (assoc nm types)))) (assoc nm types) types))
                    (setq types (cons (cons nm 1) types)))
                  (if (member nm '("TEXT" "MTEXT"))
                    (princ (strcat "\n  " nm ": " (cdr (assoc 1 (entget ent))))))
                )
              )
            )
          )
        )
      )
      (princ "\nTipos de objeto dentro del viewport:")
      (foreach tp types (princ (strcat "\n  " (car tp) " x" (itoa (cdr tp)))))
    )
    (princ "\nNo seleccionaste un viewport.")
  )
  (princ)
)

(princ "\nCargado: AutoRangoCivil3D v4  /  SECDIAG")
(princ)
