;;; ============================================================
;;; PUNTO-A-EARTH.LSP
;;; Abre Google Earth Pro en la ubicación de un punto COGO
;;; Sistema: UTM WGS84 Zona 16N con corrección Nicaragua
;;; ============================================================

(defun c:VERENPUNTO ( / *error* ent obj x y lat lon
                        a f b e2 e4 e6 e1 k0 fe fn lambda0
                        x1 y1 M1 mu phi1 N1 T1 C1 R1 D
                        lat-deg lon-deg rad-a-deg
                        earth-path kml-path kml-file
                        sinphi1 cosphi1 tanphi1
                        ctrl-calc ctrl-lat-calc ctrl-lon-calc
                        offset-lat offset-lon resultado)

  (vl-load-com)

  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; ── Funciones trigonométricas ───────────────────────────────
  (defun TAN (x) (/ (sin x) (cos x)))

  ;; ── Punto de control para corrección ───────────────────────
  ;; UTM:  581730.919 E,  1338418.710 N
  ;; Real: Rotonda Jean Paul Génie, Managua, Nicaragua
  (setq ctrl-utm-e    581730.919)
  (setq ctrl-utm-n    1338418.710)
  (setq ctrl-lat-real  12.10633)
  (setq ctrl-lon-real -86.24895)

  ;; ── 1. Seleccionar punto COGO ───────────────────────────────
  (setq ent (car (entsel "\nSelecciona el punto COGO: ")))

  (if (null ent)
    (progn (princ "\nNo seleccionaste ningún punto.") (exit))
  )

  (if (not (= (cdr (assoc 0 (entget ent))) "AECC_COGO_POINT"))
    (progn (princ "\nEl objeto seleccionado no es un punto COGO.") (exit))
  )

  (setq obj (vlax-ename->vla-object ent))
  (setq x (vl-catch-all-apply 'vlax-get-property (list obj 'Easting)))
  (setq y (vl-catch-all-apply 'vlax-get-property (list obj 'Northing)))

  (if (or (vl-catch-all-error-p x) (vl-catch-all-error-p y))
    (progn (princ "\nNo se pudieron leer las coordenadas.") (exit))
  )

  (princ (strcat "\nCoordenadas UTM leídas:"
                 "\n  Easting:  " (rtos x 2 3)
                 "\n  Northing: " (rtos y 2 3)))

  ;; ── 2. Constantes WGS84 ────────────────────────────────────
  (setq a  6378137.0)
  (setq f  (/ 1.0 298.257223563))
  (setq b  (* a (- 1.0 f)))
  (setq e2 (- 1.0 (/ (* b b) (* a a))))
  (setq e4 (* e2 e2))
  (setq e6 (* e2 e4))
  (setq k0 0.9996)
  (setq fe 500000.0)
  (setq fn 0.0)
  (setq rad-a-deg (/ 45.0 (atan 1.0)))
  (setq lambda0 (/ -87.0 rad-a-deg))

  ;; ── 3. Función de conversión UTM → Lat/Lon ─────────────────
  (defun utm-a-latlon (utm-e utm-n / e1 x1 y1 M1 mu phi1
                                     sinphi1 cosphi1 tanphi1
                                     N1 T1 C1 R1 D lat lon)
    (setq e1 (/ (- 1.0 (sqrt (- 1.0 e2)))
                (+ 1.0 (sqrt (- 1.0 e2)))))
    (setq x1 (- utm-e fe))
    (setq y1 (- utm-n fn))
    (setq M1 (/ y1 k0))
    (setq mu
      (/ M1
         (* a (- 1.0
                 (* (/ 3.0 4.0)     e2)
                 (* (/ 45.0 64.0)   e4)
                 (* (/ 175.0 256.0) e6))))
    )
    (setq phi1
      (+ mu
         (* (- (* (/ 3.0 2.0) e1)
               (* (/ 27.0 32.0) (* e1 e1 e1)))
            (sin (* 2.0 mu)))
         (* (+ (* (/ 21.0 16.0) (* e1 e1))
               (* (/ -55.0 32.0) (* e1 e1 e1 e1)))
            (sin (* 4.0 mu)))
         (* (* (/ 151.0 96.0) (* e1 e1 e1))
            (sin (* 6.0 mu)))
         (* (* (/ 1097.0 512.0) (* e1 e1 e1 e1))
            (sin (* 8.0 mu)))
      )
    )
    (setq sinphi1 (sin phi1))
    (setq cosphi1 (cos phi1))
    (setq tanphi1 (TAN phi1))
    (setq N1 (/ a (sqrt (- 1.0 (* e2 sinphi1 sinphi1)))))
    (setq T1 (* tanphi1 tanphi1))
    (setq C1 (* (/ e2 (- 1.0 e2)) cosphi1 cosphi1))
    (setq R1 (/ (* a (- 1.0 e2))
                (expt (- 1.0 (* e2 sinphi1 sinphi1)) 1.5)))
    (setq D (/ x1 (* N1 k0)))
    (setq lat
      (- phi1
         (* (/ (* N1 tanphi1) R1)
            (- (* (/ 1.0 2.0) (* D D))
               (* (/ 1.0 24.0)
                  (+ (* -3.0 C1)
                     (* 9.0 T1 C1)
                     (* 5.0 C1 C1)
                     (* -4.0 e2 e2 cosphi1 cosphi1))
                  (* D D D D)))))
    )
    (setq lon
      (+ lambda0
         (/ (- D
               (* (/ 1.0 6.0)
                  (+ (* -1.0 C1) T1 2.0)
                  (* D D D)))
            cosphi1))
    )
    (list (* lat rad-a-deg) (* lon rad-a-deg))
  )

  ;; ── 4. Calcular corrección desde punto de control ──────────
  (setq ctrl-calc     (utm-a-latlon ctrl-utm-e ctrl-utm-n))
  (setq ctrl-lat-calc (car  ctrl-calc))
  (setq ctrl-lon-calc (cadr ctrl-calc))

  (setq offset-lat (- ctrl-lat-real ctrl-lat-calc))
  (setq offset-lon (- ctrl-lon-real ctrl-lon-calc))

  (princ (strcat "\nOffset de corrección:"
                 "\n  ΔLat: " (rtos offset-lat 2 8)
                 "\n  ΔLon: " (rtos offset-lon 2 8)))

  ;; ── 5. Convertir punto y aplicar corrección ────────────────
  (setq resultado (utm-a-latlon x y))
  (setq lat-deg (+ (car  resultado) offset-lat))
  (setq lon-deg (+ (cadr resultado) offset-lon))

  (princ (strcat "\nCoordenadas geográficas corregidas:"
                 "\n  Latitud:  " (rtos lat-deg 2 8)
                 "\n  Longitud: " (rtos lon-deg 2 8)))

  ;; ── 6. Crear archivo KML ───────────────────────────────────
  (setq kml-path (strcat (getenv "TEMP") "\\punto_civil3d.kml"))
  (setq kml-file (open kml-path "w"))
  (write-line "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" kml-file)
  (write-line "<kml xmlns=\"http://www.opengis.net/kml/2.2\">" kml-file)
  (write-line "<Document>" kml-file)
  (write-line "<name>Punto Civil 3D</name>" kml-file)
  (write-line "<Placemark>" kml-file)
  (write-line "<name>Punto COGO</name>" kml-file)
  (write-line "<LookAt>" kml-file)
  (write-line (strcat "<longitude>" (rtos lon-deg 2 8) "</longitude>") kml-file)
  (write-line (strcat "<latitude>"  (rtos lat-deg 2 8) "</latitude>")  kml-file)
  (write-line "<altitude>0</altitude>" kml-file)
  (write-line "<range>500</range>" kml-file)
  (write-line "<tilt>0</tilt>" kml-file)
  (write-line "<heading>0</heading>" kml-file)
  (write-line "</LookAt>" kml-file)
  (write-line "<Point>" kml-file)
  (write-line (strcat "<coordinates>"
                      (rtos lon-deg 2 8) ","
                      (rtos lat-deg 2 8) ",0"
                      "</coordinates>") kml-file)
  (write-line "</Point>" kml-file)
  (write-line "</Placemark>" kml-file)
  (write-line "</Document>" kml-file)
  (write-line "</kml>" kml-file)
  (close kml-file)

  (princ (strcat "\nArchivo KML creado en: " kml-path))

  ;; ── 7. Abrir Google Earth Pro ──────────────────────────────
  (setq earth-path
    "C:\\Program Files\\Google\\Google Earth Pro\\client\\googleearth.exe"
  )
  (if (findfile earth-path)
    (progn
      (startapp earth-path kml-path)
      (princ "\nAbriendo Google Earth Pro...")
    )
    (progn
      (setq earth-path
        "C:\\Program Files (x86)\\Google\\Google Earth Pro\\client\\googleearth.exe"
      )
      (if (findfile earth-path)
        (progn
          (startapp earth-path kml-path)
          (princ "\nAbriendo Google Earth Pro...")
        )
        (progn
          (princ "\nNo se encontró Google Earth Pro.")
          (princ "\nAbre manualmente este archivo KML:")
          (princ (strcat "\n  " kml-path))
        )
      )
    )
  )

  (princ)
)
