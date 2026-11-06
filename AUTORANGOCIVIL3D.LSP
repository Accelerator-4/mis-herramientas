(defun c:AutoRangoCivil3D ( / acadDoc layouts layName validLayouts totalLays layIndex batchSize allStations numStations startIdx endIdx sheetStations minSt maxSt ssBlk j blkEnt blkObj atts att pTxt cleanTxt ssTxt k txtEnt txtData txtVal )
  (vl-load-com)
  (setq acadDoc (vla-get-activedocument (vlax-get-acad-object)))
  (setq layouts (vla-get-layouts acadDoc))
  
  ; 1. Recopilar todos los layouts válidos ordenados (excluyendo MODEL)
  (setq validLayouts '())
  (vlax-for lay layouts
    (setq layName (vla-get-name lay))
    (if (/= (strcase layName) "MODEL")
      (setq validLayouts (append validLayouts (list layName)))
    )
  )
  (setq validLayouts (vl-sort validLayouts '<))
  (setq totalLays (length validLayouts))
  
  (princ (strcat "\nTotal de láminas detectadas: " (itoa totalLays)))
  
  ; 2. Recopilar TODOS los títulos de sección válidos (que contengan '+') del Model Space
  (setq allStations '())
  (setq ssTxt (ssget "X" '((0 . "TEXT,MTEXT") (410 . "Model"))))
  (if ssTxt
    (progn
      (setq k (sslength ssTxt))
      (while (> k 0)
        (setq k (1- k))
        (setq txtEnt (ssname ssTxt k))
        (setq txtData (entget txtEnt))
        (setq txtVal (cdr (assoc 1 txtData)))
        
        ; Filtrar únicamente textos que tengan formato de estación (ej: 1+860.00)
        (if (and txtVal (vl-string-search "+" txtVal))
          (if (not (vl-position txtVal allStations))
            (setq allStations (cons txtVal allStations))
          )
        )
      )
    )
  )
  
  ; Ordenar numéricamente las estaciones de menor a mayor
  (setq allStations (vl-sort allStations '<))
  (setq numStations (length allStations))
  (princ (strcat "\nTotal de estaciones encontradas en el modelo: " (itoa numStations)))
  
  (if (> numStations 0)
    (progn
      ; Calcular proporción de estaciones por lámina
      (setq batchSize (/ (float numStations) (float totalLays)))
      
      (setq layIndex 0)
      (foreach layName validLayouts
        ; Activar la pestaña actual
        (vla-put-activelayout acadDoc (vla-item layouts layName))
        
        ; Definir el rango de índices para esta hoja
        (setq startIdx (fix (* layIndex batchSize)))
        (setq endIdx (fix (* (1+ layIndex) batchSize)))
        (if (>= endIdx numStations) (setq endIdx (1- numStations)))
        (if (< startIdx 0) (setq startIdx 0))
        (if (> startIdx endIdx) (setq startIdx endIdx))
        
        ; Extraer las estaciones correspondientes a esta lámina
        (setq sheetStations '())
        (setq idx startIdx)
        (while (<= idx endIdx)
          (setq sheetStations (cons (nth idx allStations) sheetStations))
          (setq idx (1+ idx))
        )
        
        (if sheetStations
          (progn
            (setq sheetStations (vl-sort sheetStations '<))
            (setq minSt (car sheetStations))
            (setq maxSt (last sheetStations))
            (princ (strcat "\n[OK] Hoja " layName " -> Rango asignado: " minSt " a " maxSt))
            
            ; 3. Actualizar el cajetín de esta lámina de forma limpia y segura (sin mover texto)
            (setq ssBlk (ssget "X" (list '(0 . "TEXT,MTEXT,INSERT") (cons 410 layName))))
            (if ssBlk
              (progn
                (setq j (sslength ssBlk))
                (while (> j 0)
                  (setq j (1- j))
                  (setq blkEnt (ssname ssBlk j))
                  (setq blkObj (vlax-ename->vla-object blkEnt))
                  
                  ; Si es texto o MText suelto en el layout
                  (if (or (= (cdr (assoc 0 (entget blkEnt))) "TEXT") (= (cdr (assoc 0 (entget blkEnt))) "MTEXT"))
                    (progn
                      (setq pTxt (vla-get-textstring blkObj))
                      (setq cleanTxt (strcase pTxt))
                      (if (or (vl-string-search "EST" cleanTxt) (vl-string-search "A EST" cleanTxt))
                        (vla-put-textstring blkObj (strcat "DE EST. " minSt " A " maxSt))
                      )
                    )
                  )
                  
                  ; Si es un Bloque (Cajetín), buscar en sus atributos internos
                  (if (= (cdr (assoc 0 (entget blkEnt))) "INSERT")
                    (if (vlax-method-applicable-p blkObj 'GetAttributes)
                      (progn
                        (setq atts (vlax-invoke blkObj 'GetAttributes))
                        (foreach att atts
                          (setq pTxt (vla-get-textstring att))
                          (setq cleanTxt (strcase pTxt))
                          (if (or (vl-string-search "EST" cleanTxt) (vl-string-search "A EST" cleanTxt))
                            (vla-put-textstring att (strcat "DE EST. " minSt " A " maxSt))
                          )
                        )
                      )
                    )
                  )
                  
                )
              )
            )
          )
        )
        (setq layIndex (1+ layIndex))
      )
    )
    (princ "\n[AVISO] No se encontraron textos con formato de estación '+' en el modelo.")
  )
  
  (princ "\n--- Proceso completado con éxito en todas las láminas. Presiona F2 para detalles. ---")
  (princ)
)