(defun c:AutoRangoCivil3D ( / acadDoc modelSpace layouts layName vpObj vCenter vWidth vHeight minX maxX minY maxY svObj svIns stVal stationList minSt maxSt ssPs j pEnt pData pTxt )
  (vl-load-com)
  (setq acadDoc (vla-get-activedocument (vlax-get-acad-object)))
  (setq modelSpace (vla-get-modelspace acadDoc))
  (setq layouts (vla-get-layouts acadDoc))
  
  ; Recorrer todos los layouts
  (vlax-for lay layouts
    (setq layName (vla-get-name lay))
    (if (/= (strcase layName) "MODEL")
      (progn
        (vla-put-activelayout acadDoc lay)
        (princ (strcat "\nAnalizando layout: " layName))
        
        ; Buscar la ventana gráfica (Viewport) activa en el Paper Space del layout actual
        (setq vpObj nil)
        (vlax-for ent (vla-get-paperspace acadDoc)
          (if (and (= (vla-get-objectname ent) "AcDbViewport") 
                   (/= (vla-get-sheetviewportid ent) 0)) ; Excluir el viewport general de la hoja
            (setq vpObj ent)
          )
        )
        
        (if vpObj
          (progn
            ; Obtener los límites del Viewport en coordenadas del Modelo
            (setq vCenter (vla-get-center vpObj))
            (setq vWidth (vla-get-width vpObj))
            (setq vHeight (vla-get-height vpObj))
            
            ; Escala del viewport para ajustar la caja de búsqueda
            (setq vScale (vla-get-customscale vpObj))
            (if (or (not vScale) (= vScale 0)) (setq vScale 1.0))
            
            (setq halfW (* 0.5 (/ vWidth vScale)))
            (setq halfH (* 0.5 (/ vHeight vScale)))
            
            (setq cX (car vCenter))
            (setq cY (cadr vCenter))
            
            (setq minX (- cX halfW))
            (setq maxX (+ cX halfW))
            (setq minY (- cY halfH))
            (setq maxY (+ cY halfH))
            
            ; Recorrer los objetos del Model Space buscando Civil 3D Section Views
            (setq stationList '())
            (vlax-for obj modelSpace
              (and 
                ; Comprobar si el objeto es una Vista de Sección de Civil 3D
                (wcmatch (vla-get-objectname obj) "*SectionView*")
                (progn
                  (setq svIns (vla-get-location obj))
                  (setq insX (car svIns))
                  (setq insY (cadr svIns))
                  
                  ; Verificar si la sección cae dentro de las coordenadas visibles del Viewport
                  (if (and (>= insX minX) (<= insX maxX) (>= insY minY) (<= insY maxY))
                    (progn
                      ; Intentar extraer la estación de la propiedad de Civil 3D
                      (if (vlax-property-available-p obj 'Station)
                        (progn
                          (setq stVal (vlax-get-property obj 'Station))
                          ; Convertir la estación numérica a formato de texto con formato (ej: 0+120.00)
                          (setq stTxt (rtos stVal 2 2))
                          ; Asegurar formato de ceros y más (si Civil 3D lo devuelve en metros puros, lo adaptamos)
                          (setq stationList (cons stVal stationList))
                        )
                      )
                    )
                  )
                )
              )
            )
            
            ; Si se encontraron estaciones en esta ventana, ordenar y sacar el rango
            (if stationList
              (progn
                (setq stationList (vl-sort stationList '<))
                ; Formatear estación mínima y máxima
                (setq minSt (rtos (car stationList) 2 2))
                (setq maxSt (rtos (last stationList) 2 2))
                
                ; Buscar el texto del cajetín en el Layout actual y actualizarlo
                (setq ssPs (ssget "X" '((410 . "~Model") (0 . "TEXT,MTEXT"))))
                (if ssPs
                  (progn
                    (setq j (sslength ssPs))
                    (while (> j 0)
                      (setq j (1- j))
                      (setq pEnt (ssname ssPs j))
                      (setq pData (entget pEnt))
                      (setq pTxt (cdr (assoc 1 pData)))
                      
                      ; Buscar la etiqueta clave del cajetín
                      (if (or (vl-string-search "DE EST." (strcase pTxt)) (vl-string-search "EST. A EST." (strcase pTxt)))
                        (progn
                          (setq pData (subst (cons 1 (strcat "DE EST. " minSt " A " maxSt)) (assoc 1 pData) pData))
                          (entmod pData)
                          (entupd pEnt)
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
    )
  )
  (princ "\n¡Proceso completado en todos los layouts de Civil 3D!")
  (princ)
)