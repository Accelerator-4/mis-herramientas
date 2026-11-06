;;; ===========================================================================
;;; SOLODIAM.lsp  -  Comando: SOLODIAM
;;; Borra del texto todo lo que no sea el DIAMETRO del tubo
;;; ===========================================================================
;;;
;;; DESCRIPCION
;;;   Para los textos que acompanan a una tuberia y detallan varias cosas a la
;;;   vez, por ejemplo (%%c representa el simbolo de diametro):
;;;         L=87.62m  S=1.50%  TCR- 42"%%c
;;;   El comando deja en cada texto SOLO el diametro (con o sin el prefijo del
;;;   material), borrando la longitud, la pendiente y cualquier otro dato:
;;;         TCR- 42"%%c        (con prefijo)
;;;         42"%%c             (sin prefijo)
;;;
;;; QUE HACE
;;;   1. Busca en cada texto el diametro: la parte que tiene un numero junto a
;;;      comillas (") y/o el simbolo de diametro (ej. 42"%%c  o  %%c12").
;;;      Lo reescribe como NUMERO + el SIMBOLO que se elija al iniciar
;;;      (comillas, simbolo de diametro, ambos o ninguno).
;;;   2. Si se pidio conservar el prefijo, tambien deja las palabras que lo
;;;      acompanan inmediatamente antes (ej. "TCR-", "PVC", "CCP"), siempre
;;;      que no tengan numeros. Lo que viene despues del diametro y no tiene
;;;      numeros tambien se conserva (menos los simbolos sueltos, ej. una "%%c"
;;;      aparte, porque el simbolo ya lo pone el comando).
;;;   3. Todo lo demas se borra: L=..., S=..., Q=..., cifras sueltas, etc.
;;;   4. Si un texto no tiene ningun diametro con comillas, intenta borrar
;;;      solo los datos del tipo "LETRA=valor" (L=87.62m, S=1.50%...) y deja
;;;      el resto. Los textos que ya estan limpios no se cambian.
;;;   5. Funciona con TEXT y MTEXT (en MTEXT se pierde el formato interno:
;;;      fuentes o colores mezclados dentro del mismo texto).
;;;   Los textos NO se mueven ni se rotan: quedan en su mismo punto de
;;;   insercion y con su mismo angulo, solo cambia el contenido.
;;;
;;; COMO USARLO
;;;   1. Cargar el archivo con APPLOAD (o arrastrarlo a la ventana de CAD).
;;;   2. Escribir SOLODIAM.
;;;   3. "Simbolo al final del numero del diametro [Comillas/Diametro/Ambos/
;;;      Ninguno]": define como termina el numero. Ejemplo con 42:
;;;         Comillas -> 42"      Diametro -> 42%%c      Ambos -> 42"%%c
;;;         Ninguno  -> 42
;;;      Enter usa el valor guardado.
;;;   3b. "Conservar el prefijo del material? [Si/No]": Si deja "TCR- 42..."
;;;      y No deja solo el diametro "42...". Enter usa el valor guardado.
;;;   4. Seleccionar TODOS los textos que se quieran limpiar (se pueden
;;;      seleccionar muchos a la vez con una ventana; se ignora todo lo que
;;;      no sea TEXT o MTEXT, como las lineas de la tuberia).
;;;   5. Muestra cuantos textos cambio y algunos ejemplos "antes -> despues".
;;;      Se puede repetir con otra seleccion; Enter sin seleccionar termina.
;;;
;;; VALORES GUARDADOS
;;;   El simbolo elegido y la opcion del prefijo se guardan en el registro de
;;;   Windows (variables SOLODIAM_SIMBOLO y SOLODIAM_PREFIJO) y se reutilizan
;;;   hasta que se cambien. Se conservan al cerrar el CAD.
;;;
;;; NOTAS Y LIMITACIONES
;;;   - El diametro se reconoce por un numero junto a comillas (") o al
;;;     simbolo de diametro (%%c). Si algun texto lo escribe de otra forma (ej.
;;;     42 pulg, 1.07m) no se detecta y solo se aplica la limpieza de datos
;;;     "LETRA=valor".
;;;   - Si el texto esta alineado a la izquierda, al acortarse queda anclado
;;;     en su punto de inicio; si esta centrado, se acorta hacia el centro.
;;;   - El simbolo de diametro se escribe como %%c, que AutoCAD muestra como el
;;;     simbolo de diametro en TEXT y MTEXT.
;;;   - Todo el proceso se puede deshacer con UNDO.
;;; ===========================================================================

(vl-load-com)

;; Reemplaza todas las apariciones de OLD por NEW en STR
(defun sd:repl (str old new / p)
  (while (setq p (vl-string-search old str))
    (setq str (strcat (substr str 1 p) new (substr str (+ p (strlen old) 1)))))
  str)

;; Quita codigos de formato de MTEXT (\fArial;  \H2;  \C1;  { }  etc.)
;; \P (salto de parrafo) se convierte en espacio
(defun sd:plano (str / out i c n)
  (setq out "" i 1 n (strlen str))
  (while (<= i n)
    (setq c (substr str i 1))
    (cond
      ((= c "\\")
       (setq c (if (< i n) (substr str (1+ i) 1) ""))
       (cond
         ((member c '("P" "p")) (setq out (strcat out " ") i (+ i 2)))
         ((member c '("A" "C" "c" "F" "f" "H" "L" "l" "O" "o" "Q" "T" "W" "K" "k" "S"))
          ;; saltar hasta el ";" que cierra el codigo
          (setq i (+ i 2))
          (while (and (<= i n) (/= (substr str i 1) ";")) (setq i (1+ i)))
          (setq i (1+ i)))
         ((= c "\\") (setq out (strcat out "\\") i (+ i 2)))
         (T (setq i (+ i 2)))))
      ((member c '("{" "}")) (setq i (1+ i)))
      (T (setq out (strcat out c) i (1+ i)))))
  out)

;; Divide una cadena en palabras separadas por espacios
(defun sd:split (str / res tok i c)
  (setq res nil tok "" i 1)
  (while (<= i (strlen str))
    (setq c (substr str i 1))
    (if (= c " ")
      (progn
        (if (/= tok "") (setq res (cons tok res)))
        (setq tok ""))
      (setq tok (strcat tok c)))
    (setq i (1+ i)))
  (if (/= tok "") (setq res (cons tok res)))
  (reverse res))

;; La palabra contiene algun digito?
(defun sd:digito (tok / i r)
  (setq i 1 r nil)
  (while (and (not r) (<= i (strlen tok)))
    (if (wcmatch (substr tok i 1) "#") (setq r T))
    (setq i (1+ i)))
  r)

;; Palabra del tipo "L=87.62m", "S=1.50%", "Q=12"
(defun sd:dato (tok)
  (wcmatch tok "?=*,??=*,???=*"))

;; Une una lista de palabras con espacios
(defun sd:unir (lst / out w)
  (setq out "")
  (foreach w lst
    (setq out (if (= out "") w (strcat out " " w))))
  out)

;; Formas en que puede venir escrito el simbolo de diametro dentro de un texto
;; (se comparan en mayusculas): %%c, la letra O con barra (codigo 216 / 248) y
;; los codigos Unicode de MTEXT (\U+00D8, \U+00F8, \U+2205)
(defun sd:sindiam (str / x)
  (foreach x (list "%%C" (chr 216) (chr 248) "\\U+00D8" "\\U+00F8" "\\U+2205")
    (while (vl-string-search x (strcase str))
      (setq str (strcat (substr str 1 (vl-string-search x (strcase str)))
                        (substr str (+ (vl-string-search x (strcase str)) (strlen x) 1))))))
  str)

;; La palabra tiene comillas o simbolo de diametro?
(defun sd:consimbolo (tok)
  (or (vl-string-search (chr 34) tok)
      (/= (strlen (sd:sindiam tok)) (strlen tok))))

;; La palabra es SOLO simbolos (comillas y/o diametro)?
(defun sd:solosimbolo (tok)
  (and (/= tok "")
       (= (sd:repl (sd:sindiam tok) (chr 34) "") "")))

;; Palabra que es un diametro: tiene un numero y comillas/simbolo de diametro
(defun sd:esdiam (tok)
  (and (sd:digito tok) (sd:consimbolo tok)))

;; Primer numero de la palabra (digitos, punto y barra de fraccion): 42"%%c -> 42
(defun sd:numero (tok / i c out ini)
  (setq i 1 out "" ini nil)
  (while (<= i (strlen tok))
    (setq c (substr tok i 1))
    (cond
      ((wcmatch c "#") (setq ini T out (strcat out c)))
      ((and ini (member c '("." "/"))) (setq out (strcat out c)))
      (ini (setq i (strlen tok))))
    (setq i (1+ i)))
  out)

;; Devuelve el texto reducido al diametro (o el mismo texto si no hay nada que quitar).
;; prefijo = "Si"/"No" (conservar el material, ej. TCR-)
;; simbolo = texto que se pone al final del numero ("" = ninguno)
(defun sd:diametro (str prefijo simbolo / toks n iq ilast i j res k)
  (setq toks (sd:split (sd:plano str))
        n    (length toks)
        iq   nil
        ilast nil
        i    0)
  ;; primera palabra que sea un diametro (numero + comillas/simbolo); o un
  ;; numero seguido de una palabra que es solo simbolo (ej. 42  "%%c)
  (while (and (null iq) (< i n))
    (cond
      ((sd:esdiam (nth i toks)) (setq iq i ilast i))
      ((and (> i 0)
            (sd:solosimbolo (nth i toks))
            (sd:digito (nth (1- i) toks))
            (not (vl-string-search "=" (nth (1- i) toks))))
       (setq iq (1- i) ilast i)))
    (setq i (1+ i)))
  (cond
    (iq
     ;; el diametro se reconstruye: numero + simbolo elegido
     (setq res (list (strcat (sd:numero (nth iq toks)) simbolo)))
     ;; palabras de antes (prefijo del material) sin numeros ni "="
     (if (= prefijo "Si")
       (progn
         (setq j (1- iq))
         (while (and (>= j 0)
                     (not (sd:digito (nth j toks)))
                     (not (vl-string-search "=" (nth j toks))))
           (setq res (cons (nth j toks) res) j (1- j)))))
     ;; palabras de despues sin numeros ni "=" ni simbolos sueltos
     (setq k (1+ ilast))
     (while (and (< k n)
                 (not (sd:digito (nth k toks)))
                 (not (vl-string-search "=" (nth k toks))))
       (if (not (sd:solosimbolo (nth k toks)))
         (setq res (append res (list (nth k toks)))))
       (setq k (1+ k)))
     (sd:unir res))
    (T
     ;; sin diametro reconocible: solo borrar datos tipo LETRA=valor
     (sd:unir (vl-remove-if 'sd:dato toks)))))

;; ---------------- COMANDO ----------------
(defun c:SOLODIAM ( / *error* doc ss i e ed obj tipo antes despues pref opt
                      ncamb nsin nej simb simbtxt)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg)))
    (princ))

  (princ "\n--- SOLODIAM: dejar solo el diametro en los textos ---")

  ;; Simbolo al final del numero del diametro (se guarda)
  (setq simb (getenv "SOLODIAM_SIMBOLO"))
  (if (not (member simb '("Comillas" "Diametro" "Ambos" "Ninguno"))) (setq simb "Ambos"))
  (initget "Comillas Diametro Ambos Ninguno")
  (setq opt (getkword (strcat "\nSimbolo al final del numero del diametro [Comillas/Diametro/Ambos/Ninguno] <"
                              simb ">: ")))
  (if opt (setq simb opt))
  (setenv "SOLODIAM_SIMBOLO" simb)
  (setq simbtxt (cond ((= simb "Comillas") (chr 34))
                      ((= simb "Diametro") "%%c")
                      ((= simb "Ambos")    (strcat (chr 34) "%%c"))
                      (T "")))
  (princ (strcat "\nSimbolo guardado: " simb))

  ;; Prefijo del material (se guarda)
  (setq pref (getenv "SOLODIAM_PREFIJO"))
  (if (not (member pref '("Si" "No"))) (setq pref "Si"))
  (initget "Si No")
  (setq opt (getkword (strcat "\nConservar el prefijo del material (ej. TCR-)? [Si/No] <"
                              pref ">: ")))
  (if opt (setq pref opt))
  (setenv "SOLODIAM_PREFIJO" pref)
  (princ (strcat "\nPrefijo del material: " pref))

  (while (setq ss (progn
                    (princ "\nSeleccione los textos (TEXT/MTEXT), Enter para salir: ")
                    (ssget '((0 . "TEXT,MTEXT")))))
    (vla-StartUndoMark doc)
    (setq ncamb 0 nsin 0 nej 0 i 0)
    (repeat (sslength ss)
      (setq e    (ssname ss i)
            ed   (entget e)
            obj  (vlax-ename->vla-object e)
            tipo (cdr (assoc 0 ed))
            antes (vla-get-TextString obj)
            despues (sd:diametro antes pref simbtxt)
            i (1+ i))
      (if (and (/= despues "") (/= despues antes))
        (progn
          (vla-put-TextString obj despues)
          (setq ncamb (1+ ncamb))
          (if (< nej 5)
            (progn
              (princ (strcat "\n  [" antes "]  ->  [" despues "]"))
              (setq nej (1+ nej)))))
        (setq nsin (1+ nsin))))
    (vla-EndUndoMark doc)
    (princ (strcat "\nTextos modificados: " (itoa ncamb)
                   "  |  Sin cambios: " (itoa nsin))))
  (princ))

(princ "\nSOLODIAM cargado. Escriba SOLODIAM para iniciar.")
(princ)
