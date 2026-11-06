# Mapa → AutoCAD (DXF)

Web 100% local (HTML + JS, sin framework ni backend) que selecciona un **área rectangular** sobre un mapa y exporta a **DXF** todos los elementos de OpenStreetMap dentro de ella (carreteras, ríos, vías férreas y opcionalmente huellas de edificios), con coordenadas reales listo para abrir en AutoCAD / LibreCAD / QGIS.

## Cómo usar

1. Abre `index.html` en un navegador (internet se necesita para los tiles y para la consulta a la API de OpenStreetMap).
2. Dibuja el **rectángulo** del área con el botón ▭ de la toolbar del mapa. La app consulta automáticamente OpenStreetMap y muestra la vista previa.
3. Opciones: **ancho de calzada** de carreteras (banda a cada lado de la línea central, por defecto 2 m), **color** de cada capa (paleta ACI), incluir **edificios** (solo huellas), **nombres de lugares**, unidades del dibujo (m, cm, ft) y vista previa.
4. Pulsa **Exportar DXF**. El archivo queda con coordenadas **UTM** (zona y hemisferio calculados del centro del área, datum WGS84).

## Capas del DXF

| Capa | Contenido | Color ACI por defecto |
|---|---|---|
| `CARRETERAS` | banda cerrada alrededor de la línea central (ancho configurable, ±2 m por defecto) | 1 rojo |
| `RIOS` | línea principal de ríos y arroyos (`way["waterway"]`) | 4 cian |
| `FERROCARRILES` | ways con `railway` | 2 amarillo |
| `EDIFICIOS` | ways con `building` (polígonos recortados, opcional) | 3 verde |
| `NOMBRES` | nodos `place` con nombre (ciudades, pueblos, colonias…) como entidades TEXT | 8 gris |
| `AREA` | rectángulo del área + etiqueta `AREA: … - ZONA UTM nX` | blanco |

El color de cada capa se elige en el panel con los cuadros de muestra (9 colores ACI 1–9, los que soporta el DXF R12).

Los polígonos se recortan exactamente al borde del área (Sutherland–Hodgman) y las líneas por intersección paramétrica de segmentos (Liang–Barsky); nada se sale del rectángulo. Las carreteras se convierten en **bandas**: paralelas a ±W metros de la línea central proyectada en UTM, con uniones a inglete (con bisel de respaldo para curvas cerradas) y tapas rectas; las rotondas (anillos cerrados, `Geo.isRing`) se abren para generar la banda.

## Características técnicas

- **Selección de área:** Leaflet + Leaflet.draw (rectángulo, edición y borrado).
- **Datos:** Overpass API (`way["highway"|"waterway"|"railway"]`, `way["building"]`), con límite de 5000 geometrías para carreteras y 7000 para edificios. Si una capa supera los 100 000 vértices (áreas enormes), se recorta la geometría sobrante y se avisa en la barra de estado en lugar de descartarla en silencio.
- **Reproyección:** `proj4` — UTM automática (zona y hemisferio del centro del área), datum WGS84.
- **Cálculo de área:** fórmula esférica exacta `(R²/2)·|Σ Δλ·(sinφ₁+sinφ₂)|` (validada contra la referencia analítica y contra el área plana UTM en `tools/test-dxf.js`).
- **Generación DXF:** writer propio en `js/dxf.js` que emite DXF **R12 (AC1009)** con la misma estructura que genera ezdxf: handles únicos con `$HANDSEED` coherente, tablas VPORT/LTYPE/LAYER/STYLE/APPID, polígonos como `POLYLINE` cerrada (flag 70=1), líneas abiertas como `POLYLINE` flag 70=0, textos con grupo 11/21/31, textos transliterados a ASCII y `$DWGCODEPAGE`. La geometría (área, recortes, banda de carretera) vive en `js/geom.js`, pruebas unitarias incluidas.
- Sin dependencias externas en runtime: todas las librerías están en `vendor/`.

## Cómo obtener DWG

DWG es propietario de Autodesk; no hay librería JS fiable para escribirlo. Opciones:
1. Abrir el DXF en AutoCAD y `GUARDARCOMO` → DWG.
2. **Autodesk ODA File Converter** (gratis, https://www.opendesign.com) para conversión por lotes DXF↔DWG.
3. Conversión server-side con Autodesk Platform Services o Aspose.CAD si se quiere el DWG descargable desde la web.

## Estructura

```
mapa-exportar-cad/
├── index.html
├── css/style.css
├── js/app.js                # interacción: mapa, consulta Overpass, recorte, proyección y export
├── js/geom.js               # geometría pura: área, Liang-Barsky, Sutherland-Hodgman, banda de carretera
├── js/dxf.js                # writer DXF R12 (clásico, compatible)
├── vendor/                  # Leaflet, Leaflet.draw, proj4 (local, offline)
└── tools/
    ├── test-dxf.js               # pruebas: área + writer → node tools/test-dxf.js
    ├── gen_reference.py          # genera DXF de referencia con ezdxf desde JSON
    ├── compare_dxf.py            # compara el DXF generado contra el de ezdxf (idéntico + auditoría)
    └── validate_dxf.py           # valida cualquier DXF con ezdxf
```

## Notas

- El área se divide en **cuadrantes** (lado de 0,2°–0,5° según el tamaño) y cada capa se consulta por separado en peticiones independientes y concurrentes (máx. 4), con respaldo automático entre servidores Overpass (por defecto `maps.mail.ru`, que admite CORS, y como contingencia `overpass-api.de` y `overpass.kumi.systems`), reintento y timeout por petición; la barra de estado muestra el progreso. Esto evita que un área grande agote a los servidores y que una capa se pierda silenciosamente (en Overpass, cuando un `out` con límite se dispara bajo carga, las capas siguientes de la misma petición se descartan).
- Al terminar, los datos de todos los cuadrantes se fusionan y se aplican los límites globales por capa (carreteras 5.000, edificios 7.000, nombres 1.500; presupuesto de vértices 100.000 por capa), recortando con aviso en lugar de descartar.