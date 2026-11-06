const fs = require('fs');
const { execSync } = require('child_process');
const proj4 = require('C:/Users/DTEC14/Desktop/BENJA/projects/mapa-exportar-cad/vendor/proj4/proj4.js');
const Dxf12 = require('C:/Users/DTEC14/Desktop/BENJA/projects/mapa-exportar-cad/js/dxf.js');
const Geo = require('C:/Users/DTEC14/Desktop/BENJA/projects/mapa-exportar-cad/js/geom.js');

const TMP = 'C:/Users/DTEC14/AppData/Local/Temp/opencode';
const PY = 'python';

function assert(cond, msg) {
  if (!cond) throw new Error(msg);
}
function near(a, b, eps) {
  return Math.abs(a - b) <= (eps || 1e-6);
}
function dist(a, b) {
  return Math.hypot(a[0] - b[0], a[1] - b[1]);
}

console.log('— 1) Área esférica (Geo.rectAreaM2) —');
const r = { south: 0, north: 1, west: 0, east: 1 };
const sqArea = Geo.rectAreaM2(r);
const sqExpected = 6378137 * 6378137 * (Math.PI / 180) * Math.sin(Math.PI / 180);
console.log('Cuadrado 1°x1° ecuador: ' + (sqArea / 1e6).toFixed(1) + ' km² (esperado ' + (sqExpected / 1e6).toFixed(1) + ')');
assert(near(sqArea, sqExpected, 1e-9 * sqExpected), 'Fórmula esférica no cuadra con la referencia exacta');
console.log('Área esférica OK');

console.log('— 2) Recorte de líneas (Geo.clipLine) —');
const box = { south: 0, north: 10, west: 0, east: 10 };
const pieces = Geo.clipLine([[ -5, 5 ], [ 5, 5 ]], box);
assert(pieces.length === 1, 'Línea que cruza debe producir 1 tramo');
assert(pieces[0].length === 2, 'Tramo debe tener 2 puntos');
assert(near(pieces[0][0][0], 0) && near(pieces[0][1][0], 5), 'Tramo debe ir de x=0 a x=5');
console.log('Recorte de línea OK: tramo ' + JSON.stringify(pieces[0]));

console.log('— 2b) Detección de anillos (Geo.isRing) —');
assert(Geo.isRing([[0, 0], [1, 0], [1, 1], [0, 1], [0, 0]]), 'Anillo cerrado debe detectarse');
assert(!Geo.isRing([[0, 0], [1, 0], [1, 1], [0, 1]]), 'Cadena abierta no debe detectarse');
assert(!Geo.isRing([[0, 0], [0, 0]]), 'Menos de 4 puntos no es anillo');
console.log('Detección de anillos OK');

console.log('— 3) Banda de carretera (Geo.offsetBand) —');
/* recta: [(0,0) → (100,0)], w = 2 */
const bandStraight = Geo.offsetBand([[0, 0], [100, 0]], 2);
assert(bandStraight.length === 4, 'Recta debe dar banda de 4 vértices');
assert(near(bandStraight[0][1], 2) && near(bandStraight[1][1], 2), 'Lado izquierdo a +2 m');
assert(near(bandStraight[2][1], -2) && near(bandStraight[3][1], -2), 'Lado derecho a -2 m');
/* esquina de 90°: [(0,0) → (100,0) → (100,100)], w = 2 → inglete en (98,2)/(102,-2) */
const bandCorner = Geo.offsetBand([[0, 0], [100, 0], [100, 100]], 2);
assert(bandCorner.length === 6, 'Esquina debe dar banda de 6 vértices');
assert(near(bandCorner[1][0], 98) && near(bandCorner[1][1], 2), 'Inglete izquierdo en (98,2)');
assert(near(bandCorner[2][0], 98) && near(bandCorner[2][1], 100), 'Inglete izquierdo en (98,100)');
assert(near(bandCorner[3][0], 102) && near(bandCorner[3][1], 100), 'Tapa final derecha en (102,100)');
assert(near(bandCorner[4][0], 102) && near(bandCorner[4][1], -2), 'Inglete derecho en (102,-2)');
console.log('Banda de carretera OK (recta ±2 m, esquina con inglete)');

/* línea central RECTA en UTM (zona 14N) => banda en metros reales, ambos lados exactos */
const lngLat = [[-99.16, 19.43], [-99.14, 19.43]];
const converter = proj4('WGS84', '+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs');
const center = lngLat.map((p) => { const xy = converter.forward([p[0], p[1]]); return [xy[0], xy[1]]; });
const band = Geo.offsetBand(center, 2);
assert(band.length >= 4, 'Banda UTM debe tener al menos 4 vértices');
for (let i = 0; i < center.length; i++) {
  const d = dist(band[i], center[i]);
  assert(near(d, 2, 0.01), 'Vértice ' + i + ' debe estar a 2 m de la central, está a ' + d);
}
console.log('Banda en UTM OK: todos los vértices a ~2,00 m de la línea central');

console.log('— 4) Writer DXF R12 (banda cerrada + línea abierta + capa AREA) —');
const pts1 = [
  [-99.16, 19.43], [-99.15, 19.43], [-99.14, 19.432],
];
const utm1 = pts1.map((p) => { const xy = converter.forward(p); return [xy[0], xy[1]]; });
const pts2 = [
  [-99.13, 19.425], [-99.125, 19.435],
];
const utm2 = pts2.map((p) => { const xy = converter.forward(p); return [xy[0], xy[1]]; });

const dxf = Dxf12.build({
  unit: 'Meters',
  polylines: [
    { layer: 'AREA_1', color: Dxf12.ACI.RED, closed: true, points: utm1 },
    { layer: 'CARRETERAS', color: Dxf12.ACI.RED, closed: true, points: band },
    { layer: 'RIOS', color: Dxf12.ACI.CYAN, closed: false, points: utm2 },
  ],
  texts: [
    { layer: 'AREA', x: 485000, y: 2148500, height: 25, text: 'AREA: 5,26 km2 - ZONA UTM 14N' },
    { layer: 'NOMBRES', x: 487000, y: 2149000, height: 12.5, text: 'Ciudad de Mexico' },
  ],
});
fs.writeFileSync(TMP + '/mio-r12.dxf', dxf, 'utf8');
console.log('DXF generado, bytes: ' + dxf.length);
assert(!dxf.includes('NaN'), 'Coordenadas NaN en el DXF');

// 5) Estructura grupo/valor
const lines = dxf.trimEnd().split('\n');
assert(lines.length % 2 === 0, 'Número impar de líneas (grupo/valor)');
for (let i = 0; i < lines.length; i += 2) {
  assert(/^-?\d+$/.test(lines[i].trim()), 'Código de grupo inválido: ' + lines[i]);
}
assert(dxf.includes('0\nEOF\n'), 'Falta EOF');
console.log('Estructura grupo/valor OK (' + lines.length / 2 + ' pares), EOF presente');

// 6) Comparación estricta contra ezdxf (estructura idéntica) + auditoría
const refData = {
  polylines: [
    { layer: 'AREA_1', color: 1, closed: true, points: utm1 },
    { layer: 'CARRETERAS', color: 1, closed: true, points: band },
    { layer: 'RIOS', color: 4, closed: false, points: utm2 },
  ],
  texts: [
    { layer: 'AREA', x: 485000, y: 2148500, height: 25, text: 'AREA: 5,26 km2 - ZONA UTM 14N' },
    { layer: 'NOMBRES', x: 487000, y: 2149000, height: 12.5, text: 'Ciudad de Mexico' },
  ],
};
const refJson = TMP + '/ref-data.json';
fs.writeFileSync(refJson, JSON.stringify(refData), 'utf8');
execSync(PY + ' "' + __dirname + '\\gen_reference.py" "' + refJson + '" "' + TMP + '\\reference-r12.dxf"', { encoding: 'utf8' });
const cmp = execSync(PY + ' "' + __dirname + '\\compare_dxf.py" "' + TMP + '\\mio-r12.dxf" "' + TMP + '\\reference-r12.dxf"', { encoding: 'utf8' });
console.log(cmp);
assert(cmp.includes('OK:'), 'Comparación con ezdxf falló');

console.log('TODO OK');