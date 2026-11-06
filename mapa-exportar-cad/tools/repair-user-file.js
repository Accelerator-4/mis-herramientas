const fs = require('fs');
const { execSync } = require('child_process');
const Dxf12 = require('C:/Users/DTEC14/Desktop/BENJA/projects/mapa-exportar-cad/js/dxf.js');

const out = 'C:/Users/DTEC14/Downloads/area-mapas-corregido.dxf';

const dxf = Dxf12.build({
  unit: 'Meters',
  polylines: [
    {
      layer: 'AREA_1',
      color: Dxf12.ACI.RED,
      points: [
        [575378.77243, 1293080.206633],
        [578138.434715, 1293045.259231],
        [578126.143623, 1290512.385477],
        [575319.456348, 1290556.531556],
      ],
    },
  ],
  texts: [
    { layer: 'AREA_1', x: 576740.701779, y: 1291798.595724, height: 76.263607, text: 'Area 1 - 7.09 km2' },
    { layer: 'AREA_1', x: NaN, y: 5, height: 10, text: 'este texto no debe aparecer' },
  ],
});

if (dxf.includes('NaN') || dxf.includes('Infinity')) throw new Error('NaN/Infinity en el DXF generado');

fs.writeFileSync(out, dxf, 'utf8');
console.log('Escrito: ' + out + ' (' + dxf.length + ' bytes)');

const val = execSync('python "C:/Users/DTEC14/Desktop/BENJA/projects/mapa-exportar-cad/tools/validate_dxf.py" "' + out + '"', { encoding: 'utf8' });
console.log(val);
if (!val.includes('OK:')) throw new Error('Validación ezdxf falló');
console.log('TODO OK: el DXF corregido es válido y abre con ezdxf sin errores');