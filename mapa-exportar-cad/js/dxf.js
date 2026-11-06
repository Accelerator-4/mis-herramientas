(function (global) {
  'use strict';

  /* Writer de DXF R12 (AC1009) — estructura espejo del que genera ezdxf,
     el formato que abren AutoCAD, LibreCAD, DraftSight, etc. */

  var ACI = { RED: 1, YELLOW: 2, GREEN: 3, CYAN: 4, BLUE: 5, MAGENTA: 6, WHITE: 7 };

  var UNITS = {
    Unitless: 0,
    Inches: 1,
    Feet: 2,
    Miles: 3,
    Millimeters: 4,
    Centimeters: 5,
    Meters: 6,
    Kilometers: 7,
  };

  function ascii(s) {
    return String(s)
      .normalize('NFD')
      .replace(/[\u0300-\u036f]/g, '')
      .replace(/\u00B2/g, '2')
      .replace(/\u00B3/g, '3')
      .replace(/[\u2014\u2013]/g, '-')
      .replace(/[^\x20-\x7E]/g, '?');
  }

  /* Real con punto decimal obligatorio (formato AutoCAD R12) */
  function num(v) {
    var n = Number(v);
    if (!isFinite(n)) return '0.0';
    var s = n.toFixed(6).replace(/(\.\d*?)0+$/, '$1').replace(/\.$/, '.0');
    return s;
  }

  function tag(code, value) {
    return code + '\n' + value + '\n';
  }

  function build(opts) {
    opts = opts || {};
    var unit = UNITS[opts.unit];
    if (typeof unit === 'undefined') unit = UNITS.Unitless;

    var polylines = (opts.polylines || [])
      .map(function (p) {
        return {
          layer: p.layer,
          color: p.color,
          closed: !!p.closed,
          points: (p.points || []).filter(function (pt) {
            return isFinite(Number(pt[0])) && isFinite(Number(pt[1]));
          }),
        };
      })
      .filter(function (p) { return p.points.length >= 2; });
    var texts = (opts.texts || []).filter(function (t) {
      return isFinite(Number(t.x)) && isFinite(Number(t.y)) && isFinite(Number(t.height));
    });

    /* Tabla de capas: todas las capas usadas (polilíneas y textos) + 0 y Defpoints */
    var layerColor = {};
    polylines.forEach(function (p) {
      if (typeof layerColor[p.layer] === 'undefined') layerColor[p.layer] = p.color || ACI.WHITE;
    });
    texts.forEach(function (t) {
      if (typeof layerColor[t.layer] === 'undefined') layerColor[t.layer] = ACI.WHITE;
    });
    layerColor['0'] = ACI.WHITE;
    layerColor['Defpoints'] = ACI.WHITE;
    var layerNames = Object.keys(layerColor);

    var vertexCount = 0;
    polylines.forEach(function (p) { vertexCount += p.points.length; });
    var totalHandles = 1 /* VPORT */ + 3 /* LTYPE */ + layerNames.length /* LAYER */
      + 1 /* STYLE */ + 1 /* APPID */ + 4 /* 2× BLOCK+ENDBLK */
      + polylines.length * 2 + vertexCount /* POLYLINE+SEQEND+VERTEXs */
      + texts.length;
    var h = 0x14; /* siguiente handle */
    var handseedHex = (h + totalHandles).toString(16).toUpperCase();
    function handle() {
      return h.toString(16).toUpperCase();
    }
    function nextHandle() {
      var v = handle();
      h++;
      return v;
    }

    var s = '';

    /* ================= HEADER ================= */
    s += '0\nSECTION\n2\nHEADER\n';
    s += tag(9, '$ACADVER') + tag(1, 'AC1009');
    s += tag(9, '$DWGCODEPAGE') + tag(3, 'ANSI_1252');
    s += tag(9, '$INSBASE') + tag(10, '0.0') + tag(20, '0.0') + tag(30, '0.0');
    s += tag(9, '$EXTMIN') + tag(10, '1e+20') + tag(20, '1e+20') + tag(30, '1e+20');
    s += tag(9, '$EXTMAX') + tag(10, '-1e+20') + tag(20, '-1e+20') + tag(30, '-1e+20');
    s += tag(9, '$LIMMIN') + tag(10, '0.0') + tag(20, '0.0');
    s += tag(9, '$LIMMAX') + tag(10, '420.0') + tag(20, '297.0');
    s += tag(9, '$CLAYER') + tag(8, '0');
    s += tag(9, '$CELTYPE') + tag(6, 'ByLayer');
    s += tag(9, '$CECOLOR') + tag(62, 256);
    s += tag(9, '$TILEMODE') + tag(70, 1);
    s += tag(9, '$INSUNITS') + tag(70, unit);
    s += tag(9, '$HANDLING') + tag(70, 1);
    s += tag(9, '$HANDSEED') + tag(5, handseedHex);
    s += '0\nENDSEC\n';

    /* ================= TABLES ================= */
    s += '0\nSECTION\n2\nTABLES\n';

    /* VPORT */
    s += '0\nTABLE\n2\nVPORT\n' + tag(70, 1);
    s += '0\nVPORT\n' + tag(5, nextHandle()) + tag(2, '*Active') + tag(70, 0);
    s += tag(10, '0.0') + tag(20, '0.0') + tag(11, '1.0') + tag(21, '1.0') +
         tag(12, '0.0') + tag(22, '0.0') + tag(13, '0.0') + tag(23, '0.0') +
         tag(14, '0.5') + tag(24, '0.5') + tag(15, '0.5') + tag(25, '0.5') +
         tag(16, '0.0') + tag(26, '0.0') + tag(36, '1.0') +
         tag(17, '0.0') + tag(27, '0.0') + tag(37, '0.0') +
         tag(40, '1000.0') + tag(41, '1.34') + tag(42, '50.0') +
         tag(43, '0.0') + tag(44, '0.0') + tag(50, '0.0') + tag(51, '0.0') +
         tag(71, 0) + tag(72, 1000) + tag(73, 1) + tag(74, 3) +
         tag(75, 0) + tag(76, 0) + tag(77, 0) + tag(78, 0);
    s += '0\nENDTAB\n';

    /* LTYPE */
    s += '0\nTABLE\n2\nLTYPE\n' + tag(70, 3);
    [['ByBlock'], ['ByLayer'], ['Continuous']].forEach(function (ltype) {
      s += '0\nLTYPE\n' + tag(5, nextHandle()) + tag(2, ltype[0]) + tag(70, 0) +
           tag(3, '') + tag(72, 65) + tag(73, 0) + tag(40, '0.0');
    });
    s += '0\nENDTAB\n';

    /* LAYER */
    s += '0\nTABLE\n2\nLAYER\n' + tag(70, layerNames.length);
    layerNames.forEach(function (name) {
      s += '0\nLAYER\n' + tag(5, nextHandle()) + tag(2, ascii(name)) + tag(70, 0) + tag(62, layerColor[name] || ACI.WHITE) + tag(6, 'Continuous');
    });
    s += '0\nENDTAB\n';

    /* STYLE */
    s += '0\nTABLE\n2\nSTYLE\n' + tag(70, 1);
    s += '0\nSTYLE\n' + tag(5, nextHandle()) + tag(2, 'Standard') + tag(70, 0) +
         tag(40, '0.0') + tag(41, '1.0') + tag(50, '0.0') + tag(71, 0) +
         tag(42, '2.5') + tag(3, 'txt') + tag(4, '');
    s += '0\nENDTAB\n';

    /* VIEW, UCS vacías */
    s += '0\nTABLE\n2\nVIEW\n' + tag(70, 0) + '0\nENDTAB\n';
    s += '0\nTABLE\n2\nUCS\n' + tag(70, 0) + '0\nENDTAB\n';

    /* APPID */
    s += '0\nTABLE\n2\nAPPID\n' + tag(70, 1);
    s += '0\nAPPID\n' + tag(5, nextHandle()) + tag(2, 'ACAD') + tag(70, 0);
    s += '0\nENDTAB\n';

    s += '0\nENDSEC\n';

    /* ================= BLOCKS ================= */
    s += '0\nSECTION\n2\nBLOCKS\n';
    s += '0\nBLOCK\n' + tag(5, nextHandle()) + tag(8, '0') + tag(2, '$Model_Space') + tag(70, 0) +
         tag(10, '0.0') + tag(20, '0.0') + tag(30, '0.0') + tag(3, '$Model_Space') + tag(1, '');
    s += '0\nENDBLK\n' + tag(5, nextHandle()) + tag(8, '0');
    s += '0\nBLOCK\n' + tag(5, nextHandle()) + tag(8, '0') + tag(2, '$Paper_Space') + tag(70, 0) +
         tag(10, '0.0') + tag(20, '0.0') + tag(30, '0.0') + tag(3, '$Paper_Space') + tag(1, '');
    s += '0\nENDBLK\n' + tag(5, nextHandle()) + tag(8, '0');
    s += '0\nENDSEC\n';

    /* ================= ENTITIES ================= */
    s += '0\nSECTION\n2\nENTITIES\n';

    polylines.forEach(function (p) {
      var layer = ascii(p.layer);
      s += '0\nPOLYLINE\n' + tag(5, nextHandle()) + tag(8, layer) + tag(66, 1) +
           tag(10, '0.0') + tag(20, '0.0') + tag(30, '0.0') + tag(70, p.closed ? 1 : 0);
      p.points.forEach(function (pt) {
        s += '0\nVERTEX\n' + tag(5, nextHandle()) + tag(8, layer) +
             tag(10, num(pt[0])) + tag(20, num(pt[1])) + tag(30, '0.0') + tag(70, 0);
      });
      s += '0\nSEQEND\n' + tag(5, nextHandle()) + tag(8, layer);
    });

    texts.forEach(function (t) {
      var layer = ascii(t.layer);
      s += '0\nTEXT\n' + tag(5, nextHandle()) + tag(8, layer);
      s += tag(10, num(t.x)) + tag(20, num(t.y)) + tag(30, '0.0');
      s += tag(40, num(t.height));
      s += tag(1, ascii(t.text));
      s += tag(11, num(t.x)) + tag(21, num(t.y)) + tag(31, '0.0');
    });

    s += '0\nENDSEC\n';
    s += '0\nEOF\n';
    return s;
  }

  var Dxf12 = { build: build, ACI: ACI, UNITS: UNITS };

  if (typeof module !== 'undefined' && module.exports) {
    module.exports = Dxf12;
  } else {
    global.Dxf12 = Dxf12;
  }
})(typeof window !== 'undefined' ? window : typeof globalThis !== 'undefined' ? globalThis : this);