(function (global) {
  'use strict';

  /* Utilidades geométricas puras compartidas entre la app y las pruebas de node. */

  var ACI_HEX = {
    1: '#e03131', /* rojo */
    2: '#f0a500', /* amarillo */
    3: '#2f9e44', /* verde */
    4: '#0c8599', /* cian */
    5: '#1971c2', /* azul */
    6: '#c2255c', /* magenta */
    7: '#ffffff', /* blanco */
    8: '#808080', /* gris oscuro */
    9: '#c0c0c0', /* gris claro */
  };

  function aciHex(aci) {
    return ACI_HEX[aci] || '#ffffff';
  }

  /* Área esférica exacta de un rectángulo geográfico (m²) */
  function rectAreaM2(r) {
    var R = 6378137;
    var dLng = ((r.east - r.west) * Math.PI) / 180;
    var sinN = Math.sin((r.north * Math.PI) / 180);
    var sinS = Math.sin((r.south * Math.PI) / 180);
    return Math.abs(dLng * (sinN - sinS) * R * R);
  }

  function fmtArea(m2) {
    if (m2 >= 1000000) return (m2 / 1000000).toFixed(2) + ' km²';
    if (m2 >= 10000) return (m2 / 10000).toFixed(2) + ' ha';
    if (m2 >= 1000) return Math.round(m2).toLocaleString('es') + ' m²';
    return m2.toFixed(1) + ' m²';
  }

  function fmtAreaCad(m2, unitKey) {
    if (unitKey === 'Centimeters') return (m2 * 10000).toLocaleString('es') + ' cm²';
    if (unitKey === 'Feet') return (m2 * 10.7639).toLocaleString('es', { maximumFractionDigits: 0 }) + ' ft²';
    return fmtArea(m2);
  }

  function utmZoneOf(c) {
    return {
      zone: Math.floor((c.lng + 180) / 6) + 1,
      hemi: c.lat >= 0 ? 'N' : 'S',
    };
  }

  function rectCenter(r) {
    return {
      lat: (r.north + r.south) / 2,
      lng: (r.east + r.west) / 2,
    };
  }

  /* ¿El contorno es un anillo cerrado (p. ej. rotonda)? */
  function isRing(pts) {
    if (!pts || pts.length < 4) return false;
    var a = pts[0];
    var b = pts[pts.length - 1];
    return Math.abs(a[0] - b[0]) < 1e-9 && Math.abs(a[1] - b[1]) < 1e-9;
  }

  /* Liang-Barsky para segmento vs caja alineada a ejes (grados).
     Devuelve [p0vis, p1vis] o null si no hay parte visible. */
  function clipSegment(p0, p1, r) {
    var t0 = 0;
    var t1 = 1;
    var dx = p1[0] - p0[0];
    var dy = p1[1] - p0[1];
    var p = [-dx, dx, -dy, dy];
    var q = [p0[0] - r.west, r.east - p0[0], p0[1] - r.south, r.north - p0[1]];
    for (var i = 0; i < 4; i++) {
      if (p[i] === 0) {
        if (q[i] < 0) return null;
      } else {
        var t = q[i] / p[i];
        if (p[i] < 0) {
          if (t > t1) return null;
          if (t > t0) t0 = t;
        } else {
          if (t < t0) return null;
          if (t < t1) t1 = t;
        }
      }
    }
    return [
      [p0[0] + t0 * dx, p0[1] + t0 * dy],
      [p0[0] + t1 * dx, p0[1] + t1 * dy],
    ];
  }

  /* Recorta una polilínea a la caja: devuelve una lista de tramos visibles */
  function clipLine(pts, r) {
    var pieces = [];
    var cur = [];
    for (var i = 0; i < pts.length - 1; i++) {
      var seg = clipSegment(pts[i], pts[i + 1], r);
      if (!seg) {
        if (cur.length >= 2) pieces.push(cur);
        cur = [];
        continue;
      }
      if (cur.length === 0) cur.push(seg[0]);
      cur.push(seg[1]);
    }
    if (cur.length >= 2) pieces.push(cur);
    return pieces;
  }

  /* Sutherland-Hodgman: recorta un polígono a la caja (grados). */
  function clipPolygon(ring, r) {
    var out = ring;
    var edges = [
      ['west', 'east'],
      ['south', 'north'],
      ['east', 'west'],
      ['north', 'south'],
    ];
    for (var e = 0; e < edges.length; e++) {
      var inKey = edges[e][0];
      var keepIn = function (p) { return (inKey === 'west') ? p[0] >= r.west
        : (inKey === 'east') ? p[0] <= r.east
        : (inKey === 'south') ? p[1] >= r.south : p[1] <= r.north; };
      var next = [];
      for (var i = 0; i < out.length; i++) {
        var a = out[i];
        var b = out[(i + 1) % out.length];
        var aIn = keepIn(a);
        var bIn = keepIn(b);
        if (aIn && bIn) {
          next.push(b);
        } else if (aIn && !bIn) {
          var ix = (inKey === 'west') ? r.west : (inKey === 'east') ? r.east
            : (inKey === 'south') ? r.south : r.north;
          var t = (inKey === 'south' || inKey === 'north')
            ? (ix - a[1]) / (b[1] - a[1] || (ix - a[1] === 0 ? 1 : 0))
            : (ix - a[0]) / (b[0] - a[0] || (ix - a[0] === 0 ? 1 : 0));
          next.push([a[0] + t * (b[0] - a[0]), a[1] + t * (b[1] - a[1])]);
        } else if (!aIn && bIn) {
          var ix2 = (inKey === 'west') ? r.west : (inKey === 'east') ? r.east
            : (inKey === 'south') ? r.south : r.north;
          var t2 = (inKey === 'south' || inKey === 'north')
            ? (ix2 - a[1]) / (b[1] - a[1] || (ix2 - a[1] === 0 ? 1 : 0))
            : (ix2 - a[0]) / (b[0] - a[0] || (ix2 - a[0] === 0 ? 1 : 0));
          next.push([a[0] + t2 * (b[0] - a[0]), a[1] + t2 * (b[1] - a[1])]);
          next.push(b);
        }
      }
      out = next;
    }
    if (out.length >= 3) out.push(out[0]); /* cierra el anillo */
    return out;
  }

  /* Banda de carretera: paralelas a ±w (m, en XY proyectado) de la línea
     central, con uniones a inglete (con bisel de respaldo) y tapas rectas
     (butt). Devuelve el polígono cerrado del borde de la banda (listo para
     POLYLINE cerrada en DXF) o null si no hay línea válida. */
  function offsetBand(pts, w) {
    w = Number(w);
    if (!isFinite(w) || w <= 0) return null;
    var clean = [];
    pts.forEach(function (p) {
      var last = clean[clean.length - 1];
      if (!last || Math.hypot(p[0] - last[0], p[1] - last[1]) > 1e-6) clean.push([p[0], p[1]]);
    });
    if (clean.length < 2) return null;

    var segN = []; /* normales a la izquierda de cada segmento */
    for (var i = 0; i < clean.length - 1; i++) {
      var dx = clean[i + 1][0] - clean[i][0];
      var dy = clean[i + 1][1] - clean[i][1];
      var len = Math.hypot(dx, dy);
      if (len < 1e-9) { segN.push([0, 0]); continue; }
      segN.push([-dy / len, dx / len]);
    }

    var left = [];
    var right = [];
    var MITER_LIMIT = 3.0;

    for (var k = 0; k < clean.length; k++) {
      var x = clean[k][0];
      var y = clean[k][1];
      function pushBoth(nx, ny) {
        left.push([x + nx * w, y + ny * w]);
        right.push([x - nx * w, y - ny * w]);
      }
      if (k === 0) { pushBoth(segN[0][0], segN[0][1]); continue; }
      if (k === clean.length - 1) { var ln = segN[k - 1]; pushBoth(ln[0], ln[1]); continue; }

      var n1 = segN[k - 1];
      var n2 = segN[k];
      var mx = n1[0] + n2[0];
      var my = n1[1] + n2[1];
      var mlen = Math.hypot(mx, my);
      if (mlen > 1e-9) {
        var f = 1 / ((n1[0] * mx + n1[1] * my) / mlen);
        if (Math.abs(f) <= MITER_LIMIT) {
          var ox = (mx / mlen) * w * f;
          var oy = (my / mlen) * w * f;
          left.push([x + ox, y + oy]);
          right.push([x - ox, y - oy]);
          continue;
        }
      }
      /* bisel: dos vértices por lado */
      pushBoth(n1[0], n1[1]);
      pushBoth(n2[0], n2[1]);
    }

    var band = left.slice();
    for (var j = right.length - 1; j >= 0; j--) band.push(right[j]);
    return band;
  }

  var Geo = {
    ACI_HEX: ACI_HEX,
    aciHex: aciHex,
    rectAreaM2: rectAreaM2,
    fmtArea: fmtArea,
    fmtAreaCad: fmtAreaCad,
    utmZoneOf: utmZoneOf,
    rectCenter: rectCenter,
    isRing: isRing,
    clipSegment: clipSegment,
    clipLine: clipLine,
    clipPolygon: clipPolygon,
    offsetBand: offsetBand,
  };

  if (typeof module !== 'undefined' && module.exports) {
    module.exports = Geo;
  } else {
    global.Geo = Geo;
  }
})(typeof window !== 'undefined' ? window : typeof globalThis !== 'undefined' ? globalThis : this);