/* ============================================================
   Calles detalladas — geometría derivada + consulta OSM + DXF
   ============================================================ */
var Streets = (function () {
  'use strict';

  var M_LANES = 3.5;
  var D_MEDIAN = 1.5;
  var D_SIDEWALK = 1.5;
  var D_CYCLEWAY = 1.5;

  /* ---------- helpers de tags ---------- */

  function roadWidth(tags) {
    if (tags.width) { var w = parseFloat(tags.width); if (w > 0 && w < 100) return w; }
    if (tags.lanes) { var l = parseInt(tags.lanes, 10); if (l > 0 && l < 20) return l * M_LANES; }
    return 7;
  }

  function turnInfo(tags) {
    return tags['turn:lanes'] || tags['turn:lanes:forward'] || '';
  }

  /* Mediana: ancho explícito → estimación de lanes → 0 si no hay */
  function medianWidth(tags) {
    if (tags['median:width']) { var w = parseFloat(tags['median:width']); if (w > 0) return w; }
    if (tags.median === 'yes' || tags.median === 'plant' || tags.median === 'grass' ||
        tags.median === 'curb' || tags.median === 'island') {
      /* estimar de lanes si hay datos: ancho ≈ 1m por carril a cada lado */
      if (tags.lanes) { var l = parseInt(tags.lanes, 10); if (l >= 4) return Math.min(l * 0.5, 6); }
      return D_MEDIAN;
    }
    return 0;
  }

  /* Andenes: detección independiente izquierda/derecha */
  function sidewalkLeft(tags) {
    if (tags['sidewalk:left'] === 'no' || tags['sidewalk:left'] === 'none') return 0;
    if (tags['sidewalk:left:width']) { var w = parseFloat(tags['sidewalk:left:width']); if (w > 0) return w; }
    if (tags['sidewalk:width']) { var w2 = parseFloat(tags['sidewalk:width']); if (w2 > 0) return w2; }
    if (tags.sidewalk === 'both' || tags.sidewalk === 'left') return D_SIDEWALK;
    return 0;
  }

  function sidewalkRight(tags) {
    if (tags['sidewalk:right'] === 'no' || tags['sidewalk:right'] === 'none') return 0;
    if (tags['sidewalk:right:width']) { var w = parseFloat(tags['sidewalk:right:width']); if (w > 0) return w; }
    if (tags['sidewalk:width']) { var w2 = parseFloat(tags['sidewalk:width']); if (w2 > 0) return w2; }
    if (tags.sidewalk === 'both' || tags.sidewalk === 'right') return D_SIDEWALK;
    return 0;
  }

  /* Ciclovía: detección independiente izquierda/derecha */
  function cyclewayLeft(tags) {
    if (tags['cycleway:left'] === 'no' || tags['cycleway:left'] === 'none') return 0;
    if (tags['cycleway:left:width']) { var w = parseFloat(tags['cycleway:left:width']); if (w > 0) return w; }
    if (tags['cycleway:width']) { var w2 = parseFloat(tags['cycleway:width']); if (w2 > 0) return w2; }
    if (tags.cycleway === 'lane' || tags.cycleway === 'track' || tags.cycleway === 'shared_lane') return D_CYCLEWAY;
    if (tags['cycleway:left'] && tags['cycleway:left'] !== 'no') return D_CYCLEWAY;
    return 0;
  }

  function cyclewayRight(tags) {
    if (tags['cycleway:right'] === 'no' || tags['cycleway:right'] === 'none') return 0;
    if (tags['cycleway:right:width']) { var w = parseFloat(tags['cycleway:right:width']); if (w > 0) return w; }
    if (tags['cycleway:width']) { var w2 = parseFloat(tags['cycleway:width']); if (w2 > 0) return w2; }
    if (tags.cycleway === 'lane' || tags.cycleway === 'track' || tags.cycleway === 'shared_lane') return D_CYCLEWAY;
    if (tags['cycleway:right'] && tags['cycleway:right'] !== 'no') return D_CYCLEWAY;
    return 0;
  }

  function sidewalkWidth(tags) {
    return Math.max(sidewalkLeft(tags), sidewalkRight(tags));
  }

  function cyclewayWidth(tags) {
    return Math.max(cyclewayLeft(tags), cyclewayRight(tags));
  }

  /* ---------- geometría ---------- */

  /* Vector perpendicular unitario de p1→p2 (lado izquierdo) */
  function perp(p1, p2) {
    var dx = p2[0] - p1[0], dy = p2[1] - p1[1];
    var len = Math.sqrt(dx * dx + dy * dy);
    if (len < 1e-12) return [0, 0];
    return [-dy / len, dx / len];
  }

  /* Punto de intersección de dos líneas (p1+d1, p2+d2) */
  function lineIntersect(p1, d1, p2, d2) {
    var det = d1[0] * d2[1] - d1[1] * d2[0];
    if (Math.abs(det) < 1e-12) return null;
    var dx = p2[0] - p1[0], dy = p2[1] - p1[1];
    var t = (dx * d2[1] - dy * d2[0]) / det;
    return [p1[0] + t * d1[0], p1[1] + t * d1[1]];
  }

  /* Offset paralelo limpio con miter joints.
     Para cada esquina calcula el punto de intersección de los offsets
     de los dos segmentos adyacentes. Si el miter es demasiado largo
     (> 3× la distancia de offset) usa bisel (bevel). */
  function offsetPolyline(pts, dist) {
    if (!pts || pts.length < 2) return pts ? pts.slice() : [];
    if (pts.length === 2) {
      var n = perp(pts[0], pts[1]);
      return [[pts[0][0] + n[0] * dist, pts[0][1] + n[1] * dist],
              [pts[1][0] + n[0] * dist, pts[1][1] + n[1] * dist]];
    }

    var result = [];
    var maxMiter = Math.abs(dist) * 3;

    for (var i = 0; i < pts.length; i++) {
      if (i === 0) {
        /* primer punto: offset del primer segmento */
        var n0 = perp(pts[0], pts[1]);
        result.push([pts[0][0] + n0[0] * dist, pts[0][1] + n0[1] * dist]);
      } else if (i === pts.length - 1) {
        /* último punto: offset del último segmento */
        var nLast = perp(pts[pts.length - 2], pts[pts.length - 1]);
        result.push([pts[i][0] + nLast[0] * dist, pts[i][1] + nLast[1] * dist]);
      } else {
        /* esquina: intersección de offsets de segmentos adyacentes */
        var nPrev = perp(pts[i - 1], pts[i]);
        var nNext = perp(pts[i], pts[i + 1]);
        var d1 = [nPrev[0] * dist, nPrev[1] * dist];
        var d2 = [nNext[0] * dist, nNext[1] * dist];
        var miter = lineIntersect(
          [pts[i - 1][0] + d1[0], pts[i - 1][1] + d1[1]], [pts[i][0] - pts[i - 1][0], pts[i][1] - pts[i - 1][1]],
          [pts[i][0] + d2[0], pts[i][1] + d2[1]], [pts[i + 1][0] - pts[i][0], pts[i + 1][1] - pts[i][1]]);

        if (miter) {
          var mx = miter[0] - pts[i][0], my = miter[1] - pts[i][1];
          var miterLen = Math.sqrt(mx * mx + my * my);
          if (miterLen <= maxMiter) {
            result.push(miter);
          } else {
            /* bisel: dos puntos en lugar de uno */
            var bevelDist = Math.min(maxMiter, miterLen);
            result.push([pts[i][0] + nPrev[0] * dist, pts[i][1] + nPrev[1] * dist]);
            result.push([pts[i][0] + nNext[0] * dist, pts[i][1] + nNext[1] * dist]);
          }
        } else {
          /* fallback: offset promedio */
          var nx = (nPrev[0] + nNext[0]) / 2, ny = (nPrev[1] + nNext[1]) / 2;
          var nLen = Math.sqrt(nx * nx + ny * ny);
          if (nLen > 1e-12) { nx /= nLen; ny /= nLen; }
          result.push([pts[i][0] + nx * dist, pts[i][1] + ny * dist]);
        }
      }
    }
    return result;
  }

  /* Simplificación Douglas-Peucker: elimina vértices redundantes
     manteniendo la forma general. Tolerance en unidades del coordinate system. */
  function simplifyRing(ring, tol) {
    if (!ring || ring.length <= 3) return ring;
    var keep = [0];
    var stack = [[0, ring.length - 1]];
    while (stack.length) {
      var pair = stack.pop();
      var start = pair[0], end = pair[1];
      var maxD = 0, maxI = start;
      var ax = ring[start][0], ay = ring[start][1];
      var bx = ring[end][0], by = ring[end][1];
      var dx = bx - ax, dy = by - ay;
      var lenSq = dx * dx + dy * dy;
      for (var i = start + 1; i < end; i++) {
        var px = ring[i][0], py = ring[i][1];
        var t = lenSq > 1e-12 ? Math.max(0, Math.min(1, ((px - ax) * dx + (py - ay) * dy) / lenSq)) : 0;
        var projX = ax + t * dx, projY = ay + t * dy;
        var d = Math.sqrt((px - projX) * (px - projX) + (py - projY) * (py - projY));
        if (d > maxD) { maxD = d; maxI = i; }
      }
      if (maxD > tol) {
        stack.push([start, maxI]);
        stack.push([maxI, end]);
        keep.push(maxI);
      }
    }
    keep.sort(function (a, b) { return a - b; });
    return keep.map(function (i) { return ring[i]; });
  }

  /* Cerrar anillo: si el primer y último punto no son iguales, repetir el primero */
  function closeRing(ring) {
    if (!ring || ring.length < 3) return ring;
    var first = ring[0], last = ring[ring.length - 1];
    if (first[0] !== last[0] || first[1] !== last[1]) ring.push(first.slice());
    return ring;
  }

  function makeRing(left, right) {
    return closeRing(left.concat(right.slice().reverse()));
  }

  function clipRing(ring, rect) {
    var clipped = Geo.clipPolygon(ring, rect);
    return clipped.length >= 4 ? clipped : null;
  }

  /* ---------- polígonos derivados ---------- */

  /* Tolerancia de simplificación: 5% del ancho de la calle */
  function computeRoadPolygon(cl, width) {
    var half = width / 2;
    var left = offsetPolyline(cl, half);
    var right = offsetPolyline(cl, -half);
    var ring = makeRing(left, right);
    return simplifyRing(ring, width * 0.05);
  }

  function computeMedian(cl, roadW, medW) {
    if (medW <= 0 || cl.length < 2) return null;
    var half = medW / 2;
    var left = offsetPolyline(cl, half);
    var right = offsetPolyline(cl, -half);
    var ring = makeRing(left, right);
    return simplifyRing(ring, medW * 0.1);
  }

  function computeSidewalks(cl, roadW, swW) {
    if (swW <= 0 || cl.length < 2) return [];
    var halfRoad = roadW / 2;
    var leftInner = offsetPolyline(cl, halfRoad);
    var leftOuter = offsetPolyline(cl, halfRoad + swW);
    var rightInner = offsetPolyline(cl, -halfRoad);
    var rightOuter = offsetPolyline(cl, -(halfRoad + swW));
    var result = [];
    if (leftInner.length >= 2) result.push(simplifyRing(makeRing(leftOuter, leftInner), swW * 0.15));
    if (rightInner.length >= 2) result.push(simplifyRing(makeRing(rightInner, rightOuter), swW * 0.15));
    return result;
  }

  function computeBikeLanes(cl, roadW, cwW) {
    if (cwW <= 0 || cl.length < 2) return [];
    var halfRoad = roadW / 2;
    var leftInner = offsetPolyline(cl, halfRoad - cwW);
    var leftOuter = offsetPolyline(cl, halfRoad);
    var rightInner = offsetPolyline(cl, -(halfRoad - cwW));
    var rightOuter = offsetPolyline(cl, -halfRoad);
    var result = [];
    if (leftInner.length >= 2) result.push(simplifyRing(makeRing(leftOuter, leftInner), cwW * 0.15));
    if (rightInner.length >= 2) result.push(simplifyRing(makeRing(rightInner, rightOuter), cwW * 0.15));
    return result;
  }

  function computeRoundabout(cl, width) {
    if (cl.length < 4) return null;
    if (cl[0][0] === cl[cl.length - 1][0] && cl[0][1] === cl[cl.length - 1][1]) {
      return simplifyRing(cl.slice(), width * 0.05);
    }
    var half = width / 2;
    var left = offsetPolyline(cl, half);
    var right = offsetPolyline(cl, -half);
    return simplifyRing(makeRing(left, right), width * 0.05);
  }

  /* ---------- parseo ---------- */

  function parseElement(e) {
    var tags = e.tags || {};
    var hw = tags.highway || '';
    var EXCLUDE = { cycleway:1, footway:1, path:1, pedestrian:1, service:1,
                    raceway:1, corridor:1, construction:1, steps:1, bus_guideway:1 };
    if (EXCLUDE[hw]) return null;
    if (tags.building && tags.building !== 'no') return null;
    var cl = [];
    if (e.geometry && e.geometry.length) {
      cl = e.geometry.map(function (g) { return [g.lon, g.lat]; });
    }
    if (cl.length < 2) return null;
    var w = roadWidth(tags);
    var mw = medianWidth(tags);
    var swL = sidewalkLeft(tags);
    var swR = sidewalkRight(tags);
    var cwL = cyclewayLeft(tags);
    var cwR = cyclewayRight(tags);
    /* Boulevard: calle con mediana + andenes en ambos lados + ciclovía */
    var isBoulevard = mw > 0 && swL > 0 && swR > 0 && (cwL > 0 || cwR > 0);
    return {
      id: e.id, cl: cl, tags: tags,
      name: tags.name || '', highway: tags.highway || '',
      width: w, lanes: parseInt(tags.lanes, 10) || 0,
      medianW: mw,
      sidewalkW: Math.max(swL, swR), sidewalkLeft: swL, sidewalkRight: swR,
      cyclewayW: Math.max(cwL, cwR), cyclewayLeft: cwL, cyclewayRight: cwR,
      oneway: tags.oneway === 'yes', surface: tags.surface || '',
      turn: turnInfo(tags), isRoundabout: tags.highway === 'roundabout',
      isBoulevard: isBoulevard,
    };
  }

  /* ---------- consulta Overpass por capas ---------- */

  var ENDPOINTS = [
    'https://maps.mail.ru/osm/tools/overpass/api/interpreter',
    'https://overpass-api.de/api/interpreter',
    'https://overpass.kumi.systems/api/interpreter',
    'https://overpass.osm.ch/api/interpreter',
  ];

  function fetchWithTimeout(url, opts, ms) {
    var ctrl = new AbortController();
    var timer = setTimeout(function () { ctrl.abort(); }, ms);
    return fetch(url, opts).then(
      function (r) { clearTimeout(timer); return r; },
      function (e) { clearTimeout(timer); throw e; });
  }

  function fetchJob(job) {
    var body = job.body;
    var errors = [];
    var attempt = 0;
    function tryServer(i) {
      if (i >= ENDPOINTS.length) {
        if (attempt < 2) { attempt++; i = 0; }
        else return Promise.reject(new Error(job.label + ': ' +
          (errors.length ? errors.join(' · ') : 'todos los servidores Overpass fallaron')));
      }
      return fetchWithTimeout(ENDPOINTS[i], {
        method: 'POST', body: body,
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      }, 45000).then(function (res) {
        if (!res.ok) throw new Error('HTTP ' + res.status);
        return res.json();
      }).then(function (json) {
        if (!json.elements && json.remark) throw new Error(json.remark);
        return { json: json, server: ENDPOINTS[i].replace(/^https?:\/\//, '').split('/')[0] };
      }).catch(function (err) {
        errors.push(err.message);
        return tryServer(i + 1);
      });
    }
    return tryServer(0);
  }

  function buildTiles(rect) {
    var w = rect.east - rect.west;
    var h = rect.north - rect.south;
    var size = Math.max(0.2, Math.min(0.5, Math.sqrt((w * h) / 12)));
    var cols = Math.max(1, Math.ceil(w / size));
    var rows = Math.max(1, Math.ceil(h / size));
    var tiles = [];
    for (var r = 0; r < rows; r++) {
      var south = rect.south + (h / rows) * r;
      var north = rect.south + (h / rows) * (r + 1);
      for (var c = 0; c < cols; c++) {
        tiles.push({
          south: south, west: rect.west + (w / cols) * c,
          north: north, east: rect.west + (w / cols) * (c + 1),
        });
      }
    }
    return tiles;
  }

  function query(rect, onProgress) {
    var tiles = buildTiles(rect);
    var queryBody = '[out:json][timeout:90];(way["highway"]' +
      '(__BBOX__););out geom;';

    var jobs = [];
    tiles.forEach(function (tile, ti) {
      var bbox = [tile.south, tile.west, tile.north, tile.east].join(',');
      jobs.push({
        body: queryBody.replace('__BBOX__', bbox),
        label: 'Calle F' + (Math.floor(ti / Math.ceil(Math.sqrt(tiles.length))) + 1) +
               'C' + (ti % Math.ceil(Math.sqrt(tiles.length)) + 1),
      });
    });

    var results = [];
    var servers = {};
    var idx = 0;

    function worker() {
      if (idx >= jobs.length) return Promise.resolve();
      var i = idx++;
      if (onProgress) onProgress(i + 1, jobs.length);
      return fetchJob(jobs[i]).then(function (res) {
        results[i] = res.json;
        servers[res.server] = true;
        return worker();
      });
    }

    var pool = [];
    for (var w = 0; w < Math.min(4, jobs.length); w++) pool.push(worker());

    return Promise.all(pool).then(function () {
      var nodeMap = {};
      var wayMap = {};
      results.forEach(function (json) {
        (json.elements || []).forEach(function (e) {
          if (e.type === 'node' && !(e.id in nodeMap)) {
            nodeMap[e.id] = [e.lon, e.lat];
          }
          if (e.type === 'way' && !wayMap[e.id]) {
            wayMap[e.id] = e;
          }
        });
      });

      var streets = [];
      Object.keys(wayMap).forEach(function (id) {
        var e = wayMap[id];
        var s = parseElement(e);
        if (s) streets.push(s);
      });

      return {
        streets: streets,
        servers: Object.keys(servers).join(', '),
        total: streets.length,
      };
    });
  }

  /* ---------- DXF ---------- */

  function exportDxf(data, zone) {
    var ACI = Dxf12.ACI;
    var COLORS = {
      CALLES: ACI.WHITE, CALLES_ANCHO: ACI.YELLOW, CALLES_GIROS: ACI.RED,
      CALLES_MEDIANA: ACI.GREEN, CALLES_ACERAS: ACI.CYAN,
      CALLES_CICLOVIA: ACI.MAGENTA, CALLES_ROTONDA: ACI.BLUE,
      CALLES_BOULEVARD: 30, /* naranja oscuro */
    };

    var unit = (typeof $ !== 'undefined' && $('selUnits')) ? $('selUnits').value : 'Meters';
    var toFt = unit === 'Feet' ? 3.28084 : 1;

    var polylines = [];
    var texts = [];

    data.streets.forEach(function (s) {
      var converter = proj4('WGS84',
        '+proj=utm +zone=' + zone.zone + (zone.hemi === 'S' ? ' +south' : '') +
        ' +datum=WGS84 +units=m +no_defs');
      function proj(p) { var r = converter.forward([p[0], p[1]]); return [r[0], r[1]]; }

      var projCl = s.cl.map(proj);

      /* CALLES — polígono de la calzada (solo si tiene ancho razonable) */
      if (s.width > 0) {
        var roadPoly = computeRoadPolygon(projCl, s.width * toFt);
        if (roadPoly.length >= 4) {
          polylines.push({ layer: 'CALLES', color: COLORS.CALLES, closed: true, points: roadPoly });
        }
      }

      /* CALLES_ROTONDA */
      if (s.isRoundabout) {
        var rPoly = computeRoundabout(projCl, s.width * toFt);
        if (rPoly && rPoly.length >= 4) {
          polylines.push({ layer: 'CALLES_ROTONDA', color: COLORS.CALLES_ROTONDA, closed: true, points: rPoly });
        }
      }

      /* CALLES_MEDIANA — solo si el tag median existe */
      if (s.medianW > 0) {
        var mPoly = computeMedian(projCl, s.width * toFt, s.medianW * toFt);
        if (mPoly && mPoly.length >= 4) {
          polylines.push({ layer: 'CALLES_MEDIANA', color: COLORS.CALLES_MEDIANA, closed: true, points: mPoly });
        }
      }

      /* CALLES_ACERAS — solo si los tags sidewalk existen */
      if (s.sidewalkLeft > 0) {
        var halfRoad = s.width / 2;
        var leftInner = offsetPolyline(projCl, halfRoad * toFt);
        var leftOuter = offsetPolyline(projCl, (halfRoad + s.sidewalkLeft) * toFt);
        var swLeft = simplifyRing(makeRing(leftOuter, leftInner), s.sidewalkLeft * toFt * 0.15);
        if (swLeft.length >= 4) {
          polylines.push({ layer: 'CALLES_ACERAS', color: COLORS.CALLES_ACERAS, closed: true, points: swLeft });
        }
      }
      if (s.sidewalkRight > 0) {
        var halfRoad2 = s.width / 2;
        var rightInner = offsetPolyline(projCl, -halfRoad2 * toFt);
        var rightOuter = offsetPolyline(projCl, -(halfRoad2 + s.sidewalkRight) * toFt);
        var swRight = simplifyRing(makeRing(rightInner, rightOuter), s.sidewalkRight * toFt * 0.15);
        if (swRight.length >= 4) {
          polylines.push({ layer: 'CALLES_ACERAS', color: COLORS.CALLES_ACERAS, closed: true, points: swRight });
        }
      }

      /* CALLES_CICLOVIA — solo si los tags cycleway existen */
      if (s.cyclewayLeft > 0) {
        var halfRoadL = s.width / 2;
        var clInner = offsetPolyline(projCl, (halfRoadL - s.cyclewayLeft) * toFt);
        var clOuter = offsetPolyline(projCl, halfRoadL * toFt);
        var cwLeft = simplifyRing(makeRing(clOuter, clInner), s.cyclewayLeft * toFt * 0.15);
        if (cwLeft.length >= 4) {
          polylines.push({ layer: 'CALLES_CICLOVIA', color: COLORS.CALLES_CICLOVIA, closed: true, points: cwLeft });
        }
      }
      if (s.cyclewayRight > 0) {
        var halfRoadR = s.width / 2;
        var crInner = offsetPolyline(projCl, -(halfRoadR - s.cyclewayRight) * toFt);
        var crOuter = offsetPolyline(projCl, -halfRoadR * toFt);
        var cwRight = simplifyRing(makeRing(crInner, crOuter), s.cyclewayRight * toFt * 0.15);
        if (cwRight.length >= 4) {
          polylines.push({ layer: 'CALLES_CICLOVIA', color: COLORS.CALLES_CICLOVIA, closed: true, points: cwRight });
        }
      }

      /* CALLES_BOULEVARD — outline completo: bordes externos de andenes */
      if (s.isBoulevard) {
        var totalHalf = (s.width / 2 + Math.max(s.sidewalkLeft, s.sidewalkRight)) * toFt;
        var bLeft = offsetPolyline(projCl, totalHalf);
        var bRight = offsetPolyline(projCl, -totalHalf);
        var bPoly = simplifyRing(makeRing(bLeft, bRight), totalHalf * 0.05);
        if (bPoly.length >= 4) {
          polylines.push({ layer: 'CALLES_BOULEVARD', color: COLORS.CALLES_BOULEVARD, closed: true, points: bPoly });
        }
      }

      /* CALLES_ANCHO (width label) */
      var midPt = projCl[Math.floor(projCl.length / 2)];
      var txt = s.lanes ? s.width.toFixed(1) + 'm (' + s.lanes + 'c)' : s.width.toFixed(1) + 'm';
      var textH = Math.max(s.width * 0.3 * toFt, 0.5 * toFt);
      texts.push({ layer: 'CALLES_ANCHO', x: midPt[0], y: midPt[1], height: textH, text: txt });

      /* CALLES_GIROS (turn arrows) */
      if (s.turn) {
        var lanes = s.turn.split('|');
        lanes.forEach(function (lane, li) {
          if (!lane || lane === 'through') return;
          var label = '';
          if (lane.indexOf('left') >= 0) label = 'GIRO-IZQ';
          else if (lane.indexOf('right') >= 0) label = 'GIRO-DER';
          else if (lane.indexOf('slight_left') >= 0) label = 'GIRO-SUAVE-IZQ';
          else if (lane.indexOf('slight_right') >= 0) label = 'GIRO-SUAVE-DER';
          else if (lane.indexOf('sharp_left') >= 0) label = 'GIRO-CERRADO-IZQ';
          else if (lane.indexOf('sharp_right') >= 0) label = 'GIRO-CERRADO-DER';
          else if (lane.indexOf('uturn') >= 0) label = 'U-TURN';
          if (!label) return;
          var laneOffset = (li - (lanes.length - 1) / 2) * M_LANES * toFt;
          var p = projCl[Math.floor(projCl.length / 2)];
          texts.push({ layer: 'CALLES_GIROS', x: p[0] + laneOffset, y: p[1],
            height: Math.max(s.width * 0.15 * toFt, 0.3 * toFt), text: label });
        });
      }
    });

    if (polylines.length === 0 && texts.length === 0) return null;
    return Dxf12.build({ unit: unit, polylines: polylines, texts: texts });
  }

  /* ---------- API pública ---------- */
  return {
    roadWidth: roadWidth, medianWidth: medianWidth,
    sidewalkWidth: sidewalkWidth, cyclewayWidth: cyclewayWidth,
    sidewalkLeft: sidewalkLeft, sidewalkRight: sidewalkRight,
    cyclewayLeft: cyclewayLeft, cyclewayRight: cyclewayRight,
    turnInfo: turnInfo,
    computeRoadPolygon: computeRoadPolygon, computeMedian: computeMedian,
    computeSidewalks: computeSidewalks, computeBikeLanes: computeBikeLanes,
    computeRoundabout: computeRoundabout,
    offsetPolyline: offsetPolyline, makeRing: makeRing,
    simplifyRing: simplifyRing, closeRing: closeRing,
    query: query, exportDxf: exportDxf, parseElement: parseElement,
  };
})();
