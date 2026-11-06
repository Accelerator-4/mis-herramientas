(function () {
  'use strict';

  var $ = function (id) { return document.getElementById(id); };

  /* ============================================================
     Mapa + control de dibujo (rectángulo único)
     ============================================================ */

  /* Servidores Overpass con respaldo automático. mail.ru es el único que
     responde datos reales y acepta CORS desde file:// (aunque va lento y a
     veces da 504); osm.ch contesta CORS pero su espejo suele devolver JSON
     vacío, por eso va al final. */
  var OVERPASS_ENDPOINTS = [
    'https://maps.mail.ru/osm/tools/overpass/api/interpreter',
    'https://overpass-api.de/api/interpreter',
    'https://overpass.kumi.systems/api/interpreter',
    'https://overpass.osm.ch/api/interpreter',
  ];

  var BUSY_TEXT = {
    query: 'Consultando OpenStreetMap (Overpass API)…',
  };

  var ACI = Dxf12.ACI;
  var SWATCHES = [1, 2, 3, 4, 5, 6, 7, 8, 9];

  var map = L.map('map').setView([19.4326, -99.1332], 13);

  var TILE_LAYERS = {
    /* CartoDB eliminado: desde 2025 sus basemaps exigen API key
       (tile de error "api key required — carto.com/basemaps"). */
    esri: {
      name: 'Esri World Imagery',
      url: 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
      attribution: 'Tiles &copy; <a href="https://www.esri.com/">Esri</a> &mdash; Source: Esri, Maxar, Earthstar Geographics',
      maxZoom: 23,
      subdomains: '',
    },
    esri_topo: {
      name: 'Esri Topographic',
      url: 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',
      attribution: 'Tiles &copy; <a href="https://www.esri.com/">Esri</a>',
      maxZoom: 23,
      subdomains: '',
    },
    osm_de: {
      name: 'OSM Germany',
      url: 'https://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png',
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors | Tiles by <a href="https://www.openstreetmap.de/">OSM Germany</a>',
      maxZoom: 20,
      subdomains: 'abc',
    },
    osm_fr: {
      name: 'OSM France',
      url: 'https://{s}.tile.openstreetmap.fr/osmfr/{z}/{x}/{y}.png',
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors | Tiles by <a href="https://www.openstreetmap.fr/">OSM France</a>',
      maxZoom: 20,
      subdomains: 'abc',
    },
    osm_hot: {
      name: 'OSM Humanitarian',
      url: 'https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png',
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors | Tiles by <a href="https://www.hotosm.org/">HOT</a>',
      maxZoom: 20,
      subdomains: 'abc',
    },
  };

  var currentTileKey = 'esri_topo';
  var tileLayer = null;
  var failCount = 0;

  /* Crea la capa de tiles SIN crossOrigin: las imágenes se cargan como
     <img> normal, que funciona con cualquier servidor. crossOrigin forzaría
     CORS y rompería los servidores que no envían Access-Control-Allow-Origin. */
  function createTileLayer(key) {
    var cfg = TILE_LAYERS[key];
    return L.tileLayer(cfg.url, {
      maxZoom: cfg.maxZoom,
      attribution: cfg.attribution,
      subdomains: cfg.subdomains || 'abc',
      tileSize: 256,
    });
  }

  function activateTileLayer(key) {
    if (tileLayer) {
      map.removeLayer(tileLayer);
      tileLayer.off();
    }
    currentTileKey = key;
    tileLayer = createTileLayer(key);
    tileLayer.on('tileload', function (e) {
      failCount = 0;
      console.log('Tile OK [' + currentTileKey + ']', e.tile && e.tile.src);
    });
    tileLayer.on('tileerror', function (e) {
      var src = e.tile && e.tile.src;
      console.error('TILE ERROR [' + currentTileKey + ']', src);
      failCount++;
      setStatus('Tile falló (' + failCount + 'x) en ' + TILE_LAYERS[currentTileKey].name + ': ' + (src || 'sin URL'), true);
      /* solo cambia tras varios errores consecutivos y sin tiles cargados */
      if (failCount >= 5) {
        failCount = 0;
        var keys = Object.keys(TILE_LAYERS);
        var idx = keys.indexOf(key);
        var nextKey = keys[(idx + 1) % keys.length];
        if (nextKey === key) return;
        setStatus('Cambiando capa base: ' + TILE_LAYERS[key].name + ' → ' + TILE_LAYERS[nextKey].name, true);
        activateTileLayer(nextKey);
      }
    });
    tileLayer.addTo(map);
    var sel = $('selTileLayer');
    if (sel && sel.value !== key) sel.value = key;
    return tileLayer;
  }

  /* Si el navegador restaura la página desde bfcache (volver/avanzar),
     podría mostrar tiles viejos en caché. Forzamos recarga real. */
  window.addEventListener('pageshow', function (e) {
    if (e.persisted) window.location.reload();
  });

  activateTileLayer('esri_topo');

  var drawLayer = new L.FeatureGroup();
  map.addLayer(drawLayer);

  map.addControl(new L.Control.Draw({
    position: 'topleft',
    draw: {
      polygon: false,
      polyline: false,
      rectangle: {
        shapeOptions: {
          color: '#1a73e8',
          weight: 2,
          dashArray: '6 4',
          fillOpacity: 0.06,
        },
      },
      circle: false,
      marker: false,
      circlemarker: false,
    },
    edit: { featureGroup: drawLayer, remove: true },
  }));

  /* ============================================================
     Estado
     ============================================================ */

  var rectInfo = null;      /* {south, north, west, east} del rectángulo */
  var lastData = null;      /* resultado de la última consulta ya recortado */
  var lastServer = null;    /* servidor Overpass que respondió la última consulta */
  var lastTrunc = {};       /* capas recortadas por tamaño en la última consulta */
  var previewGroup = null;
  var streetsPreviewGroup = null;
  var requestSeq = 0;
  var MAX_VERTICES = 100000; /* presupuesto de vértices por capa */

  /* ---- estado de calles detalladas ---- */
  var streetsActive = false;
  var streetsData = null;   /* {streets, servers, total} de Streets.query */
  var streetsSeq = 0;

  var LAYER_DEFS = [
    { key: 'highways',   name: 'CARRETERAS',    type: 'line', cap: 5000, offset: true },
    { key: 'waterways',  name: 'RIOS',          type: 'line', cap: 0 },
    { key: 'railways',   name: 'FERROCARRILES', type: 'line', cap: 0 },
    { key: 'buildings',  name: 'EDIFICIOS',     type: 'poly', cap: 7000 },
    { key: 'places',     name: 'NOMBRES',       type: 'point', cap: 1500 },
  ];

  /* color ACI elegido por el usuario para cada capa */
  var layerAci = {
    highways: ACI.RED,
    waterways: ACI.CYAN,
    railways: ACI.YELLOW,
    buildings: ACI.GREEN,
    places: 8,
  };

  /* ============================================================
     Panel lateral: filas por capa
     ============================================================ */

  function updatePanel() {
    var box = $('exportList');
    box.innerHTML = '';
    enabledDefs().forEach(function (d) {
      var card = document.createElement('div');
      card.className = 'xl-card';

      var row = document.createElement('div');
      row.className = 'xl-row';

      var dot = document.createElement('span');
      dot.className = 'dot';
      dot.style.background = Geo.aciHex(layerAci[d.key]);
      row.appendChild(dot);

      var name = document.createElement('b');
      name.textContent = d.name;
      row.appendChild(name);

      var meta = document.createElement('span');
      meta.className = 'meta';
      var n = (lastData && lastData[d.key].length) || 0;
      meta.textContent = n === 1 ? '1 elemento' : n + ' elementos';
      row.appendChild(meta);

      card.appendChild(row);

      var swatches = document.createElement('div');
      swatches.className = 'xl-swatches';
      SWATCHES.forEach(function (aci) {
        var b = document.createElement('button');
        b.type = 'button';
        b.className = 'swatch' + (layerAci[d.key] === aci ? ' active' : '');
        b.style.background = Geo.aciHex(aci);
        b.title = 'Color ACI ' + aci;
        b.addEventListener('click', function () {
          layerAci[d.key] = aci;
          updatePanel();
          renderPreview();
        });
        swatches.appendChild(b);
      });
      card.appendChild(swatches);

      box.appendChild(card);
    });
  }

  /* ============================================================
     Consulta a OpenStreetMap
     ============================================================ */

  function enabledDefs() {
    return LAYER_DEFS.filter(function (d) {
      if (d.key === 'buildings') return $('chkBuildings').checked;
      if (d.key === 'places') return $('chkPlaces').checked;
      return true;
    });
  }

  function layerKeyOf(tags) {
    if (tags.highway) return 'highways';
    if (tags.waterway) return 'waterways';
    if (tags.railway) return 'railways';
    if (tags.building) return 'buildings';
    return null;
  }

  function roadOffsetM() {
    var v = parseFloat($('roadWidth').value);
    if (!isFinite(v)) return 2;
    return Math.min(50, Math.max(0.5, v));
  }

  function fetchData() {
    var defs = enabledDefs();
    /* una petición independiente por capa: en Overpass, cuando un `out` con
       límite se dispara bajo carga, las capas siguientes de la misma petición
       se pierden silenciosamente. Separando por capa esto no puede ocurrir.
       El tope de cada cuadrante es solo para que la respuesta sea pequeña;
       el límite global de la capa se aplica al final, sobre datos fusionados. */
    var GROUPS = [
      { key: 'highways',  filter: 'way["highway"]', cap: 2000 },
      { key: 'waterways', filter: 'way["waterway"]' },
      { key: 'railways',  filter: 'way["railway"]' },
      { key: 'buildings', filter: 'way["building"][building!=no]', cap: 2000 },
      { key: 'places',    filter: 'node["place"][name]', cap: 1500 },
    ];

    function bodyFor(group, bboxStr) {
      return '[out:json][timeout:90];(' + group.filter + '(' + bboxStr + '););out geom' +
        (group.cap ? ' ' + group.cap : '') + ';';
    }

    function shortName(i) {
      return OVERPASS_ENDPOINTS[i].replace(/^https?:\/\//, '').split('/')[0];
    }

    function fetchWithTimeout(url, opts, ms) {
      var ctrl = new AbortController();
      var timer = setTimeout(function () { ctrl.abort(); }, ms);
      return fetch(url, opts).then(
        function (res) { clearTimeout(timer); return res; },
        function (err) { clearTimeout(timer); throw err; });
    }

    /* un trabajo (capa + cuadrante): prueba cada servidor en orden y, si todos
       fallan, reintenta una vez la cadena completa antes de rendirse */
    function fetchJob(job) {
      var body = bodyFor(job.group, job.bbox);
      var errors = [];
      var attempt = 0;
      function tryServer(i) {
        if (i >= OVERPASS_ENDPOINTS.length) {
          if (attempt < 2) { attempt++; i = 0; }
          else return Promise.reject(new Error(job.label + ': ' +
            (errors.length ? errors.join(' · ') : 'todos los servidores Overpass fallaron')));
        }
        return fetchWithTimeout(OVERPASS_ENDPOINTS[i], {
          method: 'POST',
          body: body,
          headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
        }, 45000).then(function (res) {
          if (!res.ok) throw new Error('HTTP ' + res.status + ' (' + shortName(i) + ')');
          return res.json();
        }).then(function (json) {
          if (!json.elements && json.remark) throw new Error('(' + shortName(i) + ') ' + json.remark);
          return { json: json, server: shortName(i) };
        }).catch(function (err) {
          errors.push(err.message);
          return tryServer(i + 1);
        });
      }
      return tryServer(0);
    }

    /* divide el rectángulo en cuadrantes (~12 por área, lado 0.2°–0.5°)
       para que cada consulta sea pequeña y no agote a los servidores */
    function buildTiles() {
      var w = rectInfo.east - rectInfo.west;
      var h = rectInfo.north - rectInfo.south;
      var size = Math.max(0.2, Math.min(0.5, Math.sqrt((w * h) / 12)));
      var cols = Math.max(1, Math.ceil(w / size));
      var rows = Math.max(1, Math.ceil(h / size));
      var tiles = [];
      for (var r = 0; r < rows; r++) {
        var south = rectInfo.south + (h / rows) * r;
        var north = rectInfo.south + (h / rows) * (r + 1);
        for (var c = 0; c < cols; c++) {
          tiles.push({
            south: south,
            west: rectInfo.west + (w / cols) * c,
            north: north,
            east: rectInfo.west + (w / cols) * (c + 1),
            label: 'F' + (r + 1) + 'C' + (c + 1),
          });
        }
      }
      return tiles;
    }

    var tiles = buildTiles();
    var jobs = [];
    var jobsByKey = {};
    defs.forEach(function (d) {
      var g = null;
      for (var gi = 0; gi < GROUPS.length; gi++) {
        if (GROUPS[gi].key === d.key) { g = GROUPS[gi]; break; }
      }
      if (!g) return;
      jobsByKey[d.key] = [];
      tiles.forEach(function (tile, ti) {
        var job = {
          group: g,
          bbox: [tile.south, tile.west, tile.north, tile.east].join(','),
          label: g.key + ' ' + tile.label,
        };
        jobs.push(job);
        jobsByKey[d.key].push({ idx: jobs.length - 1, tile: ti });
      });
    });

    var results = {};
    var servers = {};
    var idx = 0;

    function worker() {
      if (idx >= jobs.length) return Promise.resolve();
      var i = idx++;
      setStatus(BUSY_TEXT.query + ' (' + (i + 1) + '/' + jobs.length + ')', true);
      return fetchJob(jobs[i]).then(function (res) {
        results[i] = res.json;
        servers[res.server] = true;
        return worker();
      });
    }

    var pool = [];
    for (var w = 0; w < Math.min(4, jobs.length); w++) pool.push(worker());
    return Promise.all(pool).then(function () {
      lastServer = Object.keys(servers).join(', ') || null;
      var nodeMap = {};
      var byKey = {};
      defs.forEach(function (d) { byKey[d.key] = []; });
      defs.forEach(function (d) {
        (jobsByKey[d.key] || []).forEach(function (entry) {
          var json = results[entry.idx];
          if (!json) return;
          var elements = json.elements || [];
          elements.forEach(function (e) {
            if (e.type === 'node') {
              if (!(e.id in nodeMap)) nodeMap[e.id] = [e.lon, e.lat];
              if (d.key === 'places' && e.tags && e.tags.place &&
                  (e.tags.name || '').trim() &&
                  e.lon >= rectInfo.west && e.lon <= rectInfo.east &&
                  e.lat >= rectInfo.south && e.lat <= rectInfo.north) {
                byKey.places.push({ pt: [e.lon, e.lat], name: e.tags.name.trim() });
              }
              return;
            }
            if (e.type !== 'way') return;
            var key = layerKeyOf(e.tags || {});
            if (!key || key !== d.key) return;
            var coords = [];
            if (e.geometry && e.geometry.length) {
              coords = e.geometry.map(function (g) { return [g.lon, g.lat]; });
            } else if (e.nodes && e.nodes.length) {
              coords = e.nodes.map(function (id) { return nodeMap[id]; });
              if (coords.some(function (c) { return !c; })) return;
            }
            if (coords.length >= 2) byKey[key].push(coords);
          });
        });
      });
      /* límite por capa sobre los datos fusionados: si la geometría es enorme,
         recorta (no descarta) y avisa */
      lastTrunc = {};
      defs.forEach(function (d) {
        var arr = byKey[d.key] || [];
        if (d.type === 'point') {
          if (d.cap && arr.length > d.cap) { lastTrunc[d.key] = true; arr = arr.slice(0, d.cap); }
          byKey[d.key] = arr;
          return;
        }
        if (d.cap && arr.length > d.cap) { lastTrunc[d.key] = true; arr = arr.slice(0, d.cap); }
        var kept = [];
        var used = 0;
        arr.forEach(function (coords) {
          if (used + coords.length > MAX_VERTICES && kept.length) {
            lastTrunc[d.key] = true;
            return;
          }
          kept.push(coords);
          used += coords.length;
        });
        byKey[d.key] = kept;
      });
      return byKey;
    }).catch(function (err) {
      throw new Error('No se pudo consultar OpenStreetMap: ' + err.message);
    });
  }

  /* ============================================================
     Recorte al área
     ============================================================ */

  function clipAll() {
    var defs = enabledDefs();
    var clipped = {};
    defs.forEach(function (d) {
      if (d.type === 'point') return;
      var out = [];
      (lastData[d.key] || []).forEach(function (coords) {
        /* los anillos cerrados (p. ej. rotondas) se recortan como polígono */
        var asPoly = d.type === 'poly' || Geo.isRing(coords);
        if (asPoly) {
          var ring = Geo.clipPolygon(coords, rectInfo);
          if (ring.length >= 4) out.push(ring);
        } else {
          var pieces = Geo.clipLine(coords, rectInfo);
          pieces.forEach(function (p) { if (p.length >= 2) out.push(p); });
        }
      });
      clipped[d.key] = out;
    });
    return clipped;
  }

  function runExport() {
    if (!rectInfo) return;
    var seq = ++requestSeq;
    lastServer = null;
    setStatus(BUSY_TEXT.query, true);
    fetchData().then(function (byKey) {
      if (seq !== requestSeq) return;
      lastData = byKey;
      updatePanel();
      renderPreview();
      setStatus(exportSummary(), false);
    }).catch(function (err) {
      if (seq !== requestSeq) return;
      lastData = null;
      updatePanel();
      setStatus(err.message, false);
    });
  }

  function exportSummary() {
    var parts = [];
    enabledDefs().forEach(function (d) {
      parts.push(d.name + ': ' + lastData[d.key].length);
    });
    var warns = [];
    enabledDefs().forEach(function (d) {
      if (lastTrunc[d.key]) warns.push(d.name + ' recortada por tamaño');
    });
    var via = lastServer ? ' (servidor: ' + lastServer + ')' : '';
    return 'Consulta lista' + via + '. ' + parts.join(' · ') +
      (warns.length ? ' Aviso: ' + warns.join(', ') + '.' : '');
  }

  function setStatus(msg, busy) {
    var el = $('statusLine');
    el.textContent = msg;
    el.classList.toggle('busy', !!busy);
  }

  /* ============================================================
     Proyección UTM y construcción de las entidades de dibujo
     ============================================================ */

  function buildProjected() {
    var unit = $('selUnits').value;
    var zone = Geo.utmZoneOf(Geo.rectCenter(rectInfo));
    var hemiSuffix = zone.hemi === 'S' ? ' +south' : '';
    var converter = proj4('WGS84',
      '+proj=utm +zone=' + zone.zone + hemiSuffix + ' +datum=WGS84 +units=m +no_defs');

    function proj(p) { /* p = [lng, lat] */
      var xy = converter.forward([p[0], p[1]]);
      return [xy[0], xy[1]];
    }
    function inverse(p) {
      var ll = converter.inverse([p[0], p[1]]);
      return [ll[0], ll[1]];
    }

    var corners = [
      [rectInfo.west, rectInfo.south],
      [rectInfo.east, rectInfo.north],
    ].map(proj);
    var diag = Math.hypot(corners[1][0] - corners[0][0], corners[1][1] - corners[0][1]) || 1;
    var textHeight = Math.max(diag * 0.02, 0.5);
    var placeHeight = Math.max(diag * 0.01, 0.25);
    var offset = roadOffsetM();

    var clipped = clipAll();
    var layers = [];
    enabledDefs().forEach(function (d) {
      if (d.type === 'point') return;
      var items = [];
      (clipped[d.key] || []).forEach(function (piece) {
        var pts = piece.map(proj);
        if (d.offset) {
          /* abre anillos cerrados (rotondas) para poder generar la banda */
          if (pts.length > 1 &&
              pts[0][0] === pts[pts.length - 1][0] &&
              pts[0][1] === pts[pts.length - 1][1]) {
            pts.pop();
          }
          var band = Geo.offsetBand(pts, offset);
          if (band) items.push({ pts: band, closed: true });
        } else if (d.type === 'poly' || Geo.isRing(piece)) {
          items.push({ pts: pts, closed: true });
        } else {
          items.push({ pts: pts, closed: false });
        }
      });
      layers.push({ def: d, items: items });
    });

    /* textos de nombres de lugares */
    var places = [];
    enabledDefs().forEach(function (d) {
      if (d.type !== 'point') return;
      (lastData[d.key] || []).forEach(function (pl) {
        var p = proj(pl.pt);
        places.push({ x: p[0], y: p[1], text: pl.name, pt: pl.pt });
      });
    });

    var c = proj([Geo.rectCenter(rectInfo).lng, Geo.rectCenter(rectInfo).lat]);
    var label = 'AREA: ' + Geo.fmtAreaCad(Geo.rectAreaM2(rectInfo), unit) +
      ' - ZONA UTM ' + zone.zone + zone.hemi;

    return {
      layers: layers,
      places: places,
      inverse: inverse,
      textHeight: textHeight,
      placeHeight: placeHeight,
      label: label,
      zone: zone,
      center: c,
    };
  }

  function buildExport() {
    if (!rectInfo || !lastData) return null;
    var unit = $('selUnits').value;
    var built = buildProjected();

    var polylines = [];
    built.layers.forEach(function (l) {
      l.items.forEach(function (item) {
        polylines.push({
          layer: l.def.name,
          color: layerAci[l.def.key],
          closed: item.closed,
          points: item.pts,
        });
      });
    });

    var texts = [{
      layer: 'AREA',
      x: built.center[0],
      y: built.center[1],
      height: built.textHeight,
      text: built.label,
    }];
    built.places.forEach(function (pl) {
      texts.push({
        layer: 'NOMBRES',
        x: pl.x,
        y: pl.y,
        height: built.placeHeight,
        text: pl.text,
      });
    });

    return {
      text: Dxf12.build({ unit: unit, polylines: polylines, texts: texts }),
      unit: unit,
      count: polylines.length,
    };
  }

  /* ============================================================
     Vista previa (bandas y líneas recortadas sobre el mapa)
     ============================================================ */

  function renderPreview() {
    if (previewGroup) {
      map.removeLayer(previewGroup);
      previewGroup = null;
    }
    if (!rectInfo || !lastData || !$('chkPreview').checked) return;
    var built = buildProjected();
    previewGroup = L.layerGroup();
    built.layers.forEach(function (l) {
      var color = Geo.aciHex(layerAci[l.def.key]);
      l.items.forEach(function (item) {
        var latlngs = item.pts.map(built.inverse).map(function (p) { return [p[1], p[0]]; });
        if (item.closed && latlngs.length >= 3) {
          L.polygon(latlngs, { color: color, weight: 2, opacity: 0.95, fillOpacity: 0.25 }).addTo(previewGroup);
        } else if (latlngs.length >= 2) {
          L.polyline(latlngs, { color: color, weight: 3, opacity: 0.9 }).addTo(previewGroup);
        }
      });
    });
    built.places.forEach(function (pl) {
      L.circleMarker([pl.pt[1], pl.pt[0]], {
        radius: 3,
        color: Geo.aciHex(layerAci.places),
        fillOpacity: 0.9,
      }).bindTooltip(pl.text, {
        permanent: true,
        direction: 'top',
        offset: [0, -6],
        opacity: 0.9,
        className: 'place-tip',
      }).addTo(previewGroup);
    });
    previewGroup.addTo(map);
  }

  /* ============================================================
     Eventos de dibujo
     ============================================================ */

  function setRectInfo(b) {
    rectInfo = {
      south: b.getSouth(),
      north: b.getNorth(),
      west: b.getWest(),
      east: b.getEast(),
    };
    $('rectInfo').textContent = 'Área ≈ ' + Geo.fmtArea(Geo.rectAreaM2(rectInfo)) +
      ' · N ' + rectInfo.north.toFixed(5) +
      ' · S ' + rectInfo.south.toFixed(5) +
      ' · O ' + rectInfo.west.toFixed(5) +
      ' · E ' + rectInfo.east.toFixed(5);
    $('btnClear').disabled = false;
  }

  map.on(L.Draw.Event.CREATED, function (e) {
    if (e.layerType !== 'rectangle') return;
    if (drawLayer.getLayers().length) drawLayer.clearLayers();
    drawLayer.addLayer(e.layer);
    setRectInfo(e.layer.getBounds());
    if (streetsActive) runStreetsQuery(); else runExport();
  });

  map.on(L.Draw.Event.EDITED, function (e) {
    var updated = false;
    e.layers.eachLayer(function (layer) {
      if (drawLayer.hasLayer(layer)) {
        setRectInfo(layer.getBounds());
        updated = true;
      }
    });
    if (updated) {
      if (streetsActive) runStreetsQuery(); else runExport();
    }
  });

  map.on(L.Draw.Event.DELETED, function (e) {
    var removed = false;
    e.layers.eachLayer(function (layer) {
      if (drawLayer.hasLayer(layer)) removed = true;
    });
    if (removed) clearRect();
  });

  function clearRect() {
    drawLayer.clearLayers();
    rectInfo = null;
    lastData = null;
    streetsData = null;
    ++requestSeq;
    ++streetsSeq;
    $('rectInfo').textContent = 'Todavía no hay área. Usa el botón \u25AD (rectángulo) sobre el mapa para definirla.';
    $('btnClear').disabled = true;
    $('btnStreetsExport').disabled = true;
    $('streetsSummary').classList.add('hidden');
    $('streetsLoading').classList.add('hidden');
    streetsStatus('', false);
    updatePanel();
    renderPreview();
    if (streetsPreviewGroup) { map.removeLayer(streetsPreviewGroup); streetsPreviewGroup = null; }
    setStatus('Listo.', false);
  }

  function hasRect() {
    return !!rectInfo;
  }

  /* ============================================================
     Exportación
     ============================================================ */

  function download(filename, text, mime) {
    var blob = new Blob([text], { type: mime });
    var url = URL.createObjectURL(blob);
    var a = document.createElement('a');
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    setTimeout(function () {
      URL.revokeObjectURL(url);
      a.remove();
    }, 100);
  }

  $('btnExport').addEventListener('click', function () {
    if (!hasRect()) {
      toast('Primero dibuja el rectángulo del área a exportar.');
      return;
    }
    if (!lastData) {
      toast('Todavía se está consultando el mapa. Espera un momento.');
      return;
    }
    var result = buildExport();
    if (!result) {
      toast('Nada que exportar: el área no contiene elementos.');
      return;
    }
    download('area-cad.dxf', result.text, 'application/dxf');
    toast('Exportados ' + result.count + ' tramo(s) a DXF (' + result.unit + ').');
  });

  /* ============================================================
     Controles del panel
     ============================================================ */

  $('chkBuildings').addEventListener('change', function () {
    if (hasRect()) runExport();
  });

  $('chkPlaces').addEventListener('change', function () {
    if (hasRect()) runExport();
  });

  $('roadWidth').addEventListener('change', function () {
    renderPreview();
  });

  $('btnClear').addEventListener('click', function () {
    clearRect();
  });

  $('btnZoomRect').addEventListener('click', function () {
    if (!hasRect()) {
      toast('Todavía no hay área dibujada.');
      return;
    }
    map.fitBounds(drawLayer.getBounds(), { padding: [40, 40] });
  });

  $('btnJump').addEventListener('click', function () {
    var lng = parseFloat($('jumpLng').value);
    var lat = parseFloat($('jumpLat').value);
    if (isNaN(lng) || isNaN(lat)) { toast('Ingresa longitud y latitud válidas.'); return; }
    map.setView([lat, lng], 15);
  });

  /* ============================================================
     Calles detalladas
     ============================================================ */

  function streetsStatus(msg, busy) {
    var el = $('streetsStatus');
    el.textContent = msg;
    el.classList.toggle('busy', !!busy);
  }

  function streetsSummary() {
    var s = streetsData.streets;
    var n = s.length;
    if (n === 0) { $('streetsSummary').classList.add('hidden'); return; }
    var withW = 0, withT = 0, withM = 0, withSW = 0, withCW = 0, rounds = 0, boul = 0;
    s.forEach(function (st) {
      if (st.width !== 7) withW++;
      if (st.turn) withT++;
      if (st.medianW > 0) withM++;
      if (st.sidewalkW > 0) withSW++;
      if (st.cyclewayW > 0) withCW++;
      if (st.isRoundabout) rounds++;
      if (st.isBoulevard) boul++;
    });
    $('strTotal').textContent = n;
    $('strWidth').textContent = Math.round(withW / n * 100) + '%';
    $('strTurns').textContent = Math.round(withT / n * 100) + '%';
    $('strMedian').textContent = Math.round(withM / n * 100) + '%';
    $('strSidewalk').textContent = Math.round(withSW / n * 100) + '%';
    $('strCycleway').textContent = Math.round(withCW / n * 100) + '%';
    $('strRound').textContent = rounds;
    $('strBoulevard').textContent = boul;
    $('streetsSummary').classList.remove('hidden');
  }

  function runStreetsQuery() {
    if (!streetsActive || !rectInfo) return;
    var seq = ++streetsSeq;
    streetsData = null;
    $('btnStreetsExport').disabled = true;
    $('streetsLoading').classList.remove('hidden');
    $('streetsSummary').classList.add('hidden');
    streetsStatus('Consultando calles detalladas...', true);
    Streets.query(rectInfo, function (done, total) {
      streetsStatus('Consultando calles detalladas... (' + done + '/' + total + ')', true);
    }).then(function (result) {
      if (seq !== streetsSeq) return;
      streetsData = result;
      $('streetsLoading').classList.add('hidden');
      streetsSummary();
      $('btnStreetsExport').disabled = false;
      streetsStatus(result.total + ' calles extraídas (servidor: ' + result.servers + ')', false);
      renderStreetsPreview();
    }).catch(function (err) {
      if (seq !== streetsSeq) return;
      $('streetsLoading').classList.add('hidden');
      $('streetsSummary').classList.add('hidden');
      streetsStatus('Error: ' + err.message, false);
    });
  }

  function renderStreetsPreview() {
    if (streetsPreviewGroup) {
      map.removeLayer(streetsPreviewGroup);
      streetsPreviewGroup = null;
    }
    if (!streetsData || !streetsActive || !$('chkPreview').checked) return;
    streetsPreviewGroup = L.layerGroup();
    var zone = Geo.utmZoneOf(Geo.rectCenter(rectInfo));
    var converter = proj4('WGS84',
      '+proj=utm +zone=' + zone.zone + (zone.hemi === 'S' ? ' +south' : '') +
      ' +datum=WGS84 +units=m +no_defs');
    function inv(p) { var r = converter.inverse([p[0], p[1]]); return [r[1], r[0]]; }

    streetsData.streets.forEach(function (s) {
      var cl = s.cl.map(function (p) {
        var r = converter.forward([p[0], p[1]]);
        return [r[0], r[1]];
      });
      var w = s.width;
      var left = Streets.offsetPolyline(cl, w / 2);
      var right = Streets.offsetPolyline(cl, -w / 2);
      var ring = Streets.makeRing(left, right);
      if (ring.length >= 4) {
        var latlngs = ring.map(inv);
        L.polygon(latlngs, {
          color: '#1a73e8', weight: 1.5, opacity: 0.7, fillOpacity: 0.12,
        }).bindTooltip(s.name || s.highway + ' ' + s.width + 'm' +
          (s.lanes ? ' (' + s.lanes + 'c)' : '') +
          (s.turn ? ' | giros: ' + s.turn : '') +
          (s.medianW ? ' | mediana ' + s.medianW + 'm' : '') +
          (s.sidewalkW ? ' | andenes ' + s.sidewalkW + 'm' : '') +
          (s.cyclewayW ? ' | ciclovía ' + s.cyclewayW + 'm' : '') +
          (s.surface ? ' | ' + s.surface : ''),
          { sticky: true, className: 'place-tip' })
        .addTo(streetsPreviewGroup);
      }
    });
    streetsPreviewGroup.addTo(map);
  }

  function exportStreetsDxf() {
    if (!streetsData || !rectInfo) return;
    var zone = Geo.utmZoneOf(Geo.rectCenter(rectInfo));
    var dxfText = Streets.exportDxf(streetsData, zone);
    if (!dxfText) { toast('Nada que exportar.'); return; }
    download('calles-detalladas.dxf', dxfText, 'application/dxf');
    toast('Exportadas ' + streetsData.streets.length + ' calles a DXF detallado.');
  }

  /* ============================================================
     Modal DWG
     ============================================================ */

  $('btnDwg').addEventListener('click', function () {
    $('dwgModal').classList.remove('hidden');
  });
  $('btnModalClose').addEventListener('click', function () {
    $('dwgModal').classList.add('hidden');
  });
  $('dwgModal').addEventListener('click', function (e) {
    if (e.target === this) this.classList.add('hidden');
  });

  /* ============================================================
     Toast
     ============================================================ */

  var toastTimer = null;
  function toast(msg) {
    var el = $('toast');
    if (!el) {
      el = document.createElement('div');
      el.id = 'toast';
      document.body.appendChild(el);
    }
    el.textContent = msg;
    el.classList.add('show');
    clearTimeout(toastTimer);
    toastTimer = setTimeout(function () { el.classList.remove('show'); }, 2600);
  }

  /* ============================================================
     Calles detalladas — eventos
     ============================================================ */

  $('btnStreetsToggle').addEventListener('click', function () {
    streetsActive = !streetsActive;
    $('btnStreetsToggle').textContent = streetsActive ? 'Desactivar' : 'Activar';
    $('streetsPanel').classList.toggle('hidden', !streetsActive);
    if (streetsActive) {
      /* modo calles: limpiar datos y preview del exportador normal */
      lastData = null;
      updatePanel();
      renderPreview();
      if (hasRect()) runStreetsQuery();
    } else {
      /* modo normal: limpiar datos y preview de calles */
      streetsData = null;
      streetsSummary();
      $('btnStreetsExport').disabled = true;
      if (streetsPreviewGroup) { map.removeLayer(streetsPreviewGroup); streetsPreviewGroup = null; }
      if (hasRect()) runExport();
    }
  });

  $('btnStreetsExport').addEventListener('click', function () {
    exportStreetsDxf();
  });

  $('chkPreview').addEventListener('change', function () {
    renderPreview();
    renderStreetsPreview();
  });

  /* ============================================================
     Selector de capa base
     ============================================================ */

  function populateTileSelector() {
    var sel = $('selTileLayer');
    Object.keys(TILE_LAYERS).forEach(function (key) {
      var opt = document.createElement('option');
      opt.value = key;
      opt.textContent = TILE_LAYERS[key].name;
      if (key === currentTileKey) opt.selected = true;
      sel.appendChild(opt);
    });
    sel.addEventListener('change', function () {
      var key = this.value;
      if (key === currentTileKey) return;
      activateTileLayer(key);
      setStatus('Capa base: ' + TILE_LAYERS[key].name, false);
    });
  }
  populateTileSelector();

  /* ============================================================
     Arranque
     ============================================================ */

  setStatus('Listo. Dibuja la \u25AD (área) sobre el mapa.', false);

  /* Indicador de versión: si aquí no aparece v24, el navegador está usando
     una copia vieja en caché (causa probable del error "api key required"). */
  var verEl = document.getElementById('appVer');
  if (verEl) verEl.textContent = 'v30';
})();