import sys
import ezdxf

path = sys.argv[1]
doc = ezdxf.readfile(path)
msp = doc.modelspace()

polylines = [e for e in msp if e.dxftype() == "POLYLINE"]
texts = [e for e in msp if e.dxftype() == "TEXT"]

print("ACADVER:", doc.header.get("$ACADVER", "?"))
print("INSUNITS:", doc.header.get("$INSUNITS", "?"))
print("layers:", sorted(l.dxf.name for l in doc.layers))

for pl in polylines:
    pts = list(pl.points())
    print(f"POLYLINE layer={pl.dxf.layer} closed={pl.is_closed} vertices={len(pts)}")
    if not pl.is_closed:
        print("ERROR: polilinea abierta"); sys.exit(1)
    if len(pts) < 3:
        print("ERROR: menos de 3 vertices"); sys.exit(1)

for t in texts:
    print(f"TEXT layer={t.dxf.layer} value={t.dxf.text!r} x={t.dxf.insert.x:.2f} y={t.dxf.insert.y:.2f} h={t.dxf.height}")

print(f"OK: {len(polylines)} polilinea(s), {len(texts)} texto(s)")