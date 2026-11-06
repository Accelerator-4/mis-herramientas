import sys
import ezdxf

def dump_entities(path):
    doc = ezdxf.readfile(path)  # estricto: sin modo "recover"
    msp = doc.modelspace()
    pls = [(e.dxf.layer, e.is_closed, [(round(v[0], 6), round(v[1], 6)) for v in e.points()])
           for e in msp if e.dxftype() == "POLYLINE"]
    txts = [(t.dxf.layer, t.dxf.text, round(t.dxf.insert.x, 6), round(t.dxf.insert.y, 6), t.dxf.height)
            for t in msp if t.dxftype() == "TEXT"]
    layers = sorted(l.dxf.name for l in doc.layers)
    acad = doc.header.get("$ACADVER", "?")
    return acad, layers, pls, txts

path_a, path_b = sys.argv[1], sys.argv[2]
a = dump_entities(path_a)
b = dump_entities(path_b)

print("A (mio):   ACADVER=%s layers=%s" % (a[0], a[1]))
print("B (ezdxf): ACADVER=%s layers=%s" % (b[0], b[1]))
print("PoliA:", a[2])
print("PoliB:", b[2])
print("TextA:", a[3])
print("TextB:", b[3])

audit = ezdxf.readfile(path_a).audit()
if audit.has_errors:
    print("ERROR: auditoría de A detecta errores:")
    for e in audit.errors:
        print("  -", e)
    sys.exit(1)

if a[0] != b[0] or a[1] != b[1]:
    print("ERROR: cabecera/capas distintas")
    sys.exit(1)
if a[2] != b[2]:
    print("ERROR: polilíneas distintas")
    sys.exit(1)
if a[3] != b[3]:
    print("ERROR: textos distintos")
    sys.exit(1)

print("OK: estructura idéntica a ezdxf y sin errores de auditoría")