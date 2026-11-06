"""Genera el DXF de referencia con ezdxf (R12/AC1009) a partir de un JSON.

Uso: python gen_reference.py datos.json [salida.dxf]

El JSON tiene la forma:
{
  "polylines": [ { "layer": "AREA_1", "color": 1, "closed": true,
                   "points": [[x, y], ...] }, ... ],
  "texts":     [ { "layer": "AREA", "x": ..., "y": ..., "height": ...,
                   "text": "..." }, ... ]
}
"""
import json
import sys

import ezdxf


def main():
    src = sys.argv[1]
    out = sys.argv[2] if len(sys.argv) > 2 else "reference-r12.dxf"
    data = json.load(open(src, encoding="utf-8"))

    doc = ezdxf.new("R12", setup=False)
    if "Defpoints" not in doc.layers:
        doc.layers.add("Defpoints", color=7)
    msp = doc.modelspace()

    for poly in data.get("polylines", []):
        doc.layers.add(poly["layer"], color=poly.get("color", 7))
        msp.add_polyline2d(
            [(p[0], p[1]) for p in poly["points"]],
            dxfattribs={
                "layer": poly["layer"],
                "closed": poly.get("closed", True),
            },
        )

    for t in data.get("texts", []):
        doc.layers.add(t["layer"], color=7)
        msp.add_text(
            t["text"],
            dxfattribs={"layer": t["layer"], "height": t["height"]},
        ).set_placement((t["x"], t["y"]))

    doc.saveas(out)
    print("referencia:", out)


if __name__ == "__main__":
    main()