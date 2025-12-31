import { Card } from './ui/Card'
import { useState } from 'react'
import { useAircraftStore } from '../store/aircraft-store'
import { Download, Box, FileCode, Loader } from 'lucide-react'
import { Button } from './ui/Button'

type ExportFormat = 'STL' | 'OBJ' | 'STEP'

// Generate NACA 4-digit airfoil coordinates
function generateNACAProfile(tc: number, numPoints: number = 40): { x: number[], yu: number[], yl: number[] } {
  const x: number[] = []
  const yu: number[] = []
  const yl: number[] = []

  for (let i = 0; i <= numPoints; i++) {
    const beta = (i / numPoints) * Math.PI
    const xc = 0.5 * (1 - Math.cos(beta)) // Cosine spacing for better LE resolution

    const yt = 5 * tc * (
      0.2969 * Math.sqrt(xc) -
      0.1260 * xc -
      0.3516 * xc * xc +
      0.2843 * xc * xc * xc -
      0.1015 * xc * xc * xc * xc
    )

    x.push(xc)
    yu.push(yt)
    yl.push(-yt)
  }

  return { x, yu, yl }
}

// Generate STL content for the aircraft
function generateSTL(store: ReturnType<typeof useAircraftStore.getState>): string {
  const { body, wing, wingSection, hTail, hTailSection, vTail, vTailSection, synthesis, enableHTail, enableVTail } = store

  let stl = 'solid aircraft\n'

  // Helper to add a triangle
  const addTriangle = (v1: number[], v2: number[], v3: number[]) => {
    // Calculate normal
    const u = [v2[0] - v1[0], v2[1] - v1[1], v2[2] - v1[2]]
    const w = [v3[0] - v1[0], v3[1] - v1[1], v3[2] - v1[2]]
    const n = [
      u[1] * w[2] - u[2] * w[1],
      u[2] * w[0] - u[0] * w[2],
      u[0] * w[1] - u[1] * w[0]
    ]
    const len = Math.sqrt(n[0] * n[0] + n[1] * n[1] + n[2] * n[2])
    if (len > 0) {
      n[0] /= len
      n[1] /= len
      n[2] /= len
    }

    stl += `  facet normal ${n[0].toExponential(6)} ${n[1].toExponential(6)} ${n[2].toExponential(6)}\n`
    stl += '    outer loop\n'
    stl += `      vertex ${v1[0].toExponential(6)} ${v1[1].toExponential(6)} ${v1[2].toExponential(6)}\n`
    stl += `      vertex ${v2[0].toExponential(6)} ${v2[1].toExponential(6)} ${v2[2].toExponential(6)}\n`
    stl += `      vertex ${v3[0].toExponential(6)} ${v3[1].toExponential(6)} ${v3[2].toExponential(6)}\n`
    stl += '    endloop\n'
    stl += '  endfacet\n'
  }

  // Generate fuselage triangles (simplified revolution surface)
  const nSegs = 16
  for (let i = 0; i < body.x.length - 1; i++) {
    for (let j = 0; j < nSegs; j++) {
      const theta1 = (j / nSegs) * 2 * Math.PI
      const theta2 = ((j + 1) / nSegs) * 2 * Math.PI

      const v1 = [body.x[i], body.r[i] * Math.cos(theta1), body.r[i] * Math.sin(theta1)]
      const v2 = [body.x[i + 1], body.r[i + 1] * Math.cos(theta1), body.r[i + 1] * Math.sin(theta1)]
      const v3 = [body.x[i + 1], body.r[i + 1] * Math.cos(theta2), body.r[i + 1] * Math.sin(theta2)]
      const v4 = [body.x[i], body.r[i] * Math.cos(theta2), body.r[i] * Math.sin(theta2)]

      addTriangle(v1, v2, v3)
      addTriangle(v1, v3, v4)
    }
  }

  // Generate wing surfaces
  const generateWingSurface = (
    planform: typeof wing,
    section: typeof wingSection,
    xPos: number,
    zPos: number,
    isVertical: boolean = false
  ) => {
    const { chrdr, chrdtp, sspn, savsi } = planform
    const { tovc } = section
    const sweepRad = (savsi * Math.PI) / 180
    const nSpan = 10
    const nChord = 15

    const profile = generateNACAProfile(tovc, nChord)

    for (let i = 0; i < nSpan; i++) {
      const y1 = (i / nSpan) * sspn
      const y2 = ((i + 1) / nSpan) * sspn
      const chord1 = chrdr + (chrdtp - chrdr) * (i / nSpan)
      const chord2 = chrdr + (chrdtp - chrdr) * ((i + 1) / nSpan)
      const xOffset1 = y1 * Math.tan(sweepRad)
      const xOffset2 = y2 * Math.tan(sweepRad)

      for (let j = 0; j < profile.x.length - 1; j++) {
        // Upper surface
        const xu1 = xPos + xOffset1 + profile.x[j] * chord1
        const xu2 = xPos + xOffset1 + profile.x[j + 1] * chord1
        const xu3 = xPos + xOffset2 + profile.x[j + 1] * chord2
        const xu4 = xPos + xOffset2 + profile.x[j] * chord2

        const yu1 = profile.yu[j] * chord1
        const yu2 = profile.yu[j + 1] * chord1
        const yu3 = profile.yu[j + 1] * chord2
        const yu4 = profile.yu[j] * chord2

        if (isVertical) {
          // Vertical tail: Y becomes Z
          addTriangle([xu1, zPos + yu1, 0], [xu2, zPos + yu2, 0], [xu3, zPos + yu3, 0])
          addTriangle([xu1, zPos + yu1, 0], [xu3, zPos + yu3, 0], [xu4, zPos + yu4, 0])
        } else {
          // Right wing
          addTriangle([xu1, zPos + yu1, y1], [xu2, zPos + yu2, y1], [xu3, zPos + yu3, y2])
          addTriangle([xu1, zPos + yu1, y1], [xu3, zPos + yu3, y2], [xu4, zPos + yu4, y2])
          // Left wing
          addTriangle([xu1, zPos + yu1, -y1], [xu4, zPos + yu4, -y2], [xu3, zPos + yu3, -y2])
          addTriangle([xu1, zPos + yu1, -y1], [xu3, zPos + yu3, -y2], [xu2, zPos + yu2, -y1])
        }

        // Lower surface
        const yl1 = profile.yl[j] * chord1
        const yl2 = profile.yl[j + 1] * chord1
        const yl3 = profile.yl[j + 1] * chord2
        const yl4 = profile.yl[j] * chord2

        if (isVertical) {
          addTriangle([xu1, zPos + yl1, 0], [xu3, zPos + yl3, 0], [xu2, zPos + yl2, 0])
          addTriangle([xu1, zPos + yl1, 0], [xu4, zPos + yl4, 0], [xu3, zPos + yl3, 0])
        } else {
          // Right wing
          addTriangle([xu1, zPos + yl1, y1], [xu3, zPos + yl3, y2], [xu2, zPos + yl2, y1])
          addTriangle([xu1, zPos + yl1, y1], [xu4, zPos + yl4, y2], [xu3, zPos + yl3, y2])
          // Left wing
          addTriangle([xu1, zPos + yl1, -y1], [xu2, zPos + yl2, -y1], [xu3, zPos + yl3, -y2])
          addTriangle([xu1, zPos + yl1, -y1], [xu3, zPos + yl3, -y2], [xu4, zPos + yl4, -y2])
        }
      }
    }
  }

  // Wing
  generateWingSurface(wing, wingSection, synthesis.xw ?? 3, synthesis.zw ?? 0)

  // Horizontal tail
  if (enableHTail && hTail && hTailSection) {
    generateWingSurface(hTail, hTailSection, synthesis.xh ?? 8, synthesis.zh ?? 0)
  }

  // Vertical tail
  if (enableVTail && vTail && vTailSection) {
    const maxR = Math.max(...body.r)
    generateWingSurface(vTail, vTailSection, synthesis.xv ?? 8, maxR + 0.05, true)
  }

  stl += 'endsolid aircraft\n'
  return stl
}

// Generate OBJ format
function generateOBJ(store: ReturnType<typeof useAircraftStore.getState>): string {
  const { body } = store

  let obj = '# DATCOM Aircraft Model\n'
  obj += '# Generated by DATCOM UI\n\n'

  const vertices: number[][] = []
  const faces: number[][] = []

  // Fuselage vertices
  const nSegs = 16
  for (let i = 0; i < body.x.length; i++) {
    for (let j = 0; j <= nSegs; j++) {
      const theta = (j / nSegs) * 2 * Math.PI
      vertices.push([
        body.x[i],
        body.r[i] * Math.cos(theta),
        body.r[i] * Math.sin(theta)
      ])
    }
  }

  // Fuselage faces
  for (let i = 0; i < body.x.length - 1; i++) {
    for (let j = 0; j < nSegs; j++) {
      const base = i * (nSegs + 1) + j
      faces.push([
        base + 1,
        base + 2,
        base + nSegs + 3,
        base + nSegs + 2
      ])
    }
  }

  // Write vertices
  for (const v of vertices) {
    obj += `v ${v[0].toFixed(6)} ${v[1].toFixed(6)} ${v[2].toFixed(6)}\n`
  }

  obj += '\n'

  // Write faces
  for (const f of faces) {
    obj += `f ${f.join(' ')}\n`
  }

  return obj
}

export function CADExport() {
  const [exporting, setExporting] = useState(false)
  const [selectedFormat, setSelectedFormat] = useState<ExportFormat>('STL')

  const handleExport = async (format: ExportFormat) => {
    setExporting(true)

    try {
      let content: string
      let filename: string
      let mimeType: string

      const currentStore = useAircraftStore.getState()

      switch (format) {
        case 'STL':
          content = generateSTL(currentStore)
          filename = 'aircraft.stl'
          mimeType = 'application/sla'
          break
        case 'OBJ':
          content = generateOBJ(currentStore)
          filename = 'aircraft.obj'
          mimeType = 'text/plain'
          break
        case 'STEP':
          // STEP format is complex, show a placeholder message
          alert('STEP export requires additional libraries. Please use STL for now and convert using FreeCAD or similar tool.')
          setExporting(false)
          return
        default:
          throw new Error('Unknown format')
      }

      // Create and download file
      const blob = new Blob([content], { type: mimeType })
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = filename
      document.body.appendChild(a)
      a.click()
      document.body.removeChild(a)
      URL.revokeObjectURL(url)
    } catch (error) {
      console.error('Export error:', error)
      alert('Export failed: ' + (error as Error).message)
    } finally {
      setExporting(false)
    }
  }

  return (
    <Card title="CAD Export">
      <div className="space-y-4">
        <p className="text-sm text-text-muted">
          Export the 3D geometry for use in SolidWorks, FreeCAD, or other CAD software.
        </p>

        <div className="grid grid-cols-3 gap-2">
          {(['STL', 'OBJ', 'STEP'] as ExportFormat[]).map((format) => (
            <button
              key={format}
              onClick={() => setSelectedFormat(format)}
              className={`flex flex-col items-center gap-1 p-3 rounded-lg transition-colors ${
                selectedFormat === format
                  ? 'bg-primary text-white'
                  : 'bg-surface-light text-text-muted hover:text-text'
              }`}
            >
              {format === 'STL' && <Box className="w-5 h-5" />}
              {format === 'OBJ' && <FileCode className="w-5 h-5" />}
              {format === 'STEP' && <Download className="w-5 h-5" />}
              <span className="text-sm font-medium">{format}</span>
            </button>
          ))}
        </div>

        <div className="bg-surface-light rounded p-3 text-sm">
          {selectedFormat === 'STL' && (
            <>
              <p className="font-medium text-text">STL Format</p>
              <p className="text-text-muted mt-1">Standard format for 3D printing and CAD import. Widely supported by SolidWorks, FreeCAD, Blender, etc.</p>
            </>
          )}
          {selectedFormat === 'OBJ' && (
            <>
              <p className="font-medium text-text">OBJ Format</p>
              <p className="text-text-muted mt-1">Wavefront format for 3D visualization. Good for rendering and mesh editing.</p>
            </>
          )}
          {selectedFormat === 'STEP' && (
            <>
              <p className="font-medium text-text">STEP Format</p>
              <p className="text-text-muted mt-1">Native CAD format with NURBS surfaces. Best for SolidWorks, CATIA, NX. Requires conversion from STL.</p>
            </>
          )}
        </div>

        <Button
          variant="primary"
          className="w-full"
          onClick={() => handleExport(selectedFormat)}
          disabled={exporting}
        >
          {exporting ? (
            <>
              <Loader className="w-4 h-4 animate-spin" />
              Exporting...
            </>
          ) : (
            <>
              <Download className="w-4 h-4" />
              Export as {selectedFormat}
            </>
          )}
        </Button>

        <div className="border-t border-border pt-4 text-xs text-text-muted">
          <p><strong>Tip:</strong> For SolidWorks import:</p>
          <ol className="list-decimal list-inside mt-1 space-y-1">
            <li>Export as STL</li>
            <li>Open in SolidWorks (File → Open)</li>
            <li>Use "ScanTo3D" add-in for surface conversion</li>
          </ol>
        </div>
      </div>
    </Card>
  )
}
