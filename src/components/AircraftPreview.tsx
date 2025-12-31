import { Canvas } from '@react-three/fiber'
import { OrbitControls, Grid, Environment, Text, Html } from '@react-three/drei'
import { useAircraftStore } from '../store/aircraft-store'
import { useMemo, useState, useRef } from 'react'
import * as THREE from 'three'
import { Eye, Plane, Box, ArrowUpFromLine } from 'lucide-react'

type ViewMode = 'all' | 'wing' | 'body' | 'tail'

// Generate NACA 4-digit airfoil coordinates with cosine spacing
function generateAirfoilPoints(tc: number, numPoints: number = 30): THREE.Vector2[] {
  const points: THREE.Vector2[] = []

  // Upper surface (trailing edge to leading edge)
  for (let i = numPoints; i >= 0; i--) {
    const beta = (i / numPoints) * Math.PI
    const x = 0.5 * (1 - Math.cos(beta))
    const yt = 5 * tc * (
      0.2969 * Math.sqrt(x) -
      0.1260 * x -
      0.3516 * x * x +
      0.2843 * x * x * x -
      0.1015 * x * x * x * x
    )
    points.push(new THREE.Vector2(x, yt))
  }

  // Lower surface (leading edge to trailing edge, skip first point to avoid duplicate)
  for (let i = 1; i <= numPoints; i++) {
    const beta = (i / numPoints) * Math.PI
    const x = 0.5 * (1 - Math.cos(beta))
    const yt = 5 * tc * (
      0.2969 * Math.sqrt(x) -
      0.1260 * x -
      0.3516 * x * x +
      0.2843 * x * x * x -
      0.1015 * x * x * x * x
    )
    points.push(new THREE.Vector2(x, -yt))
  }

  return points
}

// Create a smooth wing surface using lofted geometry
function createWingGeometry(
  rootChord: number,
  tipChord: number,
  span: number,
  sweepAngle: number,
  dihedralAngle: number,
  twistAngle: number,
  tcRoot: number,
  tcTip: number,
  numSpanSections: number = 20,
  numChordPoints: number = 25
): THREE.BufferGeometry {
  const sweepRad = (sweepAngle * Math.PI) / 180
  const dihedralRad = (dihedralAngle * Math.PI) / 180
  const twistRad = (twistAngle * Math.PI) / 180

  const vertices: number[] = []
  const indices: number[] = []

  // Generate airfoil sections along the span
  const sections: THREE.Vector3[][] = []

  for (let i = 0; i <= numSpanSections; i++) {
    const spanFraction = i / numSpanSections
    const y = span * spanFraction

    // Interpolate parameters along span
    const chord = rootChord + (tipChord - rootChord) * spanFraction
    const tc = tcRoot + (tcTip - tcRoot) * spanFraction
    const twist = twistRad * spanFraction

    // Calculate section position
    const xOffset = y * Math.tan(sweepRad)
    const zOffset = y * Math.sin(dihedralRad)
    const yActual = y * Math.cos(dihedralRad)

    // Generate airfoil points for this section
    const airfoil = generateAirfoilPoints(tc, numChordPoints)
    const sectionPoints: THREE.Vector3[] = []

    for (const pt of airfoil) {
      // Scale by chord
      const x = pt.x * chord
      const z = pt.y * chord

      // Apply twist (rotate around quarter chord)
      const quarterChord = 0.25 * chord
      const dx = x - quarterChord
      const rotatedX = quarterChord + dx * Math.cos(twist) + z * Math.sin(twist)
      const rotatedZ = -dx * Math.sin(twist) + z * Math.cos(twist)

      // Add offsets
      sectionPoints.push(new THREE.Vector3(
        rotatedX + xOffset,
        yActual,
        rotatedZ + zOffset
      ))
    }

    sections.push(sectionPoints)
  }

  // Create mesh from sections
  const pointsPerSection = sections[0].length

  for (let i = 0; i < sections.length; i++) {
    for (let j = 0; j < pointsPerSection; j++) {
      const pt = sections[i][j]
      vertices.push(pt.x, pt.y, pt.z)
    }
  }

  // Create faces
  for (let i = 0; i < sections.length - 1; i++) {
    for (let j = 0; j < pointsPerSection; j++) {
      const nextJ = (j + 1) % pointsPerSection

      const a = i * pointsPerSection + j
      const b = i * pointsPerSection + nextJ
      const c = (i + 1) * pointsPerSection + nextJ
      const d = (i + 1) * pointsPerSection + j

      // Two triangles per quad
      indices.push(a, b, c)
      indices.push(a, c, d)
    }
  }

  // Create tip cap
  const tipCenter = new THREE.Vector3(0, 0, 0)
  const tipSection = sections[sections.length - 1]
  for (const pt of tipSection) {
    tipCenter.add(pt)
  }
  tipCenter.divideScalar(tipSection.length)

  const tipCenterIdx = vertices.length / 3
  vertices.push(tipCenter.x, tipCenter.y, tipCenter.z)

  const lastSectionStart = (sections.length - 1) * pointsPerSection
  for (let j = 0; j < pointsPerSection; j++) {
    const nextJ = (j + 1) % pointsPerSection
    indices.push(lastSectionStart + j, lastSectionStart + nextJ, tipCenterIdx)
  }

  const geometry = new THREE.BufferGeometry()
  geometry.setAttribute('position', new THREE.Float32BufferAttribute(vertices, 3))
  geometry.setIndex(indices)
  geometry.computeVertexNormals()

  return geometry
}

// Selectable mesh component
function SelectableMesh({
  geometry,
  position,
  rotation,
  color,
  selectedColor,
  isSelected,
  onClick,
}: {
  geometry: THREE.BufferGeometry
  position: [number, number, number]
  rotation?: [number, number, number]
  color: string
  selectedColor: string
  isSelected: boolean
  onClick: () => void
}) {
  const [hovered, setHovered] = useState(false)
  const meshRef = useRef<THREE.Mesh>(null)

  return (
    <mesh
      ref={meshRef}
      geometry={geometry}
      position={position}
      rotation={rotation}
      onClick={(e) => {
        e.stopPropagation()
        onClick()
      }}
      onPointerOver={(e) => {
        e.stopPropagation()
        setHovered(true)
        document.body.style.cursor = 'pointer'
      }}
      onPointerOut={() => {
        setHovered(false)
        document.body.style.cursor = 'auto'
      }}
    >
      <meshStandardMaterial
        color={isSelected ? selectedColor : hovered ? '#88bbee' : color}
        metalness={0.4}
        roughness={0.5}
        side={THREE.DoubleSide}
        emissive={isSelected ? selectedColor : '#000000'}
        emissiveIntensity={isSelected ? 0.2 : 0}
      />
    </mesh>
  )
}

function Fuselage({ isSelected, onSelect }: { isSelected: boolean; onSelect: () => void }) {
  const { body } = useAircraftStore()

  const geometry = useMemo(() => {
    const points: THREE.Vector2[] = []
    for (let i = 0; i < body.x.length; i++) {
      points.push(new THREE.Vector2(body.r[i], body.x[i]))
    }
    return new THREE.LatheGeometry(points, 48)
  }, [body.x, body.r])

  return (
    <SelectableMesh
      geometry={geometry}
      position={[0, 0, 0]}
      rotation={[0, 0, Math.PI / 2]}
      color="#5a9fd4"
      selectedColor="#ff9900"
      isSelected={isSelected}
      onClick={onSelect}
    />
  )
}

function Wing({ isSelected, onSelect }: { isSelected: boolean; onSelect: () => void }) {
  const { wing, wingSection, synthesis } = useAircraftStore()

  const geometryRight = useMemo(() => {
    return createWingGeometry(
      wing.chrdr,
      wing.chrdtp,
      wing.sspn,
      wing.savsi,
      wing.dhdadi ?? 0,
      wing.twista ?? 0,
      wingSection.tovc,
      wingSection.tovco ?? wingSection.tovc
    )
  }, [wing, wingSection])

  const geometryLeft = useMemo(() => {
    const geom = geometryRight.clone()
    const positions = geom.attributes.position
    for (let i = 0; i < positions.count; i++) {
      positions.setY(i, -positions.getY(i))
    }
    positions.needsUpdate = true
    geom.computeVertexNormals()
    return geom
  }, [geometryRight])

  const xPos = synthesis.xw ?? 3
  const zPos = synthesis.zw ?? 0

  return (
    <group>
      <SelectableMesh
        geometry={geometryRight}
        position={[xPos, 0, zPos]}
        color="#4a8bc4"
        selectedColor="#ff9900"
        isSelected={isSelected}
        onClick={onSelect}
      />
      <SelectableMesh
        geometry={geometryLeft}
        position={[xPos, 0, zPos]}
        color="#4a8bc4"
        selectedColor="#ff9900"
        isSelected={isSelected}
        onClick={onSelect}
      />
    </group>
  )
}

function HorizontalTail({ isSelected, onSelect }: { isSelected: boolean; onSelect: () => void }) {
  const { hTail, hTailSection, synthesis, enableHTail } = useAircraftStore()

  const geometryRight = useMemo(() => {
    if (!hTail || !hTailSection) return null
    return createWingGeometry(
      hTail.chrdr,
      hTail.chrdtp,
      hTail.sspn,
      hTail.savsi,
      hTail.dhdadi ?? 0,
      hTail.twista ?? 0,
      hTailSection.tovc,
      hTailSection.tovco ?? hTailSection.tovc
    )
  }, [hTail, hTailSection])

  const geometryLeft = useMemo(() => {
    if (!geometryRight) return null
    const geom = geometryRight.clone()
    const positions = geom.attributes.position
    for (let i = 0; i < positions.count; i++) {
      positions.setY(i, -positions.getY(i))
    }
    positions.needsUpdate = true
    geom.computeVertexNormals()
    return geom
  }, [geometryRight])

  if (!enableHTail || !geometryRight || !geometryLeft) return null

  const xPos = synthesis.xh ?? 8
  const zPos = synthesis.zh ?? 0

  return (
    <group>
      <SelectableMesh
        geometry={geometryRight}
        position={[xPos, 0, zPos]}
        color="#6b9fd4"
        selectedColor="#ff9900"
        isSelected={isSelected}
        onClick={onSelect}
      />
      <SelectableMesh
        geometry={geometryLeft}
        position={[xPos, 0, zPos]}
        color="#6b9fd4"
        selectedColor="#ff9900"
        isSelected={isSelected}
        onClick={onSelect}
      />
    </group>
  )
}

function VerticalTail({ isSelected, onSelect }: { isSelected: boolean; onSelect: () => void }) {
  const { vTail, vTailSection, synthesis, enableVTail, body } = useAircraftStore()

  const geometry = useMemo(() => {
    if (!vTail || !vTailSection) return null
    const geom = createWingGeometry(
      vTail.chrdr,
      vTail.chrdtp,
      vTail.sspn,
      vTail.savsi,
      0,
      0,
      vTailSection.tovc,
      vTailSection.tovco ?? vTailSection.tovc
    )

    const positions = geom.attributes.position
    for (let i = 0; i < positions.count; i++) {
      const y = positions.getY(i)
      const z = positions.getZ(i)
      positions.setY(i, z)
      positions.setZ(i, y)
    }
    positions.needsUpdate = true
    geom.computeVertexNormals()
    return geom
  }, [vTail, vTailSection])

  if (!enableVTail || !geometry) return null

  const xPos = synthesis.xv ?? 8
  const maxRadius = Math.max(...body.r)

  return (
    <SelectableMesh
      geometry={geometry}
      position={[xPos, 0, maxRadius]}
      color="#7ab0e0"
      selectedColor="#ff9900"
      isSelected={isSelected}
      onClick={onSelect}
    />
  )
}

function Axes() {
  return (
    <group>
      <mesh position={[6, 0, 0]} rotation={[0, 0, -Math.PI / 2]}>
        <cylinderGeometry args={[0.015, 0.015, 12, 8]} />
        <meshBasicMaterial color="#ff4444" />
      </mesh>
      <mesh position={[12.2, 0, 0]} rotation={[0, 0, -Math.PI / 2]}>
        <coneGeometry args={[0.06, 0.2, 8]} />
        <meshBasicMaterial color="#ff4444" />
      </mesh>
      <Text position={[12.8, 0, 0]} fontSize={0.3} color="#ff4444">X</Text>

      <mesh position={[0, 0, 4]}>
        <cylinderGeometry args={[0.015, 0.015, 8, 8]} />
        <meshBasicMaterial color="#44ff44" />
      </mesh>
      <mesh position={[0, 0, 8.2]}>
        <coneGeometry args={[0.06, 0.2, 8]} />
        <meshBasicMaterial color="#44ff44" />
      </mesh>
      <Text position={[0, 0, 8.8]} fontSize={0.3} color="#44ff44">Y</Text>

      <mesh position={[0, 2, 0]} rotation={[0, 0, 0]}>
        <cylinderGeometry args={[0.015, 0.015, 4, 8]} />
        <meshBasicMaterial color="#4444ff" />
      </mesh>
      <mesh position={[0, 4.2, 0]}>
        <coneGeometry args={[0.06, 0.2, 8]} />
        <meshBasicMaterial color="#4444ff" />
      </mesh>
      <Text position={[0, 4.8, 0]} fontSize={0.3} color="#4444ff">Z</Text>
    </group>
  )
}

function CenterOfGravity() {
  const { synthesis } = useAircraftStore()

  return (
    <group position={[synthesis.xcg, 0, synthesis.zcg]}>
      <mesh>
        <sphereGeometry args={[0.08, 16, 16]} />
        <meshBasicMaterial color="#ffcc00" />
      </mesh>
      <Html center distanceFactor={15}>
        <div className="text-xs bg-yellow-500/80 text-black px-1 rounded whitespace-nowrap">
          CG
        </div>
      </Html>
    </group>
  )
}

function ComponentInfo({ selected, viewMode }: { selected: string; viewMode: ViewMode }) {
  const store = useAircraftStore()

  // Show info based on view mode or selection
  const displayComponent = viewMode !== 'all' ? viewMode : selected

  if (displayComponent === 'none' || displayComponent === 'all') return null

  const info: { [key: string]: { title: string; data: { label: string; value: string }[] } } = {
    body: {
      title: 'Fuselage Analysis',
      data: [
        { label: 'Length', value: `${Math.max(...store.body.x).toFixed(2)} ft` },
        { label: 'Max Radius', value: `${Math.max(...store.body.r).toFixed(2)} ft` },
        { label: 'Fineness Ratio', value: `${(Math.max(...store.body.x) / (2 * Math.max(...store.body.r))).toFixed(2)}` },
        { label: 'Stations', value: `${store.body.nx}` },
        { label: 'Nose Type', value: store.body.bnose === 1 ? 'Conical' : store.body.bnose === 2 ? 'Ogive' : 'Power' },
        { label: 'Nose Length', value: `${store.body.bln.toFixed(2)} ft` },
        { label: 'Wetted Area', value: `≈ ${(2 * Math.PI * Math.max(...store.body.r) * Math.max(...store.body.x) * 0.8).toFixed(1)} ft²` },
      ]
    },
    fuselage: {
      title: 'Fuselage Analysis',
      data: [
        { label: 'Length', value: `${Math.max(...store.body.x).toFixed(2)} ft` },
        { label: 'Max Radius', value: `${Math.max(...store.body.r).toFixed(2)} ft` },
        { label: 'Fineness Ratio', value: `${(Math.max(...store.body.x) / (2 * Math.max(...store.body.r))).toFixed(2)}` },
        { label: 'Stations', value: `${store.body.nx}` },
        { label: 'Nose Type', value: store.body.bnose === 1 ? 'Conical' : store.body.bnose === 2 ? 'Ogive' : 'Power' },
      ]
    },
    wing: {
      title: 'Wing Analysis',
      data: [
        { label: 'Span', value: `${(store.wing.sspn * 2).toFixed(2)} ft` },
        { label: 'Root Chord', value: `${store.wing.chrdr.toFixed(2)} ft` },
        { label: 'Tip Chord', value: `${store.wing.chrdtp.toFixed(2)} ft` },
        { label: 'Mean Chord', value: `${((store.wing.chrdr + store.wing.chrdtp) / 2).toFixed(2)} ft` },
        { label: 'LE Sweep', value: `${store.wing.savsi.toFixed(1)}°` },
        { label: 'Dihedral', value: `${(store.wing.dhdadi ?? 0).toFixed(1)}°` },
        { label: 'Twist', value: `${(store.wing.twista ?? 0).toFixed(1)}°` },
        { label: 'Taper Ratio', value: `${(store.wing.chrdtp / store.wing.chrdr).toFixed(3)}` },
        { label: 'Aspect Ratio', value: `${((2 * store.wing.sspn) ** 2 / store.reference.sref).toFixed(2)}` },
        { label: 'Wing Area', value: `${store.reference.sref.toFixed(1)} ft²` },
        { label: 't/c Root', value: `${(store.wingSection.tovc * 100).toFixed(1)}%` },
        { label: 't/c Tip', value: `${((store.wingSection.tovco ?? store.wingSection.tovc) * 100).toFixed(1)}%` },
      ]
    },
    tail: {
      title: 'Tail Surfaces Analysis',
      data: store.hTail && store.vTail ? [
        { label: '--- H-Tail ---', value: '' },
        { label: 'Span', value: `${(store.hTail.sspn * 2).toFixed(2)} ft` },
        { label: 'Root Chord', value: `${store.hTail.chrdr.toFixed(2)} ft` },
        { label: 'LE Sweep', value: `${store.hTail.savsi.toFixed(1)}°` },
        { label: 'Volume Coeff.', value: `${((store.hTail.sspn * 2 * (store.hTail.chrdr + store.hTail.chrdtp) / 2 * ((store.synthesis.xh ?? 8) - store.synthesis.xcg)) / (store.reference.sref * store.reference.cbar)).toFixed(3)}` },
        { label: '--- V-Tail ---', value: '' },
        { label: 'Height', value: `${store.vTail.sspn.toFixed(2)} ft` },
        { label: 'Root Chord', value: `${store.vTail.chrdr.toFixed(2)} ft` },
        { label: 'LE Sweep', value: `${store.vTail.savsi.toFixed(1)}°` },
      ] : []
    },
    htail: {
      title: 'Horizontal Tail',
      data: store.hTail ? [
        { label: 'Span', value: `${(store.hTail.sspn * 2).toFixed(2)} ft` },
        { label: 'Root Chord', value: `${store.hTail.chrdr.toFixed(2)} ft` },
        { label: 'Sweep', value: `${store.hTail.savsi.toFixed(1)}°` },
        { label: 't/c', value: `${((store.hTailSection?.tovc ?? 0) * 100).toFixed(1)}%` },
      ] : []
    },
    vtail: {
      title: 'Vertical Tail',
      data: store.vTail ? [
        { label: 'Height', value: `${store.vTail.sspn.toFixed(2)} ft` },
        { label: 'Root Chord', value: `${store.vTail.chrdr.toFixed(2)} ft` },
        { label: 'Sweep', value: `${store.vTail.savsi.toFixed(1)}°` },
        { label: 't/c', value: `${((store.vTailSection?.tovc ?? 0) * 100).toFixed(1)}%` },
      ] : []
    }
  }

  const component = info[displayComponent]
  if (!component) return null

  return (
    <div className="absolute top-14 left-2 bg-background/95 border border-primary/50 rounded-lg p-3 text-sm max-w-[220px] shadow-lg">
      <h4 className="font-bold text-primary mb-2">{component.title}</h4>
      <div className="space-y-1">
        {component.data.map((item, i) => (
          <div key={i} className={`flex justify-between gap-2 ${item.value === '' ? 'border-t border-border pt-1 mt-1' : ''}`}>
            <span className={item.value === '' ? 'text-primary font-medium' : 'text-text-muted'}>{item.label}</span>
            <span className="font-mono text-text">{item.value}</span>
          </div>
        ))}
      </div>
    </div>
  )
}

function ViewModeSelector({ viewMode, onChange }: { viewMode: ViewMode; onChange: (mode: ViewMode) => void }) {
  const modes: { value: ViewMode; label: string; icon: React.ReactNode }[] = [
    { value: 'all', label: 'All', icon: <Eye className="w-4 h-4" /> },
    { value: 'wing', label: 'Wing', icon: <Plane className="w-4 h-4" /> },
    { value: 'body', label: 'Body', icon: <Box className="w-4 h-4" /> },
    { value: 'tail', label: 'Tail', icon: <ArrowUpFromLine className="w-4 h-4" /> },
  ]

  return (
    <div className="absolute top-2 left-2 flex gap-1 bg-background/90 rounded-lg p-1">
      {modes.map((mode) => (
        <button
          key={mode.value}
          onClick={() => onChange(mode.value)}
          className={`flex items-center gap-1 px-2 py-1 rounded text-xs transition-colors ${
            viewMode === mode.value
              ? 'bg-primary text-white'
              : 'text-text-muted hover:text-text hover:bg-surface-light'
          }`}
          title={`Show ${mode.label}`}
        >
          {mode.icon}
          <span>{mode.label}</span>
        </button>
      ))}
    </div>
  )
}

export function AircraftPreview() {
  const { body, enableHTail, enableVTail } = useAircraftStore()
  const maxX = Math.max(...body.x)
  const [selectedComponent, setSelectedComponent] = useState<'none' | 'fuselage' | 'wing' | 'htail' | 'vtail'>('none')
  const [viewMode, setViewMode] = useState<ViewMode>('all')

  // Calculate camera position based on view mode
  const getCameraTarget = () => {
    const store = useAircraftStore.getState()
    switch (viewMode) {
      case 'wing':
        return [(store.synthesis.xw ?? 3) + store.wing.chrdr / 2, 0, 0]
      case 'body':
        return [maxX / 2, 0, 0]
      case 'tail':
        return [(store.synthesis.xh ?? 8), 0, 0]
      default:
        return [maxX / 2, 0, 0]
    }
  }

  const showFuselage = viewMode === 'all' || viewMode === 'body'
  const showWing = viewMode === 'all' || viewMode === 'wing'
  const showHTail = (viewMode === 'all' || viewMode === 'tail') && enableHTail
  const showVTail = (viewMode === 'all' || viewMode === 'tail') && enableVTail

  return (
    <div className="w-full h-full min-h-[400px] rounded-lg overflow-hidden bg-gradient-to-b from-slate-900 to-slate-800 relative">
      <Canvas
        camera={{ position: [maxX * 1.5, maxX * 0.8, maxX * 1.2], fov: 45 }}
        onPointerMissed={() => setSelectedComponent('none')}
      >
        <ambientLight intensity={0.4} />
        <directionalLight position={[10, 10, 5]} intensity={1} castShadow />
        <directionalLight position={[-5, 5, -5]} intensity={0.3} />
        <pointLight position={[0, 5, 0]} intensity={0.2} />
        <hemisphereLight args={['#87ceeb', '#362312', 0.3]} />

        <group rotation={[0, 0, 0]}>
          {showFuselage && (
            <Fuselage
              isSelected={selectedComponent === 'fuselage'}
              onSelect={() => setSelectedComponent('fuselage')}
            />
          )}
          {showWing && (
            <Wing
              isSelected={selectedComponent === 'wing'}
              onSelect={() => setSelectedComponent('wing')}
            />
          )}
          {showHTail && (
            <HorizontalTail
              isSelected={selectedComponent === 'htail'}
              onSelect={() => setSelectedComponent('htail')}
            />
          )}
          {showVTail && (
            <VerticalTail
              isSelected={selectedComponent === 'vtail'}
              onSelect={() => setSelectedComponent('vtail')}
            />
          )}
          {viewMode === 'all' && <CenterOfGravity />}
          <Axes />
        </group>

        <Grid
          args={[30, 30]}
          cellSize={1}
          cellThickness={0.5}
          cellColor="#334155"
          sectionSize={5}
          sectionThickness={1}
          sectionColor="#475569"
          fadeDistance={40}
          position={[maxX / 2, -2, 0]}
          rotation={[Math.PI / 2, 0, 0]}
        />

        <OrbitControls
          enableDamping
          dampingFactor={0.05}
          minDistance={2}
          maxDistance={50}
          target={getCameraTarget() as [number, number, number]}
        />
        <Environment preset="city" />
      </Canvas>

      <ViewModeSelector viewMode={viewMode} onChange={setViewMode} />
      <ComponentInfo selected={selectedComponent} viewMode={viewMode} />

      <div className="absolute bottom-2 left-2 text-xs text-text-muted bg-background/80 px-2 py-1 rounded">
        Click to select | Drag: Rotate | Scroll: Zoom | Right-click: Pan
      </div>

      <div className="absolute top-2 right-2 text-xs bg-background/80 px-2 py-1 rounded space-y-1">
        {showFuselage && (
          <div className="flex items-center gap-2">
            <span className="w-3 h-3 rounded bg-[#5a9fd4]"></span>
            <span className="text-text-muted">Body</span>
          </div>
        )}
        {showWing && (
          <div className="flex items-center gap-2">
            <span className="w-3 h-3 rounded bg-[#4a8bc4]"></span>
            <span className="text-text-muted">Wing</span>
          </div>
        )}
        {showHTail && (
          <div className="flex items-center gap-2">
            <span className="w-3 h-3 rounded bg-[#6b9fd4]"></span>
            <span className="text-text-muted">H-Tail</span>
          </div>
        )}
        {showVTail && (
          <div className="flex items-center gap-2">
            <span className="w-3 h-3 rounded bg-[#7ab0e0]"></span>
            <span className="text-text-muted">V-Tail</span>
          </div>
        )}
        {viewMode === 'all' && (
          <div className="flex items-center gap-2">
            <span className="w-3 h-3 rounded-full bg-yellow-400"></span>
            <span className="text-text-muted">CG</span>
          </div>
        )}
      </div>
    </div>
  )
}
