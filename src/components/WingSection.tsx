import { Card } from './ui/Card'
import { Slider } from './ui/Slider'
import { useAircraftStore } from '../store/aircraft-store'
import { useState, useMemo } from 'react'
import { ChevronDown, ChevronUp, Search, Check, Info } from 'lucide-react'

// NACA Airfoil Database
interface NACAProfile {
  name: string
  designation: string
  category: '4-digit' | '5-digit' | '6-series' | 'custom'
  thickness: number      // t/c
  maxThicknessLoc: number // x/c for max thickness
  maxCamber: number      // as percentage of chord
  maxCamberLoc: number   // x/c for max camber
  cli: number           // design lift coefficient
  description: string
}

const nacaDatabase: NACAProfile[] = [
  // NACA 4-digit series (symmetric)
  { name: 'NACA 0006', designation: '0006', category: '4-digit', thickness: 0.06, maxThicknessLoc: 0.30, maxCamber: 0, maxCamberLoc: 0, cli: 0, description: 'Thin symmetric, high-speed applications' },
  { name: 'NACA 0009', designation: '0009', category: '4-digit', thickness: 0.09, maxThicknessLoc: 0.30, maxCamber: 0, maxCamberLoc: 0, cli: 0, description: 'Symmetric, general purpose tail surfaces' },
  { name: 'NACA 0012', designation: '0012', category: '4-digit', thickness: 0.12, maxThicknessLoc: 0.30, maxCamber: 0, maxCamberLoc: 0, cli: 0, description: 'Classic symmetric airfoil, widely used reference' },
  { name: 'NACA 0015', designation: '0015', category: '4-digit', thickness: 0.15, maxThicknessLoc: 0.30, maxCamber: 0, maxCamberLoc: 0, cli: 0, description: 'Thick symmetric, high lift devices' },
  { name: 'NACA 0018', designation: '0018', category: '4-digit', thickness: 0.18, maxThicknessLoc: 0.30, maxCamber: 0, maxCamberLoc: 0, cli: 0, description: 'Very thick symmetric, low speed' },

  // NACA 4-digit series (cambered)
  { name: 'NACA 2412', designation: '2412', category: '4-digit', thickness: 0.12, maxThicknessLoc: 0.30, maxCamber: 0.02, maxCamberLoc: 0.40, cli: 0.25, description: 'Classic light aircraft wing, Cessna 172' },
  { name: 'NACA 2415', designation: '2415', category: '4-digit', thickness: 0.15, maxThicknessLoc: 0.30, maxCamber: 0.02, maxCamberLoc: 0.40, cli: 0.25, description: 'Medium thickness cambered, good all-around' },
  { name: 'NACA 4412', designation: '4412', category: '4-digit', thickness: 0.12, maxThicknessLoc: 0.30, maxCamber: 0.04, maxCamberLoc: 0.40, cli: 0.50, description: 'High lift, slow speed aircraft' },
  { name: 'NACA 4415', designation: '4415', category: '4-digit', thickness: 0.15, maxThicknessLoc: 0.30, maxCamber: 0.04, maxCamberLoc: 0.40, cli: 0.50, description: 'High lift thick airfoil, STOL aircraft' },
  { name: 'NACA 6409', designation: '6409', category: '4-digit', thickness: 0.09, maxThicknessLoc: 0.30, maxCamber: 0.06, maxCamberLoc: 0.40, cli: 0.70, description: 'Very high camber, extreme lift' },

  // NACA 5-digit series
  { name: 'NACA 23012', designation: '23012', category: '5-digit', thickness: 0.12, maxThicknessLoc: 0.30, maxCamber: 0.019, maxCamberLoc: 0.15, cli: 0.30, description: 'Popular 5-digit, good L/D, Beechcraft' },
  { name: 'NACA 23015', designation: '23015', category: '5-digit', thickness: 0.15, maxThicknessLoc: 0.30, maxCamber: 0.019, maxCamberLoc: 0.15, cli: 0.30, description: 'Thick 5-digit, general aviation' },
  { name: 'NACA 23018', designation: '23018', category: '5-digit', thickness: 0.18, maxThicknessLoc: 0.30, maxCamber: 0.019, maxCamberLoc: 0.15, cli: 0.30, description: 'Very thick 5-digit, root sections' },
  { name: 'NACA 24012', designation: '24012', category: '5-digit', thickness: 0.12, maxThicknessLoc: 0.30, maxCamber: 0.025, maxCamberLoc: 0.20, cli: 0.40, description: 'Higher lift 5-digit variant' },
  { name: 'NACA 25012', designation: '25012', category: '5-digit', thickness: 0.12, maxThicknessLoc: 0.30, maxCamber: 0.031, maxCamberLoc: 0.25, cli: 0.50, description: 'Maximum camber forward, STOL' },

  // NACA 6-series (laminar flow)
  { name: 'NACA 63-210', designation: '63-210', category: '6-series', thickness: 0.10, maxThicknessLoc: 0.35, maxCamber: 0.01, maxCamberLoc: 0.50, cli: 0.20, description: 'Laminar flow, P-51 Mustang root' },
  { name: 'NACA 64-210', designation: '64-210', category: '6-series', thickness: 0.10, maxThicknessLoc: 0.40, maxCamber: 0.01, maxCamberLoc: 0.50, cli: 0.20, description: 'Extended laminar flow, low drag' },
  { name: 'NACA 64A210', designation: '64A210', category: '6-series', thickness: 0.10, maxThicknessLoc: 0.40, maxCamber: 0.01, maxCamberLoc: 0.50, cli: 0.20, description: 'Modified 6-series, F-86 Sabre wing' },
  { name: 'NACA 65-210', designation: '65-210', category: '6-series', thickness: 0.10, maxThicknessLoc: 0.45, maxCamber: 0.01, maxCamberLoc: 0.50, cli: 0.20, description: 'Maximum laminar run, low drag' },
  { name: 'NACA 65-410', designation: '65-410', category: '6-series', thickness: 0.10, maxThicknessLoc: 0.45, maxCamber: 0.02, maxCamberLoc: 0.50, cli: 0.40, description: 'Cambered laminar flow section' },
  { name: 'NACA 66-210', designation: '66-210', category: '6-series', thickness: 0.10, maxThicknessLoc: 0.50, maxCamber: 0.01, maxCamberLoc: 0.50, cli: 0.20, description: 'Far aft max thickness, very low drag' },
]

// Calculate NACA 4-digit airfoil coordinates
function calculateNACA4Digit(designation: string, numPoints: number = 50): { x: number[], yUpper: number[], yLower: number[] } {
  const m = parseInt(designation[0]) / 100  // max camber
  const p = parseInt(designation[1]) / 10   // max camber position
  const t = parseInt(designation.slice(2)) / 100  // thickness

  const x: number[] = []
  const yUpper: number[] = []
  const yLower: number[] = []

  for (let i = 0; i <= numPoints; i++) {
    const beta = (i / numPoints) * Math.PI
    const xc = 0.5 * (1 - Math.cos(beta))

    // Thickness distribution
    const yt = 5 * t * (
      0.2969 * Math.sqrt(xc) -
      0.1260 * xc -
      0.3516 * xc * xc +
      0.2843 * xc * xc * xc -
      0.1015 * xc * xc * xc * xc
    )

    // Camber line
    let yc = 0
    let dyc = 0
    if (m > 0 && p > 0) {
      if (xc < p) {
        yc = (m / (p * p)) * (2 * p * xc - xc * xc)
        dyc = (2 * m / (p * p)) * (p - xc)
      } else {
        yc = (m / ((1 - p) * (1 - p))) * ((1 - 2 * p) + 2 * p * xc - xc * xc)
        dyc = (2 * m / ((1 - p) * (1 - p))) * (p - xc)
      }
    }

    const theta = Math.atan(dyc)

    x.push(xc)
    yUpper.push(yc + yt * Math.cos(theta))
    yLower.push(yc - yt * Math.cos(theta))
  }

  return { x, yUpper, yLower }
}

// Airfoil preview SVG component
function AirfoilPreview({ designation, category }: { designation: string; category: string }) {
  const coords = useMemo(() => {
    if (category === '4-digit' && designation.length === 4) {
      return calculateNACA4Digit(designation)
    }
    // Default symmetric for other types
    return calculateNACA4Digit('0012')
  }, [designation, category])

  const pathUpper = coords.x.map((xi, i) =>
    `${i === 0 ? 'M' : 'L'} ${(xi * 200 + 10).toFixed(1)} ${(60 - coords.yUpper[i] * 300).toFixed(1)}`
  ).join(' ')

  const pathLower = [...coords.x].reverse().map((xi, i) =>
    `L ${(xi * 200 + 10).toFixed(1)} ${(60 - coords.yLower[coords.x.length - 1 - i] * 300).toFixed(1)}`
  ).join(' ')

  return (
    <svg viewBox="0 0 220 80" className="w-full h-20 bg-surface-light rounded">
      <path
        d={`${pathUpper} ${pathLower} Z`}
        fill="rgba(59, 130, 246, 0.3)"
        stroke="#3b82f6"
        strokeWidth="1.5"
      />
      {/* Chord line */}
      <line x1="10" y1="60" x2="210" y2="60" stroke="#94a3b8" strokeWidth="0.5" strokeDasharray="4,4" />
      {/* Leading edge marker */}
      <circle cx="10" cy="60" r="3" fill="#22c55e" />
      {/* Trailing edge marker */}
      <circle cx="210" cy="60" r="3" fill="#ef4444" />
    </svg>
  )
}

export function WingSection() {
  const { wingSection, setWingSection, synthesis, setSynthesis } = useAircraftStore()
  const [showAdvanced, setShowAdvanced] = useState(false)
  const [searchTerm, setSearchTerm] = useState('')
  const [selectedCategory, setSelectedCategory] = useState<string>('all')
  const [showSelector, setShowSelector] = useState(false)
  const [selectedAirfoil, setSelectedAirfoil] = useState<NACAProfile | null>(null)
  const [customDesignation, setCustomDesignation] = useState('')

  const filteredAirfoils = useMemo(() => {
    return nacaDatabase.filter(af => {
      const matchesSearch = af.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
                           af.description.toLowerCase().includes(searchTerm.toLowerCase())
      const matchesCategory = selectedCategory === 'all' || af.category === selectedCategory
      return matchesSearch && matchesCategory
    })
  }, [searchTerm, selectedCategory])

  const applyAirfoil = (profile: NACAProfile) => {
    setWingSection({
      tovc: profile.thickness,
      tovco: profile.thickness * 0.85, // Slightly thinner at tip
      xovc: profile.maxThicknessLoc,
      cli: profile.cli,
      camber: profile.maxCamber > 0,
      cmo: profile.maxCamber > 0 ? -0.02 * profile.cli : 0,
      leri: 1.1019 * profile.thickness * profile.thickness, // NACA LE radius formula
    })
    setSelectedAirfoil(profile)
    setShowSelector(false)
  }

  const parseCustomDesignation = () => {
    const d = customDesignation.trim()
    if (d.length === 4 && /^\d{4}$/.test(d)) {
      const thickness = parseInt(d.slice(2)) / 100
      const maxCamber = parseInt(d[0]) / 100
      const maxCamberLoc = parseInt(d[1]) / 10
      const profile: NACAProfile = {
        name: `NACA ${d}`,
        designation: d,
        category: '4-digit',
        thickness,
        maxThicknessLoc: 0.30,
        maxCamber,
        maxCamberLoc,
        cli: maxCamber * 10,
        description: 'Custom NACA 4-digit airfoil'
      }
      applyAirfoil(profile)
    }
  }

  return (
    <Card title="Wing Airfoil Section">
      <div className="space-y-4">
        {/* NACA Airfoil Selector */}
        <div className="bg-gradient-to-r from-primary/10 to-transparent p-3 rounded-lg border border-primary/30">
          <div className="flex items-center justify-between mb-2">
            <div className="flex items-center gap-2">
              <span className="text-sm font-semibold text-text">NACA Airfoil</span>
              {selectedAirfoil && (
                <span className="px-2 py-0.5 bg-primary/20 text-primary text-xs font-mono rounded">
                  {selectedAirfoil.name}
                </span>
              )}
            </div>
            <button
              onClick={() => setShowSelector(!showSelector)}
              className="text-sm text-primary hover:text-primary-light flex items-center gap-1"
            >
              {showSelector ? 'Close' : 'Select Airfoil'}
              {showSelector ? <ChevronUp className="w-4 h-4" /> : <ChevronDown className="w-4 h-4" />}
            </button>
          </div>

          {selectedAirfoil && (
            <div className="mb-3">
              <AirfoilPreview designation={selectedAirfoil.designation} category={selectedAirfoil.category} />
              <p className="text-xs text-text-muted mt-1">{selectedAirfoil.description}</p>
            </div>
          )}

          {showSelector && (
            <div className="space-y-3 mt-3 pt-3 border-t border-primary/20">
              {/* Search and filter */}
              <div className="flex gap-2">
                <div className="flex-1 relative">
                  <Search className="absolute left-2 top-1/2 -translate-y-1/2 w-4 h-4 text-text-muted" />
                  <input
                    type="text"
                    placeholder="Search airfoils..."
                    value={searchTerm}
                    onChange={(e) => setSearchTerm(e.target.value)}
                    className="w-full pl-8 pr-3 py-1.5 bg-surface text-sm rounded border border-border focus:border-primary focus:outline-none"
                  />
                </div>
                <select
                  value={selectedCategory}
                  onChange={(e) => setSelectedCategory(e.target.value)}
                  className="px-2 py-1.5 bg-surface text-sm rounded border border-border focus:border-primary focus:outline-none"
                >
                  <option value="all">All</option>
                  <option value="4-digit">4-Digit</option>
                  <option value="5-digit">5-Digit</option>
                  <option value="6-series">6-Series</option>
                </select>
              </div>

              {/* Custom designation input */}
              <div className="flex gap-2">
                <input
                  type="text"
                  placeholder="Enter NACA 4-digit (e.g., 2412)"
                  value={customDesignation}
                  onChange={(e) => setCustomDesignation(e.target.value.replace(/\D/g, '').slice(0, 4))}
                  className="flex-1 px-3 py-1.5 bg-surface text-sm font-mono rounded border border-border focus:border-primary focus:outline-none"
                  maxLength={4}
                />
                <button
                  onClick={parseCustomDesignation}
                  disabled={customDesignation.length !== 4}
                  className="px-3 py-1.5 bg-primary text-white text-sm rounded disabled:opacity-50 disabled:cursor-not-allowed hover:bg-primary-dark transition-colors"
                >
                  Apply
                </button>
              </div>

              {/* Airfoil grid */}
              <div className="max-h-64 overflow-y-auto space-y-1">
                {filteredAirfoils.map((af) => (
                  <button
                    key={af.designation}
                    onClick={() => applyAirfoil(af)}
                    className={`w-full text-left p-2 rounded transition-colors flex items-center gap-3 ${
                      selectedAirfoil?.designation === af.designation
                        ? 'bg-primary/20 border border-primary'
                        : 'bg-surface hover:bg-surface-light border border-transparent'
                    }`}
                  >
                    <div className="flex-1">
                      <div className="flex items-center gap-2">
                        <span className="font-mono text-sm text-text">{af.name}</span>
                        <span className={`px-1.5 py-0.5 text-xs rounded ${
                          af.category === '4-digit' ? 'bg-blue-500/20 text-blue-400' :
                          af.category === '5-digit' ? 'bg-green-500/20 text-green-400' :
                          'bg-purple-500/20 text-purple-400'
                        }`}>
                          {af.category}
                        </span>
                      </div>
                      <p className="text-xs text-text-muted mt-0.5">{af.description}</p>
                      <div className="flex gap-3 mt-1 text-xs text-text-muted">
                        <span>t/c: {(af.thickness * 100).toFixed(0)}%</span>
                        {af.maxCamber > 0 && <span>Camber: {(af.maxCamber * 100).toFixed(1)}%</span>}
                        {af.cli > 0 && <span>Cl: {af.cli.toFixed(2)}</span>}
                      </div>
                    </div>
                    {selectedAirfoil?.designation === af.designation && (
                      <Check className="w-5 h-5 text-primary" />
                    )}
                  </button>
                ))}
              </div>
            </div>
          )}
        </div>

        {/* Info box about NACA series */}
        <div className="flex items-start gap-2 p-2 bg-surface-light rounded text-xs text-text-muted">
          <Info className="w-4 h-4 mt-0.5 flex-shrink-0 text-primary" />
          <div>
            <p><strong>4-digit:</strong> First digit = max camber (%), second = location (/10), last two = thickness (%)</p>
            <p className="mt-1"><strong>5-digit:</strong> Designed for specific lift coefficient, better stall characteristics</p>
            <p className="mt-1"><strong>6-series:</strong> Laminar flow airfoils, lower drag at design conditions</p>
          </div>
        </div>

        <div className="border-t border-border pt-4">
          <h4 className="text-xs font-semibold text-text-muted uppercase tracking-wide mb-3">Position & Incidence</h4>
          <Slider
            label="Wing X Position"
            value={synthesis.xw ?? 3}
            onChange={(v) => setSynthesis({ xw: v })}
            min={0}
            max={10}
            step={0.1}
            unit="ft"
          />
          <Slider
            label="Wing Z Position"
            value={synthesis.zw ?? 0}
            onChange={(v) => setSynthesis({ zw: v })}
            min={-2}
            max={2}
            step={0.1}
            unit="ft"
          />
          <Slider
            label="Wing Incidence"
            value={synthesis.aliw ?? 2}
            onChange={(v) => setSynthesis({ aliw: v })}
            min={-5}
            max={10}
            step={0.5}
            unit="°"
          />
        </div>

        <div className="border-t border-border pt-4">
          <h4 className="text-xs font-semibold text-text-muted uppercase tracking-wide mb-3">Thickness Distribution</h4>
          <Slider
            label="Root Thickness (t/c)"
            value={wingSection.tovc}
            onChange={(v) => setWingSection({ tovc: v })}
            min={0.04}
            max={0.25}
            step={0.005}
            unit=""
          />
          <Slider
            label="Tip Thickness (t/c)"
            value={wingSection.tovco ?? wingSection.tovc}
            onChange={(v) => setWingSection({ tovco: v })}
            min={0.04}
            max={0.20}
            step={0.005}
            unit=""
          />
          <Slider
            label="Max Thickness Location (x/c)"
            value={wingSection.xovc ?? 0.35}
            onChange={(v) => setWingSection({ xovc: v })}
            min={0.2}
            max={0.5}
            step={0.01}
            unit=""
          />
        </div>

        <div className="border-t border-border pt-4">
          <h4 className="text-xs font-semibold text-text-muted uppercase tracking-wide mb-3">Camber</h4>
          <label className="flex items-center gap-2 cursor-pointer mb-3">
            <input
              type="checkbox"
              checked={wingSection.camber ?? false}
              onChange={(e) => setWingSection({ camber: e.target.checked })}
              className="w-4 h-4 rounded border-border text-primary focus:ring-primary"
            />
            <span className="text-sm text-text">Include Camber Effects</span>
          </label>

          {wingSection.camber && (
            <>
              <Slider
                label="Design Lift Coefficient"
                value={wingSection.cli ?? 0.3}
                onChange={(v) => setWingSection({ cli: v })}
                min={0}
                max={1.0}
                step={0.05}
                unit=""
              />
              <Slider
                label="Ideal Angle of Attack"
                value={wingSection.alphai ?? 2}
                onChange={(v) => setWingSection({ alphai: v })}
                min={0}
                max={10}
                step={0.5}
                unit="°"
              />
              <Slider
                label="Zero-Lift Moment (Cm0)"
                value={wingSection.cmo ?? -0.05}
                onChange={(v) => setWingSection({ cmo: v })}
                min={-0.2}
                max={0.1}
                step={0.01}
                unit=""
              />
            </>
          )}
        </div>

        <button
          onClick={() => setShowAdvanced(!showAdvanced)}
          className="flex items-center gap-1 text-sm text-primary hover:text-primary-light"
        >
          {showAdvanced ? <ChevronUp className="w-4 h-4" /> : <ChevronDown className="w-4 h-4" />}
          {showAdvanced ? 'Hide' : 'Show'} Advanced Parameters
        </button>

        {showAdvanced && (
          <div className="border-t border-border pt-4 space-y-4">
            <h4 className="text-xs font-semibold text-text-muted uppercase tracking-wide mb-3">Leading Edge</h4>
            <Slider
              label="LE Radius Index (Root)"
              value={wingSection.leri ?? 0.012}
              onChange={(v) => setWingSection({ leri: v })}
              min={0.002}
              max={0.03}
              step={0.001}
              unit=""
            />
            <Slider
              label="LE Radius Index (Tip)"
              value={wingSection.lero ?? wingSection.leri ?? 0.012}
              onChange={(v) => setWingSection({ lero: v })}
              min={0.002}
              max={0.03}
              step={0.001}
              unit=""
            />

            <div className="bg-surface-light rounded p-3 text-sm text-text-muted">
              <p className="font-medium text-text mb-1">Leading Edge Radius Index</p>
              <p>For NACA 4-digit: LEr ≈ 1.1019 × (t/c)²</p>
              <p className="mt-1">Typical values:</p>
              <ul className="list-disc list-inside ml-2">
                <li>Sharp LE: 0.002 - 0.005</li>
                <li>Round LE: 0.010 - 0.020</li>
                <li>Blunt LE: 0.025 - 0.030</li>
              </ul>
            </div>
          </div>
        )}

        <div className="border-t border-border pt-4 text-sm text-text-muted">
          <div className="grid grid-cols-2 gap-2">
            <div>Root t/c: <span className="text-text font-mono">{(wingSection.tovc * 100).toFixed(1)}%</span></div>
            <div>Tip t/c: <span className="text-text font-mono">{((wingSection.tovco ?? wingSection.tovc) * 100).toFixed(1)}%</span></div>
          </div>
        </div>
      </div>
    </Card>
  )
}
