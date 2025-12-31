import { useState, useRef } from 'react'
import { invoke } from '@tauri-apps/api/tauri'
import { FlightConditions } from './components/FlightConditions'
import { BodyGeometry } from './components/BodyGeometry'
import { WingPlanform } from './components/WingPlanform'
import { WingSection } from './components/WingSection'
import { HorizontalTail, VerticalTail } from './components/TailSurfaces'
import { SymmetricFlaps } from './components/Flaps'
import { AircraftPreview } from './components/AircraftPreview'
import { ResultsTable } from './components/ResultsTable'
import { CADExport } from './components/CADExport'
import { Button } from './components/ui/Button'
import { useAircraftStore } from './store/aircraft-store'
import { generateDatcomInput } from './lib/input-generator'
import { parseDatcomInput, applyParsedInput } from './lib/input-parser'
import { Plane, Play, RotateCcw, FileText, Settings, Box, ChevronDown, ChevronRight, Upload, Download } from 'lucide-react'

type Section = 'flight' | 'body' | 'wing' | 'htail' | 'vtail' | 'flaps' | 'export'

function App() {
  const {
    flight,
    reference,
    synthesis,
    body,
    wing,
    wingSection,
    hTail,
    hTailSection,
    vTail,
    vTailSection,
    flaps,
    enableHTail,
    enableVTail,
    enableFlaps,
    activeTab,
    setActiveTab,
    isRunning,
    setIsRunning,
    setResults,
    reset,
    setFlight,
    setReference,
    setSynthesis,
    setBody,
    setWing,
    setWingSection,
    setHTail,
    setHTailSection,
    setVTail,
    setVTailSection,
    setFlaps,
    setEnableHTail,
    setEnableVTail,
    setEnableFlaps,
  } = useAircraftStore()

  const [error, setError] = useState<string | null>(null)
  const [expandedSections, setExpandedSections] = useState<Set<Section>>(new Set(['flight', 'wing']))
  const [importSuccess, setImportSuccess] = useState<string | null>(null)
  const fileInputRef = useRef<HTMLInputElement>(null)

  // Handle file import
  const handleImport = (event: React.ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0]
    if (!file) return

    const reader = new FileReader()
    reader.onload = (e) => {
      try {
        const content = e.target?.result as string
        const parsed = parseDatcomInput(content)

        applyParsedInput(
          parsed,
          setFlight,
          setReference,
          setSynthesis,
          setBody,
          setWing,
          setWingSection,
          setHTail,
          setHTailSection,
          setVTail,
          setVTailSection,
          setFlaps,
          setEnableHTail,
          setEnableVTail,
          setEnableFlaps,
        )

        setImportSuccess(`Imported: ${file.name}`)
        setTimeout(() => setImportSuccess(null), 3000)
        setError(null)
      } catch (err) {
        setError(`Import failed: ${(err as Error).message}`)
      }
    }
    reader.readAsText(file)

    // Reset file input
    if (fileInputRef.current) {
      fileInputRef.current.value = ''
    }
  }

  // Handle file export
  const handleExport = () => {
    const input = generateDatcomInput({
      flight,
      reference,
      synthesis,
      body,
      wing,
      wingSection,
      hTail: enableHTail ? hTail ?? undefined : undefined,
      hTailSection: enableHTail ? hTailSection ?? undefined : undefined,
      vTail: enableVTail ? vTail ?? undefined : undefined,
      vTailSection: enableVTail ? vTailSection ?? undefined : undefined,
      symFlap: enableFlaps ? flaps ?? undefined : undefined,
      caseid: 'DATCOM UI Analysis',
    })

    const blob = new Blob([input], { type: 'text/plain' })
    const url = URL.createObjectURL(blob)
    const a = document.createElement('a')
    a.href = url
    a.download = 'aircraft.inp'
    document.body.appendChild(a)
    a.click()
    document.body.removeChild(a)
    URL.revokeObjectURL(url)
  }

  const toggleSection = (section: Section) => {
    setExpandedSections(prev => {
      const next = new Set(prev)
      if (next.has(section)) {
        next.delete(section)
      } else {
        next.add(section)
      }
      return next
    })
  }

  const handleRun = async () => {
    setIsRunning(true)
    setError(null)

    try {
      const input = generateDatcomInput({
        flight,
        reference,
        synthesis,
        body,
        wing,
        wingSection,
        hTail: enableHTail ? hTail ?? undefined : undefined,
        hTailSection: enableHTail ? hTailSection ?? undefined : undefined,
        vTail: enableVTail ? vTail ?? undefined : undefined,
        vTailSection: enableVTail ? vTailSection ?? undefined : undefined,
        symFlap: enableFlaps ? flaps ?? undefined : undefined,
        caseid: 'DATCOM UI Analysis',
      })

      console.log('Generated input:', input)

      const result = await invoke<{
        cases: Array<{
          case_id: string
          mach: number
          reynolds: number
          coefficients: Array<{
            alpha: number
            cd: number
            cl: number
            cm: number
            cn: number
            ca: number
          }>
        }>
        raw_output: string
      }>('run_datcom', { inputContent: input })

      setResults({
        cases: result.cases.map((c) => ({
          caseId: c.case_id,
          mach: c.mach,
          reynolds: c.reynolds,
          coefficients: c.coefficients,
        })),
        rawOutput: result.raw_output,
      })

      setActiveTab('results')
    } catch (err) {
      setError(err as string)
      console.error('DATCOM error:', err)
    } finally {
      setIsRunning(false)
    }
  }

  const SectionHeader = ({ section, title, icon: Icon }: { section: Section; title: string; icon: React.ComponentType<{ className?: string }> }) => (
    <button
      onClick={() => toggleSection(section)}
      className="w-full flex items-center gap-2 px-3 py-2 bg-surface rounded-lg hover:bg-surface-light transition-colors"
    >
      {expandedSections.has(section) ? (
        <ChevronDown className="w-4 h-4 text-text-muted" />
      ) : (
        <ChevronRight className="w-4 h-4 text-text-muted" />
      )}
      <Icon className="w-4 h-4 text-primary" />
      <span className="font-medium text-text">{title}</span>
    </button>
  )

  return (
    <div className="min-h-screen bg-background">
      {/* Header */}
      <header className="bg-surface border-b border-border px-4 py-3">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-3">
            <Plane className="w-6 h-6 text-primary" />
            <h1 className="text-xl font-bold text-text">Sharif Aero</h1>
            <span className="text-xs text-text-muted bg-surface-light px-2 py-0.5 rounded">
              v1.0.0
            </span>
          </div>

          <div className="flex items-center gap-2">
            {/* Hidden file input */}
            <input
              ref={fileInputRef}
              type="file"
              accept=".inp,.dat,.txt"
              onChange={handleImport}
              className="hidden"
            />
            <Button variant="ghost" size="sm" onClick={() => fileInputRef.current?.click()}>
              <Upload className="w-4 h-4" />
              Import
            </Button>
            <Button variant="ghost" size="sm" onClick={handleExport}>
              <Download className="w-4 h-4" />
              Export
            </Button>
            <Button variant="ghost" size="sm" onClick={reset}>
              <RotateCcw className="w-4 h-4" />
              Reset
            </Button>
            <Button
              variant="primary"
              onClick={handleRun}
              disabled={isRunning}
            >
              <Play className="w-4 h-4" />
              {isRunning ? 'Running...' : 'Run Analysis'}
            </Button>
          </div>
        </div>
      </header>

      {/* Tab bar */}
      <div className="bg-surface border-b border-border px-4">
        <div className="flex gap-1">
          <button
            onClick={() => setActiveTab('input')}
            className={`px-4 py-2 text-sm font-medium transition-colors border-b-2 -mb-px ${
              activeTab === 'input'
                ? 'text-primary border-primary'
                : 'text-text-muted border-transparent hover:text-text'
            }`}
          >
            <Settings className="w-4 h-4 inline mr-2" />
            Configuration
          </button>
          <button
            onClick={() => setActiveTab('results')}
            className={`px-4 py-2 text-sm font-medium transition-colors border-b-2 -mb-px ${
              activeTab === 'results'
                ? 'text-primary border-primary'
                : 'text-text-muted border-transparent hover:text-text'
            }`}
          >
            <FileText className="w-4 h-4 inline mr-2" />
            Results
          </button>
          <button
            onClick={() => setActiveTab('cad')}
            className={`px-4 py-2 text-sm font-medium transition-colors border-b-2 -mb-px ${
              activeTab === 'cad'
                ? 'text-primary border-primary'
                : 'text-text-muted border-transparent hover:text-text'
            }`}
          >
            <Box className="w-4 h-4 inline mr-2" />
            CAD Export
          </button>
        </div>
      </div>

      {/* Error message */}
      {error && (
        <div className="bg-error/10 border border-error/30 text-error px-4 py-2 m-4 rounded-lg">
          {error}
        </div>
      )}

      {/* Import success message */}
      {importSuccess && (
        <div className="bg-success/10 border border-success/30 text-success px-4 py-2 m-4 rounded-lg">
          {importSuccess}
        </div>
      )}

      {/* Main content */}
      <main className="p-4">
        {activeTab === 'input' ? (
          <div className="grid grid-cols-1 lg:grid-cols-3 gap-4">
            {/* Left column: Core parameters */}
            <div className="space-y-2">
              <SectionHeader section="flight" title="Flight Conditions" icon={Settings} />
              {expandedSections.has('flight') && <FlightConditions />}

              <SectionHeader section="body" title="Body Geometry" icon={Box} />
              {expandedSections.has('body') && <BodyGeometry />}
            </div>

            {/* Middle column: Wing & Tails */}
            <div className="space-y-2">
              <SectionHeader section="wing" title="Wing" icon={Plane} />
              {expandedSections.has('wing') && (
                <>
                  <WingPlanform />
                  <WingSection />
                </>
              )}

              <SectionHeader section="htail" title="Horizontal Tail" icon={Plane} />
              {expandedSections.has('htail') && <HorizontalTail />}

              <SectionHeader section="vtail" title="Vertical Tail" icon={Plane} />
              {expandedSections.has('vtail') && <VerticalTail />}

              <SectionHeader section="flaps" title="Flaps & Control Surfaces" icon={Settings} />
              {expandedSections.has('flaps') && <SymmetricFlaps />}
            </div>

            {/* Right column: 3D Preview */}
            <div className="lg:sticky lg:top-4 h-fit space-y-4">
              <div className="rounded-lg border border-border overflow-hidden">
                <div className="bg-surface px-4 py-2 border-b border-border">
                  <h3 className="font-semibold text-text">3D Preview</h3>
                </div>
                <div className="h-[500px] relative">
                  <AircraftPreview />
                </div>
              </div>

              {/* Quick Stats */}
              <div className="rounded-lg border border-border bg-surface p-4">
                <h3 className="font-semibold text-text mb-3">Configuration Summary</h3>
                <div className="space-y-2 text-sm">
                  <div className="flex justify-between">
                    <span className="text-text-muted">Reference Area</span>
                    <span className="font-mono text-text">{reference.sref.toFixed(1)} ft²</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-text-muted">MAC</span>
                    <span className="font-mono text-text">{reference.cbar.toFixed(2)} ft</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-text-muted">Wingspan</span>
                    <span className="font-mono text-text">{(wing.sspn * 2).toFixed(1)} ft</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-text-muted">Wing AR</span>
                    <span className="font-mono text-text">{((2 * wing.sspn) ** 2 / reference.sref).toFixed(2)}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-text-muted">Mach Range</span>
                    <span className="font-mono text-text">{Math.min(...flight.mach).toFixed(2)} - {Math.max(...flight.mach).toFixed(2)}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-text-muted">Alpha Range</span>
                    <span className="font-mono text-text">{Math.min(...flight.alpha)}° - {Math.max(...flight.alpha)}°</span>
                  </div>
                  <div className="border-t border-border pt-2 mt-2">
                    <div className="flex justify-between">
                      <span className="text-text-muted">H-Tail</span>
                      <span className={enableHTail ? 'text-success' : 'text-text-muted'}>{enableHTail ? 'Enabled' : 'Disabled'}</span>
                    </div>
                    <div className="flex justify-between">
                      <span className="text-text-muted">V-Tail</span>
                      <span className={enableVTail ? 'text-success' : 'text-text-muted'}>{enableVTail ? 'Enabled' : 'Disabled'}</span>
                    </div>
                    <div className="flex justify-between">
                      <span className="text-text-muted">Flaps</span>
                      <span className={enableFlaps ? 'text-success' : 'text-text-muted'}>{enableFlaps ? 'Enabled' : 'Disabled'}</span>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        ) : activeTab === 'results' ? (
          <ResultsTable />
        ) : (
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-4">
            <CADExport />
            <div className="rounded-lg border border-border overflow-hidden h-[600px]">
              <div className="bg-surface px-4 py-2 border-b border-border">
                <h3 className="font-semibold text-text">3D Preview</h3>
              </div>
              <div className="h-full relative">
                <AircraftPreview />
              </div>
            </div>
          </div>
        )}
      </main>
    </div>
  )
}

export default App
