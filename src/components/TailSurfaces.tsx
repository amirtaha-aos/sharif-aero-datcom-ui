import { Card } from './ui/Card'
import { Slider } from './ui/Slider'
import { useAircraftStore } from '../store/aircraft-store'
import { ChevronDown, ChevronUp } from 'lucide-react'
import { useState } from 'react'

export function HorizontalTail() {
  const { hTail, setHTail, hTailSection, setHTailSection, enableHTail, setEnableHTail, synthesis, setSynthesis } = useAircraftStore()
  const [showAdvanced, setShowAdvanced] = useState(false)

  return (
    <Card title="Horizontal Tail">
      <div className="space-y-4">
        <label className="flex items-center gap-2 cursor-pointer">
          <input
            type="checkbox"
            checked={enableHTail}
            onChange={(e) => setEnableHTail(e.target.checked)}
            className="w-4 h-4 rounded border-border text-primary focus:ring-primary"
          />
          <span className="text-sm text-text">Enable Horizontal Tail</span>
        </label>

        {enableHTail && hTail && hTailSection && (
          <>
            <div className="border-t border-border pt-4">
              <h4 className="text-xs font-semibold text-text-muted uppercase tracking-wide mb-3">Position</h4>
              <Slider
                label="X Position"
                value={synthesis.xh ?? 8}
                onChange={(v) => setSynthesis({ xh: v })}
                min={0}
                max={15}
                step={0.1}
                unit="ft"
              />
              <Slider
                label="Z Position"
                value={synthesis.zh ?? 0}
                onChange={(v) => setSynthesis({ zh: v })}
                min={-2}
                max={3}
                step={0.1}
                unit="ft"
              />
              <Slider
                label="Incidence Angle"
                value={synthesis.alih ?? 0}
                onChange={(v) => setSynthesis({ alih: v })}
                min={-10}
                max={10}
                step={0.5}
                unit="°"
              />
            </div>

            <div className="border-t border-border pt-4">
              <h4 className="text-xs font-semibold text-text-muted uppercase tracking-wide mb-3">Planform</h4>
              <Slider
                label="Root Chord"
                value={hTail.chrdr}
                onChange={(v) => setHTail({ chrdr: v })}
                min={0.3}
                max={3}
                step={0.05}
                unit="ft"
              />
              <Slider
                label="Tip Chord"
                value={hTail.chrdtp}
                onChange={(v) => setHTail({ chrdtp: v })}
                min={0.1}
                max={2}
                step={0.05}
                unit="ft"
              />
              <Slider
                label="Semi-Span"
                value={hTail.sspn}
                onChange={(v) => setHTail({ sspn: v })}
                min={0.5}
                max={5}
                step={0.1}
                unit="ft"
              />
              <Slider
                label="LE Sweep"
                value={hTail.savsi}
                onChange={(v) => setHTail({ savsi: v })}
                min={0}
                max={60}
                step={1}
                unit="°"
              />
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
                <h4 className="text-xs font-semibold text-text-muted uppercase tracking-wide mb-3">Section (HTSCHR)</h4>
                <Slider
                  label="Thickness Ratio (t/c)"
                  value={hTailSection.tovc}
                  onChange={(v) => setHTailSection({ tovc: v })}
                  min={0.04}
                  max={0.18}
                  step={0.01}
                  unit=""
                />
                <Slider
                  label="Max Thickness Location (x/c)"
                  value={hTailSection.xovc ?? 0.35}
                  onChange={(v) => setHTailSection({ xovc: v })}
                  min={0.2}
                  max={0.5}
                  step={0.01}
                  unit=""
                />
                <Slider
                  label="LE Radius Index"
                  value={hTailSection.leri ?? 0.008}
                  onChange={(v) => setHTailSection({ leri: v })}
                  min={0.002}
                  max={0.02}
                  step={0.001}
                  unit=""
                />
              </div>
            )}

            <div className="border-t border-border pt-4 text-sm text-text-muted">
              <div className="grid grid-cols-2 gap-2">
                <div>Aspect Ratio: <span className="text-text font-mono">
                  {((2 * hTail.sspn) ** 2 / (hTail.chrdr * hTail.sspn)).toFixed(2)}
                </span></div>
                <div>Taper Ratio: <span className="text-text font-mono">
                  {(hTail.chrdtp / hTail.chrdr).toFixed(2)}
                </span></div>
              </div>
            </div>
          </>
        )}
      </div>
    </Card>
  )
}

export function VerticalTail() {
  const { vTail, setVTail, vTailSection, setVTailSection, enableVTail, setEnableVTail, synthesis, setSynthesis } = useAircraftStore()
  const [showAdvanced, setShowAdvanced] = useState(false)

  return (
    <Card title="Vertical Tail">
      <div className="space-y-4">
        <label className="flex items-center gap-2 cursor-pointer">
          <input
            type="checkbox"
            checked={enableVTail}
            onChange={(e) => setEnableVTail(e.target.checked)}
            className="w-4 h-4 rounded border-border text-primary focus:ring-primary"
          />
          <span className="text-sm text-text">Enable Vertical Tail</span>
        </label>

        {enableVTail && vTail && vTailSection && (
          <>
            <div className="border-t border-border pt-4">
              <h4 className="text-xs font-semibold text-text-muted uppercase tracking-wide mb-3">Position</h4>
              <Slider
                label="X Position"
                value={synthesis.xv ?? 8}
                onChange={(v) => setSynthesis({ xv: v })}
                min={0}
                max={15}
                step={0.1}
                unit="ft"
              />
              <div className="flex items-center gap-4 mt-2">
                <span className="text-sm text-text-muted">Orientation:</span>
                <label className="flex items-center gap-2 cursor-pointer">
                  <input
                    type="radio"
                    checked={synthesis.vertup === true}
                    onChange={() => setSynthesis({ vertup: true })}
                    className="w-4 h-4 border-border text-primary focus:ring-primary"
                  />
                  <span className="text-sm text-text">Upper (Dorsal)</span>
                </label>
                <label className="flex items-center gap-2 cursor-pointer">
                  <input
                    type="radio"
                    checked={synthesis.vertup === false}
                    onChange={() => setSynthesis({ vertup: false })}
                    className="w-4 h-4 border-border text-primary focus:ring-primary"
                  />
                  <span className="text-sm text-text">Lower (Ventral)</span>
                </label>
              </div>
            </div>

            <div className="border-t border-border pt-4">
              <h4 className="text-xs font-semibold text-text-muted uppercase tracking-wide mb-3">Planform</h4>
              <Slider
                label="Root Chord"
                value={vTail.chrdr}
                onChange={(v) => setVTail({ chrdr: v })}
                min={0.5}
                max={4}
                step={0.1}
                unit="ft"
              />
              <Slider
                label="Tip Chord"
                value={vTail.chrdtp}
                onChange={(v) => setVTail({ chrdtp: v })}
                min={0.2}
                max={2}
                step={0.05}
                unit="ft"
              />
              <Slider
                label="Semi-Span (Height)"
                value={vTail.sspn}
                onChange={(v) => setVTail({ sspn: v })}
                min={0.3}
                max={3}
                step={0.1}
                unit="ft"
              />
              <Slider
                label="LE Sweep"
                value={vTail.savsi}
                onChange={(v) => setVTail({ savsi: v })}
                min={0}
                max={70}
                step={1}
                unit="°"
              />
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
                <h4 className="text-xs font-semibold text-text-muted uppercase tracking-wide mb-3">Section (VTSCHR)</h4>
                <Slider
                  label="Thickness Ratio (t/c)"
                  value={vTailSection.tovc}
                  onChange={(v) => setVTailSection({ tovc: v })}
                  min={0.04}
                  max={0.18}
                  step={0.01}
                  unit=""
                />
                <Slider
                  label="Max Thickness Location (x/c)"
                  value={vTailSection.xovc ?? 0.35}
                  onChange={(v) => setVTailSection({ xovc: v })}
                  min={0.2}
                  max={0.5}
                  step={0.01}
                  unit=""
                />
                <Slider
                  label="LE Radius Index"
                  value={vTailSection.leri ?? 0.010}
                  onChange={(v) => setVTailSection({ leri: v })}
                  min={0.002}
                  max={0.02}
                  step={0.001}
                  unit=""
                />
              </div>
            )}

            <div className="border-t border-border pt-4 text-sm text-text-muted">
              <div className="grid grid-cols-2 gap-2">
                <div>Aspect Ratio: <span className="text-text font-mono">
                  {((vTail.sspn) ** 2 / (0.5 * (vTail.chrdr + vTail.chrdtp) * vTail.sspn)).toFixed(2)}
                </span></div>
                <div>Taper Ratio: <span className="text-text font-mono">
                  {(vTail.chrdtp / vTail.chrdr).toFixed(2)}
                </span></div>
              </div>
            </div>
          </>
        )}
      </div>
    </Card>
  )
}
