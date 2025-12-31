import { Card } from './ui/Card'
import { Slider } from './ui/Slider'
import { useAircraftStore } from '../store/aircraft-store'
import { useState } from 'react'
import { ChevronDown, ChevronUp } from 'lucide-react'

const flapTypes = [
  { value: 1, label: 'Plain' },
  { value: 2, label: 'Single Slot' },
  { value: 3, label: 'Fowler' },
  { value: 4, label: 'Double Slot' },
  { value: 5, label: 'Split' },
  { value: 6, label: 'LE Flap' },
  { value: 7, label: 'LE Slat' },
  { value: 8, label: 'Krueger' },
]

export function SymmetricFlaps() {
  const { flaps, setFlaps, enableFlaps, setEnableFlaps } = useAircraftStore()
  const [showAdvanced, setShowAdvanced] = useState(false)

  return (
    <Card title="Symmetric Flaps">
      <div className="space-y-4">
        <label className="flex items-center gap-2 cursor-pointer">
          <input
            type="checkbox"
            checked={enableFlaps}
            onChange={(e) => setEnableFlaps(e.target.checked)}
            className="w-4 h-4 rounded border-border text-primary focus:ring-primary"
          />
          <span className="text-sm text-text">Enable Flaps</span>
        </label>

        {enableFlaps && flaps && (
          <>
            <div className="border-t border-border pt-4">
              <h4 className="text-xs font-semibold text-text-muted uppercase tracking-wide mb-3">Flap Type</h4>
              <div className="grid grid-cols-4 gap-2">
                {flapTypes.map(({ value, label }) => (
                  <button
                    key={value}
                    onClick={() => setFlaps({ ftype: value })}
                    className={`px-2 py-1.5 rounded text-xs transition-colors ${
                      flaps.ftype === value
                        ? 'bg-primary text-white'
                        : 'bg-surface-light text-text-muted hover:text-text'
                    }`}
                  >
                    {label}
                  </button>
                ))}
              </div>
            </div>

            <div className="border-t border-border pt-4">
              <h4 className="text-xs font-semibold text-text-muted uppercase tracking-wide mb-3">Span Location</h4>
              <Slider
                label="Inboard Span (%)"
                value={flaps.spanfi * 100}
                onChange={(v) => setFlaps({ spanfi: v / 100 })}
                min={0}
                max={50}
                step={1}
                unit="%"
              />
              <Slider
                label="Outboard Span (%)"
                value={flaps.spanfo * 100}
                onChange={(v) => setFlaps({ spanfo: v / 100 })}
                min={20}
                max={100}
                step={1}
                unit="%"
              />
            </div>

            <div className="border-t border-border pt-4">
              <h4 className="text-xs font-semibold text-text-muted uppercase tracking-wide mb-3">Chord Ratio</h4>
              <Slider
                label="Inboard Chord Ratio"
                value={flaps.chrdfi}
                onChange={(v) => setFlaps({ chrdfi: v })}
                min={0.1}
                max={0.4}
                step={0.01}
                unit=""
              />
              <Slider
                label="Outboard Chord Ratio"
                value={flaps.chrdfo}
                onChange={(v) => setFlaps({ chrdfo: v })}
                min={0.1}
                max={0.4}
                step={0.01}
                unit=""
              />
            </div>

            <div className="border-t border-border pt-4">
              <h4 className="text-xs font-semibold text-text-muted uppercase tracking-wide mb-3">Deflection Angles</h4>
              <div className="space-y-2">
                <div className="grid grid-cols-5 gap-2">
                  {flaps.delta.map((angle, i) => (
                    <div key={i} className="text-center">
                      <input
                        type="number"
                        value={angle}
                        onChange={(e) => {
                          const newDelta = [...flaps.delta]
                          newDelta[i] = parseFloat(e.target.value) || 0
                          setFlaps({ delta: newDelta })
                        }}
                        className="w-full px-2 py-1 text-sm text-center rounded bg-surface-light border border-border text-text focus:border-primary focus:outline-none"
                      />
                      <span className="text-xs text-text-muted">δ{i + 1}°</span>
                    </div>
                  ))}
                </div>
                <div className="flex gap-2">
                  <button
                    onClick={() => {
                      if (flaps.delta.length < 10) {
                        setFlaps({
                          delta: [...flaps.delta, flaps.delta[flaps.delta.length - 1] + 10],
                          ndelta: flaps.ndelta + 1
                        })
                      }
                    }}
                    className="text-xs text-primary hover:text-primary-light"
                  >
                    + Add Angle
                  </button>
                  <button
                    onClick={() => {
                      if (flaps.delta.length > 1) {
                        setFlaps({
                          delta: flaps.delta.slice(0, -1),
                          ndelta: flaps.ndelta - 1
                        })
                      }
                    }}
                    className="text-xs text-error hover:text-error/80"
                  >
                    - Remove
                  </button>
                </div>
              </div>
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
                <Slider
                  label="Trailing Edge Angle"
                  value={flaps.phete ?? 0.05}
                  onChange={(v) => setFlaps({ phete: v })}
                  min={0}
                  max={0.2}
                  step={0.01}
                  unit=""
                />
                {(flaps.ftype === 1 || flaps.ftype === 2) && (
                  <Slider
                    label="Balance Chord (Cb/C)"
                    value={flaps.cb ?? 0}
                    onChange={(v) => setFlaps({ cb: v })}
                    min={0}
                    max={0.3}
                    step={0.01}
                    unit=""
                  />
                )}
              </div>
            )}

            <div className="border-t border-border pt-4 text-sm text-text-muted">
              <p>Flap span: {((flaps.spanfo - flaps.spanfi) * 100).toFixed(0)}% of wing semi-span</p>
              <p>Avg chord ratio: {((flaps.chrdfi + flaps.chrdfo) / 2).toFixed(2)}</p>
            </div>
          </>
        )}
      </div>
    </Card>
  )
}
