import { Card } from './ui/Card'
import { Slider } from './ui/Slider'
import { useAircraftStore } from '../store/aircraft-store'

export function WingPlanform() {
  const { wing, setWing } = useAircraftStore()

  return (
    <Card title="Wing Planform">
      <div className="space-y-4">
        <Slider
          label="Root Chord"
          value={wing.chrdr}
          onChange={(v) => setWing({ chrdr: v })}
          min={0.5}
          max={5}
          step={0.1}
          unit="ft"
        />

        <Slider
          label="Tip Chord"
          value={wing.chrdtp}
          onChange={(v) => setWing({ chrdtp: v })}
          min={0.2}
          max={3}
          step={0.05}
          unit="ft"
        />

        <Slider
          label="Semi-Span (Total)"
          value={wing.sspn}
          onChange={(v) => setWing({ sspn: v })}
          min={1}
          max={15}
          step={0.1}
          unit="ft"
        />

        <Slider
          label="Semi-Span (Exposed)"
          value={wing.sspne}
          onChange={(v) => setWing({ sspne: v })}
          min={0.5}
          max={wing.sspn}
          step={0.1}
          unit="ft"
        />

        <Slider
          label="Leading Edge Sweep"
          value={wing.savsi}
          onChange={(v) => setWing({ savsi: v })}
          min={0}
          max={70}
          step={1}
          unit="°"
        />

        <Slider
          label="Twist at Tip"
          value={wing.twista ?? 0}
          onChange={(v) => setWing({ twista: v })}
          min={-10}
          max={10}
          step={0.5}
          unit="°"
        />

        <Slider
          label="Dihedral (Inboard)"
          value={wing.dhdadi ?? 0}
          onChange={(v) => setWing({ dhdadi: v })}
          min={-10}
          max={20}
          step={0.5}
          unit="°"
        />

        <div className="border-t border-border pt-4">
          <h4 className="text-sm font-medium text-text-muted mb-2">Wing Type</h4>
          <div className="flex gap-2">
            {[
              { value: 1, label: 'Straight Taper' },
              { value: 2, label: 'Double Delta' },
              { value: 3, label: 'Cranked' },
            ].map(({ value, label }) => (
              <button
                key={value}
                onClick={() => setWing({ type: value as 1 | 2 | 3 })}
                className={`px-3 py-1.5 rounded text-sm transition-colors ${
                  wing.type === value
                    ? 'bg-primary text-white'
                    : 'bg-surface-light text-text-muted hover:text-text'
                }`}
              >
                {label}
              </button>
            ))}
          </div>
        </div>

        <div className="border-t border-border pt-4 text-sm text-text-muted">
          <div className="grid grid-cols-2 gap-2">
            <div>Aspect Ratio: <span className="text-text font-mono">
              {((2 * wing.sspn) ** 2 / (wing.chrdr * wing.sspn)).toFixed(2)}
            </span></div>
            <div>Taper Ratio: <span className="text-text font-mono">
              {(wing.chrdtp / wing.chrdr).toFixed(2)}
            </span></div>
          </div>
        </div>
      </div>
    </Card>
  )
}
