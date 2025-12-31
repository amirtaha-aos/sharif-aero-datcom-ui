import { Card } from './ui/Card'
import { Slider } from './ui/Slider'
import { useAircraftStore } from '../store/aircraft-store'

export function FlightConditions() {
  const { flight, setFlight, reference, setReference } = useAircraftStore()

  return (
    <Card title="Flight Conditions">
      <div className="space-y-4">
        <Slider
          label="Mach Number"
          value={flight.mach[0]}
          onChange={(v) => setFlight({ mach: [v] })}
          min={0.1}
          max={3.0}
          step={0.05}
        />

        <Slider
          label="Reynolds Number"
          value={flight.reynolds[0] / 1e6}
          onChange={(v) => setFlight({ reynolds: [v * 1e6] })}
          min={0.5}
          max={50}
          step={0.5}
          unit="M"
        />

        <div className="border-t border-border pt-4 mt-4">
          <h4 className="text-sm font-medium text-text-muted mb-3">
            Reference Parameters
          </h4>

          <div className="space-y-3">
            <Slider
              label="Reference Area (Sref)"
              value={reference.sref}
              onChange={(v) => setReference({ sref: v })}
              min={1}
              max={100}
              step={0.5}
              unit="ft²"
            />

            <Slider
              label="Mean Aero Chord (Cbar)"
              value={reference.cbar}
              onChange={(v) => setReference({ cbar: v })}
              min={0.5}
              max={10}
              step={0.1}
              unit="ft"
            />

            <Slider
              label="Reference Span"
              value={reference.blref}
              onChange={(v) => setReference({ blref: v })}
              min={1}
              max={50}
              step={0.5}
              unit="ft"
            />
          </div>
        </div>

        <div className="border-t border-border pt-4">
          <h4 className="text-sm font-medium text-text-muted mb-2">
            Angle of Attack Range
          </h4>
          <div className="flex gap-2 text-sm text-text-muted font-mono">
            <span>Alpha: </span>
            <span>{flight.alpha[0]}° to {flight.alpha[flight.alpha.length - 1]}°</span>
            <span className="text-text-muted">({flight.alpha.length} points)</span>
          </div>
        </div>
      </div>
    </Card>
  )
}
