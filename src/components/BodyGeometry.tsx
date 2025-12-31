import { Card } from './ui/Card'
import { Slider } from './ui/Slider'
import { useAircraftStore } from '../store/aircraft-store'

export function BodyGeometry() {
  const { body, setBody } = useAircraftStore()

  const updateRadius = (index: number, value: number) => {
    const newR = [...body.r]
    newR[index] = value
    setBody({ r: newR })
  }

  return (
    <Card title="Body Geometry">
      <div className="space-y-4">
        <Slider
          label="Nose Length"
          value={body.bln}
          onChange={(v) => setBody({ bln: v })}
          min={0.5}
          max={5}
          step={0.1}
          unit="ft"
        />

        <Slider
          label="Afterbody Length"
          value={body.bla}
          onChange={(v) => setBody({ bla: v })}
          min={1}
          max={10}
          step={0.1}
          unit="ft"
        />

        <div className="border-t border-border pt-4">
          <h4 className="text-sm font-medium text-text-muted mb-2">Nose Type</h4>
          <div className="flex gap-2">
            {[
              { value: 1, label: 'Conical' },
              { value: 2, label: 'Ogive' },
              { value: 3, label: 'Power' },
            ].map(({ value, label }) => (
              <button
                key={value}
                onClick={() => setBody({ bnose: value as 1 | 2 | 3 })}
                className={`px-3 py-1.5 rounded text-sm transition-colors ${
                  body.bnose === value
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
          <h4 className="text-sm font-medium text-text-muted mb-3">
            Cross-Section Radii
          </h4>
          <div className="space-y-2">
            {body.r.map((radius, i) => (
              <Slider
                key={i}
                label={`Station ${i + 1} (x=${body.x[i].toFixed(1)})`}
                value={radius}
                onChange={(v) => updateRadius(i, v)}
                min={0}
                max={1.5}
                step={0.02}
                unit="ft"
              />
            ))}
          </div>
        </div>
      </div>
    </Card>
  )
}
