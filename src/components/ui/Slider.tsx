import { cn } from '../../lib/utils'

interface SliderProps {
  label: string
  value: number
  onChange: (value: number) => void
  min: number
  max: number
  step?: number
  unit?: string
  className?: string
}

export function Slider({
  label,
  value,
  onChange,
  min,
  max,
  step = 0.1,
  unit = '',
  className,
}: SliderProps) {
  return (
    <div className={cn('flex flex-col gap-1', className)}>
      <div className="flex justify-between text-sm">
        <label className="text-text-muted">{label}</label>
        <span className="text-text font-mono">
          {value.toFixed(2)} {unit}
        </span>
      </div>
      <input
        type="range"
        min={min}
        max={max}
        step={step}
        value={value}
        onChange={(e) => onChange(parseFloat(e.target.value))}
        className="w-full h-2 bg-surface-light rounded-lg appearance-none cursor-pointer accent-primary"
      />
    </div>
  )
}
