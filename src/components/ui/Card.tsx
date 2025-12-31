import { cn } from '../../lib/utils'
import type { ReactNode } from 'react'

interface CardProps {
  children: ReactNode
  className?: string
  title?: string
}

export function Card({ children, className, title }: CardProps) {
  return (
    <div
      className={cn(
        'rounded-lg bg-surface border border-border p-4',
        className
      )}
    >
      {title && (
        <h3 className="text-lg font-semibold text-text mb-3">{title}</h3>
      )}
      {children}
    </div>
  )
}
