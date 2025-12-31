import { Card } from './ui/Card'
import { Button } from './ui/Button'
import { useAircraftStore } from '../store/aircraft-store'
import { useState, useRef } from 'react'
import {
  LineChart,
  Line,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  ResponsiveContainer,
  ReferenceLine,
} from 'recharts'
import { FileDown, Table, BarChart3, FileText, Loader } from 'lucide-react'
import jsPDF from 'jspdf'
import autoTable from 'jspdf-autotable'
import html2canvas from 'html2canvas'
import { writeTextFile, writeBinaryFile } from '@tauri-apps/api/fs'
import { save } from '@tauri-apps/api/dialog'
import { desktopDir } from '@tauri-apps/api/path'

// Extend jsPDF type for autoTable
declare module 'jspdf' {
  interface jsPDF {
    lastAutoTable: { finalY: number }
  }
}

function calculateDerivedValues(data: { alpha: number; cl: number; cd: number; cm: number }[]) {
  // Find CL max
  const clMax = Math.max(...data.map(d => d.cl))
  const clMaxAlpha = data.find(d => d.cl === clMax)?.alpha ?? 0

  // Find zero-lift angle (interpolate where CL crosses zero)
  let alpha0 = 0
  for (let i = 0; i < data.length - 1; i++) {
    if ((data[i].cl <= 0 && data[i + 1].cl >= 0) || (data[i].cl >= 0 && data[i + 1].cl <= 0)) {
      const slope = (data[i + 1].cl - data[i].cl) / (data[i + 1].alpha - data[i].alpha)
      alpha0 = data[i].alpha - data[i].cl / slope
      break
    }
  }

  // Calculate lift curve slope (CLa) from linear region
  const linearRegion = data.filter(d => d.alpha >= -2 && d.alpha <= 8)
  let cla = 0
  if (linearRegion.length >= 2) {
    const n = linearRegion.length
    const sumX = linearRegion.reduce((acc, d) => acc + d.alpha, 0)
    const sumY = linearRegion.reduce((acc, d) => acc + d.cl, 0)
    const sumXY = linearRegion.reduce((acc, d) => acc + d.alpha * d.cl, 0)
    const sumX2 = linearRegion.reduce((acc, d) => acc + d.alpha * d.alpha, 0)
    cla = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)
  }

  // Calculate L/D max
  const ldData = data.map(d => ({ ...d, ld: d.cd !== 0 ? d.cl / d.cd : 0 }))
  const ldMax = Math.max(...ldData.map(d => d.ld))
  const ldMaxAlpha = ldData.find(d => d.ld === ldMax)?.alpha ?? 0

  // Find minimum drag
  const cdMin = Math.min(...data.map(d => d.cd))
  const cdMinAlpha = data.find(d => d.cd === cdMin)?.alpha ?? 0

  // Oswald efficiency (approximate from drag polar)
  // CD = CD0 + CL^2 / (pi * AR * e)
  // We can estimate from the slope of CL^2 vs CD

  return {
    clMax,
    clMaxAlpha,
    alpha0,
    cla: cla * (180 / Math.PI), // Convert to per radian
    ldMax,
    ldMaxAlpha,
    cdMin,
    cdMinAlpha,
  }
}

export function ResultsTable() {
  const store = useAircraftStore()
  const { results } = store
  const [activeView, setActiveView] = useState<'table' | 'charts' | 'raw'>('table')
  const [exporting, setExporting] = useState(false)
  const [exportStatus, setExportStatus] = useState<string | null>(null)
  const chartsRef = useRef<HTMLDivElement>(null)
  const chart1Ref = useRef<HTMLDivElement>(null)
  const chart2Ref = useRef<HTMLDivElement>(null)
  const chart3Ref = useRef<HTMLDivElement>(null)
  const chart4Ref = useRef<HTMLDivElement>(null)

  const handleExportPDF = async () => {
    if (!results || results.cases.length === 0) return

    setExporting(true)
    setExportStatus('Generating PDF...')

    try {
      const doc = new jsPDF()
      const { reference, synthesis, wing, wingSection, flight } = useAircraftStore.getState()
      const pageWidth = doc.internal.pageSize.width
      const margin = 15
      let y = 20

      // Header with logo area
      doc.setFillColor(30, 41, 59) // Dark blue header
      doc.rect(0, 0, pageWidth, 35, 'F')

      doc.setFontSize(24)
      doc.setFont('helvetica', 'bold')
      doc.setTextColor(255)
      doc.text('SHARIF AERO', pageWidth / 2, 18, { align: 'center' })

      doc.setFontSize(11)
      doc.setFont('helvetica', 'normal')
      doc.setTextColor(148, 163, 184) // Slate gray
      doc.text('Aerodynamic Analysis Report', pageWidth / 2, 28, { align: 'center' })

      y = 45

      // Report info box
      doc.setFillColor(241, 245, 249) // Light gray
      doc.roundedRect(margin, y, pageWidth - 2 * margin, 18, 3, 3, 'F')
      doc.setFontSize(9)
      doc.setTextColor(71, 85, 105)
      doc.text(`Report Generated: ${new Date().toLocaleString()}`, margin + 5, y + 7)
      doc.text(`Analysis Tool: USAF Digital DATCOM`, margin + 5, y + 13)
      doc.text(`Mach Range: ${flight.mach[0]} - ${flight.mach[flight.mach.length - 1]}`, pageWidth - margin - 50, y + 7)
      doc.text(`Alpha Range: ${flight.alpha[0]}° - ${flight.alpha[flight.alpha.length - 1]}°`, pageWidth - margin - 50, y + 13)

      y += 28

      // Configuration Summary
      doc.setFontSize(14)
      doc.setFont('helvetica', 'bold')
      doc.setTextColor(59, 130, 246)
      doc.text('Aircraft Configuration', margin, y)
      doc.setDrawColor(59, 130, 246)
      doc.setLineWidth(0.5)
      doc.line(margin, y + 2, margin + 50, y + 2)
      y += 8

      autoTable(doc, {
        startY: y,
        head: [['Parameter', 'Value', 'Parameter', 'Value']],
        body: [
          ['Reference Area (Sref)', `${reference.sref.toFixed(2)} ft²`, 'MAC (cbar)', `${reference.cbar.toFixed(2)} ft`],
          ['Wingspan', `${reference.blref.toFixed(2)} ft`, 'Aspect Ratio', `${((2 * wing.sspn) ** 2 / reference.sref).toFixed(2)}`],
          ['CG Position X', `${synthesis.xcg.toFixed(2)} ft`, 'CG Position Z', `${synthesis.zcg.toFixed(2)} ft`],
          ['Wing Root Chord', `${wing.chrdr.toFixed(2)} ft`, 'Wing Tip Chord', `${wing.chrdtp.toFixed(2)} ft`],
          ['Wing Sweep (LE)', `${wing.savsi.toFixed(1)}°`, 'Wing Dihedral', `${(wing.dhdadi ?? 0).toFixed(1)}°`],
          ['Thickness Ratio', `${(wingSection.tovc * 100).toFixed(1)}%`, 'Wing Twist', `${(wing.twista ?? 0).toFixed(1)}°`],
        ],
        theme: 'grid',
        headStyles: { fillColor: [59, 130, 246], textColor: 255 },
        styles: { fontSize: 9, cellPadding: 3 },
        margin: { left: margin, right: margin },
      })
      y = doc.lastAutoTable.finalY + 10

      // Flight Conditions
      doc.setFontSize(14)
      doc.setFont('helvetica', 'bold')
      doc.setTextColor(34, 197, 94)
      doc.text('Flight Conditions', margin, y)
      doc.setDrawColor(34, 197, 94)
      doc.setLineWidth(0.5)
      doc.line(margin, y + 2, margin + 40, y + 2)
      y += 8

      autoTable(doc, {
        startY: y,
        head: [['Parameter', 'Values']],
        body: [
          ['Mach Numbers', flight.mach.join(', ')],
          ['Angles of Attack', flight.alpha.join(', ') + '°'],
          ['Reynolds Numbers', flight.reynolds.map(r => r.toExponential(2)).join(', ')],
        ],
        theme: 'grid',
        headStyles: { fillColor: [34, 197, 94], textColor: 255 },
        styles: { fontSize: 9, cellPadding: 3 },
        margin: { left: margin, right: margin },
      })
      y = doc.lastAutoTable.finalY + 10

      // Key Performance
      const derived = calculateDerivedValues(results.cases[0].coefficients)
      doc.setFontSize(14)
      doc.setFont('helvetica', 'bold')
      doc.setTextColor(239, 68, 68)
      doc.text('Key Aerodynamic Parameters', margin, y)
      doc.setDrawColor(239, 68, 68)
      doc.setLineWidth(0.5)
      doc.line(margin, y + 2, margin + 60, y + 2)
      y += 8

      autoTable(doc, {
        startY: y,
        head: [['Parameter', 'Value']],
        body: [
          ['Maximum Lift Coefficient (CLmax)', `${derived.clMax.toFixed(4)} at α = ${derived.clMaxAlpha.toFixed(1)}°`],
          ['Lift Curve Slope (CLα)', `${derived.cla.toFixed(4)} /rad`],
          ['Zero-Lift Angle (α0)', `${derived.alpha0.toFixed(2)}°`],
          ['Maximum L/D', `${derived.ldMax.toFixed(2)} at α = ${derived.ldMaxAlpha.toFixed(1)}°`],
          ['Minimum Drag (CDmin)', `${derived.cdMin.toFixed(5)} at α = ${derived.cdMinAlpha.toFixed(1)}°`],
        ],
        theme: 'striped',
        headStyles: { fillColor: [239, 68, 68], textColor: 255 },
        styles: { fontSize: 9, cellPadding: 3 },
        margin: { left: margin, right: margin },
      })
      y = doc.lastAutoTable.finalY + 15

      // Results for each case
      results.cases.forEach((caseData) => {
        if (y > 200) {
          doc.addPage()
          y = 20
        }

        doc.setFontSize(14)
        doc.setFont('helvetica', 'bold')
        doc.setTextColor(100, 116, 139)
        doc.text(`Aerodynamic Coefficients - Mach ${caseData.mach}`, margin, y)
        doc.setDrawColor(100, 116, 139)
        doc.setLineWidth(0.3)
        doc.line(margin, y + 2, margin + 70, y + 2)
        y += 8

        autoTable(doc, {
          startY: y,
          head: [['α (deg)', 'CL', 'CD', 'CM', 'CN', 'CA', 'L/D']],
          body: caseData.coefficients.map(row => [
            row.alpha.toFixed(1),
            row.cl.toFixed(4),
            row.cd.toFixed(5),
            row.cm.toFixed(4),
            row.cn.toFixed(4),
            row.ca.toFixed(5),
            row.cd !== 0 ? (row.cl / row.cd).toFixed(2) : '-',
          ]),
          theme: 'striped',
          headStyles: { fillColor: [59, 130, 246], textColor: 255 },
          styles: { fontSize: 8, cellPadding: 2, halign: 'center' },
          margin: { left: margin, right: margin },
        })
        y = doc.lastAutoTable.finalY + 15
      })

      // Capture charts - each on separate page
      setExportStatus('Capturing charts...')

      // Temporarily switch to charts view to capture them
      const previousView = activeView
      if (activeView !== 'charts') {
        setActiveView('charts')
        await new Promise(resolve => setTimeout(resolve, 500)) // Wait for render
      }

      const chartRefs = [
        { ref: chart1Ref, title: 'Lift Curve (CL vs α)', color: [59, 130, 246] },
        { ref: chart2Ref, title: 'Drag Polar (CL vs CD)', color: [34, 197, 94] },
        { ref: chart3Ref, title: 'Pitching Moment (CM vs α)', color: [234, 179, 8] },
        { ref: chart4Ref, title: 'Lift-to-Drag Ratio (L/D vs α)', color: [239, 68, 68] },
      ]

      for (const chart of chartRefs) {
        if (chart.ref.current) {
          doc.addPage()
          y = 20

          // Chart title with colored header
          doc.setFillColor(chart.color[0], chart.color[1], chart.color[2])
          doc.rect(0, 0, pageWidth, 25, 'F')
          doc.setFontSize(18)
          doc.setFont('helvetica', 'bold')
          doc.setTextColor(255)
          doc.text(chart.title, pageWidth / 2, 16, { align: 'center' })

          y = 35

          try {
            const canvas = await html2canvas(chart.ref.current, {
              backgroundColor: '#1e293b',
              scale: 2,
              logging: false,
            })
            const imgData = canvas.toDataURL('image/png')
            const imgWidth = pageWidth - 2 * margin
            const imgHeight = (canvas.height * imgWidth) / canvas.width

            // Center the chart vertically
            const chartY = Math.max(y, (doc.internal.pageSize.height - imgHeight) / 2 - 10)
            doc.addImage(imgData, 'PNG', margin, chartY, imgWidth, Math.min(imgHeight, 200))
          } catch (e) {
            console.log(`Could not capture ${chart.title}:`, e)
          }
        }
      }

      // Restore previous view
      if (previousView !== 'charts') {
        setActiveView(previousView)
      }

      // Add Raw DATCOM Output
      if (results.rawOutput) {
        setExportStatus('Adding DATCOM output...')
        doc.addPage()
        y = 20
        doc.setFontSize(16)
        doc.setFont('helvetica', 'bold')
        doc.setTextColor(239, 68, 68)
        doc.text('DATCOM Raw Output', margin, y)
        y += 5
        doc.setDrawColor(239, 68, 68)
        doc.setLineWidth(0.5)
        doc.line(margin, y, pageWidth - margin, y)
        y += 10

        doc.setFont('courier', 'normal')
        doc.setFontSize(6)
        doc.setTextColor(0)

        const rawLines = results.rawOutput.split('\n')
        const maxLinesPerPage = 80
        let lineCount = 0

        for (const line of rawLines) {
          if (lineCount >= maxLinesPerPage) {
            doc.addPage()
            y = 20
            lineCount = 0
          }

          // Truncate long lines
          const truncatedLine = line.length > 120 ? line.substring(0, 120) + '...' : line
          doc.text(truncatedLine, margin, y)
          y += 3
          lineCount++
        }
      }

      // Footer
      const totalPages = doc.getNumberOfPages()
      for (let i = 1; i <= totalPages; i++) {
        doc.setPage(i)
        const pageHeight = doc.internal.pageSize.height

        // Footer line
        doc.setDrawColor(200)
        doc.setLineWidth(0.3)
        doc.line(margin, pageHeight - 15, pageWidth - margin, pageHeight - 15)

        // Footer text
        doc.setFontSize(8)
        doc.setTextColor(120)
        doc.text(`Page ${i} of ${totalPages}`, margin, pageHeight - 8)
        doc.text('Sharif Aero - Aerodynamic Analysis Tool', pageWidth / 2, pageHeight - 8, { align: 'center' })
        doc.text('Sharif University', pageWidth - margin, pageHeight - 8, { align: 'right' })
      }

      // Save using Tauri dialog
      setExportStatus('Opening save dialog...')
      const pdfOutput = doc.output('arraybuffer')

      try {
        const defaultPath = await desktopDir()
        const filePath = await save({
          defaultPath: `${defaultPath}/sharif-aero-report.pdf`,
          filters: [{ name: 'PDF', extensions: ['pdf'] }]
        })

        if (filePath) {
          await writeBinaryFile(filePath, new Uint8Array(pdfOutput))
          setExportStatus(`✓ Saved: ${filePath.split('/').pop()}`)
          setTimeout(() => setExportStatus(null), 5000)
        } else {
          setExportStatus(null) // User cancelled
        }
      } catch (fsError) {
        // Fallback to browser download
        console.log('Tauri save failed, using browser download:', fsError)
        doc.save(`sharif-aero-report-${Date.now()}.pdf`)
        setExportStatus(`✓ Downloaded`)
        setTimeout(() => setExportStatus(null), 5000)
      }

    } catch (error) {
      console.error('PDF export error:', error)
      setExportStatus(`Error: ${error}`)
      setTimeout(() => setExportStatus(null), 5000)
    } finally {
      setExporting(false)
    }
  }

  const handleExportCSV = async () => {
    if (!results || results.cases.length === 0) return

    setExportStatus('Exporting CSV...')

    try {
      const data = results.cases[0].coefficients
      const lines = [
        '# Sharif Aero - Aerodynamic Analysis Results',
        `# Generated: ${new Date().toLocaleString()}`,
        `# Mach: ${results.cases[0].mach}`,
        '',
        'Alpha,CL,CD,CM,CN,CA,L/D',
        ...data.map(row => [
          row.alpha,
          row.cl.toFixed(6),
          row.cd.toFixed(6),
          row.cm.toFixed(6),
          row.cn.toFixed(6),
          row.ca.toFixed(6),
          row.cd !== 0 ? (row.cl / row.cd).toFixed(4) : 0,
        ].join(','))
      ]

      const csv = lines.join('\n')

      try {
        const defaultPath = await desktopDir()
        const filePath = await save({
          defaultPath: `${defaultPath}/sharif-aero-results.csv`,
          filters: [{ name: 'CSV', extensions: ['csv'] }]
        })

        if (filePath) {
          await writeTextFile(filePath, csv)
          setExportStatus(`✓ Saved: ${filePath.split('/').pop()}`)
          setTimeout(() => setExportStatus(null), 5000)
        } else {
          setExportStatus(null) // User cancelled
        }
      } catch (fsError) {
        // Fallback to browser download
        console.log('Tauri save failed, using browser download:', fsError)
        const blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' })
        const url = URL.createObjectURL(blob)
        const link = document.createElement('a')
        link.href = url
        link.download = `sharif-aero-results-${Date.now()}.csv`
        document.body.appendChild(link)
        link.click()
        document.body.removeChild(link)
        URL.revokeObjectURL(url)
        setExportStatus(`✓ Downloaded`)
        setTimeout(() => setExportStatus(null), 5000)
      }

    } catch (error) {
      console.error('CSV export error:', error)
      setExportStatus(`Error: ${error}`)
      setTimeout(() => setExportStatus(null), 5000)
    }
  }

  if (!results || results.cases.length === 0) {
    return (
      <Card title="Results">
        <div className="text-center py-12 text-text-muted">
          <BarChart3 className="w-12 h-12 mx-auto mb-4 opacity-50" />
          <p>No results yet.</p>
          <p className="text-sm mt-2">Run DATCOM analysis to see results here.</p>
        </div>
      </Card>
    )
  }

  const data = results.cases[0].coefficients
  const derived = calculateDerivedValues(data)
  const ldData = data.map(d => ({ ...d, ld: d.cd !== 0 ? d.cl / d.cd : 0 }))

  return (
    <div className="space-y-4">
      {/* Header with export buttons */}
      <div className="flex items-center justify-between">
        <div className="flex gap-2">
          <button
            onClick={() => setActiveView('table')}
            className={`px-3 py-1.5 rounded text-sm flex items-center gap-1 transition-colors ${
              activeView === 'table' ? 'bg-primary text-white' : 'bg-surface-light text-text-muted hover:text-text'
            }`}
          >
            <Table className="w-4 h-4" />
            Table
          </button>
          <button
            onClick={() => setActiveView('charts')}
            className={`px-3 py-1.5 rounded text-sm flex items-center gap-1 transition-colors ${
              activeView === 'charts' ? 'bg-primary text-white' : 'bg-surface-light text-text-muted hover:text-text'
            }`}
          >
            <BarChart3 className="w-4 h-4" />
            Charts
          </button>
          <button
            onClick={() => setActiveView('raw')}
            className={`px-3 py-1.5 rounded text-sm flex items-center gap-1 transition-colors ${
              activeView === 'raw' ? 'bg-primary text-white' : 'bg-surface-light text-text-muted hover:text-text'
            }`}
          >
            <FileText className="w-4 h-4" />
            Raw Output
          </button>
        </div>

        <div className="flex items-center gap-2">
          {exportStatus && (
            <span className={`text-sm px-3 py-1 rounded ${exportStatus.startsWith('✓') ? 'bg-success/20 text-success' : exportStatus.startsWith('Error') ? 'bg-error/20 text-error' : 'bg-primary/20 text-primary'}`}>
              {exportStatus}
            </span>
          )}
          <Button variant="ghost" size="sm" onClick={handleExportCSV} disabled={exporting}>
            <FileDown className="w-4 h-4" />
            Export CSV
          </Button>
          <Button variant="primary" size="sm" onClick={handleExportPDF} disabled={exporting}>
            {exporting ? <Loader className="w-4 h-4 animate-spin" /> : <FileDown className="w-4 h-4" />}
            Export PDF
          </Button>
        </div>
      </div>

      {/* Key Performance Metrics */}
      <Card title="Key Aerodynamic Parameters">
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
          <div className="bg-surface-light rounded-lg p-3">
            <div className="text-xs text-text-muted uppercase">CL Max</div>
            <div className="text-xl font-bold text-primary">{derived.clMax.toFixed(3)}</div>
            <div className="text-xs text-text-muted">at α = {derived.clMaxAlpha.toFixed(1)}°</div>
          </div>
          <div className="bg-surface-light rounded-lg p-3">
            <div className="text-xs text-text-muted uppercase">L/D Max</div>
            <div className="text-xl font-bold text-success">{derived.ldMax.toFixed(1)}</div>
            <div className="text-xs text-text-muted">at α = {derived.ldMaxAlpha.toFixed(1)}°</div>
          </div>
          <div className="bg-surface-light rounded-lg p-3">
            <div className="text-xs text-text-muted uppercase">CL_α</div>
            <div className="text-xl font-bold text-warning">{derived.cla.toFixed(3)}</div>
            <div className="text-xs text-text-muted">/rad</div>
          </div>
          <div className="bg-surface-light rounded-lg p-3">
            <div className="text-xs text-text-muted uppercase">CD Min</div>
            <div className="text-xl font-bold text-error">{derived.cdMin.toFixed(4)}</div>
            <div className="text-xs text-text-muted">at α = {derived.cdMinAlpha.toFixed(1)}°</div>
          </div>
        </div>
      </Card>

      {activeView === 'table' && (
        <Card title="Aerodynamic Coefficients">
          <div className="overflow-x-auto">
            <table className="w-full text-sm">
              <thead>
                <tr className="text-left text-text-muted border-b border-border">
                  <th className="pb-2 pr-4">α (deg)</th>
                  <th className="pb-2 pr-4">CL</th>
                  <th className="pb-2 pr-4">CD</th>
                  <th className="pb-2 pr-4">CM</th>
                  <th className="pb-2 pr-4">CN</th>
                  <th className="pb-2 pr-4">CA</th>
                  <th className="pb-2 pr-4">L/D</th>
                </tr>
              </thead>
              <tbody className="font-mono">
                {data.map((row, i) => (
                  <tr key={i} className="border-b border-border/50 hover:bg-surface-light/50">
                    <td className="py-2 pr-4">{row.alpha.toFixed(1)}</td>
                    <td className="py-2 pr-4">{row.cl.toFixed(4)}</td>
                    <td className="py-2 pr-4">{row.cd.toFixed(5)}</td>
                    <td className="py-2 pr-4">{row.cm.toFixed(4)}</td>
                    <td className="py-2 pr-4">{row.cn.toFixed(4)}</td>
                    <td className="py-2 pr-4">{row.ca.toFixed(5)}</td>
                    <td className="py-2 pr-4">
                      {row.cd !== 0 ? (row.cl / row.cd).toFixed(2) : '-'}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </Card>
      )}

      {activeView === 'charts' && (
        <div ref={chartsRef}>
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-4">
            <div ref={chart1Ref}>
            <Card title="Lift Curve (CL vs α)">
              <div className="h-72">
                <ResponsiveContainer width="100%" height="100%">
                  <LineChart data={data} margin={{ top: 5, right: 20, bottom: 25, left: 10 }}>
                    <CartesianGrid strokeDasharray="3 3" stroke="#334155" />
                    <XAxis
                      dataKey="alpha"
                      stroke="#94a3b8"
                      label={{ value: 'Angle of Attack (deg)', position: 'bottom', fill: '#94a3b8', offset: 0 }}
                    />
                    <YAxis
                      stroke="#94a3b8"
                      label={{ value: 'CL', angle: -90, position: 'insideLeft', fill: '#94a3b8' }}
                    />
                    <Tooltip
                      contentStyle={{
                        backgroundColor: '#1e293b',
                        border: '1px solid #475569',
                        borderRadius: '8px',
                      }}
                      formatter={(value) => typeof value === 'number' ? value.toFixed(4) : value}
                    />
                    <ReferenceLine y={0} stroke="#666" strokeDasharray="3 3" />
                    <ReferenceLine x={0} stroke="#666" strokeDasharray="3 3" />
                    <Line
                      type="monotone"
                      dataKey="cl"
                      stroke="#3b82f6"
                      strokeWidth={2}
                      dot={{ fill: '#3b82f6', r: 4 }}
                      name="CL"
                    />
                  </LineChart>
                </ResponsiveContainer>
              </div>
            </Card>
            </div>

            <div ref={chart2Ref}>
            <Card title="Drag Polar (CL vs CD)">
              <div className="h-72">
                <ResponsiveContainer width="100%" height="100%">
                  <LineChart data={data} margin={{ top: 5, right: 20, bottom: 25, left: 10 }}>
                    <CartesianGrid strokeDasharray="3 3" stroke="#334155" />
                    <XAxis
                      dataKey="cd"
                      stroke="#94a3b8"
                      tickFormatter={(v) => v.toFixed(3)}
                      label={{ value: 'CD', position: 'bottom', fill: '#94a3b8', offset: 0 }}
                    />
                    <YAxis
                      dataKey="cl"
                      stroke="#94a3b8"
                      label={{ value: 'CL', angle: -90, position: 'insideLeft', fill: '#94a3b8' }}
                    />
                    <Tooltip
                      contentStyle={{
                        backgroundColor: '#1e293b',
                        border: '1px solid #475569',
                        borderRadius: '8px',
                      }}
                      formatter={(value) => typeof value === 'number' ? value.toFixed(4) : value}
                    />
                    <Line
                      type="monotone"
                      dataKey="cl"
                      stroke="#22c55e"
                      strokeWidth={2}
                      dot={{ fill: '#22c55e', r: 4 }}
                      name="CL"
                    />
                  </LineChart>
                </ResponsiveContainer>
              </div>
            </Card>
            </div>

            <div ref={chart3Ref}>
            <Card title="Pitching Moment (CM vs α)">
              <div className="h-72">
                <ResponsiveContainer width="100%" height="100%">
                  <LineChart data={data} margin={{ top: 5, right: 20, bottom: 25, left: 10 }}>
                    <CartesianGrid strokeDasharray="3 3" stroke="#334155" />
                    <XAxis
                      dataKey="alpha"
                      stroke="#94a3b8"
                      label={{ value: 'Angle of Attack (deg)', position: 'bottom', fill: '#94a3b8', offset: 0 }}
                    />
                    <YAxis
                      stroke="#94a3b8"
                      label={{ value: 'CM', angle: -90, position: 'insideLeft', fill: '#94a3b8' }}
                    />
                    <Tooltip
                      contentStyle={{
                        backgroundColor: '#1e293b',
                        border: '1px solid #475569',
                        borderRadius: '8px',
                      }}
                      formatter={(value) => typeof value === 'number' ? value.toFixed(4) : value}
                    />
                    <ReferenceLine y={0} stroke="#666" strokeDasharray="3 3" />
                    <Line
                      type="monotone"
                      dataKey="cm"
                      stroke="#eab308"
                      strokeWidth={2}
                      dot={{ fill: '#eab308', r: 4 }}
                      name="CM"
                    />
                  </LineChart>
                </ResponsiveContainer>
              </div>
            </Card>
            </div>

            <div ref={chart4Ref}>
            <Card title="Lift-to-Drag Ratio (L/D vs α)">
              <div className="h-72">
                <ResponsiveContainer width="100%" height="100%">
                  <LineChart data={ldData} margin={{ top: 5, right: 20, bottom: 25, left: 10 }}>
                    <CartesianGrid strokeDasharray="3 3" stroke="#334155" />
                    <XAxis
                      dataKey="alpha"
                      stroke="#94a3b8"
                      label={{ value: 'Angle of Attack (deg)', position: 'bottom', fill: '#94a3b8', offset: 0 }}
                    />
                    <YAxis
                      stroke="#94a3b8"
                      label={{ value: 'L/D', angle: -90, position: 'insideLeft', fill: '#94a3b8' }}
                    />
                    <Tooltip
                      contentStyle={{
                        backgroundColor: '#1e293b',
                        border: '1px solid #475569',
                        borderRadius: '8px',
                      }}
                      formatter={(value) => typeof value === 'number' ? value.toFixed(2) : String(value)}
                    />
                    <ReferenceLine y={0} stroke="#666" strokeDasharray="3 3" />
                    <Line
                      type="monotone"
                      dataKey="ld"
                      stroke="#ef4444"
                      strokeWidth={2}
                      dot={{ fill: '#ef4444', r: 4 }}
                      name="L/D"
                    />
                  </LineChart>
                </ResponsiveContainer>
              </div>
            </Card>
            </div>
          </div>
        </div>
      )}

      {activeView === 'raw' && results.rawOutput && (
        <Card title="Raw DATCOM Output">
          <pre className="bg-surface-light rounded p-4 text-xs font-mono overflow-x-auto max-h-[500px] overflow-y-auto whitespace-pre">
            {results.rawOutput}
          </pre>
        </Card>
      )}
    </div>
  )
}
