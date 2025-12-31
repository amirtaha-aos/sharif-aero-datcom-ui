import type { DatcomInput } from './datcom-types'

// Format number for DATCOM (Fortran) - always include decimal point
function formatNum(n: number): string {
  // Handle scientific notation - convert to Fortran format
  if (Math.abs(n) >= 1e6 || (Math.abs(n) < 0.001 && n !== 0)) {
    const exp = n.toExponential(2)
    return exp.replace('e+', 'E').replace('e-', 'E-').replace('e', 'E')
  }
  // Ensure decimal point for real numbers
  const str = n.toString()
  if (!str.includes('.') && !str.includes('E') && !str.includes('e')) {
    return str + '.0'
  }
  return str
}

function formatArray(arr: number[]): string {
  return arr.map(formatNum).join(',')
}

export function generateDatcomInput(input: DatcomInput): string {
  const lines: string[] = []

  // Build command for configuration buildup
  lines.push('BUILD')

  // Dimension
  lines.push(`DIM ${input.dim || 'FT'}`)

  // Flight conditions - compact format
  let fltcon = ` $FLTCON NMACH=${input.flight.mach.length}.0, MACH(1)=${formatArray(input.flight.mach)},`
  fltcon += `\n  NALPHA=${input.flight.alpha.length}.0, ALSCHD(1)=${formatArray(input.flight.alpha)},`
  fltcon += `\n  RNNUB(1)=${formatArray(input.flight.reynolds)}$`
  lines.push(fltcon)

  // Reference parameters - single line
  lines.push(` $OPTINS SREF=${formatNum(input.reference.sref)}, CBARR=${formatNum(input.reference.cbar)}, BLREF=${formatNum(input.reference.blref)}$`)

  // Synthesis - compact format
  let synth = ` $SYNTHS XCG=${formatNum(input.synthesis.xcg)}, ZCG=${formatNum(input.synthesis.zcg)}`
  if (input.synthesis.xw !== undefined) synth += `, XW=${formatNum(input.synthesis.xw)}`
  if (input.synthesis.zw !== undefined) synth += `, ZW=${formatNum(input.synthesis.zw)}`
  if (input.synthesis.aliw !== undefined) synth += `, ALIW=${formatNum(input.synthesis.aliw)}`
  if (input.synthesis.xh !== undefined) synth += `,\n  XH=${formatNum(input.synthesis.xh)}`
  if (input.synthesis.zh !== undefined) synth += `, ZH=${formatNum(input.synthesis.zh)}`
  if (input.synthesis.alih !== undefined) synth += `, ALIH=${formatNum(input.synthesis.alih)}`
  if (input.synthesis.xv !== undefined) synth += `, XV=${formatNum(input.synthesis.xv)}`
  if (input.synthesis.vertup !== undefined) synth += `, VERTUP=${input.synthesis.vertup ? '.TRUE.' : '.FALSE.'}`
  synth += '$'
  lines.push(synth)

  // Body geometry - compact format
  if (input.body) {
    // Ensure first R value is not zero to avoid division errors
    const rValues = [...input.body.r]
    if (rValues[0] === 0) rValues[0] = 0.01

    let body = ` $BODY NX=${input.body.nx}.0, BNOSE=${input.body.bnose}.0`
    if (input.body.btail !== undefined) body += `, BTAIL=${input.body.btail}.0`
    body += `, BLN=${formatNum(input.body.bln)}, BLA=${formatNum(input.body.bla)},`
    body += `\n  X(1)=${formatArray(input.body.x)},`
    body += `\n  R(1)=${formatArray(rValues)}$`
    lines.push(body)
  }

  // Wing planform - compact format
  if (input.wing) {
    let wing = ` $WGPLNF CHRDTP=${formatNum(input.wing.chrdtp)}, SSPNE=${formatNum(input.wing.sspne)}, SSPN=${formatNum(input.wing.sspn)}, CHRDR=${formatNum(input.wing.chrdr)},`
    wing += `\n  SAVSI=${formatNum(input.wing.savsi)}, CHSTAT=${formatNum(input.wing.chstat)}`
    if (input.wing.twista !== undefined) wing += `, TWISTA=${formatNum(input.wing.twista)}`
    if (input.wing.dhdadi !== undefined) wing += `,\n  DHDADI=${formatNum(input.wing.dhdadi)}`
    if (input.wing.dhdado !== undefined) wing += `, DHDADO=${formatNum(input.wing.dhdado)}`
    wing += `, TYPE=${input.wing.type}.0$`
    lines.push(wing)
  }

  // Wing section - compact format with required CLALPA and CLMAX
  if (input.wingSection) {
    let ws = ` $WGSCHR TOVC=${formatNum(input.wingSection.tovc)}`
    if (input.wingSection.xovc !== undefined) ws += `, XOVC=${formatNum(input.wingSection.xovc)}`
    // Always use CLI=0.0 and ALPHAI=0.0 to avoid NaN issues in DATCOM
    ws += `, CLI=0.0, ALPHAI=0.0`
    // Add required lift curve parameters
    ws += `,\n  CLALPA(1)=0.1, CLMAX(1)=1.5`
    if (input.wingSection.cmo !== undefined) ws += `, CMO=${formatNum(input.wingSection.cmo)}`
    if (input.wingSection.leri !== undefined) ws += `, LERI=${formatNum(input.wingSection.leri)}`
    ws += '$'
    lines.push(ws)
  }

  // Horizontal tail planform
  if (input.hTail) {
    let ht = ` $HTPLNF CHRDTP=${formatNum(input.hTail.chrdtp)}, SSPNE=${formatNum(input.hTail.sspne)}, SSPN=${formatNum(input.hTail.sspn)}, CHRDR=${formatNum(input.hTail.chrdr)},`
    ht += `\n  SAVSI=${formatNum(input.hTail.savsi)}, CHSTAT=${formatNum(input.hTail.chstat)}`
    if (input.hTail.twista !== undefined) ht += `, TWISTA=${formatNum(input.hTail.twista)}`
    if (input.hTail.dhdadi !== undefined) ht += `,\n  DHDADI=${formatNum(input.hTail.dhdadi)}`
    if (input.hTail.dhdado !== undefined) ht += `, DHDADO=${formatNum(input.hTail.dhdado)}`
    ht += `, TYPE=${input.hTail.type}.0$`
    lines.push(ht)
  }

  // Horizontal tail section - with required lift parameters
  if (input.hTailSection) {
    let hts = ` $HTSCHR TOVC=${formatNum(input.hTailSection.tovc)}`
    if (input.hTailSection.xovc !== undefined) hts += `, XOVC=${formatNum(input.hTailSection.xovc)}`
    // Always use CLI=0.0 and ALPHAI=0.0 to avoid NaN issues
    hts += `, CLI=0.0, ALPHAI=0.0`
    // Add required lift curve parameters
    hts += `,\n  CLALPA(1)=0.1, CLMAX(1)=1.2`
    if (input.hTailSection.cmo !== undefined) hts += `, CMO=${formatNum(input.hTailSection.cmo)}`
    if (input.hTailSection.leri !== undefined) hts += `, LERI=${formatNum(input.hTailSection.leri)}`
    hts += '$'
    lines.push(hts)
  }

  // Vertical tail planform
  if (input.vTail) {
    let vt = ` $VTPLNF CHRDTP=${formatNum(input.vTail.chrdtp)}, SSPNE=${formatNum(input.vTail.sspne)}, SSPN=${formatNum(input.vTail.sspn)}, CHRDR=${formatNum(input.vTail.chrdr)},`
    vt += `\n  SAVSI=${formatNum(input.vTail.savsi)}, CHSTAT=${formatNum(input.vTail.chstat)}`
    if (input.vTail.twista !== undefined) vt += `, TWISTA=${formatNum(input.vTail.twista)}`
    if (input.vTail.dhdadi !== undefined) vt += `,\n  DHDADI=${formatNum(input.vTail.dhdadi)}`
    if (input.vTail.dhdado !== undefined) vt += `, DHDADO=${formatNum(input.vTail.dhdado)}`
    vt += `, TYPE=${input.vTail.type}.0$`
    lines.push(vt)
  }

  // Vertical tail section
  if (input.vTailSection) {
    let vts = ` $VTSCHR TOVC=${formatNum(input.vTailSection.tovc)}`
    if (input.vTailSection.xovc !== undefined) vts += `, XOVC=${formatNum(input.vTailSection.xovc)}`
    if (input.vTailSection.cmo !== undefined) vts += `, CMO=${formatNum(input.vTailSection.cmo)}`
    if (input.vTailSection.leri !== undefined) vts += `, LERI=${formatNum(input.vTailSection.leri)}`
    vts += '$'
    lines.push(vts)
  }

  // Symmetric flaps
  if (input.symFlap) {
    let flaps = ` $SYMFLP FTYPE=${input.symFlap.ftype}.0, NDELTA=${input.symFlap.ndelta}.0,`
    flaps += `\n  DELTA(1)=${formatArray(input.symFlap.delta)},`
    flaps += `\n  SPANFI=${formatNum(input.symFlap.spanfi)}, SPANFO=${formatNum(input.symFlap.spanfo)},`
    flaps += `\n  CHRDFI=${formatNum(input.symFlap.chrdfi)}, CHRDFO=${formatNum(input.symFlap.chrdfo)}`
    if (input.symFlap.phete !== undefined) flaps += `, PHETE=${formatNum(input.symFlap.phete)}`
    flaps += '$'
    lines.push(flaps)
  }

  // Case ID and control
  lines.push(`CASEID ${input.caseid || 'Sharif Aero Analysis'}`)
  lines.push('SAVE')
  lines.push('NEXT CASE')
  lines.push('')

  return lines.join('\n')
}
