// DATCOM Input File Parser
// Parses .inp files and extracts namelist values

interface ParsedInput {
  flight?: {
    mach?: number[]
    alpha?: number[]
    reynolds?: number[]
    alt?: number[]
  }
  reference?: {
    sref?: number
    cbar?: number
    blref?: number
  }
  synthesis?: {
    xcg?: number
    zcg?: number
    xw?: number
    zw?: number
    aliw?: number
    xh?: number
    zh?: number
    alih?: number
    xv?: number
    zv?: number
    vertup?: boolean
  }
  body?: {
    nx?: number
    bnose?: number
    btail?: number
    bln?: number
    bla?: number
    x?: number[]
    r?: number[]
  }
  wing?: {
    chrdr?: number
    chrdtp?: number
    sspn?: number
    sspne?: number
    savsi?: number
    chstat?: number
    twista?: number
    dhdadi?: number
    dhdado?: number
    type?: number
  }
  wingSection?: {
    tovc?: number
    tovco?: number
    xovc?: number
    cli?: number
    alphai?: number
    cmo?: number
    leri?: number
    lero?: number
  }
  hTail?: {
    chrdr?: number
    chrdtp?: number
    sspn?: number
    sspne?: number
    savsi?: number
    chstat?: number
    twista?: number
    dhdadi?: number
    dhdado?: number
    type?: number
  }
  hTailSection?: {
    tovc?: number
    xovc?: number
    cmo?: number
    leri?: number
  }
  vTail?: {
    chrdr?: number
    chrdtp?: number
    sspn?: number
    sspne?: number
    savsi?: number
    chstat?: number
    twista?: number
    dhdadi?: number
    dhdado?: number
    type?: number
  }
  vTailSection?: {
    tovc?: number
    xovc?: number
    cmo?: number
    leri?: number
  }
  flaps?: {
    ftype?: number
    ndelta?: number
    delta?: number[]
    spanfi?: number
    spanfo?: number
    chrdfi?: number
    chrdfo?: number
    phete?: number
  }
}

// Parse a number from Fortran format
function parseNumber(str: string): number {
  // Handle Fortran scientific notation (e.g., 4.28E6, 1.5D-3)
  let cleaned = str.trim()
    .replace(/D/gi, 'E')
    .replace(/E\+/gi, 'E')
  return parseFloat(cleaned)
}

// Parse an array of numbers
function parseArray(content: string): number[] {
  // Split by comma and parse each number
  return content
    .split(',')
    .map(s => s.trim())
    .filter(s => s.length > 0 && !isNaN(parseNumber(s)))
    .map(parseNumber)
}

// Extract value for a variable from namelist content
function extractValue(content: string, varName: string): string | null {
  // Match patterns like: VARNAME=value or VARNAME(1)=value
  const patterns = [
    new RegExp(`${varName}\\s*\\(\\d+\\)\\s*=\\s*([^$]+?)(?=,\\s*[A-Z]|\\$|$)`, 'i'),
    new RegExp(`${varName}\\s*=\\s*([^$]+?)(?=,\\s*[A-Z]|\\$|$)`, 'i'),
  ]

  for (const pattern of patterns) {
    const match = content.match(pattern)
    if (match) {
      return match[1].trim()
    }
  }
  return null
}

// Extract a single number value
function extractNumber(content: string, varName: string): number | undefined {
  const value = extractValue(content, varName)
  if (value) {
    const num = parseNumber(value)
    if (!isNaN(num)) return num
  }
  return undefined
}

// Extract an array value
function extractArrayValue(content: string, varName: string): number[] | undefined {
  const value = extractValue(content, varName)
  if (value) {
    const arr = parseArray(value)
    if (arr.length > 0) return arr
  }
  return undefined
}

// Extract boolean value (.TRUE. or .FALSE.)
function extractBoolean(content: string, varName: string): boolean | undefined {
  const value = extractValue(content, varName)
  if (value) {
    if (value.toUpperCase().includes('.TRUE.')) return true
    if (value.toUpperCase().includes('.FALSE.')) return false
  }
  return undefined
}

// Extract a namelist block
function extractNamelist(content: string, name: string): string | null {
  // Match $NAME ... $ pattern
  const pattern = new RegExp(`\\$${name}([\\s\\S]*?)(?:\\$|$(?!\\s*[A-Z]))`, 'i')
  const match = content.match(pattern)
  if (match) {
    // Clean up the content - remove newlines and extra spaces
    return match[1].replace(/\n/g, ' ').replace(/\s+/g, ' ')
  }
  return null
}

export function parseDatcomInput(content: string): ParsedInput {
  const result: ParsedInput = {}

  // Normalize content - remove comments and clean up
  const cleanContent = content
    .split('\n')
    .filter(line => !line.trim().startsWith('*'))
    .join('\n')

  // Parse FLTCON
  const fltcon = extractNamelist(cleanContent, 'FLTCON')
  if (fltcon) {
    result.flight = {
      mach: extractArrayValue(fltcon, 'MACH'),
      alpha: extractArrayValue(fltcon, 'ALSCHD'),
      reynolds: extractArrayValue(fltcon, 'RNNUB'),
      alt: extractArrayValue(fltcon, 'ALT'),
    }
  }

  // Parse OPTINS
  const optins = extractNamelist(cleanContent, 'OPTINS')
  if (optins) {
    result.reference = {
      sref: extractNumber(optins, 'SREF'),
      cbar: extractNumber(optins, 'CBARR'),
      blref: extractNumber(optins, 'BLREF'),
    }
  }

  // Parse SYNTHS
  const synths = extractNamelist(cleanContent, 'SYNTHS')
  if (synths) {
    result.synthesis = {
      xcg: extractNumber(synths, 'XCG'),
      zcg: extractNumber(synths, 'ZCG'),
      xw: extractNumber(synths, 'XW'),
      zw: extractNumber(synths, 'ZW'),
      aliw: extractNumber(synths, 'ALIW'),
      xh: extractNumber(synths, 'XH'),
      zh: extractNumber(synths, 'ZH'),
      alih: extractNumber(synths, 'ALIH'),
      xv: extractNumber(synths, 'XV'),
      zv: extractNumber(synths, 'ZV'),
      vertup: extractBoolean(synths, 'VERTUP'),
    }
  }

  // Parse BODY
  const body = extractNamelist(cleanContent, 'BODY')
  if (body) {
    result.body = {
      nx: extractNumber(body, 'NX'),
      bnose: extractNumber(body, 'BNOSE'),
      btail: extractNumber(body, 'BTAIL'),
      bln: extractNumber(body, 'BLN'),
      bla: extractNumber(body, 'BLA'),
      x: extractArrayValue(body, 'X'),
      r: extractArrayValue(body, 'R'),
    }
  }

  // Parse WGPLNF (Wing Planform)
  const wgplnf = extractNamelist(cleanContent, 'WGPLNF')
  if (wgplnf) {
    result.wing = {
      chrdr: extractNumber(wgplnf, 'CHRDR'),
      chrdtp: extractNumber(wgplnf, 'CHRDTP'),
      sspn: extractNumber(wgplnf, 'SSPN'),
      sspne: extractNumber(wgplnf, 'SSPNE'),
      savsi: extractNumber(wgplnf, 'SAVSI'),
      chstat: extractNumber(wgplnf, 'CHSTAT'),
      twista: extractNumber(wgplnf, 'TWISTA'),
      dhdadi: extractNumber(wgplnf, 'DHDADI'),
      dhdado: extractNumber(wgplnf, 'DHDADO'),
      type: extractNumber(wgplnf, 'TYPE'),
    }
  }

  // Parse WGSCHR (Wing Section)
  const wgschr = extractNamelist(cleanContent, 'WGSCHR')
  if (wgschr) {
    result.wingSection = {
      tovc: extractNumber(wgschr, 'TOVC'),
      tovco: extractNumber(wgschr, 'TOVCO'),
      xovc: extractNumber(wgschr, 'XOVC'),
      cli: extractNumber(wgschr, 'CLI'),
      alphai: extractNumber(wgschr, 'ALPHAI'),
      cmo: extractNumber(wgschr, 'CMO'),
      leri: extractNumber(wgschr, 'LERI'),
      lero: extractNumber(wgschr, 'LERO'),
    }
  }

  // Parse HTPLNF (H-Tail Planform)
  const htplnf = extractNamelist(cleanContent, 'HTPLNF')
  if (htplnf) {
    result.hTail = {
      chrdr: extractNumber(htplnf, 'CHRDR'),
      chrdtp: extractNumber(htplnf, 'CHRDTP'),
      sspn: extractNumber(htplnf, 'SSPN'),
      sspne: extractNumber(htplnf, 'SSPNE'),
      savsi: extractNumber(htplnf, 'SAVSI'),
      chstat: extractNumber(htplnf, 'CHSTAT'),
      twista: extractNumber(htplnf, 'TWISTA'),
      dhdadi: extractNumber(htplnf, 'DHDADI'),
      dhdado: extractNumber(htplnf, 'DHDADO'),
      type: extractNumber(htplnf, 'TYPE'),
    }
  }

  // Parse HTSCHR (H-Tail Section)
  const htschr = extractNamelist(cleanContent, 'HTSCHR')
  if (htschr) {
    result.hTailSection = {
      tovc: extractNumber(htschr, 'TOVC'),
      xovc: extractNumber(htschr, 'XOVC'),
      cmo: extractNumber(htschr, 'CMO'),
      leri: extractNumber(htschr, 'LERI'),
    }
  }

  // Parse VTPLNF (V-Tail Planform)
  const vtplnf = extractNamelist(cleanContent, 'VTPLNF')
  if (vtplnf) {
    result.vTail = {
      chrdr: extractNumber(vtplnf, 'CHRDR'),
      chrdtp: extractNumber(vtplnf, 'CHRDTP'),
      sspn: extractNumber(vtplnf, 'SSPN'),
      sspne: extractNumber(vtplnf, 'SSPNE'),
      savsi: extractNumber(vtplnf, 'SAVSI'),
      chstat: extractNumber(vtplnf, 'CHSTAT'),
      twista: extractNumber(vtplnf, 'TWISTA'),
      dhdadi: extractNumber(vtplnf, 'DHDADI'),
      dhdado: extractNumber(vtplnf, 'DHDADO'),
      type: extractNumber(vtplnf, 'TYPE'),
    }
  }

  // Parse VTSCHR (V-Tail Section)
  const vtschr = extractNamelist(cleanContent, 'VTSCHR')
  if (vtschr) {
    result.vTailSection = {
      tovc: extractNumber(vtschr, 'TOVC'),
      xovc: extractNumber(vtschr, 'XOVC'),
      cmo: extractNumber(vtschr, 'CMO'),
      leri: extractNumber(vtschr, 'LERI'),
    }
  }

  // Parse SYMFLP (Symmetric Flaps)
  const symflp = extractNamelist(cleanContent, 'SYMFLP')
  if (symflp) {
    result.flaps = {
      ftype: extractNumber(symflp, 'FTYPE'),
      ndelta: extractNumber(symflp, 'NDELTA'),
      delta: extractArrayValue(symflp, 'DELTA'),
      spanfi: extractNumber(symflp, 'SPANFI'),
      spanfo: extractNumber(symflp, 'SPANFO'),
      chrdfi: extractNumber(symflp, 'CHRDFI'),
      chrdfo: extractNumber(symflp, 'CHRDFO'),
      phete: extractNumber(symflp, 'PHETE'),
    }
  }

  return result
}

// Apply parsed input to the store
export function applyParsedInput(
  parsed: ParsedInput,
  setFlight: (f: object) => void,
  setReference: (r: object) => void,
  setSynthesis: (s: object) => void,
  setBody: (b: object) => void,
  setWing: (w: object) => void,
  setWingSection: (ws: object) => void,
  setHTail: (ht: object) => void,
  setHTailSection: (hts: object) => void,
  setVTail: (vt: object) => void,
  setVTailSection: (vts: object) => void,
  setFlaps: (f: object) => void,
  setEnableHTail: (e: boolean) => void,
  setEnableVTail: (e: boolean) => void,
  setEnableFlaps: (e: boolean) => void,
) {
  // Apply flight conditions
  if (parsed.flight) {
    const flight: Record<string, unknown> = {}
    if (parsed.flight.mach) {
      flight.mach = parsed.flight.mach
      flight.nmach = parsed.flight.mach.length
    }
    if (parsed.flight.alpha) {
      flight.alpha = parsed.flight.alpha
      flight.nalpha = parsed.flight.alpha.length
    }
    if (parsed.flight.reynolds) flight.reynolds = parsed.flight.reynolds
    if (parsed.flight.alt) flight.alt = parsed.flight.alt
    if (Object.keys(flight).length > 0) setFlight(flight)
  }

  // Apply reference parameters
  if (parsed.reference) {
    const ref: Record<string, unknown> = {}
    if (parsed.reference.sref !== undefined) ref.sref = parsed.reference.sref
    if (parsed.reference.cbar !== undefined) ref.cbar = parsed.reference.cbar
    if (parsed.reference.blref !== undefined) ref.blref = parsed.reference.blref
    if (Object.keys(ref).length > 0) setReference(ref)
  }

  // Apply synthesis
  if (parsed.synthesis) {
    const synth: Record<string, unknown> = {}
    if (parsed.synthesis.xcg !== undefined) synth.xcg = parsed.synthesis.xcg
    if (parsed.synthesis.zcg !== undefined) synth.zcg = parsed.synthesis.zcg
    if (parsed.synthesis.xw !== undefined) synth.xw = parsed.synthesis.xw
    if (parsed.synthesis.zw !== undefined) synth.zw = parsed.synthesis.zw
    if (parsed.synthesis.aliw !== undefined) synth.aliw = parsed.synthesis.aliw
    if (parsed.synthesis.xh !== undefined) synth.xh = parsed.synthesis.xh
    if (parsed.synthesis.zh !== undefined) synth.zh = parsed.synthesis.zh
    if (parsed.synthesis.alih !== undefined) synth.alih = parsed.synthesis.alih
    if (parsed.synthesis.xv !== undefined) synth.xv = parsed.synthesis.xv
    if (parsed.synthesis.zv !== undefined) synth.zv = parsed.synthesis.zv
    if (parsed.synthesis.vertup !== undefined) synth.vertup = parsed.synthesis.vertup
    if (Object.keys(synth).length > 0) setSynthesis(synth)
  }

  // Apply body
  if (parsed.body) {
    const body: Record<string, unknown> = {}
    if (parsed.body.nx !== undefined) body.nx = parsed.body.nx
    if (parsed.body.bnose !== undefined) body.bnose = parsed.body.bnose
    if (parsed.body.btail !== undefined) body.btail = parsed.body.btail
    if (parsed.body.bln !== undefined) body.bln = parsed.body.bln
    if (parsed.body.bla !== undefined) body.bla = parsed.body.bla
    if (parsed.body.x) body.x = parsed.body.x
    if (parsed.body.r) body.r = parsed.body.r
    if (Object.keys(body).length > 0) setBody(body)
  }

  // Apply wing
  if (parsed.wing) {
    const wing: Record<string, unknown> = {}
    if (parsed.wing.chrdr !== undefined) wing.chrdr = parsed.wing.chrdr
    if (parsed.wing.chrdtp !== undefined) wing.chrdtp = parsed.wing.chrdtp
    if (parsed.wing.sspn !== undefined) wing.sspn = parsed.wing.sspn
    if (parsed.wing.sspne !== undefined) wing.sspne = parsed.wing.sspne
    if (parsed.wing.savsi !== undefined) wing.savsi = parsed.wing.savsi
    if (parsed.wing.chstat !== undefined) wing.chstat = parsed.wing.chstat
    if (parsed.wing.twista !== undefined) wing.twista = parsed.wing.twista
    if (parsed.wing.dhdadi !== undefined) wing.dhdadi = parsed.wing.dhdadi
    if (parsed.wing.dhdado !== undefined) wing.dhdado = parsed.wing.dhdado
    if (parsed.wing.type !== undefined) wing.type = parsed.wing.type
    if (Object.keys(wing).length > 0) setWing(wing)
  }

  // Apply wing section
  if (parsed.wingSection) {
    const ws: Record<string, unknown> = {}
    if (parsed.wingSection.tovc !== undefined) ws.tovc = parsed.wingSection.tovc
    if (parsed.wingSection.tovco !== undefined) ws.tovco = parsed.wingSection.tovco
    if (parsed.wingSection.xovc !== undefined) ws.xovc = parsed.wingSection.xovc
    if (parsed.wingSection.cli !== undefined) ws.cli = parsed.wingSection.cli
    if (parsed.wingSection.alphai !== undefined) ws.alphai = parsed.wingSection.alphai
    if (parsed.wingSection.cmo !== undefined) ws.cmo = parsed.wingSection.cmo
    if (parsed.wingSection.leri !== undefined) ws.leri = parsed.wingSection.leri
    if (parsed.wingSection.lero !== undefined) ws.lero = parsed.wingSection.lero
    if (Object.keys(ws).length > 0) setWingSection(ws)
  }

  // Apply H-Tail
  if (parsed.hTail) {
    setEnableHTail(true)
    const ht: Record<string, unknown> = {}
    if (parsed.hTail.chrdr !== undefined) ht.chrdr = parsed.hTail.chrdr
    if (parsed.hTail.chrdtp !== undefined) ht.chrdtp = parsed.hTail.chrdtp
    if (parsed.hTail.sspn !== undefined) ht.sspn = parsed.hTail.sspn
    if (parsed.hTail.sspne !== undefined) ht.sspne = parsed.hTail.sspne
    if (parsed.hTail.savsi !== undefined) ht.savsi = parsed.hTail.savsi
    if (parsed.hTail.chstat !== undefined) ht.chstat = parsed.hTail.chstat
    if (parsed.hTail.twista !== undefined) ht.twista = parsed.hTail.twista
    if (parsed.hTail.dhdadi !== undefined) ht.dhdadi = parsed.hTail.dhdadi
    if (parsed.hTail.dhdado !== undefined) ht.dhdado = parsed.hTail.dhdado
    if (parsed.hTail.type !== undefined) ht.type = parsed.hTail.type
    if (Object.keys(ht).length > 0) setHTail(ht)
  }

  // Apply H-Tail section
  if (parsed.hTailSection) {
    const hts: Record<string, unknown> = {}
    if (parsed.hTailSection.tovc !== undefined) hts.tovc = parsed.hTailSection.tovc
    if (parsed.hTailSection.xovc !== undefined) hts.xovc = parsed.hTailSection.xovc
    if (parsed.hTailSection.cmo !== undefined) hts.cmo = parsed.hTailSection.cmo
    if (parsed.hTailSection.leri !== undefined) hts.leri = parsed.hTailSection.leri
    if (Object.keys(hts).length > 0) setHTailSection(hts)
  }

  // Apply V-Tail
  if (parsed.vTail) {
    setEnableVTail(true)
    const vt: Record<string, unknown> = {}
    if (parsed.vTail.chrdr !== undefined) vt.chrdr = parsed.vTail.chrdr
    if (parsed.vTail.chrdtp !== undefined) vt.chrdtp = parsed.vTail.chrdtp
    if (parsed.vTail.sspn !== undefined) vt.sspn = parsed.vTail.sspn
    if (parsed.vTail.sspne !== undefined) vt.sspne = parsed.vTail.sspne
    if (parsed.vTail.savsi !== undefined) vt.savsi = parsed.vTail.savsi
    if (parsed.vTail.chstat !== undefined) vt.chstat = parsed.vTail.chstat
    if (parsed.vTail.twista !== undefined) vt.twista = parsed.vTail.twista
    if (parsed.vTail.dhdadi !== undefined) vt.dhdadi = parsed.vTail.dhdadi
    if (parsed.vTail.dhdado !== undefined) vt.dhdado = parsed.vTail.dhdado
    if (parsed.vTail.type !== undefined) vt.type = parsed.vTail.type
    if (Object.keys(vt).length > 0) setVTail(vt)
  }

  // Apply V-Tail section
  if (parsed.vTailSection) {
    const vts: Record<string, unknown> = {}
    if (parsed.vTailSection.tovc !== undefined) vts.tovc = parsed.vTailSection.tovc
    if (parsed.vTailSection.xovc !== undefined) vts.xovc = parsed.vTailSection.xovc
    if (parsed.vTailSection.cmo !== undefined) vts.cmo = parsed.vTailSection.cmo
    if (parsed.vTailSection.leri !== undefined) vts.leri = parsed.vTailSection.leri
    if (Object.keys(vts).length > 0) setVTailSection(vts)
  }

  // Apply flaps
  if (parsed.flaps) {
    setEnableFlaps(true)
    const flaps: Record<string, unknown> = {}
    if (parsed.flaps.ftype !== undefined) flaps.ftype = parsed.flaps.ftype
    if (parsed.flaps.ndelta !== undefined) flaps.ndelta = parsed.flaps.ndelta
    if (parsed.flaps.delta) flaps.delta = parsed.flaps.delta
    if (parsed.flaps.spanfi !== undefined) flaps.spanfi = parsed.flaps.spanfi
    if (parsed.flaps.spanfo !== undefined) flaps.spanfo = parsed.flaps.spanfo
    if (parsed.flaps.chrdfi !== undefined) flaps.chrdfi = parsed.flaps.chrdfi
    if (parsed.flaps.chrdfo !== undefined) flaps.chrdfo = parsed.flaps.chrdfo
    if (parsed.flaps.phete !== undefined) flaps.phete = parsed.flaps.phete
    if (Object.keys(flaps).length > 0) setFlaps(flaps)
  }
}
