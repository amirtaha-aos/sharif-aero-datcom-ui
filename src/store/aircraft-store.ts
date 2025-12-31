import { create } from 'zustand'
import type {
  FlightConditions,
  ReferenceParams,
  SynthesisParams,
  BodyGeometry,
  SurfacePlanform,
  SectionCharacteristics,
  SymmetricFlaps,
  DatcomResults,
} from '../lib/datcom-types'

interface AircraftState {
  // Flight conditions
  flight: FlightConditions
  setFlight: (flight: Partial<FlightConditions>) => void

  // Reference parameters
  reference: ReferenceParams
  setReference: (ref: Partial<ReferenceParams>) => void

  // Synthesis parameters
  synthesis: SynthesisParams
  setSynthesis: (syn: Partial<SynthesisParams>) => void

  // Body geometry
  body: BodyGeometry
  setBody: (body: Partial<BodyGeometry>) => void

  // Wing
  wing: SurfacePlanform
  setWing: (wing: Partial<SurfacePlanform>) => void
  wingSection: SectionCharacteristics
  setWingSection: (sec: Partial<SectionCharacteristics>) => void

  // Horizontal Tail
  hTail: SurfacePlanform | null
  setHTail: (tail: Partial<SurfacePlanform> | null) => void
  hTailSection: SectionCharacteristics | null
  setHTailSection: (sec: Partial<SectionCharacteristics> | null) => void
  enableHTail: boolean
  setEnableHTail: (enable: boolean) => void

  // Vertical Tail
  vTail: SurfacePlanform | null
  setVTail: (tail: Partial<SurfacePlanform> | null) => void
  vTailSection: SectionCharacteristics | null
  setVTailSection: (sec: Partial<SectionCharacteristics> | null) => void
  enableVTail: boolean
  setEnableVTail: (enable: boolean) => void

  // Flaps
  flaps: SymmetricFlaps | null
  setFlaps: (flaps: Partial<SymmetricFlaps> | null) => void
  enableFlaps: boolean
  setEnableFlaps: (enable: boolean) => void

  // Analysis results
  results: DatcomResults | null
  setResults: (results: DatcomResults | null) => void

  // Running state
  isRunning: boolean
  setIsRunning: (running: boolean) => void

  // Active tab
  activeTab: 'input' | 'results' | 'cad'
  setActiveTab: (tab: 'input' | 'results' | 'cad') => void

  // Reset to defaults
  reset: () => void
}

const defaultFlight: FlightConditions = {
  nmach: 1,
  mach: [0.6],
  nalpha: 11,
  alpha: [-4, -2, 0, 2, 4, 6, 8, 10, 12, 14, 16],
  reynolds: [4.28e6],
  hypers: false,
}

const defaultReference: ReferenceParams = {
  sref: 12.0,
  cbar: 2.5,
  blref: 6.0,
}

const defaultSynthesis: SynthesisParams = {
  xcg: 4.0,
  zcg: 0.0,
  xw: 3.0,
  zw: 0.0,
  aliw: 2.0,
  xh: 8.0,
  zh: 0.2,
  alih: 0.0,
  xv: 8.0,
  vertup: true,
}

const defaultBody: BodyGeometry = {
  nx: 10,
  x: [0, 0.5, 1.0, 1.5, 2.5, 4.0, 5.5, 7.0, 8.5, 10.0],
  r: [0, 0.2, 0.35, 0.45, 0.5, 0.5, 0.5, 0.48, 0.4, 0.25],
  bnose: 2,
  btail: 1,
  bln: 2.5,
  bla: 7.5,
}

const defaultWing: SurfacePlanform = {
  chrdr: 3.0,
  chrdtp: 1.2,
  sspn: 3.0,
  sspne: 2.7,
  savsi: 35.0,
  chstat: 0.25,
  twista: -2,
  dhdadi: 3,
  dhdado: 3,
  type: 1,
}

const defaultWingSection: SectionCharacteristics = {
  tovc: 0.12,
  tovco: 0.10,
  xovc: 0.35,
  cli: 0.3,
  alphai: 2.0,
  cmo: -0.05,
  leri: 0.012,
  camber: true,
}

const defaultHTail: SurfacePlanform = {
  chrdr: 1.5,
  chrdtp: 0.6,
  sspn: 1.5,
  sspne: 1.3,
  savsi: 30.0,
  chstat: 0.25,
  twista: 0,
  dhdadi: 0,
  dhdado: 0,
  type: 1,
}

const defaultHTailSection: SectionCharacteristics = {
  tovc: 0.09,
  xovc: 0.35,
  cmo: 0,
  leri: 0.008,
}

const defaultVTail: SurfacePlanform = {
  chrdr: 2.0,
  chrdtp: 0.8,
  sspn: 1.2,
  sspne: 1.0,
  savsi: 40.0,
  chstat: 0.25,
  twista: 0,
  dhdadi: 0,
  dhdado: 0,
  type: 1,
}

const defaultVTailSection: SectionCharacteristics = {
  tovc: 0.10,
  xovc: 0.35,
  cmo: 0,
  leri: 0.010,
}

const defaultFlaps: SymmetricFlaps = {
  ftype: 1,
  ndelta: 5,
  delta: [0, 10, 20, 30, 40],
  spanfi: 0.15,
  spanfo: 0.70,
  chrdfi: 0.25,
  chrdfo: 0.20,
  phete: 0.05,
}

export const useAircraftStore = create<AircraftState>((set) => ({
  flight: defaultFlight,
  setFlight: (flight) =>
    set((state) => ({ flight: { ...state.flight, ...flight } })),

  reference: defaultReference,
  setReference: (ref) =>
    set((state) => ({ reference: { ...state.reference, ...ref } })),

  synthesis: defaultSynthesis,
  setSynthesis: (syn) =>
    set((state) => ({ synthesis: { ...state.synthesis, ...syn } })),

  body: defaultBody,
  setBody: (body) =>
    set((state) => ({ body: { ...state.body, ...body } })),

  wing: defaultWing,
  setWing: (wing) =>
    set((state) => ({ wing: { ...state.wing, ...wing } })),

  wingSection: defaultWingSection,
  setWingSection: (sec) =>
    set((state) => ({ wingSection: { ...state.wingSection, ...sec } })),

  hTail: defaultHTail,
  setHTail: (tail) =>
    set((state) => ({
      hTail: tail ? { ...state.hTail!, ...tail } : null,
    })),
  hTailSection: defaultHTailSection,
  setHTailSection: (sec) =>
    set((state) => ({
      hTailSection: sec ? { ...state.hTailSection!, ...sec } : null,
    })),
  enableHTail: true,
  setEnableHTail: (enable) => set({ enableHTail: enable }),

  vTail: defaultVTail,
  setVTail: (tail) =>
    set((state) => ({
      vTail: tail ? { ...state.vTail!, ...tail } : null,
    })),
  vTailSection: defaultVTailSection,
  setVTailSection: (sec) =>
    set((state) => ({
      vTailSection: sec ? { ...state.vTailSection!, ...sec } : null,
    })),
  enableVTail: true,
  setEnableVTail: (enable) => set({ enableVTail: enable }),

  flaps: defaultFlaps,
  setFlaps: (flaps) =>
    set((state) => ({
      flaps: flaps ? { ...state.flaps!, ...flaps } : null,
    })),
  enableFlaps: false,
  setEnableFlaps: (enable) => set({ enableFlaps: enable }),

  results: null,
  setResults: (results) => set({ results }),

  isRunning: false,
  setIsRunning: (isRunning) => set({ isRunning }),

  activeTab: 'input',
  setActiveTab: (activeTab) => set({ activeTab }),

  reset: () =>
    set({
      flight: defaultFlight,
      reference: defaultReference,
      synthesis: defaultSynthesis,
      body: defaultBody,
      wing: defaultWing,
      wingSection: defaultWingSection,
      hTail: defaultHTail,
      hTailSection: defaultHTailSection,
      vTail: defaultVTail,
      vTailSection: defaultVTailSection,
      flaps: defaultFlaps,
      enableHTail: true,
      enableVTail: true,
      enableFlaps: false,
      results: null,
    }),
}))
