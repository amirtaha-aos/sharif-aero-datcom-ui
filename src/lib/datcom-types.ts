// ============================================
// DATCOM Complete Input Types
// Based on USAF Digital DATCOM (AFFDL-TR-79-3032)
// ============================================

// Flight Conditions ($FLTCON)
export interface FlightConditions {
  nmach: number
  mach: number[]
  nalpha: number
  alpha: number[]
  nalt?: number
  alt?: number[]
  reynolds: number[]
  vinf?: number[]          // Freestream velocity
  pinf?: number[]          // Freestream pressure
  tinf?: number[]          // Freestream temperature
  hypers?: boolean         // Hypersonic methods
  loop?: number            // Loop control
  stmach?: number          // Starting Mach for transonic
  tsmach?: number          // Ending Mach for transonic
}

// Reference Parameters ($OPTINS)
export interface ReferenceParams {
  sref: number             // Reference area (ft²)
  cbar: number             // Mean aerodynamic chord (ft)
  blref: number            // Reference span (ft)
}

// Configuration Synthesis ($SYNTHS)
export interface SynthesisParams {
  xcg: number              // CG x-location (ft)
  zcg: number              // CG z-location (ft)
  xw?: number              // Wing x-location (ft)
  zw?: number              // Wing z-location (ft)
  aliw?: number            // Wing incidence angle (deg)
  xh?: number              // H-tail x-location (ft)
  zh?: number              // H-tail z-location (ft)
  alih?: number            // H-tail incidence angle (deg)
  xv?: number              // V-tail x-location (ft)
  zv?: number              // V-tail z-location (ft)
  vertup?: boolean         // Vertical tail on top
  scale?: number           // Scale factor
  hinax?: number           // Hinge axis location
}

// Body Geometry ($BODY)
export interface BodyGeometry {
  nx: number               // Number of body stations
  x: number[]              // Station x-locations
  r: number[]              // Station radii (circular)
  s?: number[]             // Station cross-section areas
  p?: number[]             // Station perimeters
  zu?: number[]            // Upper surface z-coords (camber)
  zl?: number[]            // Lower surface z-coords (camber)
  bnose: 1 | 2 | 3         // Nose type: 1=conical, 2=ogive, 3=power
  btail?: 1 | 2 | 3        // Tail type
  bln: number              // Nose length
  bla: number              // Afterbody length
  ds?: number              // Nose bluntness diameter
  itype?: number           // Body type
  method?: number          // Calculation method
}

// Wing/Surface Planform ($WGPLNF, $HTPLNF, $VTPLNF)
export interface SurfacePlanform {
  chrdr: number            // Root chord (ft)
  chrdtp: number           // Tip chord (ft)
  chrdbp?: number          // Break chord (cranked wing)
  sspn: number             // Total semi-span (ft)
  sspne: number            // Exposed semi-span (ft)
  sspnop?: number          // Outboard panel semi-span
  sspndd?: number          // Semi-span of inboard panel
  savsi: number            // Inboard LE sweep (deg)
  savso?: number           // Outboard LE sweep (deg)
  chstat: number           // Chord station for sweep ref
  swafp?: number           // Sweep of aft panel
  twista?: number          // Twist angle at tip (deg)
  dhdadi?: number          // Inboard dihedral (deg)
  dhdado?: number          // Outboard dihedral (deg)
  type: 1 | 2 | 3          // 1=straight, 2=double delta, 3=cranked
}

// Wing/Surface Section ($WGSCHR, $HTSCHR, $VTSCHR)
export interface SectionCharacteristics {
  tovc: number             // t/c ratio at root
  tovco?: number           // t/c ratio at tip
  deltay?: number          // Airfoil camber designation
  xovc?: number            // x/c of max thickness
  xovco?: number           // x/c at tip
  cli?: number             // Design lift coefficient
  alphai?: number          // Ideal angle of attack
  clalpa?: number[]        // Lift curve slope vs Mach
  clmax?: number[]         // Max lift coefficient vs Mach
  clmaxl?: number          // Lower surface max lift
  cmo?: number             // Zero-lift pitching moment
  cmot?: number            // Tip zero-lift moment
  leri?: number            // LE radius index (root)
  lero?: number            // LE radius index (tip)
  clamo?: number           // Lift curve slope at M=0
  camber?: boolean         // Include camber effects
  tceff?: number           // Effective t/c ratio
}

// Symmetric Flaps ($SYMFLP)
export interface SymmetricFlaps {
  ftype: number            // Flap type (1=plain, 2=single slot, etc.)
  ndelta: number           // Number of deflection angles
  delta: number[]          // Deflection angles (deg)
  spanfi: number           // Inboard span station
  spanfo: number           // Outboard span station
  chrdfi: number           // Inboard flap chord
  chrdfo: number           // Outboard flap chord
  phete?: number           // Trailing edge angle
  phetep?: number          // Tip trailing edge angle
  cb?: number              // Balance chord
  tc?: number              // Tab chord
  ntype?: number           // Nose type
}

// Asymmetric Flaps/Ailerons ($ASYFLP)
export interface AsymmetricFlaps {
  stype: number            // Aileron type
  ndelta: number           // Number of deflections
  deltal?: number[]        // Left deflections
  deltar?: number[]        // Right deflections
  deltad?: number[]        // Differential deflections
  spanfi: number           // Inboard span
  spanfo: number           // Outboard span
  chrdfi: number           // Inboard chord
  chrdfo: number           // Outboard chord
  phete?: number           // Trailing edge angle
  xsprme?: number          // Spoiler location
}

// Propeller Power ($PROPWR)
export interface PropellerPower {
  aietlp?: number          // Prop location type
  nengsp?: number          // Number of engines
  thstcp?: number          // Thrust coefficient
  phaloc?: number          // Prop horizontal location
  phvloc?: number          // Prop vertical location
  prprad?: number          // Propeller radius
  engfct?: number          // Engine factor
  nopbpe?: number          // Number of blades
  bapr75?: number          // Blade angle at 75%
  yp?: number              // Propeller y-location
  crot?: boolean           // Counter-rotation
}

// Jet Power ($JETPWR)
export interface JetPower {
  aietlj?: number          // Jet location type
  nengsj?: number          // Number of engines
  thstcj?: number          // Thrust coefficient
  jialoc?: number          // Inlet location
  jevloc?: number          // Exit vertical location
  jealoc?: number          // Exit axial location
  jinlta?: number          // Inlet area
  jeangl?: number          // Exit angle
  jevelo?: number          // Exit velocity
  ambtmp?: number          // Ambient temperature
  jestmp?: number          // Jet temperature
  jelloc?: number          // Lateral location
  jetotp?: number          // Total pressure
  ambstp?: number          // Ambient static pressure
  jerad?: number           // Jet radius
}

// Complete DATCOM Input Configuration
export interface DatcomInput {
  caseid?: string
  dim?: 'FT' | 'M' | 'IN'  // Units
  deriv?: 'RAD' | 'DEG'    // Derivative units
  build?: boolean          // Configuration buildup
  part?: boolean           // Part contribution
  damp?: boolean           // Damping derivatives

  flight: FlightConditions
  reference: ReferenceParams
  synthesis: SynthesisParams
  body?: BodyGeometry

  wing?: SurfacePlanform
  wingSection?: SectionCharacteristics

  hTail?: SurfacePlanform
  hTailSection?: SectionCharacteristics

  vTail?: SurfacePlanform
  vTailSection?: SectionCharacteristics

  symFlap?: SymmetricFlaps
  asyFlap?: AsymmetricFlaps

  propeller?: PropellerPower
  jet?: JetPower
}

// ============================================
// DATCOM Output Types
// ============================================

export interface AeroCoefficients {
  alpha: number
  cd: number               // Drag coefficient
  cl: number               // Lift coefficient
  cm: number               // Pitching moment coefficient
  cn: number               // Normal force coefficient
  ca: number               // Axial force coefficient
  xcp?: number             // Center of pressure
  cla?: number             // dCL/dalpha
  cma?: number             // dCM/dalpha
  cyb?: number             // dCY/dbeta
  cnb?: number             // dCn/dbeta
  clb?: number             // dCl/dbeta
  clp?: number             // Roll damping
  cmq?: number             // Pitch damping
  cnr?: number             // Yaw damping
  clda?: number            // Roll due to aileron
  cnda?: number            // Yaw due to aileron
  cmde?: number            // Pitch due to elevator
}

export interface DatcomOutput {
  caseId: string
  mach: number
  reynolds: number
  altitude?: number
  coefficients: AeroCoefficients[]
  warnings?: string[]
  errors?: string[]
}

export interface DatcomResults {
  cases: DatcomOutput[]
  rawOutput?: string
}

// ============================================
// CAD Export Types (for STEP/SolidWorks)
// ============================================

export interface Point3D {
  x: number
  y: number
  z: number
}

export interface AirfoilProfile {
  upper: Point3D[]
  lower: Point3D[]
}

export interface CADExportOptions {
  format: 'STEP' | 'STL' | 'OBJ'
  resolution: 'low' | 'medium' | 'high'
  includeBody: boolean
  includeWing: boolean
  includeHTail: boolean
  includeVTail: boolean
}

// For backward compatibility
export type WingPlanform = SurfacePlanform
export type WingSection = SectionCharacteristics
