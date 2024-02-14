/// PBK Model for Methylmercury (Ou et al. 2018) ////

/// DOI : 10.1016/j.envint.2018.03.018

$PARAM // Partition coefficient

SEXBABY = 1 // Sex

pbr  = 30   // Brain_brain plasma
pbrb = 10   // Brain plasma_plasma
pf   = 1.5  // Fat_plasma
pg   = 10   // Gut_plasma
ph   = 248.7 // Hair_plasma
pk   = 40   // Kidney_plasma
pl   = 50   // Liver_plasma
pr   = 10   // Richly_plasma
ps   = 20   // Slowly_plasma

	
$MAIN

// Evolution of age

double year = TIME/365;
double month = TIME/(365/12);

///// Physiological changes ////
/// Body weight ///
double wbw = 1;
if(SEXBABY==1){
	wbw = 8.917-3.499e-02*month+4.152e-03*pow(month,2)-1.917e-05*pow(month,3)+3.621e-08*pow(month,4)-3.126e-11*pow(month,5)+1.019e-14*pow(month,6);}
else{wbw = 6.564+1.193e-01*month+2.412e-03*pow(month,2)-1.357e-05*pow(month,3)+2.832e-08*pow(month,4)-2.630e-11*pow(month,5)+9.097e-15*pow(month,6);}


/// Volume of organs
// Brain
double vbr = 0.02*wbw;

// Brain plasma
double vbp = 0.007*wbw;

// Kidneys
double vk = 0.004*wbw;

// Hair
double vh = 0.002*wbw;

// Fat
double vf = 0.273*wbw;

// Gut
double vg = 0.017*wbw;

// Gut lumen
double vi = 0.014*wbw;

// Liver
double vl = 0.026*wbw;

// Plasma
double vp = 0.024*wbw;

// Red cells
double vrbc = 0.024*wbw;

// Richly perfused tissue
double vr = 0.10*wbw;

// Slowly perfused tissue
double vs = 0.35*wbw;

/// Cardiac output
double Q = 20*24*pow(wbw,3/4);

// Brain
double qbr = 0.114*Q;

// Kidney
double qk = 0.175*Q;

// Liver
double ql = 0.046*Q;

// Fat
double qf = 0.052*Q;

//Richly perfused
double qr = 0.183*Q;

// Slowly perfused
double qs = 0.249*Q;

// GUT
double qg = 0.181*Q;

/// Kinetic parameters
double kbr = 1.2e-05*(24)*pow(wbw,3/4); // Demethylation rate in brain
double kbi = 1.0e-04*(24)*pow(wbw,3/4); // Biliary clearance of MeHg
double kbrup = 0.01*(24)*pow(wbw,3/4);  // Brain uptake
double ki = 1.0e-04*(24)*pow(wbw,3/4);  // Demethylation rate in gut
double kfe= 0;                       	// Fecal excretion (Pope and Rand, 2021)
  if (year<=1) {kfe = 0.00208*(24)*pow(wbw,3/4);}
  else {if(SEXBABY==1){kfe = 0.00625*(24)*pow(wbw,3/4);}
  else{kfe = 0.00500*(24)*pow(wbw,3/4);}}
double kha = 0.7;                       // Excretion into hair (Pope and Rand, 2021)
  if (year<=1) {kha = 2.3e-05*(24)*pow(wbw,3/4);}
  else {if(SEXBABY==1){kha = 4.0e-05*(24)*pow(wbw,3/4);}
  else{kha = 3.4e-05*(24)*pow(wbw,3/4);}}
double kl = 1.0e-05*(24)*pow(wbw,3/4);  // Demethylation rate in liver
double kir = 0.005*(24)*pow(wbw,3/4);   // Reabsorption rate in gut
double kabs = 0.3*(24)*pow(wbw,3/4);   	// Absorption rate in gut
double kex = 0.10*(24)*pow(wbw,3/4);   	// Excretion rate from gut tissue to gut lumen

double kRPl = 0.3*(24)*pow(wbw,3/4);   	// From RBC to plasma (Pope and Rand, 2021)
double kPlR = 3*(24)*pow(wbw,3/4);   	// From Plasma to RBC (Pope and Rand, 2021)


// Results: Concentration of MeHg in hair (in µg/kg)
double CH = HAIR/vh;

$ODE
dxdt_EXPO = - EXPO;

dxdt_INTESTINE = EXPO*wbw*0.95 - INTESTINE*kabs + LIVER/vl*kbi - INTESTINE*kfe -  INTESTINE*ki + GUT*kex;

dxdt_GUT = INTESTINE*kabs + qg*(PLASMA/vp - GUT/vg/pg) - GUT*kex;

dxdt_LIVER = PLASMA/vp*ql + GUT/vg/pg*qg - LIVER/vl/pl*(ql+qg) - LIVER/vl*kbi - LIVER/vl*kl;

dxdt_PLASMA = -(kPlR*PLASMA - kRPl*RBC) - (PLASMA/vp*qk - KIDNEY/vk/pk*qk) -
  (PLASMA/vp*qr - RICHLY/vr/pr*qr) - (PLASMA/vp*qf - FAT/vf/pf*qf) - (PLASMA/vp*qs - SLOWLY/vs/ps*qs) -
  (PLASMA/vp*qbr - BRAINB/vbp/pbrb*qbr) - (PLASMA/vp*ql+PLASMA/vp*qg - LIVER/vl/pl*(ql+qg));

dxdt_RBC = kPlR*PLASMA - kRPl*RBC;

dxdt_BRAINB = PLASMA/vp*qbr - BRAINB/vbp/pbrb*qbr + BRAIN/vbr/pbr*kbrup - BRAINB/vbp*kbrup;
dxdt_BRAIN = BRAINB/vbp*kbrup - BRAIN/vbr/pbr*kbrup - BRAIN/vbr*kbr;

dxdt_SLOWLY = PLASMA/vp*qs - SLOWLY/vs/ps*qs - SLOWLY/vs/ps*ph*kha;

dxdt_HAIR = SLOWLY/vs/ps*ph*kha - HAIR/vh*kha;

dxdt_FAT = qf*(PLASMA/vp - FAT/vf/pf);

dxdt_KIDNEY = qk*(PLASMA/vp - KIDNEY/vk/pk);

dxdt_RICHLY = qr*(PLASMA/vp - RICHLY/vr/pr);


$CMT EXPO INTESTINE GUT LIVER PLASMA RBC BRAINB BRAIN SLOWLY HAIR FAT KIDNEY RICHLY

$CAPTURE CH year
