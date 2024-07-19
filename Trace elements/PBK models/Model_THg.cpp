// Model PBK for mercury (inorganic mercury and methylmercury).

$PARAM

// Individual parameters
	SEXBABY = 1   // Sex of indivial (1 == Male, 0 == Female)

	
// Body weight
	Delta_BW = 1 // Factor of variation for height
	Delta_HT = 1 // Factor of variation for bodyweight
  Delta_creat = 1
// Variability for male
	Delta_Brain_M 	 = 1
	Delta_Kidney_M 	 = 1
	Delta_Liver_M 	 = 1
	Delta_Pancreas_M = 1
	Delta_Stomach_M  = 1
	Delta_Small_Intestine_M = 1
	Delta_Large_Intestine_M = 1
	Delta_Heart_M 	 = 1
	Delta_Bone_M 	 = 1
	Delta_Skin_M 	 = 1
	Delta_Gonad_M 	 = 1
	Delta_Lung_M 	 = 1
	Delta_Spleen_M 	 = 1
	Delta_Muscle_M 	 = 1
	Delta_Adipose_M  = 1

// Variability for female
	Delta_Brain_F 	 = 1
	Delta_Kidney_F 	 = 1
	Delta_Liver_F 	 = 1
	Delta_Pancreas_F = 1
	Delta_Stomach_F  = 1
	Delta_Small_Intestine_F = 1
	Delta_Large_Intestine_F = 1
	Delta_Heart_F 	 = 1
	Delta_Bone_F 	 = 1
	Delta_Skin_F 	 = 1
	Delta_Gonad_F 	 = 1
	Delta_Lung_F 	 = 1
	Delta_Spleen_F 	 = 1
	Delta_Muscle_F 	 = 1
	Delta_Adipose_F  = 1

// Coefficient de partition
	pbr  = 3.0   // Brain_plasma
	pf   = 0.15  // Fat_plasma
	pg   = 1.0   // Gut_plasma
	ph   = 248.7 // Hair_plasma
	pk   = 4.0   // Kidney_plasma
	pl   = 5.0   // Liver_plasma
	pr   = 1.0   // Richly_plasma
	ps   = 2.0   // Slowly_plasma

// Smoking status
age_deb = 18.6    // Beginning of smoking 
age_fin = 38.2    // End of smoking
smoke_hg = 0      // ug/ per cigaret
nb_cig = 15       // Consumption of cigarets by day


$MAIN

/// Calculation of variability considering gender
	double Delta_Brain     = SEXBABY * Delta_Brain_M     + (1 - SEXBABY) * Delta_Brain_F;
	double Delta_Kidney    = SEXBABY * Delta_Kidney_M    + (1 - SEXBABY) * Delta_Kidney_F;
	double Delta_Liver     = SEXBABY * Delta_Liver_M     + (1 - SEXBABY) * Delta_Liver_F;
	double Delta_Pancreas  = SEXBABY * Delta_Pancreas_M  + (1 - SEXBABY) * Delta_Pancreas_F;
	double Delta_Stomach   = SEXBABY * Delta_Stomach_M   + (1 - SEXBABY) * Delta_Stomach_F;
	double Delta_Intestine = SEXBABY * (0.63 * Delta_Small_Intestine_M + 0.37 * Delta_Large_Intestine_M) + (1 - SEXBABY) * (0.63 * Delta_Small_Intestine_F + 0.37 * Delta_Large_Intestine_F);
	double Delta_Heart     = SEXBABY * Delta_Heart_M     + (1 - SEXBABY) * Delta_Heart_F;
	double Delta_Bone      = SEXBABY * Delta_Bone_M      + (1 - SEXBABY) * Delta_Bone_F;
	double Delta_Skin      = SEXBABY * Delta_Skin_M      + (1 - SEXBABY) * Delta_Skin_F;
	double Delta_Gonad     = SEXBABY * Delta_Gonad_M     + (1 - SEXBABY) * Delta_Gonad_F;
	double Delta_Lung      = SEXBABY * Delta_Lung_M      + (1 - SEXBABY) * Delta_Lung_F;
	double Delta_Spleen    = SEXBABY * Delta_Spleen_M    + (1 - SEXBABY) * Delta_Spleen_F;
	double Delta_Muscle    = SEXBABY * Delta_Muscle_M    + (1 - SEXBABY) * Delta_Muscle_F;
	double Delta_Adipose   = SEXBABY * Delta_Adipose_M   + (1 - SEXBABY) * Delta_Adipose_F;

// Evolution of age over integrations
double year = TIME/365;
double month = TIME/(365/12);

///// Physiological changes ////
  /// Body weight (in kg) and Height (in cm) /// Equation performed on the French population.
 double wbw = 1;
  double ht = 1;
  if(SEXBABY==1){
    wbw = (3.938425 + 0.7518199*month - 0.02023793*pow(month,2) + 0.0002921682*pow(month,3) - 0.00000206762*pow(month,4) + 0.000000008469*pow(month,5) - 0.00000000002188427*pow(month,6) + 3.699776e-14*pow(month,7) - 4.099077e-17*pow(month,8) + 2.874804e-20*pow(month,9) - 1.159732e-23*pow(month,10) + 2.052602e-27*pow(month,11))*Delta_BW;
    ht = (64.35+8.146e-01*month - 7.474e-04*pow(month,2) - 6.322e-06*pow(month,3) + 1.903e-08*pow(month,4) - 1.995e-11*pow(month,5) + 7.297e-15*pow(month,6))*Delta_HT;}
  
  else{wbw = (3.932403 + 0.6866462*month - 0.01949911*pow(month,2) + 0.00031311*pow(month,3) - 0.000002466654*pow(month,4) + 0.00000001113217*pow(month,5) - 0.00000000003131402*pow(month,6) + 5.693737e-14*pow(month,7) - 6.706947e-17*pow(month,8) + 4.947858e-20 *pow(month,9) - 2.079251e-23*pow(month,10) + 3.800367e-27*pow(month,11))*Delta_BW;
       ht = (57.63+1.083*month-3.679e-03*pow(month,2)+4.633e-06*pow(month,3)-4.226e-12*pow(month,5)+2.302e-15*pow(month,6))*Delta_HT;}
  
  double wbw_23 = 1;
  if(year >= 23){
    if (SEXBABY == 1){wbw_23 = (3.938425 + 7.518199e-01*23*12 - 2.023793e-02*pow(23*12,2) + 2.921682e-04*pow(23*12,3) - 2.06762e-06*pow(23*12,4) + 8.469e-09*pow(23*12,5) - 2.188427e-11*pow(23*12,6) + 3.699776e-14*pow(23*12,7) - 4.099077e-17*pow(23*12,8) + 2.874804e-20*pow(23*12,9) - 1.159732e-23*pow(23*12,10) + 2.052602e-27*pow(23*12,11))*Delta_BW;}
    else{wbw_23 = (3.932403 + 0.6866462*23*12 - 0.01949911*pow(23*12,2) + 0.00031311*pow(23*12,3) - 0.000002466654*pow(23*12,4) + 0.00000001113217*pow(23*12,5) - 0.00000000003131402*pow(23*12,6) + 5.693737e-14*pow(23*12,7) - 6.706947e-17*pow(23*12,8) + 4.947858e-20 *pow(23*12,9) - 2.079251e-23*pow(23*12,10) + 3.800367e-27*pow(23*12,11))*Delta_BW;}}
  
  double wbw_18 = 1;
  double ht_18 = 1;
  if(year >= 18){if(SEXBABY==1){
    wbw_18 = (3.938425 + 0.7518199*18*12 - 0.02023793*pow(18*12,2) + 0.0002921682*pow(18*12,3) - 0.00000206762*pow(18*12,4) + 0.000000008469*pow(18*12,5) - 0.00000000002188427*pow(18*12,6) + 3.699776e-14*pow(18*12,7) - 4.099077e-17*pow(18*12,8) + 2.874804e-20*pow(18*12,9) - 1.159732e-23*pow(18*12,10) + 2.052602e-27*pow(18*12,11))*Delta_BW;
    ht_18 = (64.35+8.146e-01*18*12 - 7.474e-04*pow(18*12,2) - 6.322e-06*pow(18*12,3) + 1.903e-08*pow(18*12,4) - 1.995e-11*pow(18*12,5) + 7.297e-15*pow(18*12,6))*Delta_HT;}
  
  else{wbw_18 =  (3.932403 + 0.6866462*18*12 - 0.01949911*pow(18*12,2) + 0.00031311*pow(18*12,3) - 0.000002466654*pow(18*12,4) + 0.00000001113217*pow(18*12,5) - 0.00000000003131402*pow(18*12,6) + 5.693737e-14*pow(18*12,7) - 6.706947e-17*pow(18*12,8) + 4.947858e-20 *pow(18*12,9) - 2.079251e-23*pow(18*12,10) + 3.800367e-27*pow(18*12,11))*Delta_BW;
       ht_18 = (57.63+1.083*18*12-3.679e-03*pow(18*12,2)+4.633e-06*pow(18*12,3)-4.226e-12*pow(18*12,5)+2.302e-15*pow(18*12,6))*Delta_HT;}}

  // Body Mass Index (in kg/m^2)
  double BMI = wbw/pow(ht/100,2);
  
  // Hematocrit (proportion of RBC in blood)
  double hct = 1;
    //Mallick et al. (2020) or Pendse et al. (2020)
        if(year < 2){hct = 0.359;}
        if(year >=2 && year < 18){hct = ((1.12815e-06 * pow(year,3)) - (1.72362e-04 * pow(year,2)) + (8.15264e-03 * year) + 0.327363);}
        else {hct = ((1.12815e-06 * pow(18,3)) - (1.72362e-04 * pow(18,2)) + (8.15264e-03 * 18) + 0.327363);}
        
  /// Poids des organes
  
  // Fat (non essential adipose tissues) (in L)
  double vfat = 0.273;
       // Deepika et al. (2021)
      if(SEXBABY == 1){vfat = 1.3054356 + 0.3622685*year - 0.0025165*pow(year,2) + 0.0906119*wbw + 0.0001731*pow(wbw,2) * Delta_Adipose;}
      else{vfat = 6.132e-01+8.475e-02*year + 8.151e-05*pow(year,2) + 1.341e-01*wbw + 2.297e-03*pow(wbw,2) * Delta_Adipose;}
    
  // Brain (in L)
      double vbr = 0.02;
        // Beaudouin et al. 2010
          if(SEXBABY == 1){vbr = 1.45 + (0.350-1.45)*exp(-0.440*year) * Delta_Brain;}
          else {vbr = 1.30 + (0.347-1.30)*exp(-0.573*year) * Delta_Brain;}
      
  // Kidneys
  double vk = 0.004;
    // Beaudouin et al. 2010
      if(SEXBABY == 1){vk = (0.0042+(0.00767-0.0042)*exp(-0.206*year))*wbw * Delta_Kidney;}
      else {vk = (0.0046+(0.00709-0.0046)*exp(-0.221*year))*wbw * Delta_Kidney;}
  
  // Hair
  double vhair = 0.002*wbw;
  
  // Liver
  double vl = 0.026;
    // Beaudouin et al. 2010
      if(SEXBABY == 1){vl = (0.0247+(0.0409-0.0247)*exp(-0.218*year))*wbw * Delta_Liver;}
      else{vl = (0.0233+(0.038-0.0233)*exp(-0.122*year))*wbw * Delta_Liver;}
    
  /// BLOOD (composition = (1-HCT)% plasma + HCT% RBC)(in L)
    double vb   = 1; // Blood volume
    double vp   = 1; // Plasma volume
    double vrbc = 1; // Red blood cell volume

// Beaudouin et al. (2010)
      if(SEXBABY == 1){
        vb = (0.0761*(1/1+exp(-0.683*year+0.946)))*wbw;
      }
      else{
        if(year < 1){vb = (-0.0273*year + 0.0771)*wbw;}
        if(year >= 1 && year <= 14.019723){vb = (3.28e-05*pow(year,3) - 1.21e-03*pow(year,2) + 1.24e-02*year + 3.86e-02) * wbw;}
        else{vb = 0.065 * wbw;}
      }
      vp = (1-hct)*vb;
      vrbc = hct*vb;
    

  // Slowly perfused tissue (Heart, Diaphragm, muscles, skin, bones)
  double vheart = 1;

    // Beaudouin et al. 2010
      if(SEXBABY == 1){vheart = 0.0045 * wbw * Delta_Heart;}
      else{vheart = 0.004167 * wbw * Delta_Heart;}

  /// Muscles
    double vm = 1;

    // Beaudouin et al. 2010
      if(SEXBABY == 1){
        if(year < 24.3){vm = (0.3973+(0.201-0.3973)*exp(-0.141*year)) * wbw * Delta_Muscle;}
        else{vm = (0.3973+(0.201-0.3973)*exp(-0.141 * year))* (-0.0001264 * pow(year,2) + 0.006131*year + 0.926) * wbw * Delta_Muscle;} 
      }
      else{
        if(year <= 25.90709){vm = (0.2917+(0.207-0.2917)*exp(-0.339*year))* wbw * Delta_Muscle;}
        else{vm = (0.2917+(0.207-0.2917)*exp(-0.339*year))* (-0.0001264 * pow(year,2) + 0.006131*year + 0.926) * wbw * Delta_Muscle;}
      }

  /// Diaphragm
  double vd = (3e-04)*wbw;
  
  /// Skin
  double vs = 1;
  double vderm = 1;
  double vepid = 1;

    // Beaudouin et al. (2010)
      if(SEXBABY == 1){
        if(year < 20){vs = (-1.171e-05* pow(year,3) + 5.413e-04* pow(year,2) -  6.1966e-03* year + 4.623e-02) * wbw * Delta_Skin;}
        else{vs = 0.0452 * wbw;}}
      else{
        if(year < 20){vs = (-7.8882e-06*pow(year,3) + 4.0224e-04*pow(year,2) - 5.2146e-03*year + 4.5605e-02) * wbw * Delta_Skin;}
        else{vs = 0.0383 * wbw;}}
  

  /// Skeleton (Marrow / Bones = 80 % Cortical + 20% Trabecular)
  double vbone = 1;
  double vmarr = 1;
    // Pearce et al. (2017) - Httk
      double TBBMC = 1; // Total Body Bone Mineral Content
        if(SEXBABY == 1){
          if(year < 50){TBBMC = 0.89983 + ((2.9901-0.89989)/(1+1*exp((14.17081-year)/1.58179))) - (0*year);}
          else{TBBMC = 0.89983 + ((2.9901-0.89989)/(1+1*exp((14.17081-year)/1.58179))) - (0.0019*year);}}
        else{
          if(year < 50){TBBMC = 0.74042 + ((2.14976-0.74042)/(1+1*exp((12.35466-year)/1.35750))) - (0*year);}
          else{TBBMC = 0.74042 + ((2.14976-0.74042)/(1+1*exp((12.35466-year)/1.35750))) - (0.0056*year);}}
          
    vbone = (TBBMC/0.65)/0.5 * Delta_Bone;
    
  /// Bone marrow (71 % Red Marrow + 29% Yellow Marrow)

    // Beaudouin et al. (2010)
      if(SEXBABY == 1){vmarr = (0.05+(0.0138-0.05)*exp(-0.112*year)) * wbw;}
      else{vmarr = (0.045+(0.0138-0.045)*exp(-0.136*year))*wbw;}
    
  // Richly perfused tissue (Breast + Thyroid + Spleen + Pancreas + Adrenals + Gonads + Lungs)
  
    // Breast (Beaudouin et al. (2010))
    double vbreast = 1;
      if(SEXBABY == 1){vbreast = (3.42e-4 * (1 / (1 + exp(-1.42 * year + 20.1)))) * wbw;}
      else{vbreast = (0.00833 * 1/(1 + exp(-1.92 * year + 28.6))) * wbw;}
    
    // Thyroid
      double vthyr = 1;
      // Beaudouin et al. (2010)
        if(SEXBABY == 1){vthyr = 0.000274 * wbw;}
        else{vthyr = 0.0002833 * wbw;}

    // Spleen
      double vspleen = 1;
      // Beaudouin et al. (2010)
        if(SEXBABY == 1){vspleen = 0.0021 * wbw * Delta_Spleen;}
        else{vspleen = 0.0022 * wbw * Delta_Spleen;}

    // Pancreas
    double vpancreas = 1;
      // Beaudouin et al. (2010)
        if(SEXBABY == 1){vpancreas = 0.00192 * wbw * Delta_Pancreas;}
        else{vpancreas = 0.002 * wbw * Delta_Pancreas;}

    // Adrenals (Beaudouin et al. (2010))
    double vadrenal = 0;
    if(SEXBABY == 1){vadrenal = 2.0e-04 + (1.71e-03 - 2.0e-04) * exp(-2.02*year);}
    else{vadrenal = 0.0002 + (0.00171 - 0.0002) * exp(-2.02*year);}
    
    // Gonads / Reproductive organs
    double vgonads = 1;

      // Beaudouin et al. (2010)
        if(SEXBABY == 1){
          if(year < 20.1){vgonads = (-1.516e-07*pow(year,3) + 9.3351e-06*pow(year,2) - 1.1177e-04*year + 4.7966e-04) * wbw * Delta_Gonad;}
          else {vgonads = 0.0008 * wbw * Delta_Gonad;}}
        else{
          if(year < 1){vgonads = (-1.064e-03*year + 1.338e-03)* wbw * Delta_Gonad;}
          if(year >= 1 && year < 20){vgonads = (2.6380e-07*pow(year,3) - 1.7943e-06*pow(year,2) - 5.6465e-06*year + 2.8105e-04) * wbw * Delta_Gonad;}
          else{vgonads = 0.001552 * wbw * Delta_Gonad;}}
       
    // Lungs
    double vlungs = 1;
      // Beaudouin et al. (2010)
        if(SEXBABY == 1){vlungs = 0.0068 * wbw * Delta_Lung;}
        else{vlungs = 0.0070 * wbw * Delta_Lung;}
    



  /// Gut (Stomach + Small intestine + Large intestine)
    // Stomach
    double vstomach = 1;
      // Beaudouin et al. (2010)
        if(SEXBABY == 1){vstomach = 0.0021*wbw*Delta_Stomach;}
        else{vstomach = 0.0023 * wbw * Delta_Stomach;}
 
    // Intestinal tract (Small + Large Intestine)
    double vintestine = 1;
      // Beaudouin et al. (2010)
        if(SEXBABY == 1){
          if(year < 16){vintestine = (-8.2562e-05*pow(year,2) + 1.3523e-03*year + 1.293e-02) * wbw * Delta_Intestine;}
          else{vintestine = 0.014 * wbw * Delta_Intestine;}}
        else{
          if(year<14.453301){vintestine = (-7.421e-05*pow(year,2) + 1.276e-03*year + 1.298e-02) * wbw * Delta_Intestine;}
          else{vintestine = 0.0160 * wbw * Delta_Intestine;}}
    

    // Calculation of the total body weight
    double wbw_f = vfat + vbr + vk + vhair + vb + vl + vheart + vd + vm + vs + vbone + vmarr + vstomach + vintestine + vbreast + vthyr + vspleen + vpancreas + vadrenal + vgonads + vlungs;

    // Extrapolation of the body composition / compartimentation
    double LBM = wbw_f - 1.01*vfat; //Lean Body Mass = Body weight less adipose tissues (essential fat = 10% of adipose tissues)
    double FFM = wbw_f - vfat;      //Free Fat Mass = LBM + Essential fat (without non essential fat = FAT)
    

    double vgut = vstomach + vintestine;

    // Lumping of organs for Hg PBK model
    double vslow = vheart + vd + vm + vs + vbone + vmarr;
    double vrich = vbreast + vthyr + vspleen + vpancreas + vadrenal + vgonads + vlungs;
    
/// Cardiac output
double Q = 1;
    
  // Beaudouin et al. (2010)
    if(SEXBABY == 1){
      if(year <= 33.37){Q = (6.642+(0.6-6.642)*exp(-0.1323*year))*60*24;}
      else{Q = (-0.000895*pow(year,2) + 0.0607*year + 5.54)*60*24;}
    }
    else{
      if(year < 16.027){Q = (7.734+(0.6-7.734)*exp(-0.09747*year))*60*24;}
      else{Q = (0.000473*pow(year,2) - 0.0782*year + 7.37)*60*24;}}
   
/// The blood flow rate come from ICRP (2002)
// Fat
double qfat = 1;
  if(SEXBABY == 1){qfat = 0.05*Q;}
  else {qfat = 0.085*Q;}

// Brain
double qbr = 0.12*Q;

// Kidney
double qk = 1;
  if(SEXBABY == 1){qk = 0.19*Q;}
  else {qk = 0.17*Q;}

// Liver (only from arterial blood)
double ql = 0.065*Q;

// Heart
double qheart = 1;
  if(SEXBABY == 1){qheart = 0.04*Q;}
  else {qheart = 0.05*Q;}
  
// Diaphragm
double qd = 0.0003*Q;

// Muscles
double qm = 1;
  if(SEXBABY == 1){qm = 0.17*Q;}
  else{qm = 0.12*Q;}
  
// Skin
double qs = 0.05*Q;

// Bone
double qbone = 0.02*Q;

// Marrow
double qmarr = 0.03*Q;

// Stomach
double qstomach = 0.01*Q;

// Intestine
double qintestine = 1;
  if(SEXBABY == 1){qintestine = (0.1+0.04)*Q;}
  else{qintestine = (0.11+0.05)*Q;}

// Breast
double qbreast = 1;
  if(SEXBABY == 1){qbreast = 0.0002*Q;}
  else{qbreast = 0.004*Q;}

// Thyroid
double qthyr = 0.015*Q;

// Spleen
double qspleen = 0.03*Q;

// Pancreas
double qpancreas = 0.01*Q;

// Adrenals
double qadrenal = 0.003*Q;

// Gonads
double qgonads = 1;
  if(SEXBABY == 1){qgonads = 0.0005*Q;}
  else {qgonads = 0.0002*Q;}
  
// Lungs (as bronchial tissues)
double qlungs = 0.025*Q;

/// Relativisation to have the sum of rate equal to 1
double Q_f = qfat + qbr + qk + ql + qheart + qd + qm + qs + qbone + qmarr + qstomach + qintestine + qbreast + qthyr + qspleen + qpancreas + qadrenal + qgonads + qlungs;

    double qfat_f       = qfat*Q_f/Q;
    double qbr_f        = qbr*Q_f/Q;
    double qk_f         = qk*Q_f/Q;
    double ql_f         = ql*Q_f/Q;
    double qheart_f     = qheart*Q_f/Q;
    double qd_f         = qd*Q_f/Q;
    double qm_f         = qm*Q_f/Q;
    double qs_f         = qs*Q_f/Q;
    double qbone_f      = qbone*Q_f/Q;
    double qmarr_f      = qmarr*Q_f/Q;
    double qstomach_f   = qstomach*Q_f/Q;
    double qintestine_f = qintestine*Q_f/Q;
    double qbreast_f    = qbreast*Q_f/Q;
    double qthyr_f      = qthyr*Q_f/Q;
    double qspleen_f    = qspleen*Q_f/Q;
    double qpancreas_f  = qpancreas*Q_f/Q;
    double qadrenal_f   = qadrenal*Q_f/Q;
    double qgonads_f    = qgonads*Q_f/Q;
    double qlungs_f     = qlungs*Q_f/Q;

// Lumping of organs
double qgut  = qstomach_f + qintestine_f;
double qslow = qheart + qd + qm + qs + qbone + qmarr;
double qrich = qbreast + qthyr + qspleen + qpancreas + qadrenal + qgonads + qlungs;
    
double Vurine = 0.0294 * wbw_f;

/// Inhalation
  // Volume of tidal
  double Vtidal = 0;
    if(SEXBABY == 1){
      if(year < 21){Vtidal = 0.0337 * year + 0.0407;}
      else{Vtidal = 0.75;}}
    else{Vtidal = 0.46 + (0.0392 - 0.46) * exp(-0.127 * year);}
	
	double FqBreath = 12 + (38.9 - 12) * exp(-0.176 * year); // Breath frequency by day

  // Volume of death space in lungs
  double vds = 0;
    if(SEXBABY == 1){
      if(year <= 18.4){vds = 0.0076 * year + 0.0101;}
      else{vds = 0.15;}}
    else{vds = 0.12 + (0.0107-0.12) * exp(-0.0986 * year);}

	double VInhalation = (FqBreath * (Vtidal - vds) * 60 * 24)/1000; // Volume of inhalation by day

  // Dose of Cd by smoking exposures
  double Cig = 0;
    if(year > age_deb && year < age_fin){Cig = nb_cig * smoke_hg;}
    else {Cig = 0;}


/// Kinetic parameters
double kbr = 1.2e-05*(24)*pow(wbw_f,0.75); // Demethylation rate in brain
double kbi = 1.0e-04*(24)*pow(wbw_f,0.75); // Biliary clearance of MeHg
double kbrup = 0.01*(24)*pow(wbw_f,0.75);  // Brain uptake
double ki = 1.0e-04*24*pow(wbw_f,0.75);  // Demethylation rate in gut
double kfe= 0;                       // Fecal excretion
  if (year<=1) {kfe = 0.00208*(24);}
  else {if(SEXBABY==1){kfe = 0.00625*24;}
  else{kfe = 0.00500*(24);}}
double kha = 24*pow(wbw,0.75)*7.0e-6;        // Excretion into hair
double kl = 0.00001*24*pow(wbw_f,0.75);  // Demethylation rate in liver 
double krbc = 1.5*(24)*pow(wbw_f,0.75);    // Red cells / Plasma diffusion
double kir = 0.005*(24)*pow(wbw_f,0.75);   // Reabsorption rate in gut
double kabs = 0.3*24;   // Absorption rate in gut
double kex = 0.10*24;   // Excretion rate from gut tissue to gut lumen

double kRPl = 0.3*24;   // From RBC to plasma
double kPlR = 3*24;   // From Plasma to RBC

// Kinetic parameters for inorganic mercury
double dbl = 0.1750;     //Blood to liver transfert coefficient combined with liver metabolisme rate constant of organic mercury
double klb = 0.8940;     //Liver to blood transfer coeff.
double kbl = 0;          //Blood to liver transfer coeff.
double kbk = 17.1234;    //Blood to kidney transfer coeff.
double kkb = 0.0010;     //Kidney to blood transfer coeff.
double kku = 0.006949;   //Kidney to urine transfer coeff.
double kbh = 0.1400;     //Blood to hair transfer coeff.
double kbu = 0.06994;    //Blood to urine transfer coeff.
double kbf = 3.9917;     //Blood to feces transfer coeff.
double klf = 1.5476;     //Liver to feces transfer coeff.
double kbbr = 0.0028;    //Blood to brain transfer coeff.
double kbrb = 0.0520;    //Brain to blood transfer coeff.


// Creatinine in function of the age (C.Béchaux)
double ucr= (0.032+2.098e-02*year+9.104e-03*pow(year,2)-4.550e-04*pow(year,3)+7.578e-06*pow(year,4)-4.232e-08*pow(year,5));
if (year>70) {ucr=(0.032+2.098e-02*70+9.104e-03*pow(70,2)-4.550e-04*pow(70,3)+7.578e-06*pow(70,4)-4.232e-08*pow(70,5));}


double CH = HAIR/vhair;
double CKHg = (KIDNEY+KIDNEYI)/vk;
double CLHg = (LIVER+LIVERI)/vl;
double CUHg = (KIDNEYI/vk*kku + PLASMAI/vb*kbu)/Vurine;
double CUHgcr = (KIDNEYI/vk*kku + PLASMAI/vb*kbu)/ucr;
double CBrHg = (BRAIN + BRAINI)/vbr;

$ODE
dxdt_expohgi  = - expohgi;
dxdt_expomehg = - expomehg;

dxdt_AIR = - AIR;

dxdt_SOIL = - SOIL;

dxdt_INTESTINE = expomehg*wbw_f*0.95 - INTESTINE*kabs + LIVER/vl*kbi - INTESTINE*kfe -  INTESTINE*ki + GUT/vgut*kex;

dxdt_GUT = INTESTINE*kabs + qgut*(PLASMA/vp - GUT/vgut/pg) - GUT/vgut*kex;

dxdt_LIVER = PLASMA/vp*ql + GUT/vgut/pg*qgut - LIVER/vl/pl*(ql+qgut) - LIVER/vl*kbi - LIVER/vl*kl;

dxdt_LIVERI = PLASMAI/vb*kbl + LIVER/vl*kl - LIVERI/vl*klf - LIVERI/vl*klb;

dxdt_PLASMA = -(kPlR*PLASMA/vp - kRPl*RBC/vrbc) - (PLASMA/vp*qk - KIDNEY/vk/pk*qk) -
  (PLASMA/vp*qrich - RICHLY/vrich/pr*qrich) - (PLASMA/vp*qfat - FAT/vfat/pf*qfat) - (PLASMA/vp*qslow - SLOWLY/vslow/ps*qslow) -
  (PLASMA/vp*qbr - BRAIN/vbr/pbr*qbr) - (PLASMA/vp*ql+PLASMA/vp*qgut - LIVER/vl/pl*(ql+qgut));
dxdt_PLASMAI = (expohgi*wbw + SOIL)*0.15 + AIR * VInhalation * 0.85 + BRAINI/vbr*kbrb + KIDNEYI/vk*kkb - PLASMAI/vb*(kbh+kbf+kbu+kbbr+kbk+kbl) + LIVERI/vl*klb;

dxdt_RBC = kPlR*PLASMA/vp - kRPl*RBC/vrbc;

dxdt_BRAIN = (PLASMA/vp - BRAIN/vbr/pbr)*qbr - BRAIN/vbr*kbr;
dxdt_BRAINI = BRAIN/vbr*kbr - BRAINI/vbr*kbrb + PLASMAI/vb*kbbr;

dxdt_SLOWLY = (PLASMA/vp - SLOWLY/vslow/ps)*qslow - (SLOWLY/vslow/ps*ph)*kha ;

dxdt_HAIR = (SLOWLY/vslow/ps*ph - HAIR/vhair)*kha + PLASMAI/vb*kbh;

dxdt_FAT = qfat*(PLASMA/vp - FAT/vfat/pf);

dxdt_KIDNEY = qk*(PLASMA/vp - KIDNEY/vk/pk);
dxdt_KIDNEYI= PLASMAI/vb*kbk - KIDNEYI/vk*(kkb+kku);

dxdt_RICHLY = qrich*(PLASMA/vp - RICHLY/vrich/pr);

dxdt_URINE = (KIDNEYI/vk*kku + PLASMAI/vb*kbu);



$CMT expohgi expomehg AIR SOIL INTESTINE GUT LIVER PLASMA RBC BRAIN SLOWLY HAIR FAT KIDNEY RICHLY URINE BRAINI PLASMAI LIVERI KIDNEYI



$CAPTURE CH CUHg CKHg CUHgcr CLHg CBrHg
