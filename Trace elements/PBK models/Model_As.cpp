$PARAM

// Individual parameters
	SEXBABY = 1   // Sex of indivial (1 == Male, 0 == Female)
  smoke_As = 1
  nb_cig = 0
  age_deb = 0
  age_fin = 0
// Variability for male
	Delta_Brain_M = 1
	Delta_Kidney_M = 1
	Delta_Liver_M = 1
	Delta_Pancreas_M = 1
	Delta_Stomach_M = 1
	Delta_Small_Intestine_M = 1
	Delta_Large_Intestine_M = 1
	Delta_Heart_M = 1
	Delta_Bone_M = 1
	Delta_Gonad_M = 1
	Delta_Lung_M = 1
	Delta_Spleen_M = 1
	Delta_Muscle_M = 1
	Delta_Adipose_M = 1

// Variability for female
	Delta_Brain_F = 1
	Delta_Kidney_F = 1
	Delta_Liver_F = 1
	Delta_Pancreas_F = 1
	Delta_Stomach_F = 1
	Delta_Small_Intestine_F = 1
	Delta_Large_Intestine_F = 1
	Delta_Heart_F = 1
	Delta_Bone_F = 1
	Delta_Gonad_F = 1
	Delta_Lung_F = 1
	Delta_Spleen_F = 1
	Delta_Muscle_F = 1
	Delta_Adipose_F = 1

// Body weight
	Delta_BW = 1 // Factor of variation for height
  Delta_creat = 1
/// Coefficient de partition
	// Arsenic 5+
	pgas5  = 2.7  // Gut_plasma As(V)
	psas5  = 7.9  // Skin_plasma As(V)
	pbas5  = 2.4  // Brain_plasma As(V)
	pmas5  = 7.9  // Muscle_plasma As(V)
	pkas5  = 8.3  // Kidney_plasma As(V)
	plias5 = 15.8 // Liver_plasma As(V)
	pluas5 = 2.1  // Lung_plasma As(V)
	phas5  = 7.9  // Heart_plasma As(V)

	// Arsenic 3+
	pgas3  = 8.3  // Gut_plasma As(III)
	psas3  = 7.4  // Skin_plasma As(III)
	pbas3  = 2.4  // Brain_plasma As(III)
	pmas3  = 7.4  // Muscle_plasma As(III)
	pkas3  = 11.7 // Kidney_plasma As(III)
	plias3 = 16.5 // Liver_plasma As(III)
	pluas3 = 6.7  // Lung_plasma As(III)
	phas3  = 7.4  // Heart_plasma As(III)

	// Mono-methyl Arsenic
	pgmma  = 2.2  // Gut_plasma MMA
	psmma  = 2.61 // Skin_plasma MMA
	pbmma  = 2.2  // Brain_plasma MMA
	pmmma  = 2.61 // Muscle_plasma MMA
	pkmma  = 4.4  // Kidney_plasma MMA
	plimma = 3.3  // Liver_plasma MMA
	plumma = 1.3  // Lung_plasma MMA
	phmma  = 2.61 // Heart_plasma MMA

	// Di-methyl Arsenic
	pgdma  = 2.1  // Gut_plasma DMA
	psdma  = 2.4  // Skin_plasma DMA
	pbdma  = 3.3  // Brain_plasma DMA
	pmdma  = 2.4  // Muscle_plasma DMA
	pkdma  = 3.8  // Kidney_plasma DMA
	plidma = 3.3  // Liver_plasma DMA
	pludma = 1.3  // Lung_plasma DMA
	phdma  = 2.4  // Heart_plasma DMA

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
	double Delta_Gonad     = SEXBABY * Delta_Gonad_M     + (1 - SEXBABY) * Delta_Gonad_F;
	double Delta_Lung      = SEXBABY * Delta_Lung_M      + (1 - SEXBABY) * Delta_Lung_F;
	double Delta_Spleen    = SEXBABY * Delta_Spleen_M    + (1 - SEXBABY) * Delta_Spleen_F;
	double Delta_Muscle    = SEXBABY * Delta_Muscle_M    + (1 - SEXBABY) * Delta_Muscle_F;
	double Delta_Adipose   = SEXBABY * Delta_Adipose_M   + (1 - SEXBABY) * Delta_Adipose_F;

// Evolution of age over integrations
	double year = TIME/365;

  /// Body weight (in kg) and Height (in cm) /// Equation performed for French population.
  
	// Lifetime equation of weight (in kg)
 //Body weight (kg)
double wbw = 0;
  if(SEXBABY == 1){wbw = (3.938425 + 0.7518199*year*12 - 0.02023793*pow(year*12,2) + 0.0002921682*pow(year*12,3) - 2.06762e-06*pow(year*12,4) + 8.469e-09*pow(year*12,5) - 2.188427e-11*pow(year*12,6) + 3.699776e-14*pow(year*12,7) - 4.099077e-17*pow(year*12,8) + 2.874804e-20*pow(year*12,9) - 1.159732e-23*pow(year*12,10) + 2.052602e-27*pow(year*12,11))*Delta_BW;}
  else{wbw = (3.932403 + 0.6866462*year*12 - 0.01949911*pow(year*12,2) + 0.00031311*pow(year*12,3) - 2.466654e-06*pow(year*12,4) + 1.113217e-08*pow(year*12,5) - 3.131402e-11*pow(year*12,6) + 5.693737e-14*pow(year*12,7) - 6.706947e-17*pow(year*12,8) + 4.947858e-20*pow(year*12,9) - 2.079251e-23*pow(year*12,10) + 3.800367e-27*pow(year*12,11))*Delta_BW;}


// Volume of organs (L)

  // Hematocrit (proportion of RBC in blood)
    //Mallick et al. (2020) or Pendse et al. (2020)
  double HCT = 0;
    if(year <= 2){HCT = 0.359;}
    if(year > 2 && year < 18){HCT = 1.12815e-06 * pow(year,3) - 1.72362e-04 * pow(year,2) + 8.15264e-03 * year + 0.327363;}
    else{HCT = 1.12815e-06 * pow(18,3) - 1.72362e-04 * pow(18,2) + 8.15264e-03 * 18 + 0.327363;}
        
  // Fat (non essential adipose tissues) (in L)
    // Deepika et al. (2021)
    double vfat = 0;
        if(SEXBABY == 1){vfat = (1.3054356 + 0.3622685 * year - 0.0025165 * pow(year,2) + 0.0906119 * wbw + 0.0001731 * pow(wbw,2)) * Delta_Adipose_M;}
        else{vfat = (6.132e-01 + 8.475e-02 * year + 8.151e-05 * pow(year,2) + 1.341e-01 * wbw + 2.297e-03 * pow(wbw,2)) * Delta_Adipose_F;}

  // Brain (in L)
    // Beaudouin et al. 2010
    double vbr = 0;
        if(SEXBABY == 1){vbr = (1.45 + (0.350 - 1.45) * exp(-0.440 * year)) * Delta_Brain_M;}
        else{vbr = (1.30 + (0.347 - 1.30)*exp(-0.573 * year)) * Delta_Brain_F;}
      
  // Kidneys
    // Beaudouin et al. 2010
    double vk = 0;
        if(SEXBABY == 1){vk = (0.0042+(0.00767 - 0.0042) * exp(-0.206 * year)) * wbw * Delta_Kidney_M;}
        else{vk = (0.0046 + (0.00709 - 0.0046) * exp(-0.221 * year)) * wbw * Delta_Kidney_F;}
      
  // Hair
    double vhair = 0.002*wbw;
  
  // Liver
    // Beaudouin et al. 2010
    double vl = 0;
        if(SEXBABY == 1){vl = (0.0247 + (0.0409 - 0.0247) * exp(-0.218 * year)) * wbw * Delta_Liver_M;}
        else{vl = (0.0233 + (0.038 - 0.0233) * exp(-0.122 * year)) * wbw * Delta_Liver_F;}
    
  /// BLOOD (composition = (1-HCT)% plasma + HCT% RBC)(in L)
    // Beaudouin et al. (2010)
    double vb = 0;
        if(SEXBABY == 1){
            if(year < 1){vb = (-0.027 * pow(year,2) + 0.077 * year) * wbw;}
            else{vb = (0.0761 * (1/1 + exp(-0.683 * year + 0.946))) * wbw;}
        }
        else{
            if(year < 1){vb = (-0.0273 * pow(year,2) + 0.0771 * year) * wbw;}
            if(year >=1 && year < 14.019723){vb = (3.28e-05 * pow(year,3) - 1.21e-03 * pow(year,2) + 1.24e-02 * year + 3.86e-02) * wbw;}
            else{vb = 0.065 * wbw;}
        }

    double vp = (1-HCT)*vb;
    double vrbc = HCT*vb;
    
  // Slowly perfused tissue (Heart, Diaphragm, muscles, skin, bones)
    // Beaudouin et al. 2010
    double vheart = 0;
        if(SEXBABY == 1){vheart = 0.0045 * wbw * Delta_Heart_M;}
        else{vheart = 0.004167 * wbw * Delta_Heart_F;}

  /// Muscles
    // Beaudouin et al. 2010
    double vm = 0;
        if(SEXBABY == 1){
            if(year < 24.3){vm = (0.3973 + (0.201 - 0.3973) * exp(-0.141 * year)) * wbw * Delta_Muscle_M;}
            else{vm = (0.3973 + (0.201 - 0.3973) * exp(-0.141 * year)) * (-0.0001264 * pow(year,2) + 0.006131 * year + 0.926) * wbw * Delta_Muscle_M;}
        }
        else{
            if(year <= 25.90709){vm = (0.2917 + (0.207 - 0.2917) * exp(-0.339 * year))* wbw * Delta_Muscle_F;}
            else{vm = (0.2917 + (0.207 - 0.2917) * exp(-0.339 * year)) * (-0.0001264 * pow(year,2) + 0.006131 * year + 0.926) * wbw * Delta_Muscle_F;}
        }

  /// Diaphragm
    double vd = (3e-04)*wbw;
  
  /// Skin
    // Beaudouin et al. (2010)
    double vs = 0;
        if(SEXBABY == 1){
            if(year < 20){vs = (-1.171e-05 * pow(year,3) + 5.413e-04 * pow(year,2) -  6.1966e-03 * year + 4.623e-02) * wbw;}
            else{vs = 0.0452 * wbw;}
        }
        else{
            if(year < 20){vs = (-7.8882e-06 * pow(year,3) + 4.0224e-04 * pow(year,2) - 5.2146e-03 * year + 4.5605e-02) * wbw;}
            else{vs = 0.0383 * wbw;}
        }

  /// Skeleton (Marrow / Bones = 80 % Cortical + 20% Trabecular)
    // Pearce et al. (2017) - Httk
	// Total Body Bone Mineral Content
  double TBBMC = 0;
    if(SEXBABY == 1){
      if(year < 50){TBBMC = 0.89983 + ((2.9901 - 0.89989)/(1 + 1 * exp((14.17081 - year)/1.58179)));}
      else{TBBMC =  0.89983 + ((2.9901-0.89989)/(1+1*exp((14.17081 - year)/1.58179))) - (0.0019*year);}
    }
    else{
      if(year < 50){TBBMC = 0.89983 + ((2.9901 - 0.89989)/(1 + 1 * exp((14.17081 - year)/1.58179)));}
      else{TBBMC = 0.74042 + ((2.14976 - 0.74042)/(1 + 1 * exp((12.35466 - year)/1.35750))) - (0.0056 * year);}
    }
    
    double vbone = (TBBMC/0.65)/0.5 * Delta_Bone_M * SEXBABY + (1 - SEXBABY) * Delta_Bone_F;
    
  /// Bone marrow (71 % Red Marrow + 29% Yellow Marrow)
    // Beaudouin et al. (2010)
    double vmarr = 0;
      if(SEXBABY == 1){vmarr = (0.05 + (0.0138 - 0.05) * exp(-0.112 * year)) * wbw;}
      else{vmarr = (0.045 + (0.0138 - 0.045)*exp(-0.136 * year)) * wbw;}
    
  // Richly perfused tissue (Breast + Thyroid + Spleen + Pancreas + Adrenals + Gonads + Lungs)
  
    // Breast (Beaudouin et al. (2010))
    double vbreast = 1;
      if(SEXBABY == 1){vbreast = (3.42e-4 * (1 / (1 + exp(-1.42 * year + 20.1)))) * wbw;}
      else{vbreast = (0.00833 * 1/(1 + exp(-1.92 * year + 28.6))) * wbw;}
    
    // Thyroid
      // Beaudouin et al. (2010)
        double vthyr = 0;
            if(SEXBABY == 1){vthyr = 0.000274 * wbw;}
            else{vthyr = 0.0002833 * wbw;}
  
    // Spleen
      // Beaudouin et al. (2010)
         double vspleen = 0;
            if(SEXBABY == 1){vspleen = 0.0021 * wbw * Delta_Spleen_M;}
            else{vspleen = 0.0022 * wbw * Delta_Spleen_F;}

    // Pancreas
      // Beaudouin et al. (2010)
         double vpancreas = 0;
            if(SEXBABY == 1){vpancreas = 0.00192 * wbw * Delta_Pancreas_M;}
            else{vpancreas = 0.002 * wbw * Delta_Pancreas_F;}
  
    // Adrenals (Beaudouin et al. (2010))
         double vadrenal = 0;
            if(SEXBABY == 1){vadrenal = 2.0e-04 + (1.71e-03 - 2.0e-04) * exp(-2.02 * year);}
            else{vadrenal = 0.0002 + (0.00171 - 0.0002) * exp(-2.02 * year);}
    
    // Gonads / Reproductive organs
      // Beaudouin et al. (2010)
         double vgonads = 0;
            if(SEXBABY == 1){
            if(year < 20.1){vgonads = (-1.516e-07 * pow(year,3) + 9.3351e-06 * pow(year,2) - 1.1177e-04 * year + 4.7966e-04) * wbw * Delta_Gonad_M;}
            else{vgonads = 0.0008 * wbw * Delta_Gonad_M;}
            }
            else{
            if(year < 1){vgonads = (-1.064e-03*year + 1.338e-03)* wbw * Delta_Gonad_F;}
            if(year >= 1 && year < 20){vgonads = (2.6380e-07 * pow(year,3) - 1.7943e-06 * pow(year,2) - 5.6465e-06 * year + 2.8105e-04) * wbw * Delta_Gonad_F;}
            else{vgonads = 0.001552 * wbw * Delta_Gonad_F;}
            }
       
    // Lungs
      // Beaudouin et al. (2010)
         double vlungs = 0;
            if(SEXBABY == 1){vlungs = 0.0068 * wbw * Delta_Lung_M;}
            else{vlungs = 0.0070 * wbw * Delta_Lung_F;}

 // Gut (Stomach + Small intestine + Large intestine)
   /// Gut (Stomach + Small intestine + Large intestine)
    // Stomach
      // Beaudouin et al. (2010)
         double vstomach = 0;
            if(SEXBABY == 1){vstomach = 0.0021 * wbw * Delta_Stomach_M;}
            else{vstomach = 0.0023 * wbw * Delta_Stomach_F;}
 
    // Intestinal tract (Small + Large Intestine)
      // Beaudouin et al. (2010)
         double vintestine = 0;
            if(SEXBABY == 1){
            if(year < 16){vintestine = (-8.2562e-05*pow(year,2) + 1.3523e-03*year + 1.293e-02) * wbw * Delta_Intestine;}
            else{vintestine = 0.014 * wbw * Delta_Intestine;}
            }
            else{
            if(year < 14.453301){vintestine = (-7.421e-05*pow(year,2) + 1.276e-03*year + 1.298e-02) * wbw * Delta_Intestine;}
            else{vintestine = 0.0160 * wbw * Delta_Intestine;}
            }
// Calculation of the new fration of organ to keep balanced body weight
    double wbw_f = vfat + vbr + vk + vhair + vb + vl + vheart + vd + vm + vs + vbone + vmarr + vstomach + vintestine + vbreast + vthyr + vspleen + vpancreas + vadrenal + vgonads + vlungs;
    
    double vfat_f       = vfat*wbw_f/wbw;
    double vbr_f        = vbr*wbw_f/wbw;
    double vk_f         = vk*wbw_f/wbw;
    double vhair_f      = vhair*wbw_f/wbw;
    double vb_f         = vb*wbw_f/wbw;
    double vp_f         = vp*wbw_f/wbw;
    double vrbc_f       = vrbc*wbw_f/wbw;
    double vl_f         = vl*wbw_f/wbw;
    double vheart_f     = vheart*wbw_f/wbw;
    double vd_f         = vd*wbw_f/wbw;
    double vm_f         = vm*wbw_f/wbw;
    double vs_f         = vs*wbw_f/wbw;
    double vbone_f      = vbone*wbw_f/wbw;
    double vmarr_f      = vmarr*wbw_f/wbw;
    double vstomach_f   = vstomach*wbw_f/wbw;
    double vintestine_f = vintestine*wbw_f/wbw;
    double vbreast_f    = vbreast*wbw_f/wbw;
    double vthyr_f      = vthyr*wbw_f/wbw;
    double vspleen_f    = vspleen*wbw_f/wbw;
    double vpancreas_f  = vpancreas*wbw_f/wbw;
    double vadrenal_f   = vadrenal*wbw_f/wbw;
    double vgonads_f    = vgonads*wbw_f/wbw;
    double vlungs_f     = vlungs*wbw_f/wbw;
    
    double vgut = vstomach + vintestine;
    
 // Arterial  Venous blood 
    double vart = 0.5 * vb;
    double vve = 0.5 * vb;

    // Muscle/Others
    double vothers = wbw_f - (vgut + vs + vbr + vheart + vk + vl + vlungs + vart + vve);
    
/// Cardiac output
  	 // Beaudouin et al. (2010)
        double Q = 0;
        if(SEXBABY == 1){
            if(year < 33.37){Q = (6.642+(0.6-6.642)*exp(-0.1323*year))*60*24;}
            else{Q = (-0.000895*pow(year,2) + 0.0607*year + 5.54)*60*24;}}
        else{
            if(year < 16.027){Q = (7.734+(0.6-7.734)*exp(-0.09747*year))*60*24;}
            else{Q = (0.000473*pow(year,2) - 0.0782*year + 7.37)*60*24;}}

 //  By the current data, a blood flow rate is used to estimate the evolution over life
	 /// The blood flow rate come from ICRP (2002)
		 // Fat
            double qfat = 0;
                if (SEXBABY == 1){qfat = 0.05*Q;}
                else{qfat = 0.085*Q;}
         
         // Brain
		    double qbr = 0.12*Q;

		 // Kidney
            double qk = 0;
                if(SEXBABY == 1){qk = 0.19*Q;}
                else{qk = 0.17*Q;}

		 // Liver (only from arterial blood)
		    double ql = 0.065*Q;
	
		 // Heart
            double qheart = 0;
                if(SEXBABY == 1){qheart = 0.04 * Q;}
                else{qheart = 0.05 * Q;}
  
  		 // Diaphragm
		    double qd = 0.0003*Q;

		 // Muscles
            double qm = 0;
                if(SEXBABY == 1){qm = 0.17 * Q;}
                else{qm = 0.12 * Q;}
  
		 // Skin
		    double qs = 0.05*Q;

		 // Bone
		    double qbone = 0.02*Q;

		 // Marrow
		    double qmarr = 0.03*Q;

		 // Stomach
		    double qstomach = 0.01*Q;

		 // Intestine
		    double qintestine = 0;
                if(SEXBABY == 1){qintestine = 0.14 * Q;}
                else{qintestine = 0.16 * Q;}

		 // Breast
		    double qbreast = 0;
                if(SEXBABY == 1){qbreast = 0.0002 * Q;}
                else{qbreast = 0.004 * Q;}

		 // Thyroid
		    double qthyr = 0.015 * Q;

		 // Spleen
		    double qspleen = 0.03 * Q;

		 // Pancreas
		    double qpancreas = 0.01 * Q;

		 // Adrenals
		    double qadrenal = 0.003 * Q;

		 // Gonads
		    double qgonads = 0;
                if(SEXBABY == 1){qgonads = 0.0005 * Q;}
                else{qgonads = 0.0002 * Q;}

		 // Lungs (as bronchial tissues)
		    double qlungs = 0.025*Q;

	/// Sum of bodyweight
	double Q_f = qfat + qbr + qk + ql + qheart + qd + qm + qs + qbone + qmarr + qstomach + qintestine + qbreast + qthyr + qspleen + qpancreas + qadrenal + qgonads + qlungs;

	 // Lumping of organs
	    double qgut  = qstomach + qintestine;  
	    double qothers = Q_f - (qgut + qs + qbr + qheart + qk + ql);

	 /// Kinetic parameters
	    double MMAsi     = 141.944;	                  // Molar Mass iAs (g/mol)
	    double MMmma     = 139.97;	                  // Molar Mass MMA (g/mol)
	    double MMdma     = 138.00;	                  // Molar Mass DMA (g/mol)
		
	 // Inorganic Arsenic
	    double Ka3       = 0.004*(60*24);	          // Oral absorption As3 (day-1)
	    double Ka5       = 0.003*(60*24);	          // Oral absorption As5 (day-1)
    	    double Frac      = 0.8;			  // Fraction absorbed
    	    double kred5     = 0.003*(60*24);	          // Reduction As5 -> As3
	    double kox3      = 0.25;	                  // Oxydation As3 -> As5
	    double V3mma     = (5.3e-07)*(60*24);	  // Vmax As3 -> MMA (en mol/day)
	    double V3dma     = (2e-06)*(60*24);		  // Vmax As3 -> DMA (en mol/day)
	    double K3mma     = 3e-06;	                  // Michaelis-Menten constant As3 -> MMA
	    double K3dma     = 3e-06;	                  // Michaelis-Menten constant As3 -> DMA
	    double Kinh      = 4e-05;	                  // Constant non-competitive inhibition
	    double KurineAs  = 0.07*(60*24);	          // Urinal elimination of iAs
		
	 // MMA
	    double koxm3     = 0.63;	                  // Oxydation MMA3 -> MMA5
	    double kredm5    = 0.008*(60*24);		  // Reduction MMA5 -> MMA3
	    double Vm3dma    = (6.6e-07)*(60*24);	  // Vmax MMA3 -> DMA3 (en mol/day)
	    double Km3dma    = 3e-06;	                  // Michaelis-Menten constant MMA3 -> DMA3
	    double Kurinemma = 0.3*(60*24);		  // Elimination urinaire MMA

	 // DMA
	    double koxd3     = 0.65;	                  // Oxydation DMA3 -> DMA5
	    double kredd5    = 0.004*(60*24);		  // Reduction DMA5 -> DMA3
	    double Kurinedma = 0.13*(60*24);	          // Elimination urinaire DMA


//// Modification of the exposure over life

// Creatinine in function of the age (C.Béchaux)
double ucr= (0.032+2.098e-02*year+9.104e-03*pow(year,2)-4.550e-04*pow(year,3)+7.578e-06*pow(year,4)-4.232e-08*pow(year,5));
if (year>70) {ucr=(0.032+2.098e-02*70+9.104e-03*pow(70,2)-4.550e-04*pow(70,3)+7.578e-06*pow(70,4)-4.232e-08*pow(70,5));}


// Urinal concentration (in ug/L, with the hypothesis of 2L/day of urine)
double UAs = (KurineAs*KIDNEY5*MMAsi/pkas5 + KurineAs*KIDNEY3*MMAsi/pkas3 + Kurinemma*(LIVERM3*MMmma/plimma) + Kurinemma*LUNGM3*MMmma/plumma + Kurinemma*(KIDNEYM3*MMmma/pkmma) + Kurinemma*(KIDNEYM5*MMmma/pkmma) + Kurinedma*(LUNGD3*MMdma/pludma) + Kurinedma*(LIVERD3*MMdma/plidma) +  Kurinedma*(KIDNEYD3*MMdma/pkdma) + Kurinedma*(KIDNEYD5*MMdma/pkdma))*1e6/Vurine;
// Urinal concentration (in ug/g creat)
double UAscr = (KurineAs*KIDNEY5*MMAsi/pkas5 + KurineAs*KIDNEY3*MMAsi/pkas3 + Kurinemma*(LIVERM3*MMmma/plimma) + Kurinemma*LUNGM3*MMmma/plumma + Kurinemma*(KIDNEYM3*MMmma/pkmma) + Kurinemma*(KIDNEYM5*MMmma/pkmma) + Kurinedma*(LUNGD3*MMdma/pludma) + Kurinedma*(LIVERD3*MMdma/plidma) +  Kurinedma*(KIDNEYD3*MMdma/pkdma) + Kurinedma*(KIDNEYD5*MMdma/pkdma))*1e6/ucr;

// Concentration of Asi in kidney (in ug/L)
double KAs = (KIDNEY3*MMAsi + KIDNEY5*MMAsi + KIDNEYM3*MMmma + KIDNEYM5*MMmma + KIDNEYD3*MMdma + KIDNEYD5*MMdma)/vk;
double Vurine = 0.0294 * wbw;

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

	double VInhalation = (FqBreath * (Vtidal - vds) * 60 * 24)/1000; // Volume of inhalation (m3) by day

  // Dose of Cd by smoking exposures
  double Cig = 0;
    if(year > age_deb && year < age_fin){Cig = nb_cig * smoke_As;}
    else {Cig = 0;}

$ODE
//Inorganic arsenic
  // Lumen
//Inorganic arsenic
  // Exposures
    dxdt_DIET3 = -DIET3;

    dxdt_DIET5 = -DIET5;

    dxdt_SOIL = - SOIL;

    dxdt_DUST = - DUST;

    dxdt_AIR  = - AIR;

  // Lumen
    dxdt_LUMEN3 = (DIET3*wbw_f + SOIL * 0.5  + DUST * 0.5)*1e-06/MMAsi - LUMEN3*Ka3*Frac;
    dxdt_LUMEN5 = (DIET5*wbw_f + SOIL * 0.5  + DUST * 0.5)*1e-06/MMAsi - LUMEN5*Ka5*Frac;
    
  // Gut
    dxdt_GUT3 = LUMEN3*Ka3*Frac - qgut*GUT3/vgut*pgas3 + qgut*ARTERIAL3/vart;
    dxdt_GUT5 = LUMEN5*Ka5*Frac - qgut*GUT5/vgut*pgas5 + qgut*ARTERIAL5/vart;

  // Liver
    dxdt_LIVER3 = ql*ARTERIAL3/vart + qgut*GUT3/vgut*pgas3  - (ql+qgut)*LIVER3/vl*plias3 + kred5*LIVER5 - kox3*LIVER3 - ((V3mma*LIVER3/vl)/((K3mma+LIVER3/vl)*(1+(LIVERM3/vl/Kinh)))) - ((V3dma*LIVER3/vl)/(K3dma+LIVER3/vl));
    dxdt_LIVER5 = ql*ARTERIAL5/vart + qgut*GUT5/vgut*pgas5  - (ql+qgut)*LIVER5/vl*plias5 - kred5*LIVER5 + kox3*LIVER3;

  // Kidney
    dxdt_KIDNEY3 = qk*ARTERIAL3/vart - qk*KIDNEY3/vk*pkas3 + kred5*KIDNEY5 - kox3*KIDNEY3 - KurineAs*KIDNEY3/pkas3 - ((0.4*V3mma*KIDNEY3/vk)/((K3mma+KIDNEY3/vk)*(1+(KIDNEYM3/vk/Kinh)))) - ((0.4*V3dma*KIDNEY3/vk)/(K3dma+KIDNEY3/vk));
    dxdt_KIDNEY5 = qk*ARTERIAL5/vart - qk*KIDNEY5/vk*pkas5 - kred5*KIDNEY5 + kox3*KIDNEY3 - KurineAs*KIDNEY5/pkas5;

  // Muscles
    dxdt_MUSCLE3 = qothers*ARTERIAL3/vart - qothers*MUSCLE3/vothers*pmas3;
    dxdt_MUSCLE5 = qothers*ARTERIAL5/vart - qothers*MUSCLE5/vothers*pmas5;

  // Heart
    dxdt_HEART3 = qheart*ARTERIAL3/vart - qheart*HEART3/vheart*phas3;
    dxdt_HEART5 = qheart*ARTERIAL5/vart - qheart*HEART5/vheart*phas5;
    
  // Skin
    dxdt_SKIN3 = qs*ARTERIAL3/vart - qs*SKIN3/vs*psas3;
    dxdt_SKIN5 = qs*ARTERIAL5/vart - qs*SKIN5/vs*psas5;
    
  // Brain
    dxdt_BRAIN3 = qbr*ARTERIAL3/vart - qbr*BRAIN3/vbr*pbas3;
    dxdt_BRAIN5 = qbr*ARTERIAL5/vart - qbr*BRAIN5/vbr*pbas5;

  // Lung
    dxdt_LUNG3 = Q_f*VENOUS3/vve - Q_f*LUNG3/vlungs*pluas3 - kox3*LUNG3 + kred5*LUNG5 + (AIR * 0.5 * VInhalation + Cig)*0.3*1e-06/MMAsi;
    dxdt_LUNG5 = Q_f*VENOUS5/vve - Q_f*LUNG5/vlungs*pluas5 + kox3*LUNG3 - kred5*LUNG5 + (AIR * 0.5 * VInhalation + Cig)*0.3*1e-06/MMAsi;

  // Blood
    dxdt_ARTERIAL3 = Q_f*LUNG3/vlungs*pluas3 - Q_f*ARTERIAL3/vart;
    dxdt_ARTERIAL5 = Q_f*LUNG5/vlungs*pluas5 - Q_f*ARTERIAL5/vart;
    dxdt_VENOUS3   = (ql+qgut)*LIVER3/vl*plias3 + qk*KIDNEY3/vk*pkas3 + qothers*MUSCLE3/vothers*pmas3 + qbr*BRAIN3/vbr*pbas3 + qs*SKIN3/vs*psas3 + qheart*HEART3/vheart*phas3  - Q_f*VENOUS3/vve;
    dxdt_VENOUS5   = (ql+qgut)*LIVER5/vl*plias5 + qk*KIDNEY5/vk*pkas5 + qothers*MUSCLE5/vothers*pmas5 + qbr*BRAIN5/vbr*pbas5 + qs*SKIN5/vs*psas5 + qheart*HEART5/vheart*phas5  - Q_f*VENOUS5/vve;

// MMA
  // Gut
    dxdt_GUTM5 = qgut*ARTERIALM5/vart - qgut*GUTM5/vgut*pgmma;
    
  // Liver
    dxdt_LIVERM3 = kredm5*LIVERM5 - koxm3*LIVERM3 - ((Vm3dma*LIVERM3/vl)/((Km3dma+LIVERM3/vl)*(1+(LIVER3/vl/Kinh)))) - Kurinemma*(LIVERM3/plimma);
    dxdt_LIVERM5 = ql*ARTERIALM5/vart - (ql+qgut)*(LIVERM5/vl*plimma) + qgut*GUTM5/vgut*pgmma - kredm5*LIVERM5 + koxm3*LIVERM3 + ((V3mma*LIVER3/vl)/((K3mma+LIVER3/vl)*(1+((LIVERM3/vl)/Kinh))));
  
  // Kidney
    dxdt_KIDNEYM3 = kredm5*KIDNEYM5 - koxm3*KIDNEYM3 - ((0.4*Vm3dma*KIDNEYM3/vk)/((Km3dma+KIDNEYM3/vk)*(1+(KIDNEY3/vk/Kinh)))) - Kurinemma*(KIDNEYM3/pkmma);
    dxdt_KIDNEYM5 = qk*ARTERIALM5/vart - qk*KIDNEYM5/vk*pkmma  - kredm5*KIDNEYM5  + koxm3*KIDNEYM3 - Kurinemma*(KIDNEYM5/pkmma) + ((0.4*V3mma*KIDNEY3/vk)/((K3mma+(KIDNEY3/vk))*(1+((KIDNEYM3/vk)/Kinh))));

  // Lung
    dxdt_LUNGM3 = kredm5*LUNGM5 - koxm3*LUNGM3 - Kurinemma*LUNGM3/plumma;
    dxdt_LUNGM5 = Q_f*VENOUSM5/vve - Q_f*LUNGM5/vlungs*plumma - kredm5*LUNGM5 + koxm3*LUNGM3;
  
  // Muscle
    dxdt_MUSCLEM5 = qothers*ARTERIALM5/vart - qothers*MUSCLEM5/vothers*pmmma;
  // Heart
    dxdt_HEARTM5 = qheart*ARTERIALM5/vart - qheart*HEARTM5/vheart*phmma;
  // Skin
    dxdt_SKINM5 = qs*ARTERIALM5/vart - qs*SKINM5/vs*psmma;
  // Brain
    dxdt_BRAINM5 = qbr*ARTERIALM5/vart - qbr*BRAINM5/vbr*pbmma;
  
  // BLOOD
    dxdt_ARTERIALM3 = 0;
    dxdt_ARTERIALM5 = Q_f*LUNGM5/vlungs*plumma - Q_f*ARTERIALM5/vart;
    dxdt_VENOUSM3 = 0;
    dxdt_VENOUSM5 = (ql+qgut)*LIVERM5/vl*plimma + qk*KIDNEYM5/vk*pkmma + qbr*BRAINM5/vbr*pbmma + qs*SKINM5/vs*psmma + qheart*HEARTM5/vheart*phmma + qothers*MUSCLEM5/vothers*pmmma - Q_f*VENOUSM5/vve;
  
// DMA
  // Lung
    dxdt_LUNGD3 = kredd5*LUNGD5 - koxd3*LUNGD3 - Kurinedma*(LUNGD3/pludma);
    dxdt_LUNGD5 = Q_f*VENOUSD5/vve - kredd5*LUNGD5 - Q_f*LUNGD5/vlungs*pludma + koxd3*LUNGD3;
    
  // Gut
    dxdt_GUTD5 = qgut*ARTERIALD5/vart - qgut*GUTD5/vgut*pgdma;

  // Liver
    dxdt_LIVERD3 = kredd5*LIVERD5 - koxd3*LIVERD3 - Kurinedma*(LIVERD3/plidma);
    dxdt_LIVERD5 = ql*ARTERIALD5/vart + qgut*GUTD5/vgut*pgdma - (ql+qgut)*LIVERD5/vl*plidma - kredd5*LIVERD5 + koxd3*LIVERD3 + (V3dma*LIVER3/vl)/(K3dma+(LIVER3/vl)) + ((Vm3dma*LIVERM3/vl)/((Km3dma+LIVERM3/vl)*(1+(LIVER3/vl/Kinh))));

  // Kidney
    dxdt_KIDNEYD3 = kredd5*KIDNEYD5 - koxd3*KIDNEYD3 - Kurinedma*(KIDNEYD3/pkdma);
    dxdt_KIDNEYD5 = qk*ARTERIALD5/vart - qk*KIDNEYD5/vk*pkdma - kredd5*KIDNEYD5  + koxd3*KIDNEYD3 - Kurinedma*(KIDNEYD5/pkdma) + (0.4*V3dma*KIDNEY3/vk)/(K3dma+(KIDNEY3/vk)) + ((0.4*Vm3dma*KIDNEYM3/vk)/((Km3dma+KIDNEYM3/vk)*(1+(KIDNEY3/vk/Kinh))));  

  // MUSCLE
    dxdt_MUSCLED5 = qothers*ARTERIALD5/vart - qothers*MUSCLED5/vothers*pmdma;

  // Heart
    dxdt_HEARTD5 = qheart*ARTERIALD5/vart - qheart*HEARTD5/vheart*phdma;

  // Skin
    dxdt_SKIND5 = qs*ARTERIALD5/vart - qs*SKIND5/vs*psdma;

  // Brain
    dxdt_BRAIND5 = qbr*ARTERIALD5/vart - qbr*BRAIND5/vbr*pbdma;

  // Blood
    dxdt_ARTERIALD5 = Q_f*LUNGD5/vlungs*pludma - Q_f*ARTERIALD5/vart;
    dxdt_ARTERIALD3 = 0;
    dxdt_VENOUSD3 = 0;
    dxdt_VENOUSD5 = (ql+qgut)*LIVERD5/vl*plidma + qk*KIDNEYD5/vk*pkdma + qbr*BRAIND5/vbr*pbdma + qs*SKIND5/vs*psdma + qheart*HEARTD5/vheart*phdma + qothers*MUSCLED5/vothers*pmdma - Q_f*VENOUSD5/vve;


$CMT DIET3 DIET5 SOIL DUST AIR LUMEN3 LUMEN5 GUT3 GUT5 LIVER3 LIVER5 KIDNEY3 KIDNEY5 MUSCLE3 MUSCLE5 HEART3 HEART5 SKIN3 SKIN5 BRAIN3 BRAIN5 LUNG3 LUNG5 LIVERM3 KIDNEYM3 LIVERD3 KIDNEYD3 VENOUS3 VENOUS5 ARTERIAL3 ARTERIAL5
     GUTM5 LIVERM5 ARTERIALM5 VENOUSM5 ARTERIALM3 VENOUSM3 LUNGM3 LUNGM5 LIVERD5 KIDNEYD5 KIDNEYM5 GUTD5
     MUSCLEM5 HEARTM5 SKINM5 BRAINM5 LUNGD3 LUNGD5 ARTERIALD5 ARTERIALD3 VENOUSD3 VENOUSD5 MUSCLED5 HEARTD5 SKIND5 BRAIND5

$CAPTURE UAs UAscr KAs SEXBABY





