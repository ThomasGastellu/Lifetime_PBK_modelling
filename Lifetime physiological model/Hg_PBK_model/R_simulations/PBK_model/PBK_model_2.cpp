// Model PBK for mercury (inorganic mercury and methylmercury).

$PARAM

// Individual parameters
	SEXBABY = 1   // Sex of indivial (1 == Male, 0 == Female)

// Body weight
	Delta_BW = 1 // Factor of variation for height
	Delta_HT = 1 // Factor of variation for bodyweight

## Variability for male
	Delta_Brain_M 	 = 1
	Delta_Kidney_M 	 = 1
	Delta_Liver_M 	 = 1
	Delta_Pancreas_M = 1
	Delta_Stomach_M  = 1
	Delta_Small_Intestine_M = 1
	Delta_Large_Intestine_M = 1
	Delta_Heart_M 	 = 1
	Delta_Bone_M 	 = 1
	Delta_Gonad_M 	 = 1
	Delta_Lung_M 	 = 1
	Delta_Spleen_M 	 = 1
	Delta_Muscle_M 	 = 1
	Delta_Adipose_M  = 1

## Variability for female
	Delta_Brain_F 	 = 1
	Delta_Kidney_F 	 = 1
	Delta_Liver_F 	 = 1
	Delta_Pancreas_F = 1
	Delta_Stomach_F  = 1
	Delta_Small_Intestine_F = 1
	Delta_Large_Intestine_F = 1
	Delta_Heart_F 	 = 1
	Delta_Bone_F 	 = 1
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
	pr   = 1.0   // Rapidly_plasma
	ps   = 2.0   // Slowly_plasma


      IND_SSKIN = 1
      IND_FAT = 1
      IND_BRAIN = 1
      IND_KIDNEY = 1
      IND_LIVER = 1
      IND_BLOOD = 1
      IND_HEART = 1
      IND_MUSCLE = 1
      IND_VSKIN = 1
      IND_MARR = 1
      IND_SPLEEN = 1
      IND_PANCREAS = 1
      IND_GONAD = 1
      IND_LUNG = 1
      IND_STOMACH = 1
      IND_INTESTINE = 1
      IND_CO = 1
      IND_HCT = 1
      IND_BONE = 1
      IND_BREAST = 1
      IND_TONGUE = 1
      IND_THYR = 1
  

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
double month = TIME/(365/12);

///// Physiological changes ////
  /// Body weight ///
  double wbw = 1;
  double ht = 1;
  if(SEXBABY==1){
    wbw = (3.938425 + 0.7518199*year*12 - 0.02023793*pow(year*12,2) + 0.0002921682*pow(year*12,3) - 2.06762e-06*pow(year*12,4) + 8.469e-09*pow(year*12,5) - 2.188427e-11*pow(year*12,6) + 3.699776e-14*pow(year*12,7) - 4.099077e-17*pow(year*12,8) + 2.874804e-20*pow(year*12,9) - 1.159732e-23*pow(year*12,10) + 2.052602e-27*pow(year*12,11))*Delta_BW;
    ht = (53.14237 + 2.299147*year*12 - 0.04567729*pow(year*12,2) + 0.0005784706*pow(year*12,3) - 4.082151e-06*pow(year*12,4) + 1.729718e-08*pow(year*12,5) - 4.667123e-11*pow(year*12,6) + 8.238663e-14*pow(year*12,7) - 9.495884e-17*pow(year*12,8) + 6.892621e-20*pow(year*12,9) - 2.861566e-23*pow(year*12,10) + 5.182964e-27*pow(year*12,11))*Delta_HT;}
  
  else{wbw = (3.932403 + 0.6866462*year*12 - 0.01949911*pow(year*12,2) + 0.00031311*pow(year*12,3) - 2.466654e-06*pow(year*12,4) + 1.113217e-08*pow(year*12,5) - 3.131402e-11*pow(year*12,6) + 5.693737e-14*pow(year*12,7) - 6.706947e-17*pow(year*12,8) + 4.947858e-20*pow(year*12,9) - 2.079251e-23*pow(year*12,10) + 3.800367e-27*pow(year*12,11))*Delta_BW;
       ht = (54.7663 + 1.901851*year*12 - 0.03451895*pow(year*12,2) + 0.0004589584*pow(year*12,3) - 3.466838e-06*pow(year*12,4) + 1.557048e-08*pow(year*12,5) - 4.400249e-11*pow(year*12,6) + 8.052606e-14*pow(year*12,7) - 9.542125e-17*pow(year*12,8) + 7.073083e-20*pow(year*12,9) - 2.982651e-23*pow(year*12,10) + 5.463555e-27*pow(year*12,11))*Delta_HT;}

  /// Body Mass Index
  double BMI = wbw/pow(ht/100,2);
  
  /// Surface area (in m^2)
  double Sskin = 1;
     // Haddad et al. (2006)
    if(IND_SSKIN == 1){Sskin = (pow(wbw,0.5150)*pow(ht,0.4220)*234.9)/10000;}
      
    // Verner et al. (2008)
    if(IND_SSKIN == 2){
      if(SEXBABY == 1){
        Sskin = (pow(wbw,0.5150)*pow(ht,0.4220)*234.9)/10000;
        
      }
      else{Sskin = (pow(wbw,0.5150)*pow(ht,0.4220)*234.9)/10000;}
    }

    // Wu et al. (2015)
    if(IND_SSKIN == 3){
      if(SEXBABY == 1){
        Sskin = (pow(wbw,0.5150)*pow(ht,0.4220)*234.9)/10000;
        
      }
      else{Sskin = (pow(wbw,0.5150)*pow(ht,0.4220)*235)/10000;}
    }

    // Ring et al. (2017)
    if(IND_SSKIN == 4){
      if(year < 18){
        Sskin = 0.024265 * pow(wbw,0.5378) * pow(ht,0.3964);}
      else{Sskin = sqrt(wbw * ht / 3600);}
    }        
    
    // Mallick et al. (2019)
    if(IND_SSKIN == 5){Sskin = exp(-3.75 + 0.42*log(ht) + 0.52*log(wbw));}

    // Pendse et al. (2020)
    if(IND_SSKIN == 6){Sskin = exp(-3.75+0.42*log(ht)+0.52*log(wbw));}

        // Deepika et al. (2021)
    if(IND_SSKIN == 7){Sskin = 0.007184*pow(wbw,0.425)*pow(ht,0.725);}


/// Hematocrit
  double hct = 1;

    // Wu et al. (2015)
      if(IND_HCT == 1){
        if(SEXBABY == 1){
          if(year < 1){hct = 0.355109;}
          else{hct = 0.3475 + ((0.07 * year)/(8.2 * year));}
          
        }
        else{
          if(year < 1){hct = 0.355109;}
          else{hct = 0.3475 + ((0.07 * year)/(8.2 * year));}
        }
      }

    // Mallick et al. (2019)
      if(IND_HCT == 2){
        if(year < 2){hct = 0.359;}
        else{hct = (1.12815e-06 * pow(year,3)) - (1.72362e-04 * pow(year,2)) + (8.15264e-03 * year) + 0.327363;}
      }

    // Sarigiannis et al. (2020)
      if(IND_HCT == 3){
        if(year < 18){hct = -1e-01 * pow((year*365.25*24),9.66e-12) - 1e-01 * pow((year*365.25*24),1.55e-06) + 5.50e-07 * (year*365.25*24) + 5.80e-01;}
        else{hct = 0.47;}
      }

    // Mallick et al. (2019)
      if(IND_HCT == 4){
        if(year < 2){hct = 0.359;}
        else{hct = (1.12815e-06 * pow(year,3)) - (1.72362e-04 * pow(year,2)) + (8.15264e-03 * year) + 0.327363;}
      }

      /// Poids des organes
  
  /// Fat
    double vfat = 0.273;
      // Haddad et al. (2001)
        if(IND_FAT == 1){
          if(SEXBABY == 1){
            if(year < 18){vfat = ((0.0165 * pow(year,5) - 1.9784 * pow(year,4) + 51.963 * pow(year,3) - 459.39 * pow(year,2) + 1566.8 * year + 1004.2)/1000)*Delta_Adipose;}
            else{vfat = 7.54*Delta_Adipose;}}
          else{
            if(year < 18){vfat = ((0.038 * pow(year,5) - 2.6629 * pow(year,4) + 60.433 * pow(year,3) - 479.37 * pow(year,2) + 1592.3 * year + 912.36)/1000)*Delta_Adipose;}
            else{vfat = 20.61*Delta_Adipose;}}}

      // Price K. et al. (2003)
        if(IND_FAT == 2){
          if(SEXBABY == 1){
            if(year < 18){vfat = ((0.0165 * pow(year,5) - 1.9784 * pow(year,4) + 51.963 * pow(year,3) - 459.39 * pow(year,2) + 1566.8 * year + 1004.2)/1000)*Delta_Adipose;}
            else{vfat = 7.54*Delta_Adipose;}}
          else{
            if(year < 18){vfat = ((0.0165 * pow(year,5) - 1.9784 * pow(year,4) + 51.963 * pow(year,3) - 459.39 * pow(year,2) + 1566.8 * year + 1004.2)/1000)*Delta_Adipose;}
            else{vfat = 7.54*Delta_Adipose;}
            
            }}

      // Smith et al. (2014)
        if(IND_FAT == 3){
          if(SEXBABY == 1){vfat = ((3.484e-02 + 2.803e-05*wbw*1000 - 1.422e-09*pow(wbw*1000,2) + 2.892e-14*pow(wbw,3) - 2.718e-19*pow(wbw*1000,4) + 1.203e-24*pow(wbw*1000,5) - 2.036e-30*pow(wbw*1000,6)) * wbw)*Delta_Adipose;}
          else{vfat = ((9.217e-02 + 1.401e-05*wbw*1000 - 6.787e-10*pow(wbw*1000,2) + 1.540e-14*pow(wbw*1000,3) - 1.558e-19*pow(wbw*1000,4) + 7.249e-25*pow(wbw*1000,5) - 1.274e-30*pow(wbw*1000,6)) * wbw)*Delta_Adipose;}}
    
      // Wu et al. (2015)
        if(IND_FAT == 4){
          if(SEXBABY == 1){
            if(year < 25){vfat = (((1.5334*exp(-0.103*year) + 0.67) * BMI + 0.6276*year + 1.0301)*wbw/100)*Delta_Adipose;}
            else{vfat = (1.9224*BMI - 0.018517*pow(BMI,2) +  0.05537*year - 0.794895)*Delta_Adipose;}
            
          }
          else{
            if(year < 25){vfat = (((1.5334*exp(-0.103*year) + 0.67) * BMI + 0.6276*year + 1.0301)*wbw/100)*Delta_Adipose;}
            else{vfat = (1.9224*BMI - 0.018517*pow(BMI,2) +  0.05537*year - 0.794895) * Delta_Adipose;
            }}}

    // Mallick et al. (2020)
      if(IND_FAT == 5){
        if(SEXBABY == 1){
          if(year < 25){vfat = (((1.4471*exp(-0.0761*year) + 0.52) * BMI - 0.10124*year + 5.0465)*wbw/100)*Delta_Adipose;}
          else{vfat =  (((-6.0487*BMI + 0.1177*pow(BMI,2) + 0.03155*year + 97.2025) * 0.979)*wbw/100)*Delta_Adipose;}
       }
        else{
          if(year < 25){vfat = (((1.5334*exp(-0.103*year) + 0.67) * BMI + 0.6276*year + 1.0301)*wbw/100)*Delta_Adipose;}
          else{vfat = (1.9224*BMI - 0.018517*pow(BMI,2) +  0.05537*year - 0.794895)*Delta_Adipose;}}}

    // Sarigiannis et al. (2020)
      if(IND_FAT == 6){
        if(year < 18){vfat = ((2.54e-2 * (year*365.25*24) + 1.88e1 * pow((year*365.25*24),5.20e-1) + 9.06e02)/1000)*Delta_Adipose;}
        else{vfat = 14.40*Delta_Adipose;}}

    // Pendse et al. (2020)
      if(IND_FAT == 7){
        if(SEXBABY == 1){
          if(year < 20){vfat = (((2.8975 * exp(-0.129 * year) + 0.67) * BMI + 0.2635 * year - 4.843)*wbw / 100)*Delta_Adipose;}
          else{vfat = ((-5.33798 * BMI + 0.11149 * pow(BMI,2) + 0.09795 * year + 85.24521) * wbw / 100)*Delta_Adipose;}
        }
        else{
          if(year < 25){vfat = (((1.5334 * exp(-0.103 * year) + 0.67) * BMI + 0.6276 * year + 1.0301)*wbw / 100)*Delta_Adipose;}
          else{vfat = ((1.9224 * BMI - 0.018517 * pow(BMI,2) + 0.05537 * year + 0.794894) * wbw / 100)*Delta_Adipose;}}}

    // Deepika et al. (2021)
      if(IND_FAT == 8){
        if(SEXBABY == 1){vfat = (1.3054356 + 0.3622685 * year - 0.0025165 * pow(year,2) + 0.0906119*wbw + 0.0001731*pow(wbw,2))*Delta_Adipose;}
        else{vfat = (6.132e-1 + 8.475e-02 * year + 8.151e-5 * pow(year,2) + 1.341e-1 * wbw + 2.297e-3 * pow(wbw,2))*Delta_Adipose;}}

    double LBM = wbw - 1.01*vfat; //FAT + Essential Fat
  

  // Brain
      double vbr = 0.02;
      // Haddad et al. (2001)
        if (IND_BRAIN == 1){
          if(SEXBABY == 1){
            if(year < 18) {vbr =((1e04*((year+0.213)/(6.030+6.895*year))/1.035)/1000)*Delta_Brain;}
            else {vbr = 1.40*Delta_Brain;}}
          else{
            if(year < 18){vbr = ((1e04*((year+0.226)/(6.521+7.514*year))/1.05)/1000)*Delta_Brain;}
            else{vbr = 1.30*Delta_Brain;}}}
      
      // Price K. et al. (2003)
        if (IND_BRAIN == 2){
          if(SEXBABY == 1){
            if(year < 18) {vbr =((1e04*((year+0.213)/(6.030+6.895*year))/1.035)/1000)*Delta_Brain;}
            else {vbr = 1.40*Delta_Brain;}}
          else{
            if(year < 18){vbr = ((1e04*((year+0.213)/(6.030+6.895*year))/1.035)/1000)*Delta_Brain;}
            else{vbr = 1.40*Delta_Brain;}
           
            }}
      
      // Beaudouin et al. 2010
        if(IND_BRAIN == 3){
          if(SEXBABY == 1){vbr = (1.45 + (0.350-1.45)*exp(-0.440*year))*Delta_Brain;}
          else {vbr = (1.30 + (0.347-1.30)*exp(-0.573*year))*Delta_Brain;}}
      
      // Smith et al. 2014
        if(IND_BRAIN == 4){vbr =(1.216e-01 - 3.465e-06*(wbw*1000) + 4.354e-11*pow((wbw*1000),2) - 2.463e-16*pow((wbw*1000),3) + 5.132e-22*pow((wbw*1000),4)) * wbw * Delta_Brain;}
      
      // Ring et al. (2017)
        if(IND_BRAIN == 5){
          if(SEXBABY == 1){vbr = ((0.425 * ((3.68 - 2.68 * exp(-year/0.89)) * exp(-year/629)))/1.03)*Delta_Brain;}
          else{vbr = ((0.373 * ((3.68 - 2.68 * exp(-year/0.89)) * exp(-year/629)))/1.03)*Delta_Brain;}
        }
      
      // Mallick et al. (2019)
        if(IND_BRAIN == 6){vbr = (10*(year+0.315)/(9+6.92*year))*Delta_Brain;}
      
      // Sarigiannis et al. 2020
        if(IND_BRAIN == 7){
          if(year < 18){vbr = ((-5.03e-02*(year*365*24) + 9.07e-01*pow((year*365*24),(7.69e-01)) + 3.95e+02)/1000)*Delta_Brain;}
          else{vbr = 1.47*Delta_Brain;}}

      // Pendse et al. (2020)
        if(IND_BRAIN == 8){vbr = (10*(year+0.315)/(9+6.92*year))*Delta_Brain;}

      // Deepika et al. (2021)
        if(IND_BRAIN == 9){
          if(SEXBABY == 1){vbr = (0.218096 - 0.001590 * year - 0.003274 * wbw + 0.008626*ht)*Delta_Brain;}
          else{vbr = (0.3757397 - 0.0003031 * year - 0.0021962*wbw + 0.0065721*ht)*Delta_Brain;}
        }

        // Kidneys
  double vk = 0.004;
      // Haddad et al. (2001)
        if(IND_KIDNEY == 1){
          if(SEXBABY == 1){
            if(year < 18){vk = ((9.737e-04*pow(year,5) - 0.0561*pow(year,4) + 1.1729*pow(year,3) - 10.34*pow(year,2) + 44.604*year + 28.291)/1.05/1000)*Delta_Kidney;}
            else{vk = 0.26*Delta_Kidney;}}
          else{
            if(year < 18){vk = ((4.246e-04*pow(year,5) - 2.97679e-02*pow(year,4) + 0.6539*pow(year,3) - 5.5116*pow(year,2) + 28.486*year + 21.509)/1.05/1000)*Delta_Kidney;}
            else{vk = 0.24*Delta_Kidney;}}}
    
         // Price K. et al. (2003)
        if(IND_KIDNEY == 2){
          if(SEXBABY == 1){
            if(year < 18){vk = ((9.737e-04*pow(year,5) - 0.0561*pow(year,4) + 1.1729*pow(year,3) - 10.34*pow(year,2) + 44.604*year + 28.291)/1.05/1000)*Delta_Kidney;}
            else{vk = 0.26*Delta_Kidney;}}
        else{
          if(year < 18){vk = ((9.737e-04*pow(year,5) - 0.0561*pow(year,4) + 1.1729*pow(year,3) - 10.34*pow(year,2) + 44.604*year + 28.291)/1.05/1000)*Delta_Kidney;}
          else{vk = 0.26*Delta_Kidney;}
          
          }}

          // Beaudouin et al. (2010)
        if(IND_KIDNEY == 3){
          if(SEXBABY == 1){vk = ((0.0042+(0.00767-0.0042)*exp(-0.206*year))*wbw)*Delta_Kidney;}
          else {vk = ((0.0046+(0.00709-0.0046)*exp(-0.221*year))*wbw)*Delta_Kidney;}}

          // Smith et al. 2014
        if(IND_KIDNEY == 4){vk = ((7.260e-03 - 6.690e-08*(wbw*1000) + 3.330e-13*pow((wbw*1000),2))* wbw)*Delta_Kidney;}
  
         // Wu et al. (2015)
        double S = 1;
        if(IND_KIDNEY == 5){
          S = (0.02053 * (ht/100)*pow(wbw,0.5) + 0.01266) / (0.0154 + 0.00204 * wbw + 0.0518 * pow(ht/100,2));
          if(year <= 12){vk = (0.02053 * (ht/100) * (pow(wbw,0.5) + 0.01266))*Delta_Kidney;}
          else{vk = ((0.0154 + 0.00204 * wbw + 0.0518 * pow(ht/100, 2)) * S)*Delta_Kidney;}
         
          }

          // Ring et al. (2017)
        if(IND_KIDNEY == 6){
          if(SEXBABY == 1){vk = ((10.24 * ht/100 * sqrt(wbw + 7.85) + (9.88*ht/100 * sqrt(wbw) + 7.2)) / 1050)*Delta_Kidney;}
          else{vk = ((10.65 * ht/100 * sqrt(wbw + 6.11) + (9.88*ht/100 * sqrt(wbw) + 6.55)) / 1050)*Delta_Kidney;}}

           // Sarigiannis et al. (2020)
        if(IND_KIDNEY == 7){
          if(year < 18){vk = ((3.17e-02 * (year*365.25*24) + 1.44e-02 * pow((year*365.25*24),1.06) + 3.80e1)/20000)*Delta_Kidney;}
          else{vk = 0.48*Delta_Kidney;}}

        // Pendse et al. (2020)
        if(IND_KIDNEY == 8){vk = (exp(-2.306*(pow((ht/100),1.93))))*Delta_Kidney;}

        // Deepika et al. (2021)
        if(IND_KIDNEY == 9){
          if(SEXBABY == 1){vk = (5.668e-02 - 4.962e-04 * year + 3.501e-03 * wbw)*Delta_Kidney;}
          else{vk = (0.0458676 * year + 0.0035115 * wbw)*Delta_Kidney;}}

          // Liver
  double vl = 0.026;
    
    // Haddad et al. 2001
      if(IND_LIVER == 1){
        if(SEXBABY == 1){
         if(year < 18){vl = ((0.0072*pow(year,5) - 0.3975*pow(year,4) + 7.9052*pow(year,3) - 65.624*pow(year,2) + 262.02*year + 157.52)/1000)*Delta_Liver;}
         else{vl = ((0.0072*pow(18,5) - 0.3975*pow(18,4) + 7.9052*pow(18,3) - 65.624*pow(18,2) + 262.02*18 + 157.52)/1000)*Delta_Liver;}}
       else{
         if(year < 18){vl = (0.0057*pow(year,5) - 0.3396*pow(year,4) + 7.0134*pow(year,3) - 59.539*pow(year,2) + 251.9*year + 139.65/1000)*Delta_Liver;}
         else{vl = (0.0057*pow(18,5) - 0.3396*pow(18,4) + 7.0134*pow(18,3) - 59.539*pow(18,2) + 251.9*18 + 139.65/1000)*Delta_Liver;}}}
  
    // Price K et al. 2003
      if(IND_LIVER == 2){
        if(year < 18){vl = ((0.0072*pow(year,5) - 0.3975*pow(year,4) + 7.9052*pow(year,3) - 65.624*pow(year,2) + 262.02*year + 157.52)/1000)*Delta_Liver;}
        else{vl = ((0.0072*pow(18,5) - 0.3975*pow(18,4) + 7.9052*pow(18,3) - 65.624*pow(18,2) + 262.02*18 + 157.52)/1000)*Delta_Liver;}
        }
  
    // Haddad et al. (2006)
      if(IND_LIVER == 3){vl = (0.05012 * pow(wbw, 0.78))*Delta_Liver;}

    // Verner et al. (2008)
      if(IND_LIVER == 4){
        vl = 0.0501 * wbw * Delta_Liver;
        }

    // Beaudouin et al. 2010
      if(IND_LIVER == 5){
        if(SEXBABY == 1){vl = ((0.0247+(0.0409-0.0247)*exp(-0.218*year))*wbw)*Delta_Liver;}
        else{vl = ((0.0233+(0.038-0.0233)*exp(-0.122*year))*wbw)*Delta_Liver;}}
    
    // Smith et al. 2014
      if(IND_LIVER == 6){
        if(SEXBABY == 1){vl = ((3.939e-02 - 7.058e-07*(wbw*1000) + 1.155e-11*pow((vl*1000),2) - 8.016e-17*pow((wbw*1000),3) + 1.869e-22*pow((wbw*1000),4)) * wbw)*Delta_Liver;}
        else{vl = ((3.939e-02 - 7.058e-07*(wbw*1000) + 1.155e-11*pow((wbw*1000),2) - 8.016e-17*pow((wbw*1000),3) + 1.869e-22*pow((wbw*1000),4)) * wbw) *Delta_Liver;}}
  
    // Wu et al. (2015)
      if(IND_LIVER == 7){
        S = 0.05012 * pow(wbw, 0.78) / (1.0728 * Sskin - 0.3457);
        if(year <= 22){vl = (0.05012 * pow(wbw, 0.78))*Delta_Liver;}
        else{vl = ((1.0728 * Sskin - 0.3457) * S)*Delta_Liver;}}

    // Ring et al. (2017)
      if(IND_LIVER == 8){
        if(SEXBABY == 1){vl = ((576.9 * ht/100 + 8.9*wbw - 159.7)/1050)*Delta_Liver;}
        else{vl = ((674.3*ht/100 + 6.5*wbw - 214.4)/1050)*Delta_Liver;}}

    // Mallick et al.(2020)
      if(IND_LIVER == 9){
        if(year < 22){vl = (0.05012*pow(wbw,0.78))*Delta_Liver;}
        else{vl = ((1.0728*(Sskin/10000)-0.3457)*((0.05012*pow(wbw,0.78))/(1.0728*(Sskin/10000)-0.3457)))*Delta_Liver;}}
    
    // Sarigiannis et al. (2020)
      if(IND_LIVER == 10){
        if(year < 18){vl = ((2.79e-03*(year*365*24) + 1.10*pow((year*365*24),(6.03e-01)) + 1.60e+02)/1000)*Delta_Liver;}
        else{vl = 2.10*Delta_Liver;}}

    // Pendse et al. (2020)
      if(IND_LIVER == 11){
        if(year < 22){vl = (0.05012*pow(wbw,0.78))*Delta_Liver;}
        else{vl = ((1.0728*(Sskin/10000)-0.3457)*((0.05012*pow(wbw,0.78))/(1.0728*(Sskin/10000)-0.3457)))*Delta_Liver;}}
    
    // Deepika et al.(2021)
      if(IND_LIVER == 12){
        if(SEXBABY == 1){vl = (-0.0143744 - 0.0044728*year + 0.0264591*wbw)*Delta_Liver;}
        else{vl = (0.0017717 - 0.0030113 * year + 0.0253455*wbw)*Delta_Liver;}}

         /// BLOOD (composition = (1-hct) plasma + hct * RBC)(in L)
    double vb   = 1; // Blood volume
    double vp   = 1; // Plasma volume
    double vrbc = 1; // Red blood cell volume
    
    // Haddad et al. (2001)
    if(IND_BLOOD == 1){
      if(SEXBABY==1){
        if(year < 18){
           vb = (-0.0623*pow(year,5) + 2.4425*pow(year,4) - 31.37*pow(year,3) + 149.98*pow(year,2) + 31.305*year + 393.7)/1000;
        }
        else{vb = 5.285;}}              
      else{
        if(year < 18){
          vb = (0.0018*pow(year,5) +  0.0959*pow(year,4) - 4.4055*pow(year,3) + 45.442*pow(year,2) + 82.808*year + 292.26)/1000;
        }
        else{vb = 4.28;}}
      vp = (1-hct) * vb;
      vrbc = hct*vb;}

    // Price K. et al. (2003)
    if(IND_BLOOD == 2){
      if(SEXBABY==1){
        if(year < 18){
           vb = (-0.0623*pow(year,5) + 2.4425*pow(year,4) - 31.37*pow(year,3) + 149.98*pow(year,2) + 31.305*year + 393.7)/1000;
        }
        else{vb = 5.285;}
      }
      else{
        if(year < 18){
           vb = (-0.0623*pow(year,5) + 2.4425*pow(year,4) - 31.37*pow(year,3) + 149.98*pow(year,2) + 31.305*year + 393.7)/1000;
          }
          else{vb = 5.285;}
          
        }
      vp = (1-hct) * vb;
      vrbc = hct*vb;
    }
        
    // Beaudouin et al. (2010)
    if(IND_BLOOD == 3){
      if(SEXBABY == 1){
        if(year < 1){vb = (-0.027*pow(year,2) + 0.077*year)*wbw;}
        else{vb = (0.0761*(1/1+exp(-0.683*year+0.946)))*wbw;}
      }
      else{
        if(year < 1){vb = (-0.0273*year + 0.0771)*wbw;}
        if(year >= 1 && year <= 20){vb = (3.28e-05*pow(year,3) + 1.21e-03*pow(year,2) + 1.24e-02*year + 3.86e-02) * wbw;}
        else{vb = 0.065 * wbw;}
      }
      vp = (1-hct) * vb;
      vrbc = hct*vb;}
    
    // Smith et al. (2014)
    if(IND_BLOOD == 4){vb= (8.970e-02 - 3.500e-07*wbw*1000 + 6.540e-13*pow(wbw*1000,2)) * wbw;
                    vp = (1-hct) * vb;
                    vrbc = hct*vb;}
    
    // Wu et al. (2015)
    if(IND_BLOOD == 5){
      if(SEXBABY == 1){
        vb = pow(10, (1.2082*log(Sskin) + 3.2869))/1000;
        
      }
      else{
        vb = pow(10, (1.2082*log(Sskin) + 3.2869))/1000;
      }
      vp = (1-hct) * vb;
      vrbc = hct*vb;
    }

    // Ring et al. (2017)
    if(IND_BLOOD == 6){
      if(SEXBABY == 1){
        vb = (3.33 * Sskin - 0.81) / 1.06;
      }
      else{
        vb = (2.66 * Sskin - 0.46) / 1.06;
      }
      vp = (1-hct) * vb;
      vrbc = hct*vb;
    }

    // Mallick et al. (2019)
    if(IND_BLOOD == 7){vp = (pow(10,(1.2082*log(Sskin/10000)+3.2869))/1000*(1-hct))/1000;
                       vb = vp/(1-hct);
                       vrbc = vb*hct;}

    // Sarrigiannis et al. (2020)
    if(IND_BLOOD == 8){
      if(year < 18){
        vb = (1.15e-01*(year*365*24) - 1.10e-01*pow((year*365*24),(9.92e-01)) + 1.33e+02)/500;
      }
      else{vb = 5.02;}
      vp = (1-hct) * vb;
      vrbc = hct*vb;}
    
    // Pendse et al. (2020)
    if(IND_BLOOD == 9){vp = (pow(10,(1.2082*log(Sskin/10000)+3.2869))/1000*(1-hct))/1000;
                       vb = vp/(1-hct);
                       vrbc = vb*hct;}


  /// Arterial or Venous blood (equal)
    double var = 0.5*vb;
    double vve = 0.5*vb;
   
  // Slowly perfused tissue (Heart, Muscles (+ Diaphragm + Tongue), Skin, Bones (+ Marrow))
  
  /// Heart
    double vheart = 1;
    // Haddad et al. (2001)
      if(IND_HEART == 1){
        if(SEXBABY == 1){
          if(year < 18){vheart = ((-0.0132*pow(year,4) + 0.5051*pow(year,3) - 5.7113*pow(year,2) + 32.213*year + 20.364)/1000)*Delta_Heart;}
          else{vheart = ((-0.0132*pow(18,4) + 0.5051*pow(18,3) - 5.7113*pow(18,2) + 32.213*18 + 20.364)/1000)*Delta_Heart;}}
        else{
          if(year < 18){vheart = ((4.246e-04*pow(year,5) - 2.97679e-02*pow(year,4) + 0.6539*pow(year,3) - 5.5116*pow(year,2) + 28.486*year + 21.509)/1000)*Delta_Heart;}
          else{vheart = ((4.246e-04*pow(18,5) - 2.97679e-02*pow(18,4) + 0.6539*pow(18,3) - 5.5116*pow(18,2) + 28.486*18 + 21.509)/1000)*Delta_Heart;}}}
    
    // Price K. et al. (2003)
      if(IND_HEART == 2){
        if(year < 18){vheart = ((-0.0132*pow(year,4) + 0.5051*pow(year,3) - 5.7113*pow(year,2) + 32.213*year + 20.364)/1000)*Delta_Heart;}
        else{vheart = ((-0.0132*pow(18,4) + 0.5051*pow(18,3) - 5.7113*pow(18,2) + 32.213*18 + 20.364)/1000)*Delta_Heart;}
          }
  
    // Haddad et al. 2006
      if(IND_HEART == 3){vheart = (1.017e-07*pow(pow(ht,0.6640) *pow(wbw,0.3851)*242.7,1.420))*Delta_Heart;}
  
    // Verner et al. (2008)
      if(IND_HEART == 4){
        vheart = (1.017e-07 * pow((pow(ht,0.6862) * pow(wbw,0.3561) * 242.7),1.420))*Delta_Heart;
        }

    // Beaudouin et al. 2010
    if(IND_HEART == 5){
      if(SEXBABY == 1){vheart = (0.0045 * wbw)*Delta_Heart;}
      else{vheart = 0.004167 * wbw * Delta_Heart;}}
  
    // Sarigiannis et al. (2020)
    if(IND_HEART == 6){
      if(year < 18){vheart = ((4.68e-02*(year*365*24) - 3.81e-02*pow((year*365*24),1.01) + 2.80e+01)/1000)*Delta_Heart;}
      else{vheart = 0.32*Delta_Heart;}}

      /// Muscles
    double vm = 1;
    // Haddad et al. (2001)
      if(IND_MUSCLE == 1){
        if(SEXBABY == 1){
          if(year < 18){vm = ((0.535*pow(year,3) + 56.937*pow(year,2) - 124.25*year + 1051.3)/1000)*Delta_Muscle;}
          else{vm = ((0.535*pow(18,3) + 56.937*pow(18,2) - 124.25*18 + 1051.3)/1000)*Delta_Muscle;}}
        else{
          if(year < 18){((0.015*pow(year,6) - 0.8155*pow(year,5) + 15.849*pow(year,4) - 134.99*pow(year,3) + 549.43*pow(year,2) - 530.65*year + 958.87)/1000)*Delta_Muscle;}
          else{((0.015*pow(18,6) - 0.8155*pow(18,5) + 15.849*pow(18,4) - 134.99*pow(18,3) + 549.43*pow(18,2) - 530.65*18 + 958.87)/1000)*Delta_Muscle;}}}

    // Price K. et al. (2003)
      if(IND_MUSCLE == 2){
        if(year < 18){vm = ((0.535*pow(year,3) + 56.937*pow(year,2) - 124.25*year + 1051.3)/1000)*Delta_Muscle;}
        else{vm = ((0.535*pow(18,3) + 56.937*pow(18,2) - 124.25*18 + 1051.3)/1000)*Delta_Muscle;}
        }

    // Haddad et al. 2006
      if(IND_MUSCLE == 3){
        if(SEXBABY == 1){
          if(year < 3){vm = (9.561e-02*wbw + 1.601e-02*ht + 1.097e-01*year)*Delta_Muscle;}
          else if(year >= 3 && year < 18){vm = (2.789e-01*wbw - 6.358e-02*ht + 9.850e-01*year + 2.167)*Delta_Muscle;}
          else{vm = (2.598e-01*wbw + 1.206e-01*ht - 4.300e-03*year - 1.110)*Delta_Muscle;}
        }
        else{
          if(year < 3){vm = (9.563e-02*wbw + 1.650e-02*ht + 9.102e-02*year - 1.642e-01)*Delta_Muscle;}
          else if(year >= 3 && year < 18){vm = (1.629e-01*wbw + 2.603e-02*ht + 4.661e-01*year - 3.332)*Delta_Muscle;}
          else{vm = (6.780*pow((Sskin/10000),1.629) - 1.492e-03*year + 3.580)*Delta_Muscle;}
       }
      }

    // Verner et al. (2008)
      if(IND_MUSCLE == 4){
        if(year < 3){vm = (9.563e-02*wbw + 1.650e-02*ht + 9.102e-02*year - 1.642e-01)*Delta_Muscle;}
          else if(year >= 3 && year < 18){vm = (1.629e-01*wbw + 2.603e-02*ht + 4.661e-01*year - 3.332)*Delta_Muscle;}
          else{vm = ((6.780*pow((Sskin/10000),1.629) - 1.492e-03*year + 3.580))*Delta_Muscle;}
        
      }

    // Beaudouin et al. (2010)
      if(IND_MUSCLE == 5){
        if(SEXBABY == 1){
          if(year < 24.3){vm = ((0.3973+(0.201-0.3973)*exp(-0.141*year)) * wbw)*Delta_Muscle;}
          else{vm = ((0.3973+(0.201-0.3973)*exp(-0.141 * year))* (-0.0001264 * pow(year,2) + 0.006131*year + 0.926) * wbw)*Delta_Muscle;} 
        }
        else{
          if(year <= 25.90709){vm = ((0.2917+(0.207-0.2917)*exp(-0.339*year))*wbw)*Delta_Muscle;}
          else{vm = ((0.2917+(0.207-0.2917)*exp(-0.339*year))* (-0.0001264 * pow(year,2) + 0.006131*year + 0.926) *wbw)*Delta_Muscle;}
       }
      }
  
    // Smith et al. (2014)
      if(IND_MUSCLE == 6){vm = ((1.251e-01 + 1.458e-05*(wbw*1000) - 2.927e-10*pow(wbw*1000,2) + 2.114e-15*pow(wbw*1000,3) - 5.250e-21*pow(wbw*1000,4)) * wbw)*Delta_Muscle;}

    // Ring et al. (2017)
      if(IND_MUSCLE == 7){
        if(SEXBABY == 1){
          if(year  < 19){vm = (12.4/(1+(6.5*exp(-0.55*year)))) + (19.7 / (1 + exp(-0.85*(year - 13.7))))*Delta_Muscle;}
          else{vm = 31.86*Delta_Muscle;}
        }
        else{
          if(year  < 19){vm = (7.0/(1+(6.5*exp(-0.55*year)))) + (13.0 / (1 + exp(-0.75*(year - 11.5))))*Delta_Muscle;}
          else{vm = 19.95*Delta_Muscle;}
        }
      } 

    // Sarigiannis et al. (2020)
      if(IND_MUSCLE == 4){
        if(year < 18){vm = ((1.26e-01*(year*365*24) + 7.76e-06*pow((year*365*24),1.76) + 9.50e+02)/1000)*Delta_Muscle;}
        else{vm = 31.73*Delta_Muscle;}}
  
  
  /// Diaphragm
  double vd = (3e-04)*wbw;

  /// Tongue
  double vt = 0;
    // Haddad et al. (2006)
    if(IND_TONGUE == 1){
     vt = 1.190e-03 * wbw - 4.302e-04;
    }
    // Verner et al. (2008)
    if(IND_TONGUE == 2){
      if(SEXBABY == 1){
        vt = 0;
        
      }
      else{
        vt = 1.190e-03 * wbw - 4.302e-04;
      }
    }  
/// Skin
    double vs = 1;
    double vderm = 1;
    double vepid = 1;
      // Haddad et al. (2001)
        if(IND_VSKIN == 1){
          if(SEXBABY == 1){
            if(year < 18){vs = ((-0.0992*pow(year,4) + 4.2762*pow(year,3) - 62.165*pow(year,2) + 432.78*year + 203.2)/1000);}
            else{vs = ((-0.0992*pow(18,4) + 4.2762*pow(18,3) - 62.165*pow(18,2) + 432.78*18 + 203.2)/1000);}}
          else{
            if(year < 18){vs = ((4.76622e-03*pow(wbw,5) - 0.27924*pow(year,4) + 6.3444*pow(year,3) - 70.113*pow(wbw,2) + 429.85*year + 252.06)/1000);}
            else{vs = ((4.76622e-03*pow(18,5) - 0.27924*pow(18,4) + 6.3444*pow(18,3) - 70.113*pow(18,2) + 429.85*18 + 252.06)/1000);}}}
  
      // Price K. et al. (2003)
        if(IND_VSKIN == 2){
          if(year < 18){vs = ((-0.0992*pow(year,4) + 4.2762*pow(year,3) - 62.165*pow(year,2) + 432.78*year + 203.2)/1000);}
          else{vs = ((-0.0992*pow(18,4) + 4.2762*pow(18,3) - 62.165*pow(18,2) + 432.78*18 + 203.2)/1000);}}

      // Haddad et al. (2006)
        if(IND_VSKIN == 3){
          vepid = 7.850e-02*pow(Sskin,1.049);
          if(year < 10){vderm = 0.664*Sskin;}
          if(year >= 10 & year < 20){vderm = -9.356e-05 - 2.151e-05*year - 5.058e-01*Sskin + 1.134e-06*pow(year,2) + 0.1170*year*Sskin - 1.673e-05*pow(Sskin,2);}
          else {vderm = 1.834*Sskin;}
          vs = (vepid + vderm);}

      // Verner et al. (2008)
        if(IND_VSKIN == 4){
          if(SEXBABY == 1){
            vepid = 7.850e-02*pow(Sskin,1.049);
            if(year < 10){vderm = 0.664*Sskin;}
            if(year >= 10 & year < 20){vderm = -9.356e-05 - 2.151e-05*year - 5.058e-01*Sskin + 1.134e-06*pow(year,2) + 0.1170*year*Sskin - 1.673e-05*pow(Sskin,2);}
            else {vderm = 1.834*Sskin;}
            vs = (vepid + vderm);
            
          }
          else{
            vepid = 7.850e-02*pow(Sskin,1.049);
            if(year < 10){vderm = 0.664*Sskin;}
            if(year >= 10 & year < 20){vderm = -9.356e-05 - 2.151e-05*year - 5.058e-01*Sskin + 1.134e-06*pow(year,2) + 0.1170*year*Sskin - 1.673e-05*pow(Sskin,2);}
            else {vderm = 1.834*Sskin;}
            vs = (vepid + vderm);}}

      // Beaudouin et al. (2010)
        if(IND_VSKIN == 5){
          if(SEXBABY == 1){
            if(year < 20){vs = ((-1.171e-05* pow(year,3) + 5.413e-04* pow(year,2) -  6.1966e-03* year + 4.623e-02) * wbw);}
            else{vs = (0.0452 * wbw);}}
          else{
            if(year < 20){vs = ((-7.8882e-06*pow(year,3) + 4.0224e-04*pow(year,2) - 5.2146e-03*year + 4.5605e-02) * wbw);}
            else{vs = (0.0383 * wbw);}}}
  
      // Smith et al. (2014)
        if(IND_VSKIN == 6){vs = ((1.030e-01 - 2.560e-06*wbw*1000 + 3.680e-11*pow(wbw*1000,2) - 2.580e-16*pow(wbw*1000,3) + 8.620e-22*pow(wbw*1000,4) - 1.100e-27*pow(wbw*1000,5)) * wbw);}
  
      // Ring et al. (2017)
        if(IND_VSKIN == 7){vs = (exp(1.64 * Sskin - 1.93)/1.116);}
      
      // Sarigiannis et al. (2020)
        if(IND_VSKIN == 7){
          if(year < 18){vs = ((2.88e-01*(year*365*24) + 2.71e-01*pow((year*365*24),(9.98e-01)) + 2.00e+02)/25000);}
          else{vs = 3.49;}}

      // Pendse et al. (2020)
        if(IND_VSKIN == 6){vs = ((Sskin*10000*0.15)/1000);}
  
  /// Skeleton (Marrow / Bones = 80 % Cortical + 20% Trabecular)
    double vbone = 1;
    double vmarr = 1;
      // Haddad et al. (2001)
        if(IND_BONE == 1){
          if(SEXBABY == 1){
            if(year < 18){vbone = ((-0.0306 * pow(year,5) + 0.5222 * pow(year,4) + 9.7109 * pow(year,3) - 197.97 * pow(year,2) + 1089.7 * year + 546.6)/1000)*Delta_Bone;}
            else{vbone = 9.65*Delta_Bone;}}
          else{
            if(year < 18){vbone = ((-2.831e-03 * pow(year,5) - 0.18184 * pow(year,4) + 10.685 * pow(year,3) - 142.88 * pow(year,2) + 782.05*year + 609.64)/1000)*Delta_Bone;}
            else{vbone = 6.27*Delta_Bone;}}}

      // Price K. et al. (2001)
        if(IND_BONE == 2){
          if(SEXBABY == 1){
            if(year < 18){vbone = ((-0.0306 * pow(year,5) + 0.5222 * pow(year,4) + 9.7109 * pow(year,3) - 197.97 * pow(year,2) + 1089.7 * year + 546.6)/1000)*Delta_Bone;}
            else{vbone = 9.65*Delta_Bone;}}
          else{
            if(year < 18){vbone = ((-0.0306 * pow(year,5) + 0.5222 * pow(year,4) + 9.7109 * pow(year,3) - 197.97 * pow(year,2) + 1089.7 * year + 546.6)/1000)*Delta_Bone;}
            else{vbone = 9.65*Delta_Bone;}
            
            }}

      // Beaudouin et al. (2010)
        if(IND_BONE == 3){
          if(SEXBABY == 1){vbone = ((0.026 + (0.043-0.026) * exp(-0.091*year))*wbw)*Delta_Bone;}
          else{vbone = ((0.0253 + (0.0429-0.0253) * exp(-0.0792*year))*wbw)*Delta_Bone;}}

      // Ring et al. (2017)
        if(IND_BONE == 4){
          if(SEXBABY == 1){
            if(year < 50){vbone = ((0.89983 + (2.9901 - 0.89989)/(1 + exp(14.17081 - year)/1.58179))/0.65/0.5)*Delta_Bone;}
            else{vbone = ((0.89983 + (2.9901 - 0.89989) / (1 + exp(14.17081 - year)/1.58179) - (0.0019 * year))/0.65/0.5)*Delta_Bone;}
          }
          else{
            if(year < 50){vbone = ((0.74042 + (2.14976 - 0.74042)/(1 + exp(12.35466 - year)/1.58179))/0.65/0.5)*Delta_Bone;}
            else{vbone = ((0.74042 + (2.14976 - 0.74042) / (1 + exp(12.35466 - year)/1.58179) - (0.0056 * year))/0.65/0.5)*Delta_Bone;}}}

      // Sarrigianis et al. (2020)
        if(IND_BONE == 5){
          if(year < 18){vbone = ((5.97e-02*(year*365*24) + 1.26*pow((year*365*24),(6.10e-01)) + 4.52e+02)/1000)*Delta_Bone;}
          else{vbone = 11.73*Delta_Bone;}}
    

  /// Bone marrow (71 % Red Marrow + 29% Yellow Marrow)
      // Haddad et al. (2001)
        if(IND_MARR == 1){
          if(SEXBABY == 1){
            if(year < 18){vmarr = (1.9956e-03*pow(year,6) - 0.11169*pow(year,5) + 2.189*pow(year,4) - 17.726*pow(year,3) + 59.767*pow(year,2) + 14.405*year + 73.716)/1000;}
            else{vmarr = (1.9956e-03*pow(18,6) - 0.11169*pow(18,5) + 2.189*pow(18,4) - 17.726*pow(18,3) + 59.767*pow(18,2) + 14.405*18 + 73.716)/1000;}}
          else{
            if(year < 18){vmarr = (7.984e-04*pow(year,6) - 0.037966*pow(year,5) + 0.5272*pow(year,4) - 1.1311*pow(year,3) - 12.285*pow(year,2) + 123.87*year + 53.358)/1000;}
            else{vmarr = (7.984e-04*pow(18,6) - 0.037966*pow(18,5) + 0.5272*pow(18,4) - 1.1311*pow(18,3) - 12.285*pow(18,2) + 123.87*18 + 53.358)/1000;}}}
  
      // Price K. et al. (2003)
        if(IND_MARR == 2){if(year < 18){
          vmarr = (1.9956e-03*pow(year,6) - 0.11169*pow(year,5) + 2.189*pow(year,4) - 17.726*pow(year,3) + 59.767*pow(year,2) + 14.405*year + 73.716)/1000;}
          else{vmarr = (1.9956e-03*pow(18,6) - 0.11169*pow(18,5) + 2.189*pow(18,4) - 17.726*pow(18,3) + 59.767*pow(18,2) + 14.405*18 + 73.716)/1000;}}

      // Beaudouin et al. (2010)
        if(IND_MARR == 3){
          if(SEXBABY == 1){vmarr = (0.05+(0.0138-0.05)*exp(-0.112*year))*wbw;}
          else{vmarr = (0.045+(0.0138-0.045)*exp(-0.136*year))*wbw;}}
    
        // Smith et al. (2014)
          if(IND_MARR == 4){vmarr = 2.1e-02*wbw;}
    
  double vcort = 0.8*vbone;
  double vtrab = 0.2*vbone;
  double vredmar = 0.71*vmarr;
  double vyelmar = 0.29*vmarr;
    
 

  // Richly perfused tissue (Breast + Thyroid + Spleen + Pancreas + Adrenals + Gonads + Lungs)
  
    /// Breast 
      double vbreast = 1;
      // Verner et al. (2008)
        if(IND_BREAST == 1){
          if(SEXBABY == 1){vbreast = 0.0062 * wbw;
          }
          else{vbreast = 0.0062 * wbw;}}
      
      // Beaudouin et al. (2010)
        if(IND_BREAST == 2){
          if(SEXBABY == 1){vbreast = (3.42e-04*1/(1+exp(-1.42*year+20.1)))*wbw;}
          else{vbreast = (0.00833*1/(1+exp(-1.92*year+28.6)))*wbw;}}
    
  /// Thyroid
    double vthyr = 1;
      // Beaudouin et al. (2010)
        if(IND_THYR == 1){
          if(SEXBABY == 1){vthyr = 0.000274 * wbw;}
          else{vthyr = 0.0002833 * wbw;}
        }
    
  /// Spleen
    double vspleen = 1;
      // Haddad et al. (2001)
        if(IND_SPLEEN == 1){
          if(SEXBABY == 1){
            if(year < 18){vspleen = ((-0.0091*pow(year,4) + 0.3457*pow(year,3) - 4.0754*pow(year,2) + 22.269*year + 11.05)/1000)*Delta_Spleen;}
            else{vspleen = ((-0.0091*pow(18,4) + 0.3457*pow(18,3) - 4.0754*pow(18,2) + 22.269*18 + 11.05)/1000)*Delta_Spleen;}}
          else{
            if(year < 18){vspleen = ((6.3696e-04*pow(year,5) - 3.5327e-02*pow(year,4) + 0.7073*pow(year,3) - 6.0357*pow(year,2) + 26.311*year + 8.1692))*Delta_Spleen;}
            else{vspleen = ((6.3696e-04*pow(18,5) - 3.5327e-02*pow(18,4) + 0.7073*pow(18,3) - 6.0357*pow(18,2) + 26.311*18 + 8.1692))*Delta_Spleen;}}}

      // Price K. et al. (2003)
        if(IND_SPLEEN == 2){
          if(year < 18){vspleen = ((-0.0091*pow(year,4) + 0.3457*pow(year,3) - 4.0754*pow(year,2) + 22.269*year + 11.05)/1000)*Delta_Spleen;}
          else{vspleen = ((-0.0091*pow(18,4) + 0.3457*pow(18,3) - 4.0754*pow(18,2) + 22.269*18 + 11.05)/1000)*Delta_Spleen;}}
      
      // Beaudouin et al. (2010)
        if(IND_SPLEEN == 3){
          if(SEXBABY == 1){vspleen = 0.0021 * wbw * Delta_Spleen;}
          else{vspleen = 0.0022 * wbw * Delta_Spleen;}}
      
      // Smith et al. (2014)
        if(IND_SPLEEN == 4){vspleen = ((3.120e-03 - 5.570e-09*(wbw*1000)) * wbw)*Delta_Spleen;}
    
      // Ring et al. (2017)
        if(IND_SPLEEN == 5){
          if(SEXBABY == 1){vspleen = ((8.74 * (ht/100) * sqrt(wbw) + 11.06)/1054)*Delta_Spleen;}
          else{vspleen = ((9.36 * (ht/100) * sqrt(wbw) + 7.98)/1054)*Delta_Spleen;}}

    // Pancreas
      double vpancreas = 1;
        // Beaudouin et al. (2010)
          if(IND_PANCREAS == 1){
            if(SEXBABY == 1){vpancreas = (0.00192 * wbw)*Delta_Pancreas;}
            else{vpancreas = (0.002 * wbw)*Delta_Pancreas;}}
    
        // Smith et al. (2014)
          if(IND_PANCREAS == 2){vpancreas = (1.48e-03*wbw)*Delta_Pancreas;}

        // Ring et al. (2017)
          if(IND_PANCREAS == 3){
            if(SEXBABY == 1){vpancreas = ((7.6 * (ht/100) * sqrt(wbw) - 0.79) / 1045)*Delta_Pancreas;}
            else{vpancreas = ((7.92 * (ht/100) * sqrt(wbw) - 2.09) / 1045)*Delta_Pancreas;}}
    
    // Adrenals
      double vadrenal = 1;
        // Beaudouin et al. (2010)
          if(SEXBABY == 1){vadrenal = 2.0e-04 + (1.71e-03 - 2.0e-04) * exp(-2.02*year);}
          else{vadrenal = 0.0002 + (0.00171 - 0.0002) * exp(-2.02*year);}
    
    // Gonads / Reproductive organs
      double vgonads = 1;
        // Haddad et al. (2001)
          if(IND_GONAD == 1){
            if(SEXBABY == 1){
              if(year < 18){vgonads = ((0.0013*pow(year,4) - 0.01*pow(year,3) - 0.104*pow(year,2) + 1.0584*year + 1.78)/1000)*Delta_Gonad;}
              else{vgonads = ((0.0013*pow(18,4) - 0.01*pow(18,3) - 0.104*pow(18,2) + 1.0584*18 + 1.78)/1000)*Delta_Gonad;}}
            else{
              if(year < 18){vgonads = ((0.0044*pow(year,3) + 0.0412*pow(year,2) +  0.2333*year + 2.1725)/1000)*Delta_Gonad;}
              else{vgonads = ((0.0044*pow(18,3) + 0.0412*pow(18,2) +  0.2333*18 + 2.1725)/1000)*Delta_Gonad;}}}

        // Price K. et al. (2003)
          if(IND_GONAD == 2){
            if(year < 18){vgonads = ((0.0013*pow(year,4) - 0.01*pow(year,3) - 0.104*pow(year,2) + 1.0584*year + 1.78)/1000)*Delta_Gonad;}
            else{vgonads = ((0.0013*pow(18,4) - 0.01*pow(18,3) - 0.104*pow(18,2) + 1.0584*18 + 1.78)/1000)*Delta_Gonad;}}

        // Verner et al. (2008)
          if(IND_GONAD == 3){
            if(SEXBABY == 1){vgonads = 0.0014 * wbw  * Delta_Gonad;}
            else{vgonads = 0.0014 * wbw * Delta_Gonad;}}

        // Beaudouin et al. (2010)
          if(IND_GONAD == 2){
            if(SEXBABY == 1){
              if(year < 20.1){vgonads = ((-1.516e-07*pow(year,3) + 9.3351e-06*pow(year,2) - 1.1177e-04*year + 4.7966e-04) * wbw)*Delta_Gonad;}
              else {vgonads = 0.0008 * wbw * Delta_Gonad;}}
            else{
              if(year < 1){vgonads = ((-1.064e-03*year + 1.338e-03)*wbw)*Delta_Gonad;}
              if(year >= 1 && year < 20){vgonads = ((2.6380e-07*pow(year,3) - 1.7943e-06*pow(year,2) - 5.6465e-06*year + 2.8105e-04) * wbw)*Delta_Gonad;}
              else{vgonads = 0.001552 * wbw * Delta_Gonad;}}}
    
        // Sarigiannis et al. (2020)
          if(IND_GONAD == 3){
            if(year < 18){vgonads = ((8.52e-02*(year*365*24) + 8.31e-02*pow((year*365*24),(9.99e-01)) + 1.10)/500000)*Delta_Gonad;}
            else{vgonads = 0.05*Delta_Gonad;}}
    
    // Lungs
      double vlungs = 1;
        //Haddad et al. (2001)
          if(IND_LUNG == 1){
            if(SEXBABY == 1){
              if(year < 18){vlungs = ((-0.0346*pow(year,4) + 1.5069*pow(year,3) - 20.31*pow(year,2) + 123.99*year + 59.213)/1050)*Delta_Lung;}
              else{vlungs = ((-0.0346*pow(18,4) + 1.5069*pow(18,3) - 20.31*pow(18,2) + 123.99*18 + 59.213)/1050)*Delta_Lung;}}
            else{
              if(year < 18){vlungs = ((6.3e-03*pow(year,5) - 0.3162*pow(year,4) + 5.5896*pow(year,3) - 42.196*pow(year,2) + 160.79*year + 50.506)/1050)*Delta_Lung;}
              else{vlungs = ((6.3e-03*pow(18,5) - 0.3162*pow(18,4) + 5.5896*pow(18,3) - 42.196*pow(18,2) + 160.79*18 + 50.506)/1050)*Delta_Lung;}}}

        // Price K. et al. (2003)
          if(IND_LUNG == 2){
            if(year < 18){vlungs = ((-0.0346*pow(year,4) + 1.5069*pow(year,3) - 20.31*pow(year,2) + 123.99*year + 59.213)/1050)*Delta_Lung;}
            else{vlungs = ((-0.0346*pow(18,4) + 1.5069*pow(18,3) - 20.31*pow(18,2) + 123.99*18 + 59.213)/1050)*Delta_Lung;}}

        // Beaudouin et al. (2010)
          if(IND_LUNG == 3){
            if(SEXBABY == 1){vlungs = 0.0068 * wbw * Delta_Lung;}
            else{vlungs = 0.0070 * wbw * Delta_Lung;}}
    
        // Smith et al. (2014)
          if(IND_LUNG == 4){vlungs = ((1.860e-02 - 4.550e-08*(wbw*1000)) * wbw)*Delta_Lung;}

        // Ring et al. (2017)
          if(IND_LUNG == 5){
            if(SEXBABY == 1){
              if(year < 18){vlungs = (((29.08 * (ht/100) * sqrt(wbw) + 11.06) + (35.47 * (ht/100) * sqrt(wbw) + 5.53)) / 1050)*Delta_Lung;}
              else{vlungs = 0.89 * Delta_Lung;}}
            else{
              if(year < 18){(((31.46 * (ht/100) * sqrt(wbw) + 1.43) + (35.30 * (ht/100) * sqrt(wbw) + 1.53)) / 1050)*Delta_Lung;}
              else{vlungs = 0.80 * Delta_Lung;}}}

        // Sarigiannis et al. (2020)
          if(IND_LUNG == 6){
            if(year < 18){vlungs = ((9.74e-02*(year*365*24) + 6.33e-02*pow((year*365*24),1.03) + 8.40e+01)/25000)*Delta_Lung;}
            else{vlungs = 1.19*Delta_Lung;}}

        // Pendse et al. (2020)
          if(IND_LUNG == 7){vlungs = (exp(-2.092*pow((ht/100),2.1)))*Delta_Lung;}

        // Deepika et al. (2021)
          if(IND_LUNG == 8){vlungs = (-1.454e-02 + 7.269e-04 * (year*365.25*24) + 9.329e-06 * pow((year*365.25*24),2) + 6.43e-03 * wbw + 3.083e-05 * pow(wbw,2))*Delta_Lung;}
    
  double vrich = vbreast + vthyr + vspleen + vpancreas + vadrenal + vgonads + vlungs;

  /// Gut (Stomach + Small intestine + Large intestine)
      /// Stomach
        double vstomach = 1;
        // Haddad et al. (2001)
          if(IND_STOMACH == 1){
            if(year < 18){vstomach = ((0.0008*pow(year,5) - 0.0356*pow(year,4) + 0.5823*pow(year,3) - 4.0437*pow(year,2) + 17.888*year + 7.54)/1040)*Delta_Stomach;}
            else{vstomach = ((0.0008*pow(18,5) - 0.0356*pow(18,4) + 0.5823*pow(18,3) - 4.0437*pow(18,2) + 17.888*18 + 7.54)/1040)*Delta_Stomach;}}
    
        // Price K. et aL. (2003)
          if(IND_STOMACH == 2){
            if(year < 18){vstomach = ((0.0008*pow(year,5) - 0.0356*pow(year,4) + 0.5823*pow(year,3) - 4.0437*pow(year,2) + 17.888*year + 7.54)/1040)*Delta_Stomach;}
            else{vstomach = ((0.0008*pow(18,5) - 0.0356*pow(18,4) + 0.5823*pow(18,3) - 4.0437*pow(18,2) + 17.888*18 + 7.54)/1040)*Delta_Stomach;}}
    
        // Beaudouin et al. (2010)
          if(IND_STOMACH == 3){
            if(SEXBABY == 1){vstomach = (0.0021*wbw)*Delta_Stomach;}
            else{vstomach = (0.0023 * wbw)*Delta_Stomach;}}
    
    // Intestinal tract (Small + Large Intestine)
      double vintestine = 1;
        // Haddad et al. (2001)
          if(IND_INTESTINE == 1){
            if(SEXBABY == 1){
              if(year < 18){vintestine = ((-4.7817e-02*pow(year,4) + 1.925*pow(year,3) - 22.382*pow(year,2) + 107.09*year + 51.125)/1040)*Delta_Intestine;}
              else{vintestine = ((-4.7817e-02*pow(18,4) + 1.925*pow(18,3) - 22.382*pow(18,2) + 107.09*18 + 51.125)/1040)*Delta_Intestine;}}
            else{
              if(year < 18){vintestine =  ((-0.0513*pow(year,4) + 2.0352*pow(year,3) - 23.478*pow(year,2) + 110.61*year + 49.229)/1040)*Delta_Intestine;}
              else{vintestine =  ((-0.0513*pow(18,4) + 2.0352*pow(18,3) - 23.478*pow(18,2) + 110.61*18 + 49.229)/1040)*Delta_Intestine;}}}
    
        // Price K. et al. (2003)
          if(IND_INTESTINE == 2){
            if(year < 18){vintestine = ((-4.7817e-02*pow(year,4) + 1.925*pow(year,3) - 22.382*pow(year,2) + 107.09*year + 51.125)/1040)*Delta_Intestine;}
            else{vintestine = ((-4.7817e-02*pow(18,4) + 1.925*pow(18,3) - 22.382*pow(18,2) + 107.09*18 + 51.125)/1040)*Delta_Intestine;}}
    
        // Beaudouin et al. (2010)
          if(IND_INTESTINE == 3){
            if(SEXBABY == 1){
              if(year < 16){vintestine = ((-8.2562e-05*pow(year,2) + 1.3523e-03*year + 1.293e-02) * wbw)*Delta_Intestine;}
              else{vintestine = (0.014 * wbw)*Delta_Intestine;}}
            else{
              if(year<14.453301){vintestine = ((-7.421e-05*pow(year,2) + 1.276e-03*year + 1.298e-02) * wbw)*Delta_Intestine;}
              else{vintestine = (0.0160 * wbw)*Delta_Intestine;}}}

        // Smith et al. (2014)
          if(IND_INTESTINE == 4){vintestine = ((1.650e-02)*wbw)*Delta_Intestine;}

        // Wu et al. (2015)
          if(IND_INTESTINE == 5){
            if(SEXBABY == 1){vintestine = (0.027 * LBM)*Delta_Intestine;}
            else{vintestine = (0.027 * LBM)*Delta_Intestine;}}
    
        // Mallick et al. (2020)
          if(IND_INTESTINE == 6){
            if(SEXBABY == 1){vintestine  = (0.021*LBM)*Delta_Intestine;}
            else{vintestine = (0.027*LBM)*Delta_Intestine;}}
    
        // Sarigiannis et al. (2020)
          if(IND_INTESTINE == 4){
            if(year < 18){vintestine = ((8.20e-02*(year*365*24) + 4.41e-02*pow((year*365*24),(1.04)) + 9.00e+01)/20000)*Delta_Intestine;}
            else{vintestine = 1.21*Delta_Intestine;}}

        // Pendse et al. (2020)
          if(IND_INTESTINE == 8){
            if(SEXBABY == 1){vintestine  = (0.021*LBM)*Delta_Intestine;}
            else{vintestine = (0.027*LBM)*Delta_Intestine;}}
    
    double vsmallintestine = 0.63*vintestine;
    double vduodenum = 0.09*vsmallintestine;
    double vjejunum = 0.43*vsmallintestine;
    double vileum = 0.48*vsmallintestine;
    double vlargeintestine = 0.37*vintestine;
    
    double vgut = vstomach + vintestine;


    /// Lumping of organs for Hg PBK model
    double vslow = vheart + vm + vs + vbone + vmarr;
    double vrapid = vbreast + vthyr + vspleen + vpancreas + vadrenal + vgonads + vlungs;
   
   
    /// Hair volume
  double vhair = 0.002 * wbw; /// warnings to update !!!!!!!!!!!!!!!!!!!!!!!!! <!> <!> <!>

  // Calculation of the total body weight
  double wbw_f = vfat + vbr + vk + vhair + vb + vl + vheart + vd + vm + vs + vbone + vmarr + vstomach + vintestine + vbreast + vthyr + vspleen + vpancreas + vadrenal + vgonads + vlungs;
  
  
  /// Cardiac output
  double Q = 20*24*pow(wbw,3/4); // mother cardiac output

/// Cardiac output
/// double Q = 1;

  // Price K. et al. (2003)
    if(IND_CO == 1) {
      if(SEXBABY == 1){
        if(year < 18){Q = 0.012 * pow(year,3) - 1.2144 * pow(year,2) + 40.324 * year + 44.414;}
        else{Q = 446.8;}
      }
      else{
        if(year < 18){Q = 0.012 * pow(year,3) - 1.2144 * pow(year,2) + 40.324 * year + 44.414;}
        else{Q = 446.8;}
      }
    }

  // Haddad et al. (2006)
    if(IND_CO == 2){
      if(SEXBABY == 1){Q = (0.2519 * pow(wbw_f,0.761))*60;}
      else{Q = (0.2508 * pow(wbw_f,0.7815))*60;}
    }

  // Verner et al. (2008)
    if(IND_CO == 3){
      if(SEXBABY == 1){
        Q = 15.048 * pow(wbw_f,0.7609);}
      else{
        Q = 15.048 * pow(wbw_f,0.7609);}
    }

  // Beaudouin et al. (2010)
    if(IND_CO == 4){
      if(SEXBABY == 1){
        if(year <= 33.37){Q = (6.642 + (0.6 - 6.642) * exp(-0.1323*year))*60;}
        else{Q = (-0.000895*pow(year,2) + 0.0607*year + 5.54)*60;}
      }
      else{
        if(year <= 16.027){Q = (7.734 + (0.6 - 7.734) * exp(-0.09747 * year))*60;}
        else{Q = (0.000473 * pow(year,2) - 0.0782 * year + 7.37)*60;}
      }
    }
  
  // Wu et al. (2015)
    if(IND_CO == 5){
      if(SEXBABY == 1){
        Q = 3.5 * Sskin * 60;}
      else{
        Q = 3.5 * Sskin * 60;}
    }

  // Mallick et al. (2019)
    if(IND_CO == 6){Q = 3.5 * Sskin * 60;}

  // Pendse et al. (2020)
    if(IND_CO == 7){Q = 3.5 * Sskin * 60;}

  // Deepika et al. (2021)
    if(IND_CO == 8){
      if(SEXBABY == 1){
        Q = 6.48370 - 1.59948 * year + 214.68572 * Sskin;
      }
      else{
        Q = 5.528076 - 2.834486 * year + 0.012591 * pow(year,2) + 204.262351 * Sskin + 19.274290 * pow(Sskin,2);
      }
    }


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

  // Lumping of organs
    double qgut  = qstomach + qintestine;
    double qslow = qheart + qd + qm + qs + qbone + qmarr;
    double qrapid = qbreast + qthyr + qspleen + qpancreas + qadrenal + qgonads + qlungs;

// Volume of daily urine excreted (L/day)
  double Vurine = 0.0294 * wbw_f;

  /// Kinetic parameters
double kbr = 1.2e-05*(24)*pow(wbw_f,0.75); // Demethylation rate in brain
double kbi = 1.0e-04*(24)*pow(wbw_f,0.75); // Biliary clearance of MeHg
double ki = 1.0e-04*24*pow(wbw_f,0.75);  // Demethylation rate in gut
double kfe= 0;                       // Fecal excretion
  if (year<=1) {kfe = 0.00208*(24);}
  else {if(SEXBABY==1){kfe = 0.00625*24;}
  else{kfe = 0.00500*(24);}}
double kha = 24*pow(wbw_f,0.75)*7.0e-6;        // Excretion into hair
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
double kbl = 0;                       //Blood to liver transfer coeff.
double kbk = 17.1234;    //Blood to kidney transfer coeff.
double kkb = 0.0010;     //Kidney to blood transfer coeff.
double kku = 0.006949;   //Kidney to urine transfer coeff.
double kbh = 0.1400;     //Blood to hair transfer coeff.
double kbu = 0.06994;    //Blood to urine transfer coeff.
double kbf = 3.9917;     //Blood to feces transfer coeff.
double klf = 1.5476;     //Liver to feces transfer coeff.
double kbbr = 0.0028;    //Blood to brain transfer coeff.
double kbrb = 0.0520;    //Brain to blood transfer coeff.

double CH = HAIR/vhair;
double CKHg = (KIDNEY+KIDNEYI)/vk;
double CLHg = (LIVER+LIVERI)/vl;
double CUHg = (KIDNEYI/vk*kku + BLOODI/vb*kbu)/Vurine;
double CBrHg = (BRAIN + BRAINI)/vbr;

$ODE
dxdt_expohgi  = - expohgi;
dxdt_expomehg = - expomehg;

dxdt_INTESTINE = expomehg*wbw_f*0.95 - INTESTINE*kabs + LIVER/vl*kbi - INTESTINE*kfe -  INTESTINE*ki + GUT/vgut*kex;

dxdt_GUT = INTESTINE*kabs + qgut*(PLASMA/vp - GUT/vgut/pg) - GUT/vgut*kex;

dxdt_LIVER = PLASMA/vp*ql + GUT/vgut/pg*qgut - LIVER/vl/pl*(ql+qgut) - LIVER/vl*kbi - LIVER/vl*kl;

dxdt_LIVERI = BLOODI/vb*kbl + LIVER/vl*kl - LIVERI/vl*klf - LIVERI/vl*klb;

dxdt_PLASMA = -(kPlR*PLASMA/vp - kRPl*RBC/vrbc) - (PLASMA/vp*qk - KIDNEY/vk/pk*qk) -
  (PLASMA/vp*qrapid - RAPIDLY/vrapid/pr*qrapid) - (PLASMA/vp*qfat - FAT/vfat/pf*qfat) - (PLASMA/vp*qslow - SLOWLY/vslow/ps*qslow) -
  (PLASMA/vp*qbr - BRAIN/vbr/pbr*qbr) - (PLASMA/vp*ql+PLASMA/vp*qgut - LIVER/vl/pl*(ql+qgut));
dxdt_BLOODI = (expohgi*wbw_f)*0.15 + BRAINI/vbr*kbrb + KIDNEYI/vk*kkb - BLOODI/vb*(kbh+kbf+kbu+kbbr+kbk+kbl) + LIVERI/vl*klb;

dxdt_RBC = kPlR*PLASMA/vp - kRPl*RBC/vrbc;

dxdt_BRAIN = (PLASMA/vp - BRAIN/vbr/pbr)*qbr - BRAIN/vbr*kbr;
dxdt_BRAINI = BRAIN/vbr*kbr - BRAINI/vbr*kbrb + BLOODI/vb*kbbr;

dxdt_SLOWLY = (PLASMA/vp - SLOWLY/vslow/ps)*qslow - (SLOWLY/vslow/ps*ph)*kha ;

dxdt_HAIR = (SLOWLY/vslow/ps*ph - HAIR/vhair)*kha + BLOODI/vb*kbh;

dxdt_FAT = qfat*(PLASMA/vp - FAT/vfat/pf);

dxdt_KIDNEY = qk*(PLASMA/vp - KIDNEY/vk/pk);
dxdt_KIDNEYI= BLOODI/vb*kbk - KIDNEYI/vk*(kkb+kku);

dxdt_RAPIDLY = qrapid*(PLASMA/vp - RAPIDLY/vrapid/pr);

dxdt_URINE = (KIDNEYI/vk*kku + BLOODI/vb*kbu);

$CMT expohgi expomehg INTESTINE GUT LIVER PLASMA RBC BRAIN SLOWLY HAIR FAT KIDNEY RAPIDLY URINE BRAINI BLOODI LIVERI KIDNEYI

$CAPTURE CH CUHg vhair wbw_f vfat vbr vk vhair vb vl vheart vd vm vs vbone vmarr vstomach vintestine vbreast vthyr vspleen vpancreas vadrenal vgonads vlungs Q_f Q