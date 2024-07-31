$PARAM


AIRPRO  = 0
Delta_creat = 1
Delta_BW = 1
Delta_INGEST = 1
SEXBABY = 0
age_deb = 0
age_fin = 0
nb_cig = 0
smoke_Pb = 1

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

// Lead parameters
PK    = 50
PW    = 50
PL    = 50
PP    = 2
LEAD  = 15000     // Fractional clearance of lead from plasma into forming bone (L plasma cleared/L bone formed)
BIND  = 2700      // Maximum capacity of sites in red cells to bind lead (µg/L of red cells volume)
KBIND = 7.5       // Half-saturation concentration of lead for binding by sites in red cells (µg/L of red cells volume)
G     = 1.2       // Linear parameter for unbound lead in red cells
CONST  = 0.65

// Bone parameters
D0 = 5e-07
R0 = 5e-07
P0 = 0.02
PI = 3.141592
VEXPO = 1.188
WEXPO = 1.21
CKLOSS = 0.004/365
TKLOSS = 0.006/365


S  = 0.000126	// Surface area of canalicule (cm2/cm canalicule length)

//// Radii of the eight shells of bone in the canalicular (diffusion) region of deeper bone (cm)
RAD1 = 0.000027
RAD2 = 0.000052
RAD3 = 0.000079
RAD4 = 0.000106
RAD5 = 0.000133
RAD6 = 0.000150
RAD7 = 0.000187
RAD8 = 0.000214


$MAIN
// Sexe
//int SEXBABY = sexbaby[round(INDICE-1)];

/// Evolution in time
double year = TIME/365;
double yest = (TIME-1)/365;

//// modification de l exposition en fonction de l age (ug/kg/sem)
// Trajectoire d exposition
//Rcpp::NumericVector traj_expo = f_expo(INDICE);

//double expo=traj_expo[TIME];


double L = 1000/(PI*pow(RAD8,2));
//// Surface areas of the eight shells of bone in the canalicular (diffusion) region of deeper bone(cm2/cm canalicule length)
double S1 = 2*PI*RAD1;
double S2 = 2*PI*RAD2;
double S3 = 2*PI*RAD3;
double S4 = 2*PI*RAD4;
double S5 = 2*PI*RAD5;
double S6 = 2*PI*RAD6;
double S7 = 2*PI*RAD7;
double S8 = 2*PI*RAD8;

//// Volumes of the eight shells of bone in the canalicular (diffusion) region of deeper bone (cm3/cm canalicule length)
double V1 = PI*pow(RAD1,2);
double V2 = PI*pow(RAD2,2)-V1;
double V3 = PI*pow(RAD3,2)-(V1+V2);
double V4 = PI*pow(RAD4,2)-(V1+V2+V3);
double V5 = PI*pow(RAD5,2)-(V1+V2+V3+V4);
double V6 = PI*pow(RAD6,2)-(V1+V2+V3+V4+V5);
double V7 = PI*pow(RAD7,2)-(V1+V2+V3+V4+V5+V6);
double V8 = PI*pow(RAD8,2)-(V1+V2+V3+V4+V5+V6+V7);

//Body weight (kg)
double wbw = 0;
if(SEXBABY == 1){wbw = (3.938425 + 0.7518199*year*12 - 0.02023793*pow(year*12,2) + 0.0002921682*pow(year*12,3) - 2.06762e-06*pow(year*12,4) + 8.469e-09*pow(year*12,5) - 2.188427e-11*pow(year*12,6) + 3.699776e-14*pow(year*12,7) - 4.099077e-17*pow(year*12,8) + 2.874804e-20*pow(year*12,9) - 1.159732e-23*pow(year*12,10) + 2.052602e-27*pow(year*12,11));}
else{wbw = (3.932403 + 0.6866462*year*12 - 0.01949911*pow(year*12,2) + 0.00031311*pow(year*12,3) - 2.466654e-06*pow(year*12,4) + 1.113217e-08*pow(year*12,5) - 3.131402e-11*pow(year*12,6) + 5.693737e-14*pow(year*12,7) - 6.706947e-17*pow(year*12,8) + 4.947858e-20*pow(year*12,9) - 2.079251e-23*pow(year*12,10) + 3.800367e-27*pow(year*12,11));}
double wbw0 = 0;
if(SEXBABY == 1){wbw0 = (3.938425 + 0.7518199*yest*12 - 0.02023793*pow(yest*12,2) + 0.0002921682*pow(yest*12,3) - 2.06762e-06*pow(yest*12,4) + 8.469e-09*pow(yest*12,5) - 2.188427e-11*pow(yest*12,6) + 3.699776e-14*pow(yest*12,7) - 4.099077e-17*pow(yest*12,8) + 2.874804e-20*pow(yest*12,9) - 1.159732e-23*pow(yest*12,10) + 2.052602e-27*pow(yest*12,11));}
else{wbw0 = (3.932403 + 0.6866462*yest*12 - 0.01949911*pow(yest*12,2) + 0.00031311*pow(yest*12,3) - 2.466654e-06*pow(yest*12,4) + 1.113217e-08*pow(yest*12,5) - 3.131402e-11*pow(yest*12,6) + 5.693737e-14*pow(yest*12,7) - 6.706947e-17*pow(yest*12,8) + 4.947858e-20*pow(yest*12,9) - 2.079251e-23*pow(yest*12,10) + 3.800367e-27*pow(yest*12,11));}
double wbwa = 0;
if(SEXBABY == 1){wbwa = (3.938425 + 0.7518199*25*12 - 0.02023793*pow(25*12,2) + 0.0002921682*pow(25*12,3) - 2.06762e-06*pow(25*12,4) + 8.469e-09*pow(25*12,5) - 2.188427e-11*pow(25*12,6) + 3.699776e-14*pow(25*12,7) - 4.099077e-17*pow(25*12,8) + 2.874804e-20*pow(25*12,9) - 1.159732e-23*pow(25*12,10) + 2.052602e-27*pow(25*12,11));}
else{wbwa = (3.932403 + 0.6866462*25*12 - 0.01949911*pow(25*12,2) + 0.00031311*pow(25*12,3) - 2.466654e-06*pow(25*12,4) + 1.113217e-08*pow(25*12,5) - 3.131402e-11*pow(25*12,6) + 5.693737e-14*pow(25*12,7) - 6.706947e-17*pow(25*12,8) + 4.947858e-20*pow(25*12,9) - 2.079251e-23*pow(25*12,10) + 3.800367e-27*pow(25*12,11));}
double WBODYCMAX = 0;
if(SEXBABY == 1){WBODYCMAX = (3.938425 + 0.7518199*35*12 - 0.02023793*pow(35*12,2) + 0.0002921682*pow(35*12,3) - 2.06762e-06*pow(35*12,4) + 8.469e-09*pow(35*12,5) - 2.188427e-11*pow(35*12,6) + 3.699776e-14*pow(35*12,7) - 4.099077e-17*pow(35*12,8) + 2.874804e-20*pow(35*12,9) - 1.159732e-23*pow(35*12,10) + 2.052602e-27*pow(35*12,11));}
else{WBODYCMAX = (3.932403 + 0.6866462*35*12 - 0.01949911*pow(35*12,2) + 0.00031311*pow(35*12,3) - 2.466654e-06*pow(35*12,4) + 1.113217e-08*pow(35*12,5) - 3.131402e-11*pow(35*12,6) + 5.693737e-14*pow(35*12,7) - 6.706947e-17*pow(35*12,8) + 4.947858e-20*pow(35*12,9) - 2.079251e-23*pow(35*12,10) + 3.800367e-27*pow(35*12,11));}
double WBODYTMAX = 0;
if(SEXBABY == 1){WBODYTMAX = (3.938425 + 0.7518199*30*12 - 0.02023793*pow(30*12,2) + 0.0002921682*pow(30*12,3) - 2.06762e-06*pow(30*12,4) + 8.469e-09*pow(30*12,5) - 2.188427e-11*pow(30*12,6) + 3.699776e-14*pow(30*12,7) - 4.099077e-17*pow(30*12,8) + 2.874804e-20*pow(30*12,9) - 1.159732e-23*pow(30*12,10) + 2.052602e-27*pow(30*12,11));}
else{WBODYTMAX = (3.932403 + 0.6866462*30*12 - 0.01949911*pow(30*12,2) + 0.00031311*pow(30*12,3) - 2.466654e-06*pow(30*12,4) + 1.113217e-08*pow(30*12,5) - 3.131402e-11*pow(30*12,6) + 5.693737e-14*pow(30*12,7) - 6.706947e-17*pow(30*12,8) + 4.947858e-20*pow(30*12,9) - 2.079251e-23*pow(30*12,10) + 3.800367e-27*pow(30*12,11));}

//Physiological development
/// Skeleton (Marrow / Bones = 80 % Cortical + 20% Trabecular)
// Pearce et al. (2017) - Httk
// Total Body Bone Mineral Content
double TBBMC = 0;
if(SEXBABY == 1){
  if(year < 50){TBBMC = 0.89983 + ((2.9901 - 0.89989)/(1 + 1 * exp((14.17081 - year)/1.58179)));}
  else{TBBMC =  0.99483 + ((2.9901-0.89989)/(1+1*exp((14.17081 - year)/1.58179))) - (0.0019*year);}
}
else{
  if(year < 50){TBBMC = 0.89983 + ((2.9901 - 0.89989)/(1 + 1 * exp((14.17081 - year)/1.58179)));}
  else{TBBMC = 1.86070 + ((2.14976 - 0.74042)/(1 + 1 * exp((12.35466 - year)/1.35750))) - (0.0056 * year);}
}

double TBBMC_25 = 0;
if(SEXBABY == 1){TBBMC_25 = 0.89983 + ((2.9901 - 0.89989)/(1 + 1 * exp((14.17081 - 25)/1.58179)));}
else{TBBMC_25 = 0.89983 + ((2.9901 - 0.89989)/(1 + 1 * exp((14.17081 - 25)/1.58179)));}

double TBBMC_30 = 0;
if(SEXBABY == 1){TBBMC_30 = 0.89983 + ((2.9901 - 0.89989)/(1 + 1 * exp((14.17081 - 30)/1.58179)));}
else{TBBMC_30 = 0.89983 + ((2.9901 - 0.89989)/(1 + 1 * exp((14.17081 - 30)/1.58179)));}

double TBBMC_35 = 0;
if(SEXBABY == 1){TBBMC_35 = 0.89983 + ((2.9901 - 0.89989)/(1 + 1 * exp((14.17081 - 35)/1.58179)));}
else{TBBMC_35 = 0.89983 + ((2.9901 - 0.89989)/(1 + 1 * exp((14.17081 - 35)/1.58179)));}

double TBBMC_yest = 0;
if(SEXBABY == 1){
  if(yest < 50){TBBMC_yest = 0.89983 + ((2.9901 - 0.89989)/(1 + 1 * exp((14.17081 - yest)/1.58179)));}
  else{TBBMC_yest =  0.99483 + ((2.9901-0.89989)/(1+1*exp((14.17081 - yest)/1.58179))) - (0.0019*yest);}
}
else{
  if(yest < 50){TBBMC_yest = 0.89983 + ((2.9901 - 0.89989)/(1 + 1 * exp((14.17081 - yest)/1.58179)));}
  else{TBBMC_yest = 1.86070 + ((2.14976 - 0.74042)/(1 + 1 * exp((12.35466 - yest)/1.35750))) - (0.0056 * yest);}
}

double vbone = (TBBMC/0.65)/0.5 * (Delta_Bone_M * SEXBABY + (1 - SEXBABY) * Delta_Bone_F);

double vbone_25 = (TBBMC_25/0.65)/0.5 * (Delta_Bone_M * SEXBABY + (1 - SEXBABY) * Delta_Bone_F);

double vbone_30 = (TBBMC_30/0.65)/0.5 * (Delta_Bone_M * SEXBABY + (1 - SEXBABY) * Delta_Bone_F);

double vbone_35 = (TBBMC_35/0.65)/0.5 * (Delta_Bone_M * SEXBABY + (1 - SEXBABY) * Delta_Bone_F);

double vbone_yest = (TBBMC_yest/0.65)/0.5 * (Delta_Bone_M * SEXBABY + (1 - SEXBABY) * Delta_Bone_F);



double VBONEX     = vbone;
double CVBONEX    = 0.8*vbone;
double TVBONEX    = 0.2*vbone;
double VBONECMAX  = vbone_35;
double CVBONECMAX = 0.8*VBONECMAX;
double VBONETMAX  = vbone_30;
double TVBONETMAX = 0.2*VBONETMAX;

double WBONEX     = CWBONEX + TWBONEX;
double CWBONEX    = 0.8*1.99*VBONEX;
double TWBONEX    = 0.2*1.92*WBONEX;


double DBONE = WBONEX/VBONEX;
double P = P0/DBONE;	// Permeability constant for diffusion across the canalicule-bone interface from canalicule to bone (cm/day/0.5e-4 cm)
double Rad = R0/DBONE;	// Permeability constant for diffusion across the canalicule-bone interface from  bone to canalicule (cm/day/0.5e-4 cm)
double D = D0/DBONE;	// Permeability constant for diffusion within bone (cm/day/0.5e-4 cm)

/// Body and bone growth
double RVBONEX = vbone - vbone_yest;

double BASE  = 0.1/365;
double PART1 = 4*exp(-0.4*year);
double PART2 = 4*sin((1.57/10)*year);
double FBFR  = 1;                                           // Fractional bone formation rate to TMAXAGE (day-1)
if(year <= 11){FBFR = (BASE + PART1 + PART2)/365;}
else{FBFR = (BASE + (4*exp(-11*0.4)+4*sin((1.57/10)*11))*exp(-0.6*(year-11)))/365;}
double FBFRCMAX = 1;                                        // Fractional bone formation rate at CMAXAGE (day-1)
if(year <= 11){FBFRCMAX = (BASE + PART1 + PART2)/365;}
else{FBFRCMAX = (BASE + (4*exp(-11*0.4)+4*sin((1.57/10)*11))*exp(-0.6*(35-11)))/365;}
double FBFRTMAX = 1;					// Fractional bone formation rate at TMAXAGE (day-1)
if(year <= 11){FBFRTMAX = (BASE + PART1 + PART2)/365;}
else{FBFRTMAX = (BASE + (4*exp(-11*0.4)+4*sin((1.57/10)*11))*exp(-0.6*(30-11)))/365;}


double CVBONEO = CVBONECMAX*(1+CVB);                          // Cortical bone volume after CMAXAGE (L)
double CVBONE = 1;                                            // Cortical bone volume at any age (L)
if(year <= 35){CVBONE = CVBONEX;}
else{CVBONE = CVBONEO;}
double TVBONEO = TVBONETMAX*(1+TVB);
double TVBONE = 1;
if(year <= 30){TVBONE = TVBONEX;}
else{TVBONE = TVBONEO;}
double VBONE = CVBONE + TVBONE;                               // Total bone volume at any age (L)

double BFR = 1;                                               // Total bone formation rate (by volume) at any age (L/day)
if(year <= 30){BFR = FBFR*VBONEX;}
else{BFR = FBFR*VBONETMAX;}
double TBFR    = CONST*BFR;                                   // Trabecular bone formation rate (by volume) at any age (L/day)
double CBFR    = BFR - TBFR;                                  // Cortical bone formation rate (by volume) at any age (L/day)
double BFRCMAX = (1-CONST)*FBFRCMAX*VBONECMAX;                // Cortical bone formation rate at CMAXAGE (L/day)
double BFRTMAX = CONST*FBFRTMAX*VBONETMAX;		                // Trabecular bone formation rate at TMAXAGE (L/day)
double CFBFR   = CBFR/CVBONE;                                 // Fractional cortical bone formation rate at any age (day-1)
double TFBFR   = TBFR/TVBONE;                                 // Fractional trabecular bone formation rate at any age (day-1)

double BRRX  = BFR - RVBONEX;                                 // Total bone resorption rate to TMAXAGE (L/day)
double TBRRX = CONST*BRRX;                                    // Trabecular bone resorption rate to TMAXAGE (L/day)
double CBRRX = BRRX - TBRRX;                                  // Cortical bone resorption rate to TMAXAGE (L/day)
double BRRCMAX = BFRCMAX - (1-CONST)*RVBONEX;                 // Cortical bone resorption rate at CMAXAGE (L/day)
double BRRTMAX = BFRTMAX - CONST*RVBONEX;		                  // Trabecular bone resorption rate at TMAXAGE (L/day)

double PERIMENO = 0;
if(SEXBABY == 1){PERIMENO = 0;}
else{if(year >= 55 && year <= 65){PERIMENO = (0.01 + 0.01*sin(0.3*(year-55)))/365;}
  else{PERIMENO = 0;}}
double CBRRO = 1;                                               // Cortical bone resorption rate after CMAXAGE (L/day)
if(year >= 35){CBRRO = BRRCMAX + CKLOSS*CVBONE + PERIMENO;}
else{CBRRO = 0;}
double TBRRO = 1;					                                      // Trabecular bone resorption rate after TMAXAGE (L/day)
if(year >= 30){TBRRO = BRRTMAX + TKLOSS*TVBONE + PERIMENO;}
else{TBRRO = 0;}
double CBRR = 1;                                                // Cortical bone resorption rate at any age (L/day)
if(year < 35){CBRR = CBRRX;}
else{CBRR = CBRRO;}
double TBRR = 1;					                                      // Trabecular bone resorption rate at any age (L/day)
if(year < 30){TBRR = TBRRX;}
else{TBRR = TBRRO;}

double TRABECULAR = 1;					            // Rate of change of trabecular bone volume after TMAXAGE (L/day)
if(year < 30){TRABECULAR = 0;}
else{TRABECULAR = TBFR - TBRRO;}
double CORTICAL = 1;                        // Rate of change of cortical bone volume after CMAXAGE (L/day)
if(year < 35){CORTICAL = 0;}
else{CORTICAL = CBFR - CBRRO;}

double VBAGEA  = vbone_25;    // Total bone volume at age AGEA (L)
double CVBAGEA = 0.8*VBAGEA;                // Cortical bone volume at AGEA (L)
double TVBAGEA = VBAGEA - CVBAGEA;			    // Trabecular bone volume at AGEA (L)
double BFAGEA  = FBFR*VBAGEA;               // Total bone formation rate (by volume) at age AGEA (L/day)
double TBFAGEA = TFBFR*TVBAGEA;             // Trabecular bone formation rate (by volume) at AGEA (L/day)
double CBFAGEA = BFAGEA - TBFAGEA;          // Cortical bone formation rate (by volume) at AGEA (L/day)

// Bone growth considering age
double CVBONEA = 1.0;                                         // Cortical bone volume participating in adult-type bone remodeling (L)
double CVBONEC = 1.0;                                         // Cortical bone volume participating in active bone modeling during growth (L)
double CRFBON  = 1.0;                                         // Fractional rate of transfer of lead from BONEC to BONEA (day-1)
double CBFRA   = 1.0;                                         // Formation rate of cortical bone participating in adult-type bone remodeling (by volume of bone) (L/day)
double CBFRC   = 1.0;                                         // Formation rate of cortical bone participating in active modeling during growth (by volume of bone) (L/day)
double CBRRC   = 1.0;                                         // Resorption rate of cortical bone participating in active modeling during growth (by volume of bone) (L/day) 
double CBRRA   = 1.0;                                         // Resorption rate of cortical bone participating in adult-type remodeling (by volume of bone) (L/day)
double TVBONEC = 1.0;					// Trabecular bone volume participating in active bone modeling during growth (L)
double TVBONEA = 1.0;					// Trabecular bone volume participating in adult-type bone remodeling (L)
double TRFBON  = 1.0;					// BONEA (day-1)
double TBFRA   = 1.0;					// Formation rate of trabecular bone participating in adult-type bone remodeling (by volume of bone) (L/day)
double TBFRC   = 1.0;					// Formation rate of trabecular bone participating in active modeling during growth (by volume of bone) (L/day)
double TBRRC   = 1.0;					// Resorption rate of trabecular bone participating in active modeling during growth (by volume of bone) (L/day)
double TBRRA   = 1.0;					// Resorption rate of trabecular bone participating in adult-type bone remodeling (by volume of bone) (L/day)
if(year == 0){
  CVBONEC = CVBONEX;
  CVBONEA = 0;
  CRFBON = 0;
  CBFRA = CBFAGEA*CVBONEA/CVBONEX;
  CBFRC = CBFR - CBFRA;
  CBRRC = CBRR*CBFRC/CBFR;
  CBRRA = CBRR - CBRRC;
  
  TVBONEC = TVBONEX;
  TVBONEA = 0;
  TRFBON = 0;
  TBFRA = TBFAGEA*TVBONEA/TVBONEX;
  TBFRC = TBFR - TBFRA;
  TBRRC = TBRR*TBFRC/TBFR;
  TBRRA = TBRR - TBRRC;
}
else if(year >0  && year < 25){
  CVBONEA = CVBONEX*pow((year/25),0.6);
  CVBONEC = CVBONEX - CVBONEA;
  CRFBON  = (CVBONEX*0.6/25)*pow(((1e-33)+year/25),(0.6-1)) + 0.8*RVBONEX;
  CBFRA   = CBFR*CVBONEA/CVBAGEA;
  CBFRC   = 1;
  if(CBFRA <= CBFR){CBFRC = CBFR - CBFRA;}
  else{CBFRC = 0;}
  CBRRC   = CBRR*CBFRC/CBFR;
  CBRRA   = CBRR - CBRRC;
  
  TVBONEA = TVBONEX*pow((year/25),0.6);
  TVBONEC = TVBONEX - TVBONEA;
  TRFBON = (TVBONEX*0.6/25)*pow(((1e-33)+year/25),(0.6-1)) + 0.2*RVBONEX;
  TBFRA   = TBFR*TVBONEA/TVBAGEA;
  if(TBFRA <= TBFR){TBFRC = TBFR - TBFRA;}
  else{TBFRC = 0;}
  TBRRC   = TBRR*TBFRC/TBFR;
  TBRRA   = TBRR - TBRRC;
}
else{ 
  CVBONEA = CVBONEX;
  CVBONEC = 0;
  CRFBON  = 0;
  CBFRA   = CBFR;
  CBFRC   = CBFR - CBFRA;
  CBRRC   = CBRR*CBFRC/CBFR;
  CBRRA   = CBRR;
  
  TVBONEC = 0;
  TVBONEA = TVBONEX;
  TRFBON = 0;
  TBFRA   = TBFR;
  TBFRC   = TBFR - TBFRA;
  TBRRC   = TBRR*TBFRC/TBFR;
  TBRRA   = TBRR;
}

/// Volume of organs (L)

// Hematocrit (proportion of RBC in blood)
//Mallick et al. (2020) or Pendse et al. (2020)
double HCT = 0;
if(year <= 2){HCT = 0.359;}
if(year > 2 && year < 18){HCT = 1.12815e-06 * pow(year,3) - 1.72362e-04 * pow(year,2) + 8.15264e-03 * year + 0.34337590;}
else{HCT = 1.12815e-06 * pow(18,3) - 1.72362e-04 * pow(18,2) + 8.15264e-03 * 18 + 0.34337590;}

//// Volume of organs

// Fat (non essential adipose tissues) (in L)
// Deepika et al. (2021)
double vfat = 0;
if(SEXBABY == 1){vfat = (1.3054356 + 0.3622685 * year - 0.0025165 * pow(year,2) + 0.0906119 * wbw + 0.0001731 * pow(wbw,2)) * Delta_Adipose_M;}
else{vfat = (6.132e-01 + 8.475e-02 * year + 8.151e-05 * pow(year,2) + 1.341e-01 * wbw + 2.297e-03 * pow(wbw,2)) * Delta_Adipose_F;}

double vfat_30 = 0;
if(SEXBABY == 1){vfat_30 = (1.3054356 + 0.3622685 * 30 - 0.0025165 * pow(30,2) + 0.0906119 * WBODYTMAX + 0.0001731 * pow(WBODYTMAX,2)) * Delta_Adipose_M;}
else{vfat_30 = (6.132e-01 + 8.475e-02 * 30 + 8.151e-05 * pow(30,2) + 1.341e-01 * WBODYTMAX + 2.297e-03 * pow(WBODYTMAX,2)) * Delta_Adipose_F;}

double vfat_35 = 0;
if(SEXBABY == 1){vfat_35 = (1.3054356 + 0.3622685 * 35 - 0.0025165 * pow(35,2) + 0.0906119 * WBODYCMAX + 0.0001731 * pow(WBODYCMAX,2)) * Delta_Adipose_M;}
else{vfat_35 = (6.132e-01 + 8.475e-02 * 35 + 8.151e-05 * pow(35,2) + 1.341e-01 * WBODYCMAX + 2.297e-03 * pow(WBODYCMAX,2)) * Delta_Adipose_F;}

double vfat_yest = 0;
if(SEXBABY == 1){vfat_yest = (1.3054356 + 0.3622685 * yest - 0.0025165 * pow(yest,2) + 0.0906119 * wbw0 + 0.0001731 * pow(wbw0,2)) * Delta_Adipose_M;}
else{vfat_yest = (6.132e-01 + 8.475e-02 * yest + 8.151e-05 * pow(yest,2) + 1.341e-01 * wbw0 + 2.297e-03 * pow(wbw0,2)) * Delta_Adipose_F;}

// Brain (in L)
// Beaudouin et al. 2010
double vbr = 0;
if(SEXBABY == 1){vbr = (1.45 + (0.350 - 1.45) * exp(-0.440 * year)) * Delta_Brain_M;}
else{vbr = (1.30 + (0.347 - 1.30)*exp(-0.573 * year)) * Delta_Brain_F;}

double vbr_30 = 0;
if(SEXBABY == 1){vbr_30 = (1.45 + (0.350 - 1.45) * exp(-0.440 * 30)) * Delta_Brain_M;}
else{vbr_30 = (1.30 + (0.347 - 1.30)*exp(-0.573 * 30)) * Delta_Brain_F;}

double vbr_35 = 0;
if(SEXBABY == 1){vbr_35 = (1.45 + (0.350 - 1.45) * exp(-0.440 * 35)) * Delta_Brain_M;}
else{vbr_35 = (1.30 + (0.347 - 1.30)*exp(-0.573 * 35)) * Delta_Brain_F;}

double vbr_yest = 0;
if(SEXBABY == 1){vbr_yest = (1.45 + (0.350 - 1.45) * exp(-0.440 * yest)) * Delta_Brain_M;}
else{vbr_yest = (1.30 + (0.347 - 1.30)*exp(-0.573 * yest)) * Delta_Brain_F;}

// Kidneys
// Beaudouin et al. 2010
double vk = 0;
if(SEXBABY == 1){vk = (0.0042+(0.00767 - 0.0042) * exp(-0.206 * year)) * wbw * Delta_Kidney_M;}
else{vk = (0.0046 + (0.00709 - 0.0046) * exp(-0.221 * year)) * wbw * Delta_Kidney_F;}

double vk_30 = 0;
if(SEXBABY == 1){vk_30 = (0.0042+(0.00767 - 0.0042) * exp(-0.206 * 30)) * WBODYTMAX * Delta_Kidney_M;}
else{vk_30 = (0.0046 + (0.00709 - 0.0046) * exp(-0.221 * 30)) * WBODYTMAX * Delta_Kidney_F;}

double vk_35 = 0;
if(SEXBABY == 1){vk_35 = (0.0042+(0.00767 - 0.0042) * exp(-0.206 * 35)) * WBODYCMAX * Delta_Kidney_M;}
else{vk_35 = (0.0046 + (0.00709 - 0.0046) * exp(-0.221 * 35)) * WBODYCMAX * Delta_Kidney_F;}

double vk_yest = 0;
if(SEXBABY == 1){vk_yest = (0.0042+(0.00767 - 0.0042) * exp(-0.206 * yest)) * wbw0 * Delta_Kidney_M;}
else{vk_35 = (0.0046 + (0.00709 - 0.0046) * exp(-0.221 * yest)) * wbw0 * Delta_Kidney_F;}

// Hair
double vhair = 0.002*wbw;

double vhair_30 = 0.002*WBODYTMAX;

double vhair_35 = 0.002*WBODYCMAX;

double vhair_yest = 0.002*wbw0;

// Liver
// Beaudouin et al. 2010
double vl = 0;
if(SEXBABY == 1){vl = (0.0247 + (0.0409 - 0.0247) * exp(-0.218 * year)) * wbw * Delta_Liver_M;}
else{vl = (0.0233 + (0.038 - 0.0233) * exp(-0.122 * year)) * wbw * Delta_Liver_F;}

double vl_30 = 0;
if(SEXBABY == 1){vl_30 = (0.0247 + (0.0409 - 0.0247) * exp(-0.218 * 30)) * WBODYTMAX * Delta_Liver_M;}
else{vl_30 = (0.0233 + (0.038 - 0.0233) * exp(-0.122 * 30)) * WBODYTMAX * Delta_Liver_F;}

double vl_35 = 0;
if(SEXBABY == 1){vl_35 = (0.0247 + (0.0409 - 0.0247) * exp(-0.218 * 35)) * WBODYCMAX * Delta_Liver_M;}
else{vl_35 = (0.0233 + (0.038 - 0.0233) * exp(-0.122 * 35)) * WBODYCMAX * Delta_Liver_F;}

double vl_yest = 0;
if(SEXBABY == 1){vl_yest = (0.0247 + (0.0409 - 0.0247) * exp(-0.218 * yest)) * wbw0 * Delta_Liver_M;}
else{vl_yest = (0.0233 + (0.038 - 0.0233) * exp(-0.122 * yest)) * wbw0 * Delta_Liver_F;}

/// BLOOD (composition = (1-HCT)% plasma + HCT% RBC)(in L)
// Beaudouin et al. (2010)
double vb = 0;
if(SEXBABY == 1){
  if(year < 1.4){vb = (-0.0273 * pow(year,2) + 0.0771 * year) * wbw;}
  else{vb = (0.0761 + (0.0289 - 0.0761) * exp(-0.592 * year)) * wbw;}
}
else{
  if(year < 1){vb = (-0.0273 * pow(year,2) + 0.0771 * year) * wbw;}
  if(year >=1 && year < 20){vb = (3.28e-05 * pow(year,3) - 1.21e-03 * pow(year,2) + 1.24e-02 * year + 3.86e-02) * wbw;}
  else{vb = 0.065 * wbw;}
}

double vb_30 = 0;
if(SEXBABY == 1){vb_30 = (0.0761 + (0.0289 - 0.0761) * exp(-0.592 * 30))  * WBODYTMAX;}
else{vb_30 = 0.065 * WBODYTMAX;}

double vb_35 = 0;
if(SEXBABY == 1){vb_35 = (0.0761 + (0.0289 - 0.0761) * exp(-0.592 * 35))  * WBODYCMAX;}
else{vb_35 = 0.065 * WBODYCMAX;}

double vb_yest = 0;
if(SEXBABY == 1){
  if(yest < 1){vb_yest = (-0.027 * pow(yest,2) + 0.077 * yest) * wbw0;}
  else{vb_yest = (0.0761 + (0.0289 - 0.0761) * exp(-0.592 * yest))  * wbw0;}
}
else{
  if(yest < 1){vb_yest = (-0.0273 * pow(yest,2) + 0.0771 * yest) * wbw0;}
  if(yest >=1 && yest < 14.019723){vb_yest = (3.28e-05 * pow(yest,3) - 1.21e-03 * pow(yest,2) + 1.24e-02 * yest + 3.86e-02) * wbw0;}
  else{vb_yest = 0.065 * wbw0;}
}

double vp = (1-HCT)*vb;
double vrbc = HCT*vb;

// Slowly perfused tissue (Heart, Diaphragm, muscles, skin, bones)
// Beaudouin et al. 2010
double vheart = 0;
if(SEXBABY == 1){vheart = 0.0045 * wbw * Delta_Heart_M;}
else{vheart = 0.004167 * wbw * Delta_Heart_F;}

double vheart_30 = 0;
if(SEXBABY == 1){vheart_30 = 0.0045 * WBODYTMAX * Delta_Heart_M;}
else{vheart_30 = 0.004167 * WBODYTMAX * Delta_Heart_F;}

double vheart_35 = 0;
if(SEXBABY == 1){vheart_35 = 0.0045 * WBODYCMAX * Delta_Heart_M;}
else{vheart_35 = 0.004167 * WBODYCMAX * Delta_Heart_F;}

double vheart_yest = 0;
if(SEXBABY == 1){vheart_yest = 0.0045 * wbw0 * Delta_Heart_M;}
else{vheart_yest = 0.004167 * wbw0 * Delta_Heart_F;}

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

double vm_30 = 0;
if(SEXBABY == 1){vm_30 = (0.3973 + (0.201 - 0.3973) * exp(-0.141 * 30)) * (-0.0001264 * pow(30,2) + 0.006131 * 30 + 0.926) * WBODYTMAX * Delta_Muscle_M;}
else{vm_30 = (0.2917 + (0.207 - 0.2917) * exp(-0.339 * 30)) * (-0.0001264 * pow(30,2) + 0.006131 * 30 + 0.926) * WBODYTMAX * Delta_Muscle_F;}

double vm_35 = 0;
if(SEXBABY == 1){vm_35 = (0.3973 + (0.201 - 0.3973) * exp(-0.141 * 35)) * (-0.0001264 * pow(35,2) + 0.006131 * 35 + 0.926) * WBODYCMAX * Delta_Muscle_M;}
else{vm_35 = (0.2917 + (0.207 - 0.2917) * exp(-0.339 * 35)) * (-0.0001264 * pow(35,2) + 0.006131 * 35 + 0.926) * WBODYCMAX * Delta_Muscle_F;}

double vm_yest = 0;
if(SEXBABY == 1){
  if(yest < 24.3){vm_yest = (0.3973 + (0.201 - 0.3973) * exp(-0.141 * yest)) * wbw0 * Delta_Muscle_M;}
  else{vm_yest = (0.3973 + (0.201 - 0.3973) * exp(-0.141 * yest)) * (-0.0001264 * pow(yest,2) + 0.006131 * yest + 0.926) * wbw0 * Delta_Muscle_M;}
}
else{
  if(yest <= 25.90709){vm_yest = (0.2917 + (0.207 - 0.2917) * exp(-0.339 * yest))* wbw0 * Delta_Muscle_F;}
  else{vm_yest = (0.2917 + (0.207 - 0.2917) * exp(-0.339 * yest)) * (-0.0001264 * pow(yest,2) + 0.006131 * yest + 0.926) * wbw0 * Delta_Muscle_F;}
}

/// Diaphragm
double vd = (3e-04)*wbw;

double vd_30 = (3e-04)*WBODYTMAX;

double vd_35 = (3e-04)*WBODYCMAX;

double vd_yest = (3e-04)*wbw0;

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

double vs_30 = 0;
if(SEXBABY == 1){vs_30 = 0.0452 * WBODYTMAX;}
else{vs_30 = 0.0383 * WBODYTMAX;}

double vs_35 = 0;
if(SEXBABY == 1){vs_35 = 0.0452 * WBODYCMAX;}
else{vs_35 = 0.0383 * WBODYCMAX;}

double vs_yest = 0;
if(SEXBABY == 1){
  if(yest < 20){vs_yest = (-1.171e-05 * pow(yest,3) + 5.413e-04 * pow(yest,2) -  6.1966e-03 * yest + 4.623e-02) * wbw0;}
  else{vs_yest = 0.0452 * wbw0;}
}
else{
  if(yest < 20){vs_yest = (-7.8882e-06 * pow(yest,3) + 4.0224e-04 * pow(yest,2) - 5.2146e-03 * yest + 4.5605e-02) * wbw0;}
  else{vs_yest = 0.0383 * wbw0;}
}

/// Bone marrow (71 % Red Marrow + 29% Yellow Marrow)
// Beaudouin et al. (2010)
double vmarr = 0;
if(SEXBABY == 1){vmarr = (0.05 + (0.0138 - 0.05) * exp(-0.112 * year)) * wbw;}
else{vmarr = (0.045 + (0.0138 - 0.045)*exp(-0.136 * year)) * wbw;}

double vmarr_30 = 0;
if(SEXBABY == 1){vmarr_30 = (0.05 + (0.0138 - 0.05) * exp(-0.112 * 30)) * WBODYTMAX;}
else{vmarr_30 = (0.045 + (0.0138 - 0.045)*exp(-0.136 * 30)) * WBODYTMAX;}

double vmarr_35 = 0;
if(SEXBABY == 1){vmarr_35 = (0.05 + (0.0138 - 0.05) * exp(-0.112 * 35)) * WBODYCMAX;}
else{vmarr_35 = (0.045 + (0.0138 - 0.045)*exp(-0.136 * 35)) * WBODYCMAX;}

double vmarr_yest = 0;
if(SEXBABY == 1){vmarr_yest = (0.05 + (0.0138 - 0.05) * exp(-0.112 * yest)) * wbw0;}
else{vmarr_yest = (0.045 + (0.0138 - 0.045)*exp(-0.136 * yest)) * wbw0;}

// Rapidly perfused tissue (Breast + Thyroid + Spleen + Pancreas + Adrenals + Gonads + Lungs)

// Breast (Beaudouin et al. (2010))
double vbreast = 1;
if(SEXBABY == 1){vbreast = (3.42e-4 * (1 / (1 + exp(-1.42 * year + 20.1)))) * wbw;}
else{vbreast = (0.00833 * 1/(1 + exp(-1.92 * year + 28.6))) * wbw;}

double vbreast_30 = 1;
if(SEXBABY == 1){vbreast_30 = (3.42e-4 * (1 / (1 + exp(-1.42 * 30 + 20.1)))) * WBODYTMAX;}
else{vbreast_30 = (0.00833 * 1/(1 + exp(-1.92 * 30 + 28.6))) * WBODYTMAX;}

double vbreast_35 = 1;
if(SEXBABY == 1){vbreast_35 = (3.42e-4 * (1 / (1 + exp(-1.42 * 35 + 20.1)))) * WBODYCMAX;}
else{vbreast_35 = (0.00833 * 1/(1 + exp(-1.92 * 35 + 28.6))) * WBODYCMAX;}

double vbreast_yest = 1;
if(SEXBABY == 1){vbreast_yest = (3.42e-4 * (1 / (1 + exp(-1.42 * yest + 20.1)))) * wbw0;}
else{vbreast_yest = (0.00833 * 1/(1 + exp(-1.92 * yest + 28.6))) * wbw0;}

// Thyroid
// Beaudouin et al. (2010)
double vthyr = 0;
if(SEXBABY == 1){vthyr = 0.000274 * wbw;}
else{vthyr = 0.0002833 * wbw;}

double vthyr_30 = 0;
if(SEXBABY == 1){vthyr_30 = 0.000274 * WBODYTMAX;}
else{vthyr_30 = 0.0002833 * WBODYTMAX;}

double vthyr_35 = 0;
if(SEXBABY == 1){vthyr_35 = 0.000274 * WBODYCMAX;}
else{vthyr_35 = 0.0002833 * WBODYCMAX;}

double vthyr_yest = 0;
if(SEXBABY == 1){vthyr_yest = 0.000274 * wbw0;}
else{vthyr_yest = 0.0002833 * wbw0;}

// Spleen
// Beaudouin et al. (2010)
double vspleen = 0;
if(SEXBABY == 1){vspleen = 0.0021 * wbw * Delta_Spleen_M;}
else{vspleen = 0.0022 * wbw * Delta_Spleen_F;}

double vspleen_30 = 0;
if(SEXBABY == 1){vspleen_30 = 0.0021 * WBODYTMAX * Delta_Spleen_M;}
else{vspleen_30 = 0.0022 * WBODYTMAX * Delta_Spleen_F;}

double vspleen_35 = 0;
if(SEXBABY == 1){vspleen_35 = 0.0021 * WBODYCMAX * Delta_Spleen_M;}
else{vspleen_35 = 0.0022 * WBODYCMAX * Delta_Spleen_F;}

double vspleen_yest = 0;
if(SEXBABY == 1){vspleen_yest = 0.0021 * wbw0 * Delta_Spleen_M;}
else{vspleen_yest = 0.0022 * wbw0 * Delta_Spleen_F;}

// Pancreas
// Beaudouin et al. (2010)
double vpancreas = 0;
if(SEXBABY == 1){vpancreas = 0.00192 * wbw * Delta_Pancreas_M;}
else{vpancreas = 0.002 * wbw * Delta_Pancreas_F;}

double vpancreas_30 = 0;
if(SEXBABY == 1){vpancreas_30 = 0.00192 * WBODYTMAX * Delta_Pancreas_M;}
else{vpancreas_30 = 0.002 * WBODYTMAX * Delta_Pancreas_F;}

double vpancreas_35 = 0;
if(SEXBABY == 1){vpancreas_35 = 0.00192 * WBODYCMAX * Delta_Pancreas_M;}
else{vpancreas_35 = 0.002 * WBODYCMAX * Delta_Pancreas_F;}

double vpancreas_yest = 0;
if(SEXBABY == 1){vpancreas_yest = 0.00192 * wbw0 * Delta_Pancreas_M;}
else{vpancreas_yest = 0.002 * wbw0 * Delta_Pancreas_F;}

// Adrenals (Beaudouin et al. (2010))
double vadrenal = 0;
if(SEXBABY == 1){vadrenal = 2.0e-04 + (1.71e-03 - 2.0e-04) * exp(-2.02 * year);}
else{vadrenal = 0.0002 + (0.00171 - 0.0002) * exp(-2.02 * year);}

double vadrenal_30 = 0;
if(SEXBABY == 1){vadrenal_30 = 2.0e-04 + (1.71e-03 - 2.0e-04) * exp(-2.02 * 30);}
else{vadrenal_30 = 0.0002 + (0.00171 - 0.0002) * exp(-2.02 * 30);}

double vadrenal_35 = 0;
if(SEXBABY == 1){vadrenal_35 = 2.0e-04 + (1.71e-03 - 2.0e-04) * exp(-2.02 * 35);}
else{vadrenal_35 = 0.0002 + (0.00171 - 0.0002) * exp(-2.02 * 35);}

double vadrenal_yest = 0;
if(SEXBABY == 1){vadrenal_yest = 2.0e-04 + (1.71e-03 - 2.0e-04) * exp(-2.02 * yest);}
else{vadrenal_yest = 0.0002 + (0.00171 - 0.0002) * exp(-2.02 * yest);}

// Gonads / Reproductive organs
// Beaudouin et al. (2010)
double vgonads = 0;
if(SEXBABY == 1){
  if(year < 20.1){vgonads = (-1.516e-07 * pow(year,3) + 9.3351e-06 * pow(year,2) - 1.1177e-04 * year + 4.7966e-04) * wbw * Delta_Gonad_M;}
  else{vgonads = 0.0008 * wbw * Delta_Gonad_F;}
}
else{
  if(year < 1){vgonads = (-1.064e-03*year + 1.338e-03)* wbw * Delta_Gonad_F;}
  else if(year >= 1 && year < 20){vgonads = (2.6380e-07 * pow(year,3) - 1.7943e-06 * pow(year,2) - 5.6465e-06 * year + 2.8105e-04) * wbw * Delta_Gonad_F;}
  else{vgonads = 0.001552 * wbw * Delta_Gonad_F;}
}

double vgonads_30 = 0;
if(SEXBABY == 1){vgonads_30 = 0.0008 * WBODYTMAX * Delta_Gonad_M;}
else{vgonads_30 = 0.001552 * WBODYTMAX * Delta_Gonad_F;}

double vgonads_35 = 0;
if(SEXBABY == 1){vgonads_35 = 0.0008 * WBODYCMAX * Delta_Gonad_M;}
else{vgonads_35 = 0.001552 * WBODYCMAX * Delta_Gonad_F;}

double vgonads_yest = 0;
if(SEXBABY == 1){
  if(yest < 20.1){vgonads_yest = (-1.516e-07 * pow(yest,3) + 9.3351e-06 * pow(yest,2) - 1.1177e-04 * yest + 4.7966e-04) * wbw0 * Delta_Gonad_M;}
  else{vgonads_yest = 0.0008 * wbw0 * Delta_Gonad_F;}
}
else{
  if(yest < 1){vgonads_yest = (-1.064e-03*yest + 1.338e-03)* wbw0 * Delta_Gonad_F;}
  if(yest >= 1 && yest < 20){vgonads_yest = (2.6380e-07 * pow(yest,3) - 1.7943e-06 * pow(yest,2) - 5.6465e-06 * yest + 2.8105e-04) * wbw0 * Delta_Gonad_F;}
  else{vgonads_yest = 0.001552 * wbw0 * Delta_Gonad_F;}
}

// Lungs
// Beaudouin et al. (2010)
double vlungs = 0;
if(SEXBABY == 1){vlungs = 0.0068 * wbw * Delta_Lung_M;}
else{vlungs = 0.0070 * wbw * Delta_Lung_F;}

double vlungs_30 = 0;
if(SEXBABY == 1){vlungs_30 = 0.0068 * WBODYTMAX * Delta_Lung_M;}
else{vlungs_30 = 0.0070 * WBODYTMAX * Delta_Lung_F;}

double vlungs_35 = 0;
if(SEXBABY == 1){vlungs_35 = 0.0068 * WBODYCMAX * Delta_Lung_M;}
else{vlungs_35 = 0.0070 * WBODYCMAX * Delta_Lung_F;}

double vlungs_yest = 0;
if(SEXBABY == 1){vlungs_yest = 0.0068 * wbw0 * Delta_Lung_M;}
else{vlungs_yest = 0.0070 * wbw0 * Delta_Lung_F;}

/// Gut (Stomach + Small intestine + Large intestine)
// Stomach
// Beaudouin et al. (2010)
double vstomach = 0;
if(SEXBABY == 1){vstomach = 0.0021 * wbw * Delta_Stomach_M;}
else{vstomach = 0.0023 * wbw * Delta_Stomach_F;}

double vstomach_30 = 0;
if(SEXBABY == 1){vstomach_30 = 0.0021 * WBODYTMAX * Delta_Stomach_M;}
else{vstomach_30 = 0.0023 * WBODYTMAX * Delta_Stomach_F;}

double vstomach_35 = 0;
if(SEXBABY == 1){vstomach_35 = 0.0021 * WBODYCMAX * Delta_Stomach_M;}
else{vstomach_35 = 0.0023 * WBODYCMAX * Delta_Stomach_F;}

double vstomach_yest = 0;
if(SEXBABY == 1){vstomach_yest = 0.0021 * wbw0 * Delta_Stomach_M;}
else{vstomach_yest = 0.0023 * wbw0 * Delta_Stomach_F;}

// Intestinal tract (Small + Large Intestine)
// Beaudouin et al. (2010)
double vintestine = 0;
double Delta_Intestine = SEXBABY * (0.63 * Delta_Small_Intestine_M + 0.37 * Delta_Large_Intestine_M) + (1 - SEXBABY) * (0.63 * Delta_Small_Intestine_F + 0.37 * Delta_Large_Intestine_F);
if(SEXBABY == 1){
  if(year < 16){vintestine = (-8.2562e-05*pow(year,2) + 1.3523e-03*year + 1.293e-02) * wbw * Delta_Intestine;}
  else{vintestine = 0.014 * wbw * Delta_Intestine;}
}
else{
  if(year < 14.453301){vintestine = (-7.421e-05*pow(year,2) + 1.276e-03*year + 1.298e-02) * wbw * Delta_Intestine;}
  else{vintestine = 0.0160 * wbw * Delta_Intestine;}
}

double vintestine_30 = 0;
if(SEXBABY == 1){vintestine_30 = 0.014 * WBODYTMAX * Delta_Intestine;}
else{vintestine_30 = 0.0160 * WBODYTMAX * Delta_Intestine;}

double vintestine_35 = 0;
if(SEXBABY == 1){vintestine_35 = 0.014 * WBODYCMAX * Delta_Intestine;}
else{vintestine_35 = 0.0160 * WBODYCMAX * Delta_Intestine;}

double vintestine_yest = 0;
if(SEXBABY == 1){
  if(yest < 16){vintestine_yest = (-8.2562e-05*pow(yest,2) + 1.3523e-03*yest + 1.293e-02) * wbw0 * Delta_Intestine;}
  else{vintestine_yest = 0.014 * wbw0 * Delta_Intestine;}
}
else{
  if(yest < 14.453301){vintestine_yest = (-7.421e-05*pow(yest,2) + 1.276e-03*yest + 1.298e-02) * wbw0 * Delta_Intestine;}
  else{vintestine_yest = 0.0160 * wbw0 * Delta_Intestine;}
}

// Calculation of the total bodyweight
double wbw_f = vfat + vbr + vk + vhair + vb + vl + vheart + vd + vm + vs + vbone + vmarr + vstomach + vintestine + vbreast + vthyr + vspleen + vpancreas + vadrenal + vgonads + vlungs;

double wbw_30_f = vfat_30 + vbr_30 + vk_30 + vhair_30 + vb_30 + vl_30 + vheart_30 + vd_30 + vm_30 + vs_30 + vbone_30 + vmarr_30 + vstomach_30 + vintestine_30 + vbreast_30 + vthyr_30 + vspleen_30 + vpancreas_30 + vadrenal_30 + vgonads_30 + vlungs_30;

double wbw_35_f = vfat_35 + vbr_35 + vk_35 + vhair_35 + vb_35 + vl_35 + vheart_35 + vd_35 + vm_35 + vs_35 + vbone_35 + vmarr_35 + vstomach_35 + vintestine_35 + vbreast_35 + vthyr_35 + vspleen_35 + vpancreas_35 + vadrenal_35 + vgonads_35 + vlungs_35;

double wbw_yest_f = vfat_yest + vbr_yest + vk_yest + vhair_yest + vb_yest + vl_yest + vheart_yest + vd_yest + vm_yest + vs_yest + vbone_yest + vmarr_yest + vstomach_yest + vintestine_yest + vbreast_yest + vthyr_yest + vspleen_yest + vpancreas_yest + vadrenal_yest + vgonads_yest + vlungs_yest;


double vrapid = vbreast + vthyr + vspleen + vpancreas + vadrenal + vgonads + vlungs + vstomach + vintestine + vbr;
double vslow = vheart + vd + vm + vs + vfat + vmarr; 

/// Cardiac output
// Beaudouin et al. (2010)
double QCG = 0;
if(SEXBABY == 1){
  if(year < 33.37){QCG = (6.642+(0.6-6.642)*exp(-0.1323*year))*60*24;}
  else{QCG = (-0.000895*pow(year,2) + 0.0607*year + 5.54)*60*24;}}
else{
  if(year < 16.027){QCG = (7.734+(0.6-7.734)*exp(-0.09747*year))*60*24;}
  else{QCG = (0.000473*pow(year,2) - 0.0782*year + 7.37)*60*24;}}

//  By the current data, a blood flow rate is used to estimate the evolution over life
/// The blood flow rate come from ICRP (2002)
// Fat
double qfat = 0;
if (SEXBABY == 1){qfat = 0.05*QCG;}
else{qfat = 0.085*QCG;}

// Brain
double qbr = 0.12*QCG;

// Kidney
double qk = 0;
if(SEXBABY == 1){qk = 0.19*QCG;}
else{qk = 0.17*QCG;}

// Liver (only from arterial blood)
double qliver = 0.065*QCG;

// Heart
double qheart = 0;
if(SEXBABY == 1){qheart = 0.04 * QCG;}
else{qheart = 0.05 * QCG;}

// Diaphragm
double qd = 0.0003*QCG;

// Muscles
double qm = 0;
if(SEXBABY == 1){qm = 0.17 * QCG;}
else{qm = 0.12*QCG;}

// Skin
double qs = 0.05*QCG;

// Bone
double qbone = 0.02*QCG;

// Marrow
double qmarr = 0.03*QCG;

// Stomach
double qstomach = 0.01*QCG;

// Intestine
double qintestine = 0;
if(SEXBABY == 1){qintestine = 0.14 * QCG;}
else{qintestine = 0.16 * QCG;}

// Breast
double qbreast = 0;
if(SEXBABY == 1){qbreast = 0.0002 * QCG;}
else{qbreast = 0.004 * QCG;}

// Thyroid
double qthyr = 0.015 * QCG;

// Spleen
double qspleen = 0.03 * QCG;

// Pancreas
double qpancreas = 0.01 * QCG;

// Adrenals
double qadrenal = 0.003 * QCG;

// Gonads
double qgonads = 0;
if(SEXBABY == 1){qgonads = 0.0005 * QCG;}
else{qgonads = 0.0002 * QCG;}

// Lungs (as bronchial tissues)
double qlungs = 0.025*QCG;

/// Relativisation to have the sum of rate equal to 1
double Qtot = qfat + qbr + qk + qliver + qheart + qd + qm + qs + qbone + qmarr + qstomach + qintestine + qbreast + qthyr + qspleen + qpancreas + qadrenal + qgonads + qlungs;



// Lumping of organs
double qgut  = qstomach + qintestine;

//double qslow = qheart + qd + qm + qs + qmarr + qfat;

double qrapid = qbreast + qthyr + qspleen + qpancreas + qadrenal + qgonads + qlungs + qbr + qgut;

// Blood flow to trabecular bones (L/day)
double TQB = CONST*qbone;
double TQBC = TQB*(TBFRC/TBFR);
double TQBA = TQB - TQBC;
//Blood flow to cortical bones (L/day)
double CQB = qbone-TQB;
double CQBC = CQB*(CBFRC/CBFR);
double CQBA = CQB - CQBC;
// Poorly-perfused tissues blood flow (L/day)
double qslow = Qtot - (qliver+qk+qrapid+qbone);

/// Concentration in organs (µg/kg)
// Pb in blood

double PLASMA = 1 - HCT;				  // Plasma fraction of blood volume
double CB = BLOOD/vb;            // Concentration of lead in blood (ug/L)
// Concentration of lead in plasma (?g/L)
double CPLASMA = (((BLOOD/vb)-HCT*BIND-G*HCT*KBIND-PLASMA*KBIND)/(2*(PLASMA+HCT*G)))+(pow((pow((HCT*BIND+G*HCT*KBIND+PLASMA*KBIND-(BLOOD/vb)),2)+4*KBIND*(BLOOD/vb)*(PLASMA+HCT*G)),0.5)/(2*(PLASMA+HCT*G)));		// Concentration of lead in plasma (mg/L)
// Pb in liver
double CL   = LIVER/vl;                                   // Concentration of lead in liver (µg/L)
// Pb in kidney
double CK   = KIDNEY/vk;                                  // Concentration of lead in kidney (µg/L)
// Pb in well-perfused tissues
double CW   = RAPIDLY/vrapid;                                  // Concentration of lead in well-perfused tissues (µg/L)
// Pb in poorly-perfused tissues
double CP   = POORLY/vslow;                                  // Concentration of lead in poorly-perfused tissues (µg/L)
// Pb in metabolically active region of cortical bone
double CAB = AB1+AB2+AB3+AB4+AB5+AB6+AB7+AB8+CAMC;
double CCB  = CAB/CVBONE;

double CRVAFC = LEAD*CBFRC;                                   // Clearance of lead from blood during mineralization of newly-apposed cortical bone engaged in active modeling during growth (L/day)
double CRAFBC = CRVAFC*CPLASMA;                               // Rate of deposition of lead in cortical bone engaged in active modeling during growth (µg/day)
double CRARBC = CBRRC*CCMC;                                   // Rate of return of lead to blood with resorption of cortical bone engaged in active modeling during growth (µg/day)
double CRAMBC = CRARBC - CRAFBC;                              // Net rate of change of amount of lead in blood perfusing cortical bone engaged in active modeling during growth (µg/day)
double CCMC   = 0;                                              // Concentration of lead in cortical bone engaged in active modeling during growth (µg/L)
if(year<25){CCMC = CAMC/(CVBONEC+1e-33);}
else{CCMC = 0;}
double CCBHC  = 0;                                             // Concentration of lead in blood leaving cortical bone engaged in active modeling during growth (µg/L)
if(CBFRA < CBFR){CCBHC = (BLOOD/vb) + CRAMBC/(CQBC+1e-33);}
else{CCBHC = 0;}

double CRVAFA = LEAD*CBFRA;                                   // Clearance of lead from blood during mineralization of newly-apposed mature cortical bone engaged in remodeling (L/day)
double CRAFBA = CRVAFA*CPLASMA;                               // Rate of deposition of lead in metabolically active region of mature cortical bone engaged in remodeling (µg/day)
double CRARBA = CBRRA*CCB;                                    // Rate of return of lead to blood with resorption of mature cortical bone engaged in remodeling (µg/day)
double CRAMBA = CRARBA - CRAFBA;                              // Net rate of change of amount of lead in blood perfusing the metabolically active region of mature cortical bone engaged in remodeling (mg/day)
double CCBHA  = (BLOOD/vb) + CRAMBA/(CQBA+1e-33);                      // Concentration of lead in blood leaving the metabolically active region of mature cortical bone engages in remodelong (mg/L)
double CCBPHA = ((CCBHA - HCT*BIND - G*HCT*KBIND - PLASMA*KBIND)/(2*(PLASMA + HCT*G)))+(pow((pow((HCT*BIND + G*HCT*KBIND + PLASMA*KBIND - CCBHA),2) + 4*KBIND*CCBHA*(PLASMA + HCT*G)),0.5)/(2*(PLASMA + HCT*G)));	// Concentration of lead in plasma leaving the metabolically active region of mature cortical bone engaged in remodeling

// Pb in trabecular bone
double TRVAFC = LEAD*TBFRC;				// Clearance of lead from blood during mineralization of newly-apposed trabecular bone engaged in active modeling during ggrowth (L/day)
double TRAFBC = TRVAFC*CPLASMA;				// Rate of deposition of lead in trabecular bone engaged in active modeling during growth (mg/day)
double TCMC   = 1;					// Concentration of lead in trabecular bone engaged in active modeling during growth (mg/L)
if(year < 25){TCMC = TAMC/(TVBONEC+1e-33);}
else{TCMC = 0;}
double TRARBC = TBRRC * TCMC;				// Rate of return of lead to plasma woth resorption of trabecular bone engaged in active modeling during growth (mg/day)
double TRAMBC = TRARBC - TRAFBC;			// Net rate of change of amount of lead in blood perfusing trabecular bone engaged in active modeling during growth (mg/day)
double TCBHC = 1;					// Concentration of lead in blood leaving trabecular bone engaged in active modeling during growth (mg/L)
if(TBFRA < TBFR){TCBHC = (BLOOD/vb) + TRAMBC/(TQBC + 1e-33);}
else{TCBHC = 0;}

double TRVAFA = LEAD*TBFRA;				// Clearance of lead from blood during mineralization of newly-apposed mature trabecular bone engaged in remodeling (L/day)
double TRAFBA = TRVAFA*CPLASMA;				// Rate of  deposition of lead in mature trabecular bone engaged in remodeling (mg/day)
double TCMA   = 1;					// Concentration of lead in mature trabecular bone engaged in remodeling (mg/L)
if(year < 25){TCMA = TAMA/(TVBONEA+1e-33);}
else{TCMA = TAMA/TVBONE;}
double TRARBA = TBRRA*TCMA;				// Rate of return of lead to blood with resorption of mature trabecular bone engaged in remodeling (mg/day)
double TRAMBA = TRARBA - TRAFBA;			// Net rate of change of amount of lead in blood perfusing mature trabecular bone engaged in remodeling (mg/day)
double TCBHA = (BLOOD/vb) + TRAMBA/(TQBA + 1e-33);		// Concentration of lead in blood leaving mature trabecular bone engaged in remodeling (mg/L)

// Lead in diffusion region of mature cortical bone
double VTOTAL = PI*pow(RAD8,2);
double F1 = V1/VTOTAL;
double F2 = V2/VTOTAL;
double F3 = V3/VTOTAL;
double F4 = V4/VTOTAL;
double F5 = V5/VTOTAL;
double F6 = V6/VTOTAL;
double F7 = V7/VTOTAL;
double F8 = V8/VTOTAL;
double CB1 = AB1/(1e-33+CVBONEA*F1);
double RDBIN = (S*L*CVBONEA/1000)*(P*CCBPHA);		       // Rate of loss of lead from blood into mature cortical bone by diffusion (mg/day)
double RDBEX = (S*L*CVBONEA/1000)*(Rad*CB1); 		       // Rate of return of lead to blood from mature cortical bone by diffusion (mg/day)
double RDB   = RDBEX - RDBIN;				       // Net rate of change of amont of lead in blood perfusing the diffusion region of mature cortical bone (mg/day)
double CBDA  = 0;                                              // Concentration of lead in blood leaving the diffusion region of mature cortical bone (mg/L)
if(TIME >= 1){CBDA = CCBHA + RDB/(CQBA+1e-33);}

/// Excretion of Lead
// Glomerular filtration rate (L plasma/year)
double GFR = 1;
if(wbw <= 11.7){GFR = 708.5*(pow(wbw_f,1.3774));}
else{GFR = 4391.1*pow(wbw_f,0.636);}
double FRX = 1-0.9/(1+50*exp(-year));
double QEG  = (FRX*GFR)/365;                                // Total elimination clearance of lead from plasma (L/day)
// Renal excretion
double CLK  = 0.7*QEG;                                      // Renal clearance of lead from plasma (L/day)
double RAKX = CLK*KIDNEY/(vk*PK);                           // Rate of renal excreation (µg/day)
// Liver excretion
double CLL  = QEG - CLK;                                    // Hepatic clearance from plasma in bile (L/day)
double RALX = CLL*LIVER/(vl*PL);                            // Rate of hepatic excretion (µg/day)
//creatinine en fonction de l age (C.B?chaux) !!! Ne convient pas apr?s 70 ans !!!
  double ucr= (0.032+2.098e-02*year+9.104e-03*pow(year,2)-4.550e-04*pow(year,3)+7.578e-06*pow(year,4)-4.232e-08*pow(year,5))*Delta_creat;
if (year>70) {ucr=(0.032+2.098e-02*70+9.104e-03*pow(70,2)-4.550e-04*pow(70,3)+7.578e-06*pow(70,4)-4.232e-08*pow(70,5))*Delta_creat;}
double UPbcr = (CLK*KIDNEY/(vk*PK))/ucr;
double UPb = (CLK*KIDNEY/(vk*PK))/Vurine;

double Vurine = 0.0294 * wbw_f;

/// Exposure

// Dietary exposure
double FRABS = 0.60 - 0.52/(1+30*exp(-year));               // Fractional absorption of lead from mixed sources in gastro-intestinal tract

/// Inhalation

double FRLUNG = 0.3; //Fractional absorption from the lung
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

double VInhalation = (FqBreath * (Vtidal - vds) * 60 * 24) / 1000; // Volume of inhalation (m3) by day

// Occupational inhalation
double CWKPL = 1;
if(year > 20 && year <= 60){CWKPL = AIRPRO*40/168;}
else{CWKPL = 0;}

// Dose of Cd by smoking exposures
double Cig = 0;
if(year > age_deb && year < age_fin){Cig = nb_cig * smoke_Pb;}
else {Cig = 0;}

  double RAGI = FRABS*(DIET * wbw_f + SOIL + DUST);
  double RALUNG = FRLUNG*((AIR + CWKPL) * VInhalation + Cig);
  
$ODE

dxdt_DIET = - DIET;

dxdt_DUST = - DUST;

dxdt_SOIL = - SOIL;

dxdt_AIR  = - AIR;

dxdt_CVB = CORTICAL;

dxdt_TVB = TRABECULAR;

dxdt_LIVER = qliver*((BLOOD/vb)-(PLASMA*(LIVER/(vl*PL)) + HCT*(LIVER/(vl*PL))*(G+BIND/(KBIND+(LIVER/(vl*PL)))))) + RAGI - (CLL*LIVER/(vl*PL));

dxdt_BLOOD = qliver*(PLASMA*(LIVER/(vl*PL)) + HCT*(LIVER/(vl*PL))*(G+BIND/(KBIND+(LIVER/(vl*PL))))) + qk*(PLASMA*(KIDNEY/(vk*PK)) + HCT*(KIDNEY/(vk*PK))*(G+BIND/(KBIND+(KIDNEY/(vk*PK))))) + 
  qslow*(PLASMA*(SLOWLY/(vslow*PP)) + HCT*(SLOWLY/(vslow*PP))*(G+BIND/(KBIND+(SLOWLY/(vslow*PP))))) + 
  qrapid*(PLASMA*(RAPIDLY/(vrapid*PW)) + HCT*(RAPIDLY/(vrapid*PW))*(G+BIND/(KBIND+(RAPIDLY/(vrapid*PW))))) + 
  CQBC*CCBHC + CQBA*CBDA + TQBC*TCBHC + TQBA*TCBHA + RALUNG - Qtot*(BLOOD/vb);

dxdt_KIDNEY = qk*((BLOOD/vb)-(PLASMA*(KIDNEY/(vk*PK)) + HCT*(KIDNEY/(vk*PK))*(G+BIND/(KBIND+(KIDNEY/(vk*PK))))))-((CLK*KIDNEY/(vk*PK)));

dxdt_RAPIDLY = qrapid*((BLOOD/vb)-(PLASMA*(RAPIDLY/(vrapid*PW)) + HCT*(RAPIDLY/(vrapid*PW))*(G+BIND/(KBIND+(RAPIDLY/(vrapid*PW))))));

dxdt_SLOWLY = qslow*((BLOOD/vb)-(PLASMA*(SLOWLY/(vslow*PP)) + HCT*(SLOWLY/(vslow*PP))*(G+BIND/(KBIND+(SLOWLY/(vslow*PP))))));

dxdt_CAMC   = CRAFBC - CRARBC - CRFBON*CAMC;

dxdt_TAMC   = TRAFBC - TRARBC - TRFBON*TAMC;

dxdt_TAMA   = TRAFBA - TRARBA + TRFBON*TAMC;

dxdt_AB1 = (S*L*CVBONEA/1000)*(P*CCBPHA-Rad*(AB1/(1e-33+CVBONEA*F1)))-(D*CVBONEA*S1*L/1000)*((AB1/(1e-33+CVBONEA*F1))-(AB2/(1e-33+CVBONEA*F2))) + F1*(CRAFBA-CRARBA+CRFBON*CAMC);

dxdt_AB2 = (D*CVBONEA*L/1000)*((AB1/(1e-33+CVBONEA*F1))*S1+(AB3/(1e-33+CVBONEA*F3))*S2-(AB2/(1e-33+CVBONEA*F2))*(S1+S2))+F2*(CRAFBA-CRARBA+CRFBON*CAMC);

dxdt_AB3 = (D*CVBONEA*L/1000)*((AB2/(1e-33+CVBONEA*F2))*S2+(AB4/(1e-33+CVBONEA*F4))*S3-(AB3/(1e-33+CVBONEA*F3))*(S2+S3))+F3*(CRAFBA-CRARBA+CRFBON*CAMC);

dxdt_AB4 = (D*CVBONEA*L/1000)*((AB3/(1e-33+CVBONEA*F3))*S3+(AB5/(1e-33+CVBONEA*F5))*S4-(AB4/(1e-33+CVBONEA*F4))*(S3+S4))+F4*(CRAFBA-CRARBA+CRFBON*CAMC);

dxdt_AB5 = (D*CVBONEA*L/1000)*((AB4/(1e-33+CVBONEA*F4))*S4+(AB6/(1e-33+CVBONEA*F6))*S5-(AB5/(1e-33+CVBONEA*F5))*(S4+S5))+F5*(CRAFBA-CRARBA+CRFBON*CAMC);

dxdt_AB6 = (D*CVBONEA*L/1000)*((AB5/(1e-33+CVBONEA*F5))*S5+(AB7/(1e-33+CVBONEA*F7))*S6-(AB6/(1e-33+CVBONEA*F6))*(S5+S6))+F6*(CRAFBA-CRARBA+CRFBON*CAMC);

dxdt_AB7 = (D*CVBONEA*L/1000)*((AB6/(1e-33+CVBONEA*F6))*S6+(AB8/(1e-33+CVBONEA*F8))*S7-(AB7/(1e-33+CVBONEA*F7))*(S6+S7))+F7*(CRAFBA-CRARBA+CRFBON*CAMC);

dxdt_AB8 = (D*CVBONEA*L/1000)*((AB7/(1e-33+CVBONEA*F7))*S7-(AB8/(1e-33+CVBONEA*F8))*S7)+F8*(CRAFBA-CRARBA+CRFBON*CAMC);


$CMT DIET DUST SOIL AIR CVB TVB KIDNEY SLOWLY LIVER RAPIDLY BLOOD CAMC TAMC TAMA AB1 AB2 AB3 AB4 AB5 AB6 AB7 AB8

$CAPTURE CB RAKX UPb UPbcr ucr CK RVBONEX vbone QCG Qtot CBRR TBRR BRRCMAX CKLOSS CVBONE CBFR CBRRO vbone_35 wbw wbw0 Qtot wbw_f vbr vfat vk vhair vb vp vrbc vl vheart vd vm vs vbone vmarr vstomach
          vintestine vbreast vthyr vspleen vpancreas vgonads vlungs vadrenal CVBONECMAX FBFR vslow vrapid qslow qrapid Qtot
