**************************************************;
* Code for reading in and cleaning births dataset ;
**************************************************;

LIBNAME h "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS";

/* 2007 ******************************************************************************************/
DATA h.births_2007 (drop = mrace);

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2007US.AllCnty.txt" LRECL = 20000;
/*attrib  revision     length=$1    label="Revision U,R";                         
attrib  dob_yy       length=4     label="Birth Year U,R";                               
attrib  mager9       length=3     label="Mother's Age Recode 9 U,R";            
attrib  mrterr       length=$2    label="Mother's Residence Territory/Posession U,R";
attrib  mrcnty       length=3     label="Mother's County of Residence U,R";               
attrib  restatus     length=3     label="Residence Status U,R";                 
attrib  mbrace       length=3     label="Mother's Bridged Race** R***";         
attrib  mrace        length=3     label="Mother's Race U";                      
attrib  umhisp       length=3     label="Mother's Hispanic Origin RFP 569=f_morigin U,R";
attrib  mracehisp    length=3     label="Mother's Race/Hispanic Origin RFP 569=f_morigin U,R";
attrib  mar          length=3     label="Mother's Marital Status U,R";          
attrib  meduc        length=3     label="Mother's Education RFP 571=f_meduc R"; 
attrib  dplural      length=3     label="Plurality U,R";                        
attrib  estgest      length=3     label="Obstetric/Clinical Gestation Est. RFP 573=f_clinest U,R";
attrib  combgest     length=3     label="Gestation - Detail in Weeks U,R"; 
attrib  obgest_flg   length=3     label="Clinical Estimate of Gestation Used Flag U,R";
attrib  gest_imp     length=3     label="Gestation Imputed Flag U,R";           
attrib  dbwt         length=4     label="Birth Weight - Detail in Grams U,R";   
*/

INPUT
@7    revision          $1. 
@15   dob_yy             4. 
@93   mager9             1. 
@109  mrterr            $2. 
@114  mrcnty             3. 
@138  restatus           1. 
@139  mbrace             2.
@141  mrace              2.
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1.  
@155  meduc              1.  
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2.  
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4. 
;

* Combine mbrace and mrace into mbrace;
if mbrace = . or mbrace = "" then mbrace = mrace;

* Drop non-US (Keep PR);
if mrterr = "AB" or mrterr = "AS" or mrterr = "BC" or mrterr = "FM" or mrterr = "GU" or mrterr = "MB" or 
	mrterr = "MH" or mrterr = "MP" or mrterr = "NB" or mrterr = "NL" or mrterr = "NT" or mrterr = "NS" or 
	mrterr = "NU" or mrterr = "ON" or mrterr = "PE" or mrterr = "PW" or mrterr = "QC" or mrterr = "SK" or 
	mrterr = "VI" or mrterr = "XX" or mrterr = "YT" or mrterr = "YY" or mrterr = "ZZ" then delete;
run;

/* 2008 ******************************************************************************************/
DATA h.births_2008 (drop = mrace);

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2008US.AllCnty.txt" LRECL = 20000;
/*attrib  revision     length=$1    label="Revision U,R";                         
attrib  dob_yy       length=4     label="Birth Year U,R";                               
attrib  mager9       length=3     label="Mother's Age Recode 9 U,R";            
attrib  mrterr       length=$2    label="Mother's Residence Territory/Posession U,R";
attrib  mrcnty       length=3     label="Mother's County of Residence U,R";               
attrib  restatus     length=3     label="Residence Status U,R";                 
attrib  mbrace       length=3     label="Mother's Bridged Race** R***";         
attrib  mrace        length=3     label="Mother's Race U";                      
attrib  umhisp       length=3     label="Mother's Hispanic Origin RFP 569=f_morigin U,R";
attrib  mracehisp    length=3     label="Mother's Race/Hispanic Origin RFP 569=f_morigin U,R";
attrib  mar          length=3     label="Mother's Marital Status U,R";          
attrib  meduc        length=3     label="Mother's Education RFP 571=f_meduc R"; 
attrib  dplural      length=3     label="Plurality U,R";                        
attrib  estgest      length=3     label="Obstetric/Clinical Gestation Est. RFP 573=f_clinest U,R";
attrib  combgest     length=3     label="Gestation - Detail in Weeks U,R"; 
attrib  obgest_flg   length=3     label="Clinical Estimate of Gestation Used Flag U,R";
attrib  gest_imp     length=3     label="Gestation Imputed Flag U,R";           
attrib  dbwt         length=4     label="Birth Weight - Detail in Grams U,R";   
*/

INPUT
@7    revision          $1. 
@15   dob_yy             4. 
@93   mager9             1. 
@109  mrterr            $2. 
@114  mrcnty             3. 
@138  restatus           1. 
@139  mbrace             2. 
@141  mrace              2. 
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1. 
@155  meduc              1. 
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2. 
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4.
;

* Combine mbrace and mrace into mbrace;
if mbrace = . or mbrace = "" then mbrace = mrace;

* Drop non-US (Keep PR);
if mrterr = "AB" or mrterr = "AS" or mrterr = "BC" or mrterr = "FM" or mrterr = "GU" or mrterr = "MB" or 
	mrterr = "MH" or mrterr = "MP" or mrterr = "NB" or mrterr = "NL" or mrterr = "NT" or mrterr = "NS" or 
	mrterr = "NU" or mrterr = "ON" or mrterr = "PE" or mrterr = "PW" or mrterr = "QC" or mrterr = "SK" or 
	mrterr = "VI" or mrterr = "XX" or mrterr = "YT" or mrterr = "YY" or mrterr = "ZZ" then delete;
run;

/* 2009 ******************************************************************************************/
DATA h.births_2009 (drop = mrace);

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2009US.AllCnty.txt" LRECL = 20000;
/*attrib  revision     length=$1    label="Revision U,R";                         
attrib  dob_yy       length=4     label="Birth Year U,R";                               
attrib  mager9       length=3     label="Mother's Age Recode 9 U,R";            
attrib  mrterr       length=$2    label="Mother's Residence Territory/Posession U,R";
attrib  mrcnty       length=3     label="Mother's County of Residence U,R";               
attrib  restatus     length=3     label="Residence Status U,R";                 
attrib  mbrace       length=3     label="Mother's Bridged Race** R***";         
attrib  mrace        length=3     label="Mother's Race U";                      
attrib  umhisp       length=3     label="Mother's Hispanic Origin RFP 569=f_morigin U,R";
attrib  mracehisp    length=3     label="Mother's Race/Hispanic Origin RFP 569=f_morigin U,R";
attrib  mar          length=3     label="Mother's Marital Status U,R";          
attrib  meduc        length=3     label="Mother's Education RFP 571=f_meduc R"; 
attrib  dplural      length=3     label="Plurality U,R";                        
attrib  estgest      length=3     label="Obstetric/Clinical Gestation Est. RFP 573=f_clinest U,R";
attrib  combgest     length=3     label="Gestation - Detail in Weeks U,R"; 
attrib  obgest_flg   length=3     label="Clinical Estimate of Gestation Used Flag U,R";
attrib  gest_imp     length=3     label="Gestation Imputed Flag U,R";           
attrib  dbwt         length=4     label="Birth Weight - Detail in Grams U,R";   
*/

INPUT
@7    revision          $1. 
@15   dob_yy             4.   
@93   mager9             1. 
@109  mrterr            $2. 
@114  mrcnty             3. 
@138  restatus           1. 
@139  mbrace             2. 
@141  mrace              2. 
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1.  
@155  meduc              1.  
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2. 
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4.
;

* Combine mbrace and mrace into mbrace;
if mbrace = . or mbrace = "" then mbrace = mrace;

* Drop non-US (Keep PR);
if mrterr = "AB" or mrterr = "AS" or mrterr = "BC" or mrterr = "FM" or mrterr = "GU" or mrterr = "MB" or 
	mrterr = "MH" or mrterr = "MP" or mrterr = "NB" or mrterr = "NL" or mrterr = "NT" or mrterr = "NS" or 
	mrterr = "NU" or mrterr = "ON" or mrterr = "PE" or mrterr = "PW" or mrterr = "QC" or mrterr = "SK" or 
	mrterr = "VI" or mrterr = "XX" or mrterr = "YT" or mrterr = "YY" or mrterr = "ZZ" then delete;
run;

/* 2010 *******************************************************************************************/
DATA h.births_2010 (drop = mrace);

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2010US.AllCnty.txt" LRECL = 20000;
/*attrib  revision     length=$1    label="Revision U,R";                         
attrib  dob_yy       length=4     label="Birth Year U,R";                               
attrib  mager9       length=3     label="Mother's Age Recode 9 U,R";            
attrib  mrterr       length=$2    label="Mother's Residence Territory/Posession U,R";
attrib  mrcnty       length=3     label="Mother's County of Residence U,R";               
attrib  restatus     length=3     label="Residence Status U,R";                 
attrib  mbrace       length=3     label="Mother's Bridged Race** R***";         
attrib  mrace        length=3     label="Mother's Race U";                      
attrib  umhisp       length=3     label="Mother's Hispanic Origin RFP 569=f_morigin U,R";
attrib  mracehisp    length=3     label="Mother's Race/Hispanic Origin RFP 569=f_morigin U,R";
attrib  mar          length=3     label="Mother's Marital Status U,R";          
attrib  meduc        length=3     label="Mother's Education RFP 571=f_meduc R"; 
attrib  dplural      length=3     label="Plurality U,R";                        
attrib  estgest      length=3     label="Obstetric/Clinical Gestation Est. RFP 573=f_clinest U,R";
attrib  combgest     length=3     label="Gestation - Detail in Weeks U,R"; 
attrib  obgest_flg   length=3     label="Clinical Estimate of Gestation Used Flag U,R";
attrib  gest_imp     length=3     label="Gestation Imputed Flag U,R";           
attrib  dbwt         length=4     label="Birth Weight - Detail in Grams U,R";   
*/

INPUT
@7    revision          $1. 
@15   dob_yy             4. 
@93   mager9             1. 
@109  mrterr            $2. 
@114  mrcnty             3. 
@138  restatus           1. 
@139  mbrace             2. 
@141  mrace              2. 
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1. 
@155  meduc              1. 
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2. 
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4. 
;

* Combine mbrace and mrace into mbrace;
if mbrace = . or mbrace = "" then mbrace = mrace;

* Drop non-US (Keep PR);
if mrterr = "AB" or mrterr = "AS" or mrterr = "BC" or mrterr = "FM" or mrterr = "GU" or mrterr = "MB" or 
	mrterr = "MH" or mrterr = "MP" or mrterr = "NB" or mrterr = "NL" or mrterr = "NT" or mrterr = "NS" or 
	mrterr = "NU" or mrterr = "ON" or mrterr = "PE" or mrterr = "PW" or mrterr = "QC" or mrterr = "SK" or 
	mrterr = "VI" or mrterr = "XX" or mrterr = "YT" or mrterr = "YY" or mrterr = "ZZ" then delete;
run;

/* 2011 *******************************************************************************************/
DATA h.births_2011 (drop = mrace);

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2011US.AllCnty.txt" LRECL = 20000;
/*attrib  revision     length=$1    label="Revision U,R";                         
attrib  dob_yy       length=4     label="Birth Year U,R";                               
attrib  mager9       length=3     label="Mother's Age Recode 9 U,R";            
attrib  mrterr       length=$2    label="Mother's Residence Territory/Posession U,R";
attrib  mrcnty       length=3     label="Mother's County of Residence U,R";               
attrib  restatus     length=3     label="Residence Status U,R";                 
attrib  mbrace       length=3     label="Mother's Bridged Race** R***";         
attrib  mrace        length=3     label="Mother's Race U";                      
attrib  umhisp       length=3     label="Mother's Hispanic Origin RFP 569=f_morigin U,R";
attrib  mracehisp    length=3     label="Mother's Race/Hispanic Origin RFP 569=f_morigin U,R";
attrib  mar          length=3     label="Mother's Marital Status U,R";          
attrib  meduc        length=3     label="Mother's Education RFP 571=f_meduc R"; 
attrib  dplural      length=3     label="Plurality U,R";                        
attrib  estgest      length=3     label="Obstetric/Clinical Gestation Est. RFP 573=f_clinest U,R";
attrib  combgest     length=3     label="Gestation - Detail in Weeks U,R"; 
attrib  obgest_flg   length=3     label="Clinical Estimate of Gestation Used Flag U,R";
attrib  gest_imp     length=3     label="Gestation Imputed Flag U,R";           
attrib  dbwt         length=4     label="Birth Weight - Detail in Grams U,R";   
*/

INPUT
@7    revision          $1. 
@15   dob_yy             4. 
@93   mager9             1. 
@109  mrterr            $2. 
@114  mrcnty             3. 
@138  restatus           1. 
@139  mbrace             2. 
@141  mrace              2. 
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1.  
@155  meduc              1. 
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2. 
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4. 
;

* Combine mbrace and mrace into mbrace;
if mbrace = . or mbrace = "" then mbrace = mrace;

* Drop non-US (Keep PR);
if mrterr = "AB" or mrterr = "AS" or mrterr = "BC" or mrterr = "FM" or mrterr = "GU" or mrterr = "MB" or 
	mrterr = "MH" or mrterr = "MP" or mrterr = "NB" or mrterr = "NL" or mrterr = "NT" or mrterr = "NS" or 
	mrterr = "NU" or mrterr = "ON" or mrterr = "PE" or mrterr = "PW" or mrterr = "QC" or mrterr = "SK" or 
	mrterr = "VI" or mrterr = "XX" or mrterr = "YT" or mrterr = "YY" or mrterr = "ZZ" then delete;
run;

/* 2012 *******************************************************************************************/
DATA h.births_2012 (drop = mrace);

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2012US.AllCnty.txt" LRECL = 20000;
/*attrib  revision     length=$1    label="Revision U,R";                         
attrib  dob_yy       length=4     label="Birth Year U,R";                               
attrib  mager9       length=3     label="Mother's Age Recode 9 U,R";            
attrib  mrterr       length=$2    label="Mother's Residence Territory/Posession U,R";
attrib  mrcnty       length=3     label="Mother's County of Residence U,R";               
attrib  restatus     length=3     label="Residence Status U,R";                 
attrib  mbrace       length=3     label="Mother's Bridged Race** R***";         
attrib  mrace        length=3     label="Mother's Race U";                      
attrib  umhisp       length=3     label="Mother's Hispanic Origin RFP 569=f_morigin U,R";
attrib  mracehisp    length=3     label="Mother's Race/Hispanic Origin RFP 569=f_morigin U,R";
attrib  mar          length=3     label="Mother's Marital Status U,R";          
attrib  meduc        length=3     label="Mother's Education RFP 571=f_meduc R"; 
attrib  dplural      length=3     label="Plurality U,R";                        
attrib  estgest      length=3     label="Obstetric/Clinical Gestation Est. RFP 573=f_clinest U,R";
attrib  combgest     length=3     label="Gestation - Detail in Weeks U,R"; 
attrib  obgest_flg   length=3     label="Clinical Estimate of Gestation Used Flag U,R";
attrib  gest_imp     length=3     label="Gestation Imputed Flag U,R";           
attrib  dbwt         length=4     label="Birth Weight - Detail in Grams U,R";   
*/

INPUT
@7    revision          $1. 
@15   dob_yy             4. 
@93   mager9             1. 
@109  mrterr            $2. 
@114  mrcnty             3. 
@138  restatus           1. 
@139  mbrace             2. 
@141  mrace              2. 
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1.  
@155  meduc              1. 
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2. 
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4. 
;

* Combine mbrace and mrace into mbrace;
if mbrace = . or mbrace = "" then mbrace = mrace;

* Drop non-US (Keep PR);
if mrterr = "AB" or mrterr = "AS" or mrterr = "BC" or mrterr = "FM" or mrterr = "GU" or mrterr = "MB" or 
	mrterr = "MH" or mrterr = "MP" or mrterr = "NB" or mrterr = "NL" or mrterr = "NT" or mrterr = "NS" or 
	mrterr = "NU" or mrterr = "ON" or mrterr = "PE" or mrterr = "PW" or mrterr = "QC" or mrterr = "SK" or 
	mrterr = "VI" or mrterr = "XX" or mrterr = "YT" or mrterr = "YY" or mrterr = "ZZ" then delete;
run;

/* 2013 *******************************************************************************************/
DATA h.births_2013 (drop = mrace);

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2013US.AllCnty.txt" LRECL = 20000;
/*attrib  revision     length=$1    label="Revision U,R";                         
attrib  dob_yy       length=4     label="Birth Year U,R";                               
attrib  mager9       length=3     label="Mother's Age Recode 9 U,R";            
attrib  mrterr       length=$2    label="Mother's Residence Territory/Posession U,R";
attrib  mrcnty       length=3     label="Mother's County of Residence U,R";               
attrib  restatus     length=3     label="Residence Status U,R";                 
attrib  mbrace       length=3     label="Mother's Bridged Race** R***";         
attrib  mrace        length=3     label="Mother's Race U";                      
attrib  umhisp       length=3     label="Mother's Hispanic Origin RFP 569=f_morigin U,R";
attrib  mracehisp    length=3     label="Mother's Race/Hispanic Origin RFP 569=f_morigin U,R";
attrib  mar          length=3     label="Mother's Marital Status U,R";          
attrib  meduc        length=3     label="Mother's Education RFP 571=f_meduc R"; 
attrib  dplural      length=3     label="Plurality U,R";                        
attrib  estgest      length=3     label="Obstetric/Clinical Gestation Est. RFP 573=f_clinest U,R";
attrib  combgest     length=3     label="Gestation - Detail in Weeks U,R"; 
attrib  obgest_flg   length=3     label="Clinical Estimate of Gestation Used Flag U,R";
attrib  gest_imp     length=3     label="Gestation Imputed Flag U,R";           
attrib  dbwt         length=4     label="Birth Weight - Detail in Grams U,R";   
*/

INPUT
@7    revision          $1. 
@15   dob_yy             4. 
@93   mager9             1. 
@109  mrterr            $2. 
@114  mrcnty             3. 
@138  restatus           1. 
@139  mbrace             2. 
@141  mrace              2. 
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1. 
@155  meduc              1. 
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2. 
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4. 
;

* Combine mbrace and mrace into mbrace;
if mbrace = . or mbrace = "" then mbrace = mrace;

* Drop non-US (Keep PR);
if mrterr = "AB" or mrterr = "AS" or mrterr = "BC" or mrterr = "FM" or mrterr = "GU" or mrterr = "MB" or 
	mrterr = "MH" or mrterr = "MP" or mrterr = "NB" or mrterr = "NL" or mrterr = "NT" or mrterr = "NS" or 
	mrterr = "NU" or mrterr = "ON" or mrterr = "PE" or mrterr = "PW" or mrterr = "QC" or mrterr = "SK" or 
	mrterr = "VI" or mrterr = "XX" or mrterr = "YT" or mrterr = "YY" or mrterr = "ZZ" then delete;
run;

/* 2014 *******************************************************************************************/
DATA h.births_2014;

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2014US.AllCnty.txt" LRECL = 20000;
/*                        
attrib  dob_yy       length=4     label="Birth Year U,R";                               
attrib  mager9       length=3     label="Mother's Age Recode 9 U,R"; 
attrib  mrterr       length=$2    label="Mother's Residence Territory/Posession U,R"; 
attrib  rcnty        length=3     label="Mother's County of Residence U,R";    
attrib  restatus     length=3     label="Residence Status U,R";     
attrib  mbrace       length=3     label="Mother's Bridged Race** R***";    
attrib  mhisp_r      length=3     label="Mother's Hispanic Origin Recode 0 Non-Hispanic";
attrib  mracehisp    length=3     label="Mother's Race/Hispanic Origin 1 Non-Hispanic White (only)";
attrib  dmar         length=3     label="Marital Status";  
attrib  meduc        length=3     label="Mother's Education 1 8th grade or less";
attrib  dplural      length=3     label="Plurality U,R";                         
attrib  compgst_imp  length=3     label="Combined Gestation Imputation Flag Blank Combined Gestation is not i";
attrib  obgest_flg   length=3     label="Obstetric Estimate of Gestation Used Flag Blank Obstetric Estimate is not u";
attrib  combgest     length=3     label="Combined Gestation - Detail in Weeks 17-47 17th through 47th week of";
attrib  oegest_comb  length=3     label="Obstetric Estimate Edited 17-47 Weeks of gestation";
*/

INPUT

@9    dob_yy             4. 
@79   mager9             1. 
@89   mrterr            $2. 
@91   rcnty				 3.
@104  restatus           1. 
@110  mbrace             1. 
@115  mhisp_r            1. 
@117  mracehisp          1. 
@120  dmar               1. 
@124  meduc              1. 
@454  dplural            1. 
@488  compgst_imp        1. 
@489  obgest_flg         1. 
@490  combgest           2. 
@499  oegest_comb        2. 
@504  dbwt               4. 
;

* Rename variables;
rename rcnty = mrcnty;
rename oegest_comb = estgest;
rename compgst_imp = gest_imp;
rename dmar = mar;
rename mhisp_r = umhisp;

* Add revision;
revision = "";

* Drop non-US (Keep PR);
if mrterr = "AB" or mrterr = "AS" or mrterr = "BC" or mrterr = "FM" or mrterr = "GU" or mrterr = "MB" or 
	mrterr = "MH" or mrterr = "MP" or mrterr = "NB" or mrterr = "NL" or mrterr = "NT" or mrterr = "NS" or 
	mrterr = "NU" or mrterr = "ON" or mrterr = "PE" or mrterr = "PW" or mrterr = "QC" or mrterr = "SK" or 
	mrterr = "VI" or mrterr = "XX" or mrterr = "YT" or mrterr = "YY" or mrterr = "ZZ" then delete;
run;

/* 2015 *******************************************************************************************/
DATA h.births_2015;

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2015US.AllCnty.txt" LRECL = 20000;
/*                       
attrib  dob_yy       length=4     label="Birth Year U,R";                               
attrib  mager9       length=3     label="Mother's Age Recode 9 U,R"; 
attrib  mrterr       length=$2    label="Mother's Residence Territory/Posession U,R"; 
attrib  rcnty       length=3     label="Mother's County of Residence U,R";    
attrib  restatus     length=3     label="Residence Status U,R";     
attrib  mbrace       length=3     label="Mother's Bridged Race** R***";    
attrib  mhisp_r      length=3     label="Mother's Hispanic Origin Recode 0 Non-Hispanic";
attrib  mracehisp    length=3     label="Mother's Race/Hispanic Origin 1 Non-Hispanic White (only)";
attrib  dmar         length=3     label="Marital Status";  
attrib  meduc        length=3     label="Mother's Education 1 8th grade or less";
attrib  dplural      length=3     label="Plurality U,R";                         
attrib  compgst_imp  length=3     label="Combined Gestation Imputation Flag Blank Combined Gestation is not i";
attrib  obgest_flg   length=3     label="Obstetric Estimate of Gestation Used Flag Blank Obstetric Estimate is not u";
attrib  combgest     length=3     label="Combined Gestation - Detail in Weeks 17-47 17th through 47th week of";
attrib  oegest_comb  length=3     label="Obstetric Estimate Edited 17-47 Weeks of gestation";
*/

INPUT

@9    dob_yy             4. 
@79   mager9             1. 
@89   mrterr            $2. 
@91   rcnty				 3.
@104  restatus           1. 
@110  mbrace             1. 
@115  mhisp_r            1. 
@117  mracehisp          1. 
@120  dmar               1. 
@124  meduc              1. 
@454  dplural            1. 
@488  compgst_imp        1. 
@489  obgest_flg         1. 
@490  combgest           2. 
@499  oegest_comb        2. 
@504  dbwt               4.
;

* Rename variables;
rename rcnty = mrcnty;
rename oegest_comb = estgest;
rename compgst_imp = gest_imp;
rename dmar = mar;
rename mhisp_r = umhisp;

* Add revision;
revision = "";

* Drop non-US (Keep PR);
if mrterr = "AB" or mrterr = "AS" or mrterr = "BC" or mrterr = "FM" or mrterr = "GU" or mrterr = "MB" or 
	mrterr = "MH" or mrterr = "MP" or mrterr = "NB" or mrterr = "NL" or mrterr = "NT" or mrterr = "NS" or 
	mrterr = "NU" or mrterr = "ON" or mrterr = "PE" or mrterr = "PW" or mrterr = "QC" or mrterr = "SK" or 
	mrterr = "VI" or mrterr = "XX" or mrterr = "YT" or mrterr = "YY" or mrterr = "ZZ" then delete;
run;

/* 2016 ******************************************************************************************
DATA h.births_2016 (keep = dob_yy mager9 mrterr rcnty restatus mbrace mhisp_r mracehisp 
							mar meduc dplural compgst_imp obgest_flg combgest oegest_comb dbwt);

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2016US.AllCnty.txt" LRECL = 20000;
/*                     
attrib  dob_yy       length=4     label="Birth Year U,R";                               
attrib  mager9       length=3     label="Mother's Age Recode 9 U,R"; 
attrib  mrterr       length=$2    label="Mother's Residence Territory/Posession U,R"; 
attrib  rcnty       length=3     label="Mother's County of Residence U,R";    
attrib  restatus     length=3     label="Residence Status U,R";     
attrib  mbrace       length=3     label="Mother's Bridged Race** R***";    
attrib  mhisp_r      length=3     label="Mother's Hispanic Origin Recode 0 Non-Hispanic";
attrib  mracehisp    length=3     label="Mother's Race/Hispanic Origin 1 Non-Hispanic White (only)";
attrib  dmar         length=3     label="Marital Status";  
attrib  meduc        length=3     label="Mother's Education 1 8th grade or less";
attrib  dplural      length=3     label="Plurality U,R";                         
attrib  compgst_imp  length=3     label="Combined Gestation Imputation Flag Blank Combined Gestation is not i";
attrib  obgest_flg   length=3     label="Obstetric Estimate of Gestation Used Flag Blank Obstetric Estimate is not u";
attrib  combgest     length=3     label="Combined Gestation - Detail in Weeks 17-47 17th through 47th week of";
attrib  oegest_comb  length=3     label="Obstetric Estimate Edited 17-47 Weeks of gestation";

INPUT

@9    dob_yy             4. 
@79   mager9             1. 
@89   mrterr            $2. 
@91   rcnty				 3.
@104  restatus           1. 
@110  mbrace             1. 
@115  mhisp_r            1. 
@117  mracehisp          1. 
@120  dmar               1. 
@124  meduc              1. 
@454  dplural            1. 
@488  compgst_imp        1. 
@489  obgest_flg         1. 
@490  combgest           2. 
@499  oegest_comb        2. 
@504  dbwt               4.
;

* Rename variables;
rename rcnty = mrcnty;
rename oegest_comb = estgest;
rename compgst_imp = gest_imp;
rename dmar = mar;

* Add revision;
revision = "";

* Drop non-US (Keep PR);
if mrterr = "AB" or mrterr = "AS" or mrterr = "BC" or mrterr = "FM" or mrterr = "GU" or mrterr = "MB" or 
	mrterr = "MH" or mrterr = "MP" or mrterr = "NB" or mrterr = "NL" or mrterr = "NT" or mrterr = "NS" or 
	mrterr = "NU" or mrterr = "ON" or mrterr = "PE" or mrterr = "PW" or mrterr = "QC" or mrterr = "SK" or 
	mrterr = "VI" or mrterr = "XX" or mrterr = "YT" or mrterr = "YY" then delete;
;
*/

/*
/* 2007 Territories ********************************************************************************
DATA births_2007_ps;

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2007PS.AllCnty.txt" LRECL = 20000;
/*attrib  revision     length=$1    label="Revision U,R";                         
attrib  dob_yy       length=4     label="Birth Year U,R";                               
attrib  mager9       length=3     label="Mother's Age Recode 9 U,R";            
attrib  mrterr       length=$2    label="Mother's Residence Territory/Posession U,R";
attrib  mrcnty       length=3     label="Mother's County of Residence U,R";               
attrib  restatus     length=3     label="Residence Status U,R";                 
attrib  mbrace       length=3     label="Mother's Bridged Race** R***";         
attrib  mrace        length=3     label="Mother's Race U";                      
attrib  umhisp       length=3     label="Mother's Hispanic Origin RFP 569=f_morigin U,R";
attrib  mracehisp    length=3     label="Mother's Race/Hispanic Origin RFP 569=f_morigin U,R";
attrib  mar          length=3     label="Mother's Marital Status U,R";          
attrib  mar_imp      length=3     label="Mother's Marital Status Imputed Flag U,R";
attrib  meduc        length=3     label="Mother's Education RFP 571=f_meduc R"; 
attrib  dplural      length=3     label="Plurality U,R";                        
attrib  estgest      length=3     label="Obstetric/Clinical Gestation Est. RFP 573=f_clinest U,R";
attrib  combgest     length=3     label="Gestation - Detail in Weeks U,R"; 
attrib  obgest_flg   length=3     label="Clinical Estimate of Gestation Used Flag U,R";
attrib  gest_imp     length=3     label="Gestation Imputed Flag U,R";           
attrib  dbwt         length=4     label="Birth Weight - Detail in Grams U,R";   

INPUT
@7    revision          $1. 
@15   dob_yy             4. 
@93   mager9             1. 
@109  mrterr            $2. 
@114  mrcntyfips         3. 
@138  restatus           1. 
@139  mbrace             2. 
@141  mrace              2. 
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1. 
@154  mar_imp            1. 
@155  meduc              1. 
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2.  
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4. 
;

* Drop non-US (Keep PR);
if mrterr = "AB" or mrterr = "AS" or mrterr = "BC" or mrterr = "FM" or mrterr = "GU" or mrterr = "MB" or 
	mrterr = "MH" or mrterr = "MP" or mrterr = "NB" or mrterr = "NL" or mrterr = "NT" or mrterr = "NS" or 
	mrterr = "NU" or mrterr = "ON" or mrterr = "PE" or mrterr = "PW" or mrterr = "QC" or mrterr = "SK" or 
	mrterr = "VI" or mrterr = "XX" or mrterr = "YT" or mrterr = "YY" or mrterr = "ZZ" then delete;
;
*/



/* Combine dataset and clean ****************************************************************************************/
data allbirths;
	set h.births_2007;
run;

proc append base=allbirths data=h.births_2008;
run;
proc append base=allbirths data=h.births_2009;
run;
proc append base=allbirths data=h.births_2010;
run;
proc append base=allbirths data=h.births_2011;
run;
proc append base=allbirths data=h.births_2012;
run;
proc append base=allbirths data=h.births_2013;
run;
proc append base=allbirths data=h.births_2014;
run;
proc append base=allbirths data=h.births_2015;
run;

ods html close;
ods html;
/* Appended Dataset ****/
data h.allbirths_temp;
	set allbirths;
	if revision = "R" then revision = "";
	if mrterr = "ZZ" then delete;
run;

* Meduc by year, state, revision;
proc freq data = h.allbirths_temp;
	tables meduc*dob_yy*mrterr*revision/list missing;
run;

* mracehisp by year, state, revision;
proc freq data = h.allbirths_temp;
	tables mracehisp*dob_yy*mrterr*revision/list missing;
run;

* obgest_flg by year, state, revision;
proc freq data = h.allbirths_temp;
	tables obgest_flg*dob_yy*mrterr*revision/list missing;
run;


* Explore birthweight data;
proc freq data = h.allbirths_temp;
	tables estgest*combgest*obgest_flg/list missing;
run;

* Explore missingness by state and year and revision;
ods html close;
ods html;
proc freq data = h.allbirths_temp;
	tables mrterr revision dplural gest_imp mager9 mar mbrace meduc mracehisp mrcnty obgest_flg restatus umhisp/missing;
run;

proc means data = h.allbirths_temp n mean nmiss;
	var combgest dbwt estgest;
run;


* Meduc by year, state, revision;
proc freq data = h.allbirths_temp;
	tables meduc*dob_yy*mrterr*revision/list missing;
run;

* mracehisp by year, state, revision;
proc freq data = h.allbirths_temp;
	tables mracehisp*dob_yy*mrterr*revision/list missing;
run;

* obgest_flg by year, state, revision;
proc freq data = h.allbirths_temp;
	tables obgest_flg*dob_yy*mrterr*revision/list missing;
run;

* state by year by revision;
proc freq data = h.allbirths_temp;
	tables mrterr*dob_yy*revision/list missing;
run;


* state by year by revision;
proc freq data = h.allbirths_temp;
	tables revision * (mracehisp obgest_flg)/list missing;
run;



/* 
No revision values in 2014 or 2015
Missing values;
meduc 7264584 19.90%
mracehisp 200449 0.55%
obgest_flg 34416709 94.29%

* Missing values of OBGEST_FLG - Both A and S birth certificates, across all states, 2007-2015
* Missing values of MEDUC - all S birth certificates, across all states, 2007-2015
* Missing values of MRACEHISP - 39 states
2014 AL (2) AR (1) CA (13) CO (4) CT (35426) DC (4) DE (22) FL (36) GA (14)
	 HI (1) IL (1) IN (3) KS (1) KY (1) LA(1) MA (94) MD (17) ME (4) MI (6) MN (1)
	 MO (2) NC (15) NH (10) NJ (91098) NV (4) NY (3131) OH (7) OK (5) OR (2) PA (1364)
	 RI (87) SC (4) SD (1) TN (1) TX (19) VA (21) VT (2) WA (7) WV (3)

30 states (different 30)
2015 AL (1) AR (1) AZ (2) CA(5) CT (34912) DE (5) FL (20) GA (3) IL (2) MA (86) MD (4) ME (1) MN (1)
	 MO (2) MT (1) NC (5) NH (5) NJ (30980) NV (1) NY (2490) OH (2) PA (388) PR (1)
	 RI (78) TN (1) TX (3) VA (8) WA (1) WI (1) WV (2)
*/
data h.allbirths_rec (drop = MBRACE);

	set h.allbirths_temp;

	* Remove rest of world;
	if mrterr = "ZZ" then delete;

	***** Create new variables **************;
	* Create BESTGEST Estimate (obstetric ESTGEST if available, otherwise COMBGEST);
	BESTGEST = .;
	if OBGEST_FLG = . then BESTGEST = COMBGEST;
	if ESTGEST > 0 and OBGEST_FLG = 1 then BESTGEST = ESTGEST;
	if BESTGEST = 99 then BESTGEST = .;

	* Recode singleton births to singleton or multiple;
	PLURALITY = .;
	if DPLURAL = 1 then PLURALITY = 0;
	if DPLURAL > 1 then PLURALITY = 1;
	
	* Recode maternal education to 3 category;
	* 1 = No HS/GED, 2 = GED but no college 3 = some college or higher;
	MEDUC_R = .;
	if MEDUC = 1 or MEDUC = 2 then MEDUC_R = 1;
	if MEDUC = 3 then MEDUC_R = 2;
	if MEDUC = 4 or MEDUCE = 5 or MEDUC = 6 or MEDUC = 7 or MEDUC = 8 then MEDUC_R = 3;

	* Pre-Term Birth (Following Monica Recode);
	PTB = 0;
	VPTB = 0;
	MPTB = 0;
	LPTB = 0;
	if BESTGEST >= 20 and BESTGEST < 37 then PTB = 1;
	if BESTGEST >= 20 and BESTGEST < 32 then VPTB = 1;
	if BESTGEST >= 32 and BESTGEST < 37 then MPTB = 1;
	if BESTGEST >= 34 and BESTGEST < 37 then LPTB = 1;

	* Revised Coding for MBRACE_R;
    * 1 = White, 2 = Black, # 3 = AI/AN, 4 = A/PI;
	MBRACE_R = .;
	if MBRACE = 1 or MBRACE = 21 then MBRACE_R = 1; *White (single or multiple);
    if MBRACE = 2 or MBRACE = 22 then MBRACE_R = 2;  *Black (single or multiple);
    if MBRACE = 3 or MBRACE = 23 then MBRACE_R = 3 ; *AI/AN (single or multiple);
    else if MBRACE > 0 then MBRACE_R = 4; * A/PI (single or multiple - MBRACE = (4:14, 24) | MRACE =(4:7, 18, 28, 38, 48, 58))]; 
	
	* Race/Hisp Recode (Adapted/Edited from 2014-2016 Natality Data Use File);
    * 1 = Hispanic, 2 = NHW, 3 = NHB, 4 = NHAI/AN, 5 = NHA/PI, 9 = UNK;
	RACEHISP_RECODE = .;
	if UMHISP = 1 or UMHISP = 2 or UMHISP = 3 or UMHISP = 4 or UMHISP = 5 then RACEHISP_RECODE = 1; * Hispanic;
    if UMHISP = 0 and MBRACE_R = 1 then RACEHISP_RECODE = 2; *Non-Hispanic White;
    if UMHISP = 0 and MBRACE_R = 2 then RACEHISP_RECODE = 3; *Non-Hispanic Black;
    if UMHISP = 0 and MBRACE_R = 3 then RACEHISP_RECODE = 4; *Non-Hispanic AI/AN;
	if UMHISP = 0 and MBRACE_R = 3 then RACEHISP_RECODE = 4; *Non-Hispanic A/PI;
    if UMHISP = 9 then RACEHISP_RECODE = 9; *Unknown/Not Stated
    
	* Assign State FIPS;
	STFIPS = "";
	if mrterr = "AL" then STFIPS ="01";
	if mrterr = "AK" then STFIPS ="02";
	if mrterr = "AZ" then STFIPS ="04";
	if mrterr = "AR" then STFIPS ="05";
	if mrterr = "CA" then STFIPS ="06";
	if mrterr = "CO" then STFIPS ="08";
	if mrterr = "CT" then STFIPS ="09";
	if mrterr = "DE" then STFIPS ="10";
	if mrterr = "DC" then STFIPS ="11";
	if mrterr = "FL" then STFIPS ="12";
	if mrterr = "GA" then STFIPS ="13";
	if mrterr = "HI" then STFIPS ="15";
	if mrterr = "ID" then STFIPS ="16";
	if mrterr = "IL" then STFIPS ="17";
	if mrterr = "IN" then STFIPS ="18";
	if mrterr = "IA" then STFIPS ="19";
	if mrterr = "KS" then STFIPS ="20";
	if mrterr = "KY" then STFIPS ="21";
	if mrterr = "LA" then STFIPS ="22";
	if mrterr = "ME" then STFIPS ="23";
	if mrterr = "MD" then STFIPS ="24";
	if mrterr = "MA" then STFIPS ="25";
	if mrterr = "MI" then STFIPS ="26";
	if mrterr = "MN" then STFIPS ="27";
	if mrterr = "MS" then STFIPS ="28";
	if mrterr = "MO" then STFIPS ="29";
	if mrterr = "MT" then STFIPS ="30";
	if mrterr = "NE" then STFIPS ="31";
	if mrterr = "NV" then STFIPS ="32";
	if mrterr = "NH" then STFIPS ="33";
	if mrterr = "NJ" then STFIPS ="34";
	if mrterr = "NM" then STFIPS ="35";
	if mrterr = "NY" then STFIPS ="36";
	if mrterr = "NC" then STFIPS ="37";
	if mrterr = "ND" then STFIPS ="38";
	if mrterr = "OH" then STFIPS ="39";
	if mrterr = "OK" then STFIPS ="40";
	if mrterr = "OR" then STFIPS ="41";
	if mrterr = "PA" then STFIPS ="42";
	if mrterr = "RI" then STFIPS ="44";
	if mrterr = "SC" then STFIPS ="45";
	if mrterr = "SD" then STFIPS ="46";
	if mrterr = "TN" then STFIPS ="47";
	if mrterr = "TX" then STFIPS ="48";
	if mrterr = "UT" then STFIPS ="49";
	if mrterr = "VT" then STFIPS ="50";
	if mrterr = "VA" then STFIPS ="51";
	if mrterr = "WA" then STFIPS ="53";
	if mrterr = "WV" then STFIPS ="54";
	if mrterr = "WI" then STFIPS ="55";
	if mrterr = "WY" then STFIPS ="56";
	if mrterr = "PR" then STFIPS ="72";
	
	* Fix County FIPS;
	TEMPCNTYFIPS = substr(compress(MRCNTY), 1, 3);
	CNTYFIPS_R = substr(("000"||TEMPCNTYFIPS), length(TEMPCNTYFIPS), 3);

	* Create combined FIPS variable;
	COMBFIPS = catx("", STFIPS, CNTYFIPS_R);

	***** Restrictions **************;
    * Remove any record with Birthweight < 500 grams;
    if DBWT >= 500 then delete;

	* Remove any record missing Plurality; 
	if DPLURAL = . then delete;

	* Remove any record missing county of residence;
	if COMBFIPS = "" then delete;

    * Remove any records with Gestational Age < 20 weeks;
    if BESTGEST < 20 then delete;
 
    * Remove any records missing maternal race/ethnicity;
	if RACEHISP_RECODE = . then delete;

	* Create variable labels;
	label PLURALITY = "Plurality (0 = Singleton)"
		  MBRACE_R = "Recoded Maternal Race: White, Black, AI/AN, A/PI"
		  RACEHISP_RECODE = "Recode Race/Ethnicity: Hisp, NHW, NHB, NHAI/AN, NHA/PI, UNK"
		  MEDUC_R = "Maternal Education Recode (No HS/GED, GED no college, Some college)"
		  BESTGEST = "Best Gestational Age Estimate (Clin/Obstet if available)"
		  PTB = "PTB (Weeks): [20, 37)"
		  VPTB = "VPTB(Weeks): [20, 32)"
		  LPTB = "LPTB (Weeks): [34, 37)"
		  MPTB = "MPTB (Weeks): [32, 37)";
		  COMBFIPS = "Combined FIPS";
run;

proc print data = h.allbirths_rec (obs = 100);
	var STFIPS MRCNTY TEMPCNTYFIPS CNTYFIPS_R;
run;

proc summary data = allbirths nway;
	class dob_yy;
	var ptb vptb lptb mptb;
	output out=test(n=count_identifer sum=sum_payout;
run;

proc print data=test;
run;

                               Obs    region    identifer    payout


                                1        1          3          30

                                2        2          4          70

                                3        3          3          50

                                4        4          2          30
