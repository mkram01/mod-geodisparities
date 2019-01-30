**************************************************;
* Code for reading in and cleaning births dataset ;
**************************************************;

*LIBNAME h "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS";
LIBNAME h "C:\Users\kweiss2\Documents\March of Dimes";


/* 2007 ******************************************************************************************/
DATA h.births_2007 (drop = mrace);

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2007US.AllCnty.txt" LRECL = 20000;

attrib  revision     length=$1    label="Revision U,R";                         
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
attrib  dmeduc       length=3     label="Mother's Education RFP 647=f_med U";   
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
@114  mrcnty             3. 
@138  restatus           1. 
@139  mbrace             2.
@141  mrace              2.
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1.  
@155  meduc              1.  
@156  dmeduc             2. 
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2.  
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4. 
;

* Combine mbrace and mrace into mbrace;
if mbrace = . or mbrace = "" then mbrace = mrace;

run;

/* 2008 ******************************************************************************************/
DATA h.births_2008 (drop = mrace);

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2008US.AllCnty.txt" LRECL = 20000;

attrib  revision     length=$1    label="Revision U,R";                         
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
attrib  dmeduc       length=3     label="Mother's Education RFP 647=f_med U";   
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
@114  mrcnty             3. 
@138  restatus           1. 
@139  mbrace             2.
@141  mrace              2.
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1.  
@155  meduc              1.  
@156  dmeduc             2. 
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2.  
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4. 
;

* Combine mbrace and mrace into mbrace;
if mbrace = . or mbrace = "" then mbrace = mrace;

run;

/* 2009 ******************************************************************************************/
DATA h.births_2009 (drop = mrace);

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2009US.AllCnty.txt" LRECL = 20000;

attrib  revision     length=$1    label="Revision U,R";                         
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
attrib  dmeduc       length=3     label="Mother's Education RFP 647=f_med U";   
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
@114  mrcnty             3. 
@138  restatus           1. 
@139  mbrace             2.
@141  mrace              2.
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1.  
@155  meduc              1.  
@156  dmeduc             2. 
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2.  
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4. 
;

* Combine mbrace and mrace into mbrace;
if mbrace = . or mbrace = "" then mbrace = mrace;

run;

/* 2010 *******************************************************************************************/
DATA h.births_2010 (drop = mrace);

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2010US.AllCnty.txt" LRECL = 20000;

attrib  revision     length=$1    label="Revision U,R";                         
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
attrib  dmeduc       length=3     label="Mother's Education RFP 647=f_med U";   
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
@114  mrcnty             3. 
@138  restatus           1. 
@139  mbrace             2.
@141  mrace              2.
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1.  
@155  meduc              1.  
@156  dmeduc             2. 
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2.  
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4. 
;

* Combine mbrace and mrace into mbrace;
if mbrace = . or mbrace = "" then mbrace = mrace;

run;

/* 2011 *******************************************************************************************/
DATA h.births_2011 (drop = mrace);

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2011US.AllCnty.txt" LRECL = 20000;

attrib  revision     length=$1    label="Revision U,R";                         
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
attrib  dmeduc       length=3     label="Mother's Education RFP 647=f_med U";   
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
@114  mrcnty             3. 
@138  restatus           1. 
@139  mbrace             2.
@141  mrace              2.
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1.  
@155  meduc              1.  
@156  dmeduc             2. 
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2.  
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4. 
;

* Combine mbrace and mrace into mbrace;
if mbrace = . or mbrace = "" then mbrace = mrace;

run;

/* 2012 *******************************************************************************************/
DATA h.births_2012 (drop = mrace);

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2012US.AllCnty.txt" LRECL = 20000;

attrib  revision     length=$1    label="Revision U,R";                         
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
attrib  dmeduc       length=3     label="Mother's Education RFP 647=f_med U";   
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
@114  mrcnty             3. 
@138  restatus           1. 
@139  mbrace             2.
@141  mrace              2.
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1.  
@155  meduc              1.  
@156  dmeduc             2. 
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2.  
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4. 
;

* Combine mbrace and mrace into mbrace;
if mbrace = . or mbrace = "" then mbrace = mrace;

run;

/* 2013 *******************************************************************************************/
DATA h.births_2013 (drop = mrace);

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2013US.AllCnty.txt" LRECL = 20000;

attrib  revision     length=$1    label="Revision U,R";                         
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
attrib  dmeduc       length=3     label="Mother's Education RFP 647=f_med U";   
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
@114  mrcnty             3. 
@138  restatus           1. 
@139  mbrace             2.
@141  mrace              2.
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1.  
@155  meduc              1.  
@156  dmeduc             2. 
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2.  
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4. 
;

* Combine mbrace and mrace into mbrace;
if mbrace = . or mbrace = "" then mbrace = mrace;

run;

/* 2014 *******************************************************************************************/
DATA h.births_2014;

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2014US.AllCnty.txt" LRECL = 20000;
                       
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

* Add revision and dmeduc;
revision = "";
dmeduc = .;

run;

/* 2015 *******************************************************************************************/
DATA h.births_2015;

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2015US.AllCnty.txt" LRECL = 20000;
                     
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
rename mhisp_r = umhisp;

* Add revision and dmeduc;
revision = "";
dmeduc = .;

run;


proc freq data = births_2015;
	tables mrterr /missing;
run;

proc freq data = births_2015;
	tables oegest_comb/missing;
run;

/* 2016 ****************************************************************************************** */
DATA h.births_2016;

INFILE "C:\Users\kweiss2\Box Sync\March of Dimes_ShareFolder\data\nchs_births\SAS\Input\NATL2016US.AllCnty.txt" LRECL = 20000;
                       
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
rename mhisp_r = umhisp;

* Add revision and dmeduc;
revision = "";
dmeduc = .;

run;

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
attrib  dmeduc       length=3     label="Mother's Education RFP 647=f_med U";   
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
@114  mrcnty             3. 
@138  restatus           1. 
@139  mbrace             2.
@141  mrace              2.
@148  umhisp             1. 
@149  mracehisp          1. 
@153  mar                1.  
@155  meduc              1.  
@156  dmeduc             2. 
@423  dplural            1. 
@446  estgest            2. 
@451  combgest           2.  
@456  obgest_flg         1. 
@457  gest_imp           1. 
@463  dbwt               4. 
;

* Drop non-territory values PR);
if mrterr != 'PR' then delete;
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
proc append base=allbirths data=h.births_2016;
run;

ods html close;
ods html;

/* Create raw appended Dataset in library****/
data h.allbirths_temp;
	set allbirths;
run;

/* Data exploration
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

* year by state by revision;
proc freq data = h.allbirths_temp;
	tables dob_yy*mrterr*revision/list missing;
run;

* Race/ethnicity variables;
proc freq data = h.allbirths_temp;
	tables mracehisp*umhisp*mbrace/list missing;
run;


proc freq data = h.allbirths_temp;
	tables meduc*mrterr*dob_yy / list missing;
run;
*/

data h.allbirths_rec;

	set h.allbirths_temp;

	attrib  revision    	length = $1   	label ="Revision U,R";                         
	attrib  dob_yy      	length = 4    	label ="Birth Year U,R";                               
	attrib  mager9      	length = 3    	label ="Mother's Age Recode 9 U,R";            
	
	attrib  restatus    	length = 3    	label ="Residence Status U,R";                 

	attrib  mbrace      	length = 3    	label ="Revised (Combined U Race and R Bridged Race)";                 
	attrib  umhisp      	length = 3    	label ="Mother's Hispanic Origin RFP 569=f_morigin U,R";
	attrib  mracehisp   	length = 3    	label ="Mother's Race/Hispanic Origin RFP 569=f_morigin U,R";
	attrib 	mbrace_r 		length = 3		label = "Recoded Maternal Race: White, Black, AI/AN, A/PI";
	attrib 	racehisp_recode length = 3		label = "Recode Race/Ethnicity: Hisp, NHW, NHB, NHAI/AN, NHA/PI, UNK";

	attrib  mar         	length = 3    	label ="Mother's Marital Status U,R";          

	attrib  meduc       	length = 3    	label ="Mother's Education RFP 571=f_meduc R"; 
	attrib  dmeduc      	length = 3    	label ="Mother's Education RFP 647=f_med U";   
	attrib 	meduc_r 		length = 3		label = "Maternal Education Recode (No HS/GED, GED no college, Some college)- R";

	attrib  dplural     	length = 3    	label ="Plurality U,R";
    attrib 	plurality 		length = 3		label = "Plurality (0 = Singleton)";
 
	attrib  dbwt        	length = 4		label="Birth Weight - Detail in Grams U,R";

	attrib  estgest     	length = 3    	label ="Obstetric/Clinical Gestation Est. RFP 573=f_clinest U,R";
	attrib  combgest    	length = 3    	label ="Gestation - Detail in Weeks U,R"; 
	attrib  bestgest 		length = 3		label = "Best Gestational Age Estimate (Clin/Obstet if available)";

	attrib 	ptb				length = 3		label = "PTB (Weeks): [20, 37)";
	attrib 	vptb			length = 3		label = "VPTB (Weeks): [20, 37)";
	attrib 	lptb			length = 3		label = "LPTB (Weeks): [34, 37)";
	attrib 	mptb			length = 3		label = "MPTB (Weeks): [32, 37)";

	attrib  mrterr      	length = $2   	label ="Mother's Residence Territory/Posession U,R";
	attrib  mrcnty      	length = 3    	label ="Mother's County of Residence U,R";  
	attrib  cntyfips_r     	length = $3    	label ="Character - Mother's County of Residence U,R";
	attrib 	stfips			length = $2		label = "State FIPS Code";
	attrib 	combfips		length = $5  	label = "Combined FIPS"; 

	* Remove rest of world;
	* Drop non-US (Keep PR);
	if mrterr = 'AB' or mrterr = 'AS' or mrterr = 'BC' or mrterr = 'FM' or mrterr = 'GU' or mrterr = 'MB' or 
		mrterr = 'MH' or mrterr = 'MP' or mrterr = 'NB' or mrterr = 'NL' or mrterr = 'NT' or mrterr = 'NS' or 
		mrterr = 'NU' or mrterr = 'ON' or mrterr = 'PE' or mrterr = 'PW' or mrterr = 'QC' or mrterr = 'SK' or 
		mrterr = 'VI' or mrterr = 'XX' or mrterr = 'YT' or mrterr = 'YY' or mrterr = 'ZZ' or mrterr = 'PR' then delete;
	* Removes 84,867 records;

	***** Create new variables **************;

	* Clean ESTGEST;
	if ESTGEST < 17 or ESTGEST > 47 then ESTGEST = .;

	* Clean COMBGEST;
	if COMBGEST > 47 then COMBGEST = .;

	* Create BESTGEST Estimate (obstetric ESTGEST if available, otherwise COMBGEST);
	BESTGEST = .;
	if ESTGEST > 17 then BESTGEST = ESTGEST;
	else BESTGEST = COMBGEST;

	* Recode singleton births to singleton or multiple;
	PLURALITY = .;
	if DPLURAL = 1 then PLURALITY = 0;
	if DPLURAL > 1 then PLURALITY = 1;
	
	* Recode maternal education to 3 category;
	* 1 = No HS/GED, 2 = GED but no college 3 = some college or higher;
	MEDUC_R = .;
	if MEDUC = 1 or MEDUC = 2 then MEDUC_R = 1;
	if MEDUC = 3 then MEDUC_R = 2;
	if MEDUC = 4 or MEDUC = 5 or MEDUC = 6 or MEDUC = 7 or MEDUC = 8 then MEDUC_R = 3;

	* Pre-Term Birth (Following Monica Recode) - using clinical estimate;
	PTB = 0;
	VPTB = 0;
	MPTB = 0;
	LPTB = 0;
	if ESTGEST >= 20 and ESTGEST < 37 then PTB = 1;
	if ESTGEST >= 20 and ESTGEST < 32 then VPTB = 1;
	if ESTGEST >= 32 and ESTGEST < 37 then MPTB = 1;
	if ESTGEST >= 34 and ESTGEST < 37 then LPTB = 1;

	* Revised Coding for MBRACE_R;
    * 1 = White, 2 = Black, # 3 = AI/AN, 4 = A/PI;
	MBRACE_R = .;
	if MBRACE = 1 or MBRACE = 21 then MBRACE_R = 1; *White (single or multiple);
    else if MBRACE = 2 or MBRACE = 22 then MBRACE_R = 2;  *Black (single or multiple);
    else if MBRACE = 3 or MBRACE = 23 then MBRACE_R = 3 ; *AI/AN (single or multiple);
    else if MBRACE > 0 then MBRACE_R = 4; * A/PI (single or multiple - MBRACE = (4:14, 24) | MRACE =(4:7, 18, 28, 38, 48, 58))]; 
	
	* Note, if including Puerto Rico, re-examine race and ethnicity variables

	* Race/Hisp Recode (Adapted/Edited from 2014-2016 Natality Data Use File);
    * 1 = Hispanic, 2 = NHW, 3 = NHB, 4 = NHAI/AN, 5 = NHA/PI, 9 = UNK;
	RACEHISP_RECODE = .;
	if UMHISP = 1 or UMHISP = 2 or UMHISP = 3 or UMHISP = 4 or UMHISP = 5 then RACEHISP_RECODE = 1; * Hispanic;
    else if UMHISP = 0 and MBRACE_R = 1 then RACEHISP_RECODE = 2; *Non-Hispanic White;
    else if UMHISP = 0 and MBRACE_R = 2 then RACEHISP_RECODE = 3; *Non-Hispanic Black;
    else if UMHISP = 0 and MBRACE_R = 3 then RACEHISP_RECODE = 4; *Non-Hispanic AI/AN;
	else if UMHISP = 0 and MBRACE_R = 4 then RACEHISP_RECODE = 5; *Non-Hispanic A/PI;
    else if UMHISP = 9 then RACEHISP_RECODE = 9; *Unknown/Not Stated;

	***** Restrictions **************;
    * Remove any record with Birthweight < 500 grams;
    if DBWT < 500 then delete;
	* Removes 61,109 records

	* Remove any records with Gestational Age < 20 weeks;
    if ESTGEST < 20 then delete;
	* Removes 98,139 records;

	* Remove any record missing Plurality; 
	if DPLURAL = . then delete;
	* Removes 0 records;

    * Remove any records missing maternal race/ethnicity;
	if RACEHISP_RECODE = . then delete;
	* Removes 0 records, but 297,728 with unknown ethnicity;

	* Assign State FIPS;
	if mrterr = 'AL' then STFIPS ='01';
	if mrterr = 'AK' then STFIPS ='02';
	if mrterr = 'AZ' then STFIPS ='04';
	if mrterr = 'AR' then STFIPS ='05';
	if mrterr = 'CA' then STFIPS ='06';
	if mrterr = 'CO' then STFIPS ='08';
	if mrterr = 'CT' then STFIPS ='09';
	if mrterr = 'DE' then STFIPS ='10';
	if mrterr = 'DC' then STFIPS ='11';
	if mrterr = 'FL' then STFIPS ='12';
	if mrterr = 'GA' then STFIPS ='13';
	if mrterr = 'HI' then STFIPS ='15';
	if mrterr = 'ID' then STFIPS ='16';
	if mrterr = 'IL' then STFIPS ='17';
	if mrterr = 'IN' then STFIPS ='18';
	if mrterr = 'IA' then STFIPS ='19';
	if mrterr = 'KS' then STFIPS ='20';
	if mrterr = 'KY' then STFIPS ='21';
	if mrterr = 'LA' then STFIPS ='22';
	if mrterr = 'ME' then STFIPS ='23';
	if mrterr = 'MD' then STFIPS ='24';
	if mrterr = 'MA' then STFIPS ='25';
	if mrterr = 'MI' then STFIPS ='26';
	if mrterr = 'MN' then STFIPS ='27';
	if mrterr = 'MS' then STFIPS ='28';
	if mrterr = 'MO' then STFIPS ='29';
	if mrterr = 'MT' then STFIPS ='30';
	if mrterr = 'NE' then STFIPS ='31';
	if mrterr = 'NV' then STFIPS ='32';
	if mrterr = 'NH' then STFIPS ='33';
	if mrterr = 'NJ' then STFIPS ='34';
	if mrterr = 'NM' then STFIPS ='35';
	if mrterr = 'NY' then STFIPS ='36';
	if mrterr = 'NC' then STFIPS ='37';
	if mrterr = 'ND' then STFIPS ='38';
	if mrterr = 'OH' then STFIPS ='39';
	if mrterr = 'OK' then STFIPS ='40';
	if mrterr = 'OR' then STFIPS ='41';
	if mrterr = 'PA' then STFIPS ='42';
	if mrterr = 'RI' then STFIPS ='44';
	if mrterr = 'SC' then STFIPS ='45';
	if mrterr = 'SD' then STFIPS ='46';
	if mrterr = 'TN' then STFIPS ='47';
	if mrterr = 'TX' then STFIPS ='48';
	if mrterr = 'UT' then STFIPS ='49';
	if mrterr = 'VT' then STFIPS ='50';
	if mrterr = 'VA' then STFIPS ='51';
	if mrterr = 'WA' then STFIPS ='53';
	if mrterr = 'WV' then STFIPS ='54';
	if mrterr = 'WI' then STFIPS ='55';
	if mrterr = 'WY' then STFIPS ='56';
	*if mrterr = 'PR' then STFIPS ='72';
	
	* Create new geo variables;
	mrcnty_char = put(mrcnty, 3.);
	CNTYFIPS_R = put(input(mrcnty_char, best4.), z3.);
	COMBFIPS = cats("", STFIPS, CNTYFIPS_R);

	* Remove any record missing county of residence;
	if COMBFIPS = '' then delete;

	* Drop temporary variables;
	drop mrcnty_char obgest_flg gest_imp;

run;


* Need to look for FIPS code issues;


* Create data subsets for models;
data singletons;
	set h.allbirths_rec;
	if plurality = 0 then delete;
run;

data singletonblackwhite;
	set h.allbirths_rec;
	* Delete plural records;
	if plurality = 1 then delete;
	* Delete all but NH-Black and NH-White;
	if racehisp_recode < 2 or racehisp_recode > 3 then delete; 
run;

data allblackwhite;
	set h.allbirths_rec;
	* Delete all but NH-Black and NH-White;
	if racehisp_recode < 2 or racehisp_recode > 3 then delete; 
run;

/* MODEL 1 
## YEAR x COUNTY x RACE restricted to SINGLETONS and NH-Black/NH-White */
proc summary data = singletonblackwhite nway;
	class dob_yy combfips racehisp_recode;
	where racehisp_recode = 2 or racehisp_recode = 3;
	where plurality = 0;
	var ptb vptb lptb mptb;
	output out = h.model1 (drop = _type_)
		   sum = ptb vptb lptb mptb;
run;

/* MODEL 2
## YEAR x COUNTY x RACE x AGE restricted to SINGLETONS */
proc summary data = singletons nway;
	class dob_yy combfips racehisp_recode mager9;
	where plurality = 0;
	var ptb vptb lptb mptb;
	output out = h.model2 (drop = _type_)
		   sum = ptb vptb lptb mptb;
run;

/* MODEL 3
## YEAR x COUNTY x RACE x AGE x MARITAL x EDUCATION restricted to SINGLETONS  */
proc summary data = singletons nway;
	class dob_yy combfips racehisp_recode mager9 mar meduc_r;
	var ptb vptb lptb mptb;
	output out = h.model3 (drop = _type_)
		   sum = ptb vptb lptb mptb;
run;

/* MODEL 4 
## YEAR x COUNTY x RACE restricted to NH-Black/NH-White */
proc summary data = allblackwhite nway;
	class dob_yy combfips racehisp_recode;
	var ptb vptb lptb mptb;
	output out = h.model4 (drop = _type_)
		   sum = ptb vptb lptb mptb;
run;

/* MODEL 5
## YEAR x COUNTY x RACE x AGE */
proc summary data = h.allbirths_rec nway;
	class dob_yy combfips racehisp_recode mager9;
	var ptb vptb lptb mptb;
	output out = h.model5 (drop = _type_)
		   sum = ptb vptb lptb mptb;
run;

/* MODEL 6
## YEAR x COUNTY x RACE x AGE x MARITAL x EDUCATION */
proc summary data = h.allbirths_rec nway;
	class dob_yy combfips racehisp_recode mager9 mar meduc_r;
	var ptb vptb lptb mptb;
	output out = h.model6 (drop = _type_)
		   sum = ptb vptb lptb mptb;
run;
