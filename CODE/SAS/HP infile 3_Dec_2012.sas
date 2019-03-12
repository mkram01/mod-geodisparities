
/* FOLLOWING CODE IS PRIMARILY FROM ASHLEY SCHEMPF WITH MINOR MODIFICATIONS */
/* Dec 3, 2012 -- modifying to get age-specific counts for D Chae analysis */

/* Oct 30, 2014 -- UPDATED WITH 2008 DATA */
/* June 4, 2015 -- UPDATING TO CLEAN weight/age and delete multiples*/

/*  NOTE : INSUFFICIENT MEMORY TO SORT DENOMINATOR FILE -- NEED TO REDO */
LIBNAME HP "H:\D_CHAE";
/* Denominator File */
%MACRO GET (PATH,FILE);
DATA &FILE._D;
INFILE "T:\EpiProjs\Kramer_projs\NCHS_BirthDeath_2003_2008\&PATH.\&FILE..DUSDENOM.PART2" LRECL=1500 PAD;
   INPUT

	 	@15   	YEAR			4.
		@89		MAGE			2.
     	@109   	STATE_AB    	$2.
		@114	CNTYFIPS		3.
     	@138  	RESTATUS        1.
		@149	MRACE			1.
		@423	DPLURAL			1.
		@446  	CLINGEST       	2. 
		@451	GESTEST			2.
		@456	CLINFLAG		1.
		@467  	DBIRWT         	4.;
					
IF RESTATUS EQ 4 THEN DELETE;
IF MRACE NOT IN (6,7) THEN DELETE;
IF DPLURAL ge 2 THEN DELETE;
%MEND;
%GET (Linked_File_2005,VS05LINK);
%GET (Linked_File_2006,VS06LINK);
%GET (Linked_File_2007,VS07LINK);
%GET (Linked_File_2008,VS08LINK);
run;

*proc print data=VS05LINK_D (obs=20);run;
DATA DENOMINATOR;
SET VS05LINK_D VS06LINK_D VS07LINK_D VS08LINK_D;
IF MRACE=6 THEN RACE=0;
IF MRACE=7 THEN RACE=1;
IF 12 le MAGE lt 18 then AGE=1;
else if 18 le MAGE lt 35 then AGE=2;
else if 35 le MAGE then AGE=3;
IF DBIRWT<9999 THEN DO;
IF 500<DBIRWT<2500 THEN LBW=1; ELSE LBW=0;
IF 500<DBIRWT<1500 THEN VLBW=1; ELSE VLBW=0;
END;
IF GESTEST<99 THEN DO;
IF 22<GESTEST<37 THEN PTB=1; ELSE PTB=0;
IF 22<GESTEST<32 THEN VPTB=1; ELSE VPTB=0;
IF 32<=GESTEST<37 THEN MPTB=1; ELSE MPTB=0;
IF GESTEST IN (32,33) THEN PTB32_33=1; ELSE PTB32_33=0;
IF 34<=GESTEST<=36 THEN LPTB=1; ELSE LPTB=0;
END;
RUN;
/* Data validation and check against published numbers */
/*PROC FREQ;
TABLE YEAR*RACE*(LBW VLBW PTB VPTB MPTB PTB32_33 LPTB)/MISSING;
RUN;*/


*proc print data=denominator (obs=20);run;
/* Numerator File */
%MACRO GET (PATH,FILE);
DATA &FILE._N;
INFILE "T:\EpiProjs\Kramer_projs\NCHS_BirthDeath_2003_2008\&PATH.\&FILE..DETAILUS.PART2" LRECL=1500 PAD;
   INPUT
		@89		MAGE			2.
		@109   	STATE_AB    	$2.
		@114	CNTYFIPS		3.
     	@138  	RESTATUS        1.
		@149	MRACE			1.
		@423	DPLURAL			1.
		@467  	DBIRWT         	4.
		@872	AGED			3.
		@893	WEIGHT			8.
		@1151	D_RESTATUS		1.
		@1164	D_STRESFIP		$2.
		@1166	D_COUNTY		3.
		@1188	YEAR			4.;
IF RESTATUS EQ 4 THEN DELETE;			
IF MRACE NOT IN (6,7) THEN DELETE;
IF DPLURAL ge 2 THEN DELETE;
%MEND;
%GET (Linked_File_2005,VS05LINK);
%GET (Linked_File_2006,VS06LINK);
%GET (Linked_File_2007,VS07LINK);
%GET (Linked_File_2008,VS08LINK);
run;
*%GET (VS05LINK);
*%GET (VS06LINK);
*%GET (VS07LINK);
run;

DATA NUMERATOR;
SET VS05LINK_N VS06LINK_N VS07LINK_N VS08LINK_N;
IF MRACE=6 THEN RACE=0;
IF MRACE=7 THEN RACE=1;
IF 12 le MAGE lt 18 then AGE=1;
else if 18 le MAGE lt 35 then AGE=2;
else if 35 le MAGE then AGE=3;

DEATH=1;
IF AGED<28 THEN NEONATAL=1; ELSE NEONATAL=0; 
IF AGED>=28 THEN POSTNEONATAL=1; ELSE POSTNEONATAL=0;
RUN;

/* Data validation and check against published numbers */
/*
PROC FREQ;
TABLE YEAR*RACE*(DEATH NEONATAL POSTNEONATAL);
WEIGHT WEIGHT;
RUN;
*proc print data=numerator(obs=20);run;*/

PROC IMPORT OUT= WORK.STATE_FIPS 
            DATAFILE= "T:\EpiProjs\KRAMER_GIS\SHAPEFILES\COUNTIES_2010\region_division_to_county_fips.xls" 
            DBMS=EXCEL REPLACE;
     RANGE="Sheet1$"; 
     GETNAMES=YES;
     MIXED=YES;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

*proc print data=STATE_FIPS;run;

* STATE AND COUNTY CODES;
PROC SORT DATA=DENOMINATOR;
BY STATE_AB;
RUN;
PROC SORT DATA=NUMERATOR;
BY STATE_AB;
RUN;
PROC SORT DATA=STATE_FIPS;
BY STATE_AB;
RUN;

DATA DENOM_GEO;
MERGE DENOMINATOR STATE_FIPS;
BY STATE_AB;
COUNTY=ST_FIPS*1000+CNTYFIPS;
COUNT=1;
RUN;
DATA NUM_GEO;
MERGE NUMERATOR STATE_FIPS;
BY STATE_AB;
COUNTY=ST_FIPS*1000+CNTYFIPS;
RUN;

*proc print data=denominator (obs=10);run;
*proc print data=denom_geo (obs=10);run;

* Creating Count data;
PROC SUMMARY NWAY DATA=DENOM_GEO;
VAR LBW VLBW PTB MPTB VPTB PTB32_33 LPTB;
CLASS ST_FIPS COUNTY RACE AGE;
OUTPUT OUT=BIRTH SUM=;
RUN;
PROC SUMMARY NWAY DATA=DENOM_GEO;
VAR COUNT LBW PTB;
CLASS ST_FIPS COUNTY RACE AGE;
OUTPUT OUT=COUNT N=;
RUN;
PROC SUMMARY NWAY DATA=NUM_GEO;
VAR DEATH NEONATAL POSTNEONATAL ;
CLASS ST_FIPS COUNTY RACE AGE;
OUTPUT OUT=DEATH SUM=;
WEIGHT WEIGHT;
RUN;
*proc print data=birth (obs=25);run;
proc freq data=birth;tables COUNTY;run;

	/* this dataset defines the 3141 legit counties for analysis */
PROC IMPORT OUT= WORK.COUNTY 
            DATAFILE= "T:\EpiProjs\KRAMER_GIS\SHAPEFILES\COUNTIES_2010\US_COUNTIES.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;
data county;set county;
ST_FIPS=substr(FIPS,1,2);
COUNTY=FIPS*1;
do i=1 to 2;
	do j=1 to 3;
		RACE=i-1;
		AGE=j;
		output;
	end;
end;
keep COUNTY ST_FIPS POP2000 RACE BLACK AGE;
run;
*proc print data=county (obs=10);run;

* Creating combined data set;
PROC SORT DATA= BIRTH;
BY ST_FIPS COUNTY RACE AGE;
RUN;
PROC SORT DATA= COUNT;
BY ST_FIPS COUNTY RACE AGE;
RUN;
PROC SORT DATA= DEATH;
BY ST_FIPS COUNTY RACE AGE;
RUN;
PROC SORT DATA=COUNTY;
BY ST_FIPS COUNTY RACE AGE;

DATA FULL;
MERGE BIRTH (DROP=_TYPE_ _FREQ_) COUNT (DROP=_TYPE_ _FREQ_ RENAME=(LBW=D_LBW PTB=D_PTB)) DEATH (DROP=_TYPE_ _FREQ_) COUNTY (in=yes);
BY ST_FIPS COUNTY RACE AGE;
  array vars{13} LBW VLBW PTB MPTB VPTB PTB32_33 LPTB COUNT D_LBW D_PTB DEATH NEONATAL POSTNEONATAL; /* define the array, i.e., group the variables x y z into array vars */ 
  do i =1  to 13;       /* repeat an action for each element of the array */ 
    if vars{i} = . then vars{i} = 0;  /* select individual elements of the array */ 
  end; 
DROP i;
if yes;
RUN;
*proc print data=full (obs=25);run;
/* The denominator is COUNT for infant mortality (death neonatal postneonatal)
   The denominator is D_LBW for LBW, VLBW
   The denominator is D_PTB for PTB, VPTB, MPTB, PTB32_33, LPTB */

/* Adding back in observations of zero black births */
/*PROC SUMMARY NWAY DATA=DENOM_GEO;*/
/*VAR COUNT;*/
/*CLASS ST_FIPS COUNTY;*/
/*OUTPUT OUT=TOTAL_BW SUM=;*/
/*RUN;*/
/**/
/*data noblack;*/
/*merge COUNT (DROP=_TYPE_ _FREQ_ LBW PTB) TOTAL_BW (DROP=_TYPE_ _FREQ_ RENAME=(COUNT=COUNT_BW));*/
/*BY ST_FIPS COUNTY;*/
/*if RACE=0;*/
/*if count NE count_bw then delete;*/
/*count=0;*/
/*RACE=1;*/
/*run;*/
/*DATA FULL2;*/
/*SET FULL NOBLACK;*/
/*  array vars{13} LBW VLBW PTB MPTB VPTB PTB32_33 LPTB COUNT D_LBW D_PTB DEATH NEONATAL POSTNEONATAL; /* define the array, i.e., group the variables x y z into array vars */ */
/*  do i =1  to 13;       /* repeat an action for each element of the array */ */
/*    if vars{i} = . then vars{i} = 0;  /* select individual elements of the array */ */
/*  end; */
/*DROP i count_bw;*/
/*RUN;*/
/*PROC SORT;*/
/*BY ST_FIPS COUNTY RACE;*/
/*RUN;*/


/* ADDING IN REGION AND DIVISION THEN MERGING WITH 2010 COUNTY AND LIMITING TO LEGITIMATE COUNTIES */


proc print data=county (obs=10);run;



PROC SORT DATA=state_fips;
by st_fips;
proc sort data=full;
by st_fips;
data full2;
merge STATE_FIPS  full;
by st_fips;
run;

proc print data=full2 (obs=10);run;
/* 
data hp.FULL_2005_2007;
set full2;
*/



data hp.FULL_2005_2008; * Note: This is updated with 2008 data;
set full2; run;
proc sort;
by st_fips county race;
run;

/*
PROC EXPORT DATA= HP.FULL_2005_2007 
            OUTFILE= "T:\EpiProjs\Kramer_projs\ASchempf\full_2005_2007.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
*

	/* WITH ADDITION OF 2008 DATE */
PROC EXPORT DATA= HP.FULL_2005_2008 
            OUTFILE= "T:\EpiProjs\Kramer_projs\ASchempf\full_2005_2008.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
