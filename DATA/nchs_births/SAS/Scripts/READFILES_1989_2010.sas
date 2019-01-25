
*************************************************************************************************
*************************************************************************************************
*	Title: READFILES_1989_2010.sas																*
*	Purpose: To read denominator and numerator files from NCHS and create a summary count file	*
*	Date created: Nov 19, 2016																	*
*	Date modified: Feb 14, 2017																	*
*	Author: Monica Shah																			*
*************************************************************************************************
*************************************************************************************************;


/* FOLLOWING CODE IS PRIMARILY FROM 
	T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\SAS\From Dr. Kramer\HP infile 3_Dec_2012.sas
	WITH MINOR MODIFICATIONS */

LIBNAME NCHS "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\Datasets";



/* Denominator File 2009-2010 */
%MACRO GET (PATH,FILE);
DATA &FILE._D;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..PART2.DUSDENOM" LRECL=1500 PAD;
   INPUT

	 	@15   	YEAR			4.
		@89		MAGE			2.
     	@109   	STATE_AB    	$2.
		@114	CNTYFIPS		3.
     	@138  	RESTATUS        1.
		@149	MRACE			1.
		@155	MEDUC			1.
		@156	MEDUCYRS		2.
		@423	DPLURAL			1.
		@446  	CLINGEST       	2. 
		@451	GESTEST			2.
		@456	CLINFLAG		1.
		@467  	DBIRWT         	4.;
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;

%GET (Linked_File_Birth_2009,VS09LKBC);
%GET (Linked_File_Birth_2010,VS10LKBC);
RUN;

/* 2008 named differently */
%MACRO GET (PATH,FILE);
DATA &FILE._D;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..DUSDENOM.PART2" LRECL=1500 PAD;
   INPUT

	 	@15   	YEAR			4.
		@89		MAGE			2.
     	@109   	STATE_AB    	$2.
		@114	CNTYFIPS		3.
     	@138  	RESTATUS        1.
		@149	MRACE			1.
		@155	MEDUC			1.
		@156	MEDUCYRS		2.
		@423	DPLURAL			1.
		@446  	CLINGEST       	2. 
		@451	GESTEST			2.
		@456	CLINFLAG		1.
		@467  	DBIRWT         	4.;
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;

%GET (Linked_File_Birth_2008,VS08LKBC);
RUN;


/* Denominator File 2011-2013
%MACRO GET (PATH,FILE);
DATA &FILE._D;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..DUSDENOM.PART2" LRECL=1500 PAD;
   INPUT

	 	@15   	YEAR			4.
		@89		MAGE			2.
     	@109   	STATE_AB    	$2.
		@114	CNTYFIPS		3.
     	@138  	RESTATUS        1.
		@149	MRACE			1.
		@155	MEDUC			1.
		@423	DPLURAL			1.
		@446  	CLINGEST       	2. 
		@451	GESTEST			2.
		@456	CLINFLAG		1.
		@467  	DBIRWT         	4.;
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;
%GET (Linked_File_2011,VS11LINK);
%GET (Linked_File_2012,VS12LINK);
%GET (Linked_File_2013,VS13LINK);
run;
*/

/* verify that either meduc OR meducyrs is available, but not both
PROC FREQ DATA = Vs10lkbc_d;
	TABLES MEDUC*MEDUCYRS / nocol norow nopercent;
RUN;	
*/

	DATA DENOMINATORb;
		SET Vs08lkbc_d Vs09lkbc_d Vs10lkbc_d;

		IF MRACE=6 THEN RACE=0; 			 *non hisp white;
		IF MRACE in (1,2,3,4,5) THEN RACE=1; *hispanic;
		IF MRACE=7 THEN RACE=2; 			 *non hisp black;
		IF MRACE=8 THEN RACE=3; 			 *non hisp other;
			/*
					1 Mexican
					2 Puerto Rican
					3 Cuban
					4 Central or South American
					5 Other and Unknown Hispanic
					6 Non-Hispanic White
					7 Non-Hispanic Black
					8 Non-Hispanic Other Races
					9 Origin unknown or not stated
			*/

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

		IF CLINGEST<99 THEN DO;
			IF 22<CLINGEST<37 THEN CPTB=1; ELSE CPTB=0;
			IF 22<CLINGEST<32 THEN CVPTB=1; ELSE CVPTB=0;
			IF 32<=CLINGEST<37 THEN CMPTB=1; ELSE CMPTB=0;
			IF CLINGEST IN (32,33) THEN CPTB32_33=1; ELSE CPTB32_33=0;
			IF 34<=CLINGEST<=36 THEN CLPTB=1; ELSE CLPTB=0;
		END;

		* Education - use years if available, or degree/completion of school otherwise;
		IF (MEDUCYRS = 99 or MEDUCYRS = .) AND (MEDUC = . OR MEDUC = 9) THEN EDU = .;
		ELSE IF 0<=MEDUCYRS<=11 OR MEDUC in (1,2) THEN EDU = 1;
		ELSE IF MEDUCYRS > 11 OR (MEDUC GE 3 AND MEDUC LE 8) THEN EDU = 0;
				/* Birth Certificate R
					1 8th grade or less
					2 9th through 12th grade with no diploma
					3 High school graduate or GED completed
					4 Some college credit, but not a degree.
					5 Associate degree (AA,AS)
					6 Bachelor’s degree (BA, AB, BS)
					7 Master’s degree (MA, MS, MEng, MEd, MSW, MBA)
					8 Doctorate (PhD, EdD) or Professional Degree (MD, DDS, DVM, LLB, JD)
					9 Unknown
					Blank Not on certificate

				 Birth Certificate U
					00 No formal education
					01-08 Years of elementary school
					09 1 year of high school
					10 2 years of high school
					11 3 years of high school
					12 4 years of high school
					13 1 year of college
					14 2 years of college
					15 3 years of college

				No counties with data for both R and U
				*/

		ST_FIPS=stfips (STATE_AB);

		PD = 2;

		LABEL EDU = "MOTHER'S EDUCATION: 0 - AT LEAST HS GRAD, 1 - < HS GRAD"
			  RACE = "MOTHER'S RACE: 0 - NON HISP WHITE, 1 - HISPANIC, 2 - NON HISP BLACK, 3 - NON HISP OTHER";


	RUN;



/* Denominator File 1989-1991 */
%MACRO GET (PATH,FILE);
DATA &FILE._D;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..DUSDENOM.PART2" LRECL=1500 PAD;
   INPUT
	 	@7   	YEAR			4.
		@11  	RESTATUS        1.
		@14  	STATECODE    	$2.
		@16 	CNTYFIPS		3.
		@30		MAGE			2.
		@34 	MRACE			1.
		@39 	MEDUCYRS		2.
		@87 	DPLURAL			1.
		@73 	GESTEST			2.
		@79  	DBIRWT         	4.;	
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;
%GET (Linked_File_Birth_1989,VS89LINK);
%GET (Linked_File_Birth_1990,VS90LINK);
%GET (Linked_File_Birth_1991,VS91LINK);
run;





	DATA DENOMINATORa;
		SET Vs89link_d Vs90link_d Vs91link_d;

		IF MRACE=6 THEN RACE=0; 			 *non hisp white;
		IF MRACE in (1,2,3,4,5) THEN RACE=1; *hispanic;
		IF MRACE=7 THEN RACE=2; 			 *non hisp black;
		IF MRACE=8 THEN RACE=3; 			 *non hisp other;
			/*
					1 Mexican
					2 Puerto Rican
					3 Cuban
					4 Central or South American
					5 Other and Unknown Hispanic
					6 Non-Hispanic White
					7 Non-Hispanic Black
					8 Non-Hispanic Other Races
					9 Origin unknown or not stated
			*/



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
		
		IF MEDUCYRS = 99 or MEDUCYRS = . THEN EDU = .;
		IF 0<=MEDUCYRS<=11 THEN EDU = 1;
		IF MEDUCYRS > 11 THEN EDU = 0;


		/*		
			Mother’s Education - Detail
			00 		NO formal education
			01-08 	Years of elementary school
			09		1 year of high school
			10		2 years of high school
			11		3 years of high school
			12		4 years of high school
			13		1 year of college
			14		2 years of college
			15		3 years of college
			16		4 years of college
			17		5 or more years of college
			99		Mother’s education not stated
		*/
		

		STATE_AB=fipstate(STATECODE);
		ST_FIPS=stfips (STATE_AB);

		* Fixing FIPS codes  - change in FIPS code for some counties;
		IF ST_FIPS = 51 AND CNTYFIPS = 730 THEN CNTYFIPS = 53; * 51730 to 51053;


		PD = 1;

			LABEL EDU = "MOTHER'S EDUCATION: 0 - AT LEAST HS GRAD, 1 - < HS GRAD"
				  RACE = "MOTHER'S RACE: 0 - NON HISP WHITE, 1 - HISPANIC, 2 - NON HISP BLACK, 3 - NON HISP OTHER";

		DROP STATECODE;

	RUN;






/* Numerator File 2011-2013 
%MACRO GET (PATH,FILE);
DATA &FILE._N;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..DETAILUS.PART2" LRECL=1500 PAD;
   INPUT
		@89		MAGE			2.
		@109   	STATE_AB    	$2.
		@114	CNTYFIPS		3.
     	@138  	RESTATUS        1.
		@149	MRACE			1.
		@155	MEDUC			1.
		@423	DPLURAL			1.
		@467  	DBIRWT         	4.
		@872	AGED			3.
		@1164	D_STRESFIP		$2.
		@1166	D_COUNTY		3.
		@1188	YEAR			4.
		@893	WEIGHT			8.
		@1186	D_RESTATUS		1.;
	

	IF RESTATUS EQ 4 THEN DELETE;			
	IF DPLURAL ge 2 THEN DELETE;
%MEND;
%GET (Linked_File_2011,VS11LINK);
%GET (Linked_File_2012,VS12LINK);
%GET (Linked_File_2013,VS13LINK);
run;
*/


/* Numerator File 2008-2010 */
%MACRO GET (PATH,FILE);
DATA &FILE._N;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..DETAILUS.PART2" LRECL=1500 PAD;
   INPUT
		@89		MAGE			2.
		@109   	STATE_AB    	$2.
		@114	CNTYFIPS		3.
     	@138  	RESTATUS        1.
		@149	MRACE			1.
		@155	MEDUC			1.
		@156	MEDUCYRS		2.
		@423	DPLURAL			1.
		@467  	DBIRWT         	4.
		@872	AGED			3.
		@30		D_STRESFIP		$2.
		@37		D_COUNTY		3.
		@1188	YEAR			4.
		@893	WEIGHT			8.
		@1186	D_RESTATUS		1.;
	

	IF RESTATUS EQ 4 THEN DELETE;			
	IF DPLURAL ge 2 THEN DELETE;
%MEND;
%GET (Linked_File_Birth_2008,VS08LKBC);
%GET (Linked_File_Birth_2009,VS09LKBC);
%GET (Linked_File_Birth_2010,VS10LKBC);
RUN;


	DATA NUMERATORb;
	SET Vs08lkbc_n Vs09lkbc_n Vs10lkbc_n;

		IF MRACE=6 THEN RACE=0; 			 *non hisp white;
		IF MRACE in (1,2,3,4,5) THEN RACE=1; *hispanic;
		IF MRACE=7 THEN RACE=2; 			 *non hisp black;
		IF MRACE=8 THEN RACE=3; 			 *non hisp other;

		IF 12 le MAGE lt 18 then AGE=1;
		else if 18 le MAGE lt 35 then AGE=2;
		else if 35 le MAGE then AGE=3;

		IF AGED<28 THEN NEON=1; ELSE NEON=0; 
		IF AGED>=28 THEN POSTN=1; ELSE POSTN=0;

		* Education - use years if available, or degree/completion of school otherwise;
		IF (MEDUCYRS = 99 or MEDUCYRS = .) AND (MEDUC = . OR MEDUC = 9) THEN EDU = .;
		ELSE IF 0<=MEDUCYRS<=11 OR MEDUC in (1,2) THEN EDU = 1;
		ELSE IF MEDUCYRS > 11 OR (MEDUC GE 3 AND MEDUC LE 8) THEN EDU = 0;
				/* Birth Certificate R
					1 8th grade or less
					2 9th through 12th grade with no diploma
					3 High school graduate or GED completed
					4 Some college credit, but not a degree.
					5 Associate degree (AA,AS)
					6 Bachelor’s degree (BA, AB, BS)
					7 Master’s degree (MA, MS, MEng, MEd, MSW, MBA)
					8 Doctorate (PhD, EdD) or Professional Degree (MD, DDS, DVM, LLB, JD)
					9 Unknown
					Blank Not on certificate

				 Birth Certificate U
					00 No formal education
					01-08 Years of elementary school
					09 1 year of high school
					10 2 years of high school
					11 3 years of high school
					12 4 years of high school
					13 1 year of college
					14 2 years of college
					15 3 years of college

				No counties with data for both R and U
				*/

		ST_FIPS=stfips (STATE_AB);

		PD = 2;

		LABEL EDU = "MOTHER'S EDUCATION: 0 - AT LEAST HS GRAD, 1 - < HS GRAD"
			  RACE = "MOTHER'S RACE: 0 - NON HISP WHITE, 1 - HISPANIC, 2 - NON HISP BLACK, 3 - NON HISP OTHER"
			  NEON = "NEONATAL"
			  POSTN = "POSTNATAL";

	RUN;





/* Numerator File 1989-1991 */
%MACRO GET (PATH,FILE);
DATA &FILE._N;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..DUSNUMER.PART2" LRECL=1500 PAD;
   INPUT

		@7   	YEAR			4.
		@11  	RESTATUS        1.
		@14  	STATECODE    	$2.
		@16 	CNTYFIPS		3.
		@30		MAGE			2.
		@34 	MRACE			1.
		@39 	MEDUCYRS		2.
		@87 	DPLURAL			1.
		@73 	GESTEST			2.
		@79  	DBIRWT         	4.	
		@213	AGED			3.				
		@506	D_STRESFIPCODE	2.
		@508	D_COUNTY		3.
		@505	D_RESTATUS		1.;

	IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
	IF DPLURAL GE 2 THEN DELETE;
%MEND;
%GET (Linked_File_Birth_1989,VS89LINK);
%GET (Linked_File_Birth_1990,VS90LINK);
%GET (Linked_File_Birth_1991,VS91LINK);
run;


	DATA NUMERATORa;
	SET Vs89link_n Vs90link_n Vs91link_n;

		IF MRACE=6 THEN RACE=0; 			 *non hisp white;
		IF MRACE in (1,2,3,4,5) THEN RACE=1; *hispanic;
		IF MRACE=7 THEN RACE=2; 			 *non hisp black;
		IF MRACE=8 THEN RACE=3; 			 *non hisp other;

		IF 12 le MAGE lt 18 then AGE=1;
		else if 18 le MAGE lt 35 then AGE=2;
		else if 35 le MAGE then AGE=3;



		IF AGED<28 THEN NEON=1; ELSE NEON=0; 
		IF AGED>=28 THEN POSTN=1; ELSE POSTN=0;

		IF MEDUCYRS = 99 or MEDUCYRS = . THEN EDU = .;
		IF 0<=MEDUCYRS<=11 THEN EDU = 1;
		IF MEDUCYRS > 11 THEN EDU = 0;


		/*		
			Mother’s Education - Detail
			00 		NO formal education
			01-08 	Years of elementary school
			09		1 year of high school
			10		2 years of high school
			11		3 years of high school
			12		4 years of high school
			13		1 year of college
			14		2 years of college
			15		3 years of college
			16		4 years of college
			17		5 or more years of college
			99		Mother’s education not stated
		*/

		STATE_AB=fipstate(STATECODE);
		ST_FIPS=stfips (STATE_AB);

		D_STRESFIP=fipstate(D_STRESFIPCODE);

		PD = 1;

			LABEL EDU = "MOTHER'S EDUCATION: 0 - AT LEAST HS GRAD, 1 - < HS GRAD"
				  RACE = "MOTHER'S RACE: 0 - NON HISP WHITE, 1 - HISPANIC, 2 - NON HISP BLACK, 3 - NON HISP OTHER"
				  NEON = "NEONATAL"
				  POSTN = "POSTNATAL";

		DROP STATECODE D_STRESFIPCODE;

	RUN;

	/* THESE COUNTIES ARE MISSING ALL DATA FROM EARLIER TIME PERIOD FILES - ADDING THEM HERE SO THAT THEY DON"T DROP DURING PROC SUMMARY 
DATA add; 
   INPUT PD ST_FIPS CNTYFIPS EDU RACE; 
   DATALINES; 
	1 5 25 . . 
	1 5 13 . . 
	1 28 55 . .  
	1 28 15 . .  
	1 55 78 . .  
	1 51 53 . .  
	1 51 97 . .  
	1 51 115 . . 
	; 
PROC PRINT; RUN;*/


DATA DENOMINATOR;
	SET DENOMINATORa DENOMINATORb;

	COUNT=1;
	COUNTY=ST_FIPS*1000+CNTYFIPS;

	IF ST_FIPS in (2,15) THEN DELETE; *drop alaska (02) and hawaii (15);


	DROP MEDUC MEDUCYRS;
RUN;

DATA NUMERATOR;
	SET NUMERATORa NUMERATORb;

	DEATH=1;
	COUNTY=ST_FIPS*1000+CNTYFIPS;

	IF ST_FIPS in (2,15) THEN DELETE; *drop alaska (02) and hawaii (15);

	DROP MEDUC MEDUCYRS;
RUN;




* Creating Count data;

PROC SUMMARY NWAY DATA=DENOMINATOR;
VAR VLBW LBW PTB;
CLASS COUNTY RACE EDU PD;
OUTPUT OUT=BIRTH SUM=;
RUN;

PROC SUMMARY NWAY DATA=DENOMINATOR;
VAR COUNT LBW PTB;
CLASS COUNTY RACE EDU PD;
OUTPUT OUT=COUNT N=;
RUN;

*In the denominator files (birth and count): 47,584 records with missings and 37,374 records with complete data for all strata (i.e. without missings);

	/*PROC SUMMARY NWAY DATA=DENOMINATOR;
	VAR COUNT;
	CLASS COUNTY RACE PD ;
	OUTPUT OUT=SUMEDU SUM=;
	RUN;

	PROC SUMMARY NWAY DATA=DENOMINATOR;
	VAR COUNT;
	CLASS COUNTY EDU PD ;
	OUTPUT OUT=SUMRACE SUM=;
	RUN;
	*/



PROC SUMMARY NWAY DATA=NUMERATOR;
VAR DEATH;
CLASS COUNTY RACE EDU PD;
OUTPUT OUT=DEATH SUM=;
*WEIGHT WEIGHT;
RUN;





* Creating combined data set;
PROC SORT DATA= BIRTH;
BY COUNTY RACE EDU PD;
RUN;
PROC SORT DATA= COUNT;
BY COUNTY RACE EDU PD;
RUN;
PROC SORT DATA= DEATH;
BY COUNTY RACE EDU PD;
RUN;




/*
DATA FULL;
MERGE BIRTH (DROP=_TYPE_ _FREQ_) COUNT (DROP=_TYPE_ _FREQ_ RENAME=(LBW=D_LBW PTB=D_PTB)) DEATH (DROP=_TYPE_ _FREQ_);
BY COUNTY RACE EDU PD;
  array vars{13} LBW VLBW PTB MPTB VPTB PTB32_33 LPTB COUNT D_LBW D_PTB DEATH NEON POSTN; * define the array, i.e., group the variables x y z into array vars; 
  do i =1  to 13;       * repeat an action for each element of the array; 
    if vars{i} = . then vars{i} = 0;  * select individual elements of the array; 
  end; 
DROP i;
RUN;
*/

DATA FULL;
MERGE BIRTH (DROP=_TYPE_ _FREQ_) COUNT (DROP=_TYPE_ _FREQ_ RENAME=(LBW=D_LBW PTB=D_PTB)) DEATH (DROP=_TYPE_ _FREQ_);
BY COUNTY RACE EDU PD;
  array vars{13} LBW VLBW PTB MPTB VPTB PTB32_33 LPTB COUNT D_LBW D_PTB DEATH NEONATAL POSTNEONATAL; /* define the array, i.e., group the variables x y z into array vars */ 
  do i =1  to 13;       /* repeat an action for each element of the array */ 
    if vars{i} = . then vars{i} = 0;  /* select individual elements of the array */ 
  end; 
DROP i;

KEEP COUNTY RACE EDU PD VLBW D_LBW;
RUN;
/* The denominator is COUNT for infant mortality (death neonatal postneonatal)
   The denominator is D_LBW for LBW, VLBW
   The denominator is D_PTB for PTB, VPTB, MPTB, PTB32_33, LPTB */

* Make permenant dataset;
DATA NCHS.FULL_LONG_89_10; 
	SET full; 
RUN;

