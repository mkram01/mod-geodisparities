
*************************************************************************************************
*************************************************************************************************
*	Title: READALLBIRTHS_1989_2013.sas															*
*	Purpose: To read denominator files from NCHS and create a summary count file				*
*	Date created: Sep 30, 2017																	*
*	Date modified: Jul 01, 2018 (adding 2014 and 2015)											*
*	Author: Monica Shah																			*
*************************************************************************************************
*************************************************************************************************;


/* FOLLOWING CODE IS PRIMARILY FROM 
	T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\SAS\From Dr. Kramer\HP infile 3_Dec_2012.sas
	WITH MINOR MODIFICATIONS */


/* DATA NOTES

https://www.cdc.gov/nchs/nvss/dvs_data_release.htm

A. Public-use micro-data file content:

The release of public-use data generally coincides with the publication, or follows soon after, of DVS final annual reports on births, deaths, fetal deaths, and linked birth/infant death. DVS also publishes preliminary reports, but these are based on incomplete data, do not constitute a final data file, and are not released as micro-data. Final report publication dates vary from year to year following receipt and processing of complete data from the states and other registration areas.

Over the years, confidentiality standards have changed for the public release of geographic and date details on vital statistics micro-data files. These changes are reflected in the data available in successive time periods, as follows:

1. Birth, death, and fetal death public-use micro-data files prior to 1989 contain all counties and exact dates (year, month, and day) of death.

2. Birth, death and fetal death public-use micro-data files for data years 1989 to 2004 contain only geographic identifiers of counties and cities with a 
population of 100,000 or greater, and no exact dates. For birth, death, and fetal death files, year, month, and day of week (e.g. Monday) are available.

3. Linked birth/infant death public-use micro-data files prior to 2005 contain geographic identifiers only for counties and cities with 
250,000 or greater population and no exact dates. Year, month and day of week (e.g. Monday) of birth/death are available.

4. Birth, death, fetal death and linked birth/infant death public-use micro-data files beginning with the 2005 data year will contain 
individual-level vital event data at the national level only, that is, with no geographic identifiers (no State, county, or city identifiers). 
These files for births, deaths, fetal deaths and linked birth/infant death will generally include most other items and detail from the vital record with the exception of exact dates. 
Year,month and day of week (e.g. Monday) are included. Items may vary from year to year.

5. The Matched Multiple Birth file combines data from the six years 1995 to 2000 but excludes all geographic identifiers and exact dates of births and deaths. 
The file also excludes year, month and day of week (e.g. Monday). A description of the file is viewable. The Matched Multiple Birth file for 1995-2000 and an 
earlier version for 1995-98 are available for downloading.

*/

LIBNAME NCHS "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\Datasets";



/* Denominator Files from Public Use Files 1983-1988 

%MACRO GET (PATH,FILE);
DATA &FILE._D;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..DAT" LRECL=1500 PAD;
   INPUT

	 	@2   	YEAR			4.
		@58		MAGE			2.
     	@27   	STFIPSc    		$2.
		@29		CNTYFIPSc		$3.
     	@11  	RESTATUS        1.
		@55		MRACEa			2.
		@57		MRACEb			1.
		@50		DPLURAL			1.
		@39		GESTEST			2.
		@43  	DBIRWT         	4.
		@62		MEDUCYRS		$2.;
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;

%GET (Public_Birth_1983,LinkCO83USden);
%GET (Public_Birth_1984,LinkCO84USden);
%GET (Public_Birth_1985,LinkCO85USden);
%GET (Public_Birth_1986,LinkCO86USden);
%GET (Public_Birth_1987,LinkCO87USden);
%GET (Public_Birth_1988,LinkCO88USden);
RUN;




	DATA DENOMINATORa;
		SET LinkCO83USden_d LinkCO84USden_d LinkCO85USden_d LinkCO86USden_d LinkCO87USden_d LinkCO88USden_d;


		
	************	RACE	*************;

			*		MRACEa
					1 Mexican
					2 Puerto Rican
					3 Cuban
					4 Central or South American
					5 Other and Unknown Hispanic
					...
					88 Origin or descent of Mother not reported
					99 Origin or descent of Mother not classifiable

					MRACEb
					1 White
					2 Black
					3 American Indian
					4 Chinese
					5 Japanese
					6 Hawaiian
					7 Filipino
					8 Other Asian or Pac Islander
					0 Other races
					9 Race of Mother not stated
			;

		* only 22 states reported hispanic origin;
		IF MRACEa in (0,1,2,3,4,5) THEN RACE=2; 				*hispanic;
		ELSE IF MRACEa in (88,99) AND MRACEb=1 THEN RACE=0; 	*non hisp white;
		ELSE IF MRACEa in (88,99) AND MRACEb=2 THEN RACE=1; 	*non hisp black;
		ELSE IF MRACEb NE 9 THEN RACE=3;						*non hisp other;


	************	AGE		*************;
		IF 12 le MAGE lt 18 then AGE=1;
		else if 18 le MAGE lt 35 then AGE=2;
		else if 35 le MAGE then AGE=3;


	************	BIRTH WEIGHT		*************;
		IF DBIRWT<9999 THEN DO;
			IF 500<DBIRWT<2500 THEN LBW=1; ELSE LBW=0;
			IF 500<DBIRWT<1500 THEN VLBW=1; ELSE VLBW=0;
		END;

	************	PRE TERM BIRTH		*************;
		IF GESTEST<99 THEN DO;
			IF 22<GESTEST<37 THEN PTB=1; ELSE PTB=0;
			IF 22<GESTEST<32 THEN VPTB=1; ELSE VPTB=0;
			IF 32<=GESTEST<37 THEN MPTB=1; ELSE MPTB=0;
			IF GESTEST IN (32,33) THEN PTB32_33=1; ELSE PTB32_33=0;
			IF 34<=GESTEST<=36 THEN LPTB=1; ELSE LPTB=0;
		END;
		
	************	MOTHER'S EDUCATION		*************;

		*		
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
		*;

		IF MEDUCYRS = "99" or MEDUCYRS = . THEN EDU = .;
		IF MEDUCYRS in ("00","01","02","03","04","05","06","07","08","09","10","11") THEN EDU = 1;
		IF MEDUCYRS in ("12","13","14","15","16","17") THEN EDU = 0;


	************	FIPS CODES		*************;	

		*convert character to numeric;
		IF substr(STFIPSc,1,1)="0" THEN ST_FIPS=substr(STFIPSc,2,1)*1;
		ELSE IF substr(STFIPSc,1,1) NE "0" THEN ST_FIPS=substr(STFIPSc,1,2)*1;

		IF CNTYFIPSc = "999" THEN CNTYFIPS=.;
		ELSE IF substr(CNTYFIPSc,1,2)="00" THEN CNTYFIPS=substr(CNTYFIPSc,3,1)*1;
		ELSE IF substr(CNTYFIPSc,1,1)="0" THEN CNTYFIPS=substr(CNTYFIPSc,2,2)*1;
		ELSE IF substr(CNTYFIPSc,1,1) NE "0" THEN CNTYFIPS=substr(CNTYFIPSc,1,3)*1;

		STATE_AB=fipstate(ST_FIPS);
		*ST_FIPS=stfips (STATE_AB);

		* Fixing FIPS codes  - change in FIPS code for some counties;
		IF ST_FIPS = 51 AND CNTYFIPS = 730 THEN CNTYFIPS = 53; * 51730 to 51053;

		LABEL EDU = "MOTHER'S EDUCATION: 0 - AT LEAST HS GRAD, 1 - < HS GRAD"
			  RACE = "MOTHER'S RACE: 0 - NON HISP WHITE,  1 - NON HISP BLACK, 2 - HISPANIC, 3 - NON HISP OTHER"
			  AGE = "MOTHER'S AGE CAT: 1 - 12 to <18 years, 2- 18 to <35 years, 3- 35+ years";


	RUN;
*/







/* Denominator File 1989-1991 */
%MACRO GET (PATH,FILE);
DATA &FILE._D;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..DUSDENOM.PART2" LRECL=1500 PAD;
   INPUT
	 	@7   	YEAR			4.
		@11  	RESTATUS        1.
		@19  	STFIPSc	    	$2.
		@21 	CNTYFIPS		3.
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

/*
PROC PRINT data = vs89link_d;
	WHERE STFIPSc = '40';
RUN;

PROC FREQ data = vs89link_d;
	TABLES MRACE;
	WHERE STFIPSc = '40';
RUN;

PROC FREQ data = vs90link_d;
	TABLES MRACE;
	WHERE STFIPSc = '40';
RUN;

PROC FREQ data = vs91link_d;
	TABLES MRACE;
	WHERE STFIPSc = '40';
RUN;
*/
	DATA DENOMINATORa;
		SET Vs89link_d Vs90link_d Vs91link_d ;


	************	RACE	*************;

		IF MRACE=6 THEN RACE=0; 			 *non hisp white;
		IF MRACE in (1,2,3,4,5) THEN RACE=2; *hispanic;
		IF MRACE=7 THEN RACE=1; 			 *non hisp black;
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


	************	AGE		*************;
		IF 12 le MAGE lt 18 then AGE=1;
		else if 18 le MAGE lt 35 then AGE=2;
		else if 35 le MAGE then AGE=3;


	************	BIRTH WEIGHT		*************;
		IF DBIRWT<9999 THEN DO;
			IF 500<DBIRWT<2500 THEN LBW=1; ELSE LBW=0;
			IF 500<DBIRWT<1500 THEN VLBW=1; ELSE VLBW=0;
		END;


	************	PRE TERM BIRTH		*************;
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


		************	MOTHER'S EDUCATION		*************;
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
		

		************	FIPS CODES		*************;	
		*convert character to numeric;
		IF substr(STFIPSc,1,1)="0" THEN ST_FIPS=substr(STFIPSc,2,1)*1;
		ELSE IF substr(STFIPSc,1,1) NE "0" THEN ST_FIPS=substr(STFIPSc,1,2)*1;

		STATE_AB=fipstate(ST_FIPS);
		*ST_FIPS=stfips (STATE_AB);

		
		IF CNTYFIPS = .  AND CNTYFIPSc = "999" THEN CNTYFIPS=.;
			ELSE IF CNTYFIPS = .  AND substr(CNTYFIPSc,1,2)="00" THEN CNTYFIPS=substr(CNTYFIPSc,3,1)*1;
			ELSE IF CNTYFIPS = .  AND substr(CNTYFIPSc,1,1)="0" THEN CNTYFIPS=substr(CNTYFIPSc,2,2)*1;
			ELSE IF CNTYFIPS = .  AND substr(CNTYFIPSc,1,1) NE "0" THEN CNTYFIPS=substr(CNTYFIPSc,1,3)*1;
	
		* Fixing FIPS codes  - change in FIPS code for some counties;
		IF ST_FIPS = 51 AND CNTYFIPS = 730 THEN CNTYFIPS = 53; * 51730 to 51053;



		LABEL EDU = "MOTHER'S EDUCATION: 0 - AT LEAST HS GRAD, 1 - < HS GRAD"
			  RACE = "MOTHER'S RACE: 0 - NON HISP WHITE,  1 - NON HISP BLACK, 2 - HISPANIC, 3 - NON HISP OTHER"
			  AGE = "MOTHER'S AGE CAT: 1 - 12 to <18 years, 2- 18 to <35 years, 3- 35+ years";


		DROP STFIPSc CNTYFIPSc MEDUCYRS GESTEST DBIRWT MAGE MRACE DPLURAL RESTATUS;
	RUN;



/* Denominator File 1992-1993  */
%MACRO GET (PATH,FILE);
DATA &FILE._D;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..pub" LRECL=1500 PAD;
   INPUT
	 	@1   	YEAR			4.
		@6  	RESTATUS        1.
		@42  	STFIPSc	    	$2.
		@44 	CNTYFIPSc		$3.
		@91		MAGE			2.
		@78 	MRACE			1.
		@83 	MEDUCYRS		2.
		@94 	DPLURAL			1.
		@183 	GESTEST			2.
		@193  	DBIRWT         	4.;	
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;
%GET (Public_Birth_1992,NATL1992);
%GET (Public_Birth_1993,NATL1993);
run;


/* Denominator File 1994  */
%MACRO GET (PATH,FILE);
DATA &FILE._D;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..us" LRECL=1500 PAD;
   INPUT
	 	@1   	YEAR			4.
		@6  	RESTATUS        1.
		@42  	STFIPSc	    	$2.
		@44 	CNTYFIPSc		$3.
		@91		MAGE			2.
		@78 	MRACE			1.
		@83 	MEDUCYRS		2.
		@94 	DPLURAL			1.
		@183 	GESTEST			2.
		@193  	DBIRWT         	4.;	
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;
%GET (Public_Birth_1994,Natl94pb);
run;



	DATA DENOMINATORb;
		SET NATL1992_d NATL1993_d  Natl94pb_d ;

	************	RACE	*************;

		IF MRACE=6 THEN RACE=0; 			 *non hisp white;
		IF MRACE in (1,2,3,4,5) THEN RACE=2; *hispanic;
		IF MRACE=7 THEN RACE=1; 			 *non hisp black;
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


	************	AGE		*************;
		IF 12 le MAGE lt 18 then AGE=1;
		else if 18 le MAGE lt 35 then AGE=2;
		else if 35 le MAGE then AGE=3;


	************	BIRTH WEIGHT		*************;
		IF DBIRWT<9999 THEN DO;
			IF 500<DBIRWT<2500 THEN LBW=1; ELSE LBW=0;
			IF 500<DBIRWT<1500 THEN VLBW=1; ELSE VLBW=0;
		END;


	************	PRE TERM BIRTH		*************;
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


		************	MOTHER'S EDUCATION		*************;
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
		

		************	FIPS CODES		*************;	
		*convert character to numeric;
		IF substr(STFIPSc,1,1)="0" THEN ST_FIPS=substr(STFIPSc,2,1)*1;
		ELSE IF substr(STFIPSc,1,1) NE "0" THEN ST_FIPS=substr(STFIPSc,1,2)*1;

		STATE_AB=fipstate(ST_FIPS);
		*ST_FIPS=stfips (STATE_AB);

		
		IF CNTYFIPS = .  AND CNTYFIPSc = "999" THEN CNTYFIPS=.;
			ELSE IF CNTYFIPS = .  AND substr(CNTYFIPSc,1,2)="00" THEN CNTYFIPS=substr(CNTYFIPSc,3,1)*1;
			ELSE IF CNTYFIPS = .  AND substr(CNTYFIPSc,1,1)="0" THEN CNTYFIPS=substr(CNTYFIPSc,2,2)*1;
			ELSE IF CNTYFIPS = .  AND substr(CNTYFIPSc,1,1) NE "0" THEN CNTYFIPS=substr(CNTYFIPSc,1,3)*1;
	
		* Fixing FIPS codes  - change in FIPS code for some counties;
		IF ST_FIPS = 51 AND CNTYFIPS = 730 THEN CNTYFIPS = 53; * 51730 to 51053;



		LABEL EDU = "MOTHER'S EDUCATION: 0 - AT LEAST HS GRAD, 1 - < HS GRAD"
			  RACE = "MOTHER'S RACE: 0 - NON HISP WHITE,  1 - NON HISP BLACK, 2 - HISPANIC, 3 - NON HISP OTHER"
			  AGE = "MOTHER'S AGE CAT: 1 - 12 to <18 years, 2- 18 to <35 years, 3- 35+ years";


		DROP STFIPSc CNTYFIPSc MEDUCYRS GESTEST DBIRWT MAGE MRACE DPLURAL RESTATUS;
	RUN;


/* Denominator File 1995-1997  */
%MACRO GET (PATH,FILE);
DATA &FILE._95D;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..dat" LRECL=1500 PAD;
   INPUT
	 	@7   	YEAR			4.
		@11  	RESTATUS        1.
		@19  	STFIPSc	    	$2.
		@21 	CNTYFIPSc		$3.
		@30		MAGE			2.
		@34 	MRACE			1.
		@39 	MEDUCYRS		2.
		@89 	DPLURAL			1.
		@74 	GESTEST			2.
		@81  	DBIRWT         	4.;	
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;
%GET (Linked_File_Birth_1995,Usdenp2);
run;


%MACRO GET (PATH,FILE);
DATA &FILE._96D;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..dat" LRECL=1500 PAD;
   INPUT
	 	@7   	YEAR			4.
		@11  	RESTATUS        1.
		@19  	STFIPSc	    	$2.
		@21 	CNTYFIPSc		$3.
		@30		MAGE			2.
		@34 	MRACE			1.
		@39 	MEDUCYRS		2.
		@89 	DPLURAL			1.
		@74 	GESTEST			2.
		@81  	DBIRWT         	4.;	
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;
%GET (Linked_File_Birth_1996,Usdenp2);
run;


%MACRO GET (PATH,FILE);
DATA &FILE._97D;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..dat" LRECL=1500 PAD;
   INPUT
	 	@7   	YEAR			4.
		@11  	RESTATUS        1.
		@19  	STFIPSc	    	$2.
		@21 	CNTYFIPSc		$3.
		@30		MAGE			2.
		@34 	MRACE			1.
		@39 	MEDUCYRS		2.
		@89 	DPLURAL			1.
		@74 	GESTEST			2.
		@81  	DBIRWT         	4.;	
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;
%GET (Linked_File_Birth_1997,Usdenp2);
run;



	DATA DENOMINATORc;
		SET Usdenp2_95d Usdenp2_96d Usdenp2_97d;
************	RACE	*************;

		IF MRACE=6 THEN RACE=0; 			 *non hisp white;
		IF MRACE in (1,2,3,4,5) THEN RACE=2; *hispanic;
		IF MRACE=7 THEN RACE=1; 			 *non hisp black;
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


	************	AGE		*************;
		IF 12 le MAGE lt 18 then AGE=1;
		else if 18 le MAGE lt 35 then AGE=2;
		else if 35 le MAGE then AGE=3;


	************	BIRTH WEIGHT		*************;
		IF DBIRWT<9999 THEN DO;
			IF 500<DBIRWT<2500 THEN LBW=1; ELSE LBW=0;
			IF 500<DBIRWT<1500 THEN VLBW=1; ELSE VLBW=0;
		END;


	************	PRE TERM BIRTH		*************;
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


		************	MOTHER'S EDUCATION		*************;
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
		

		************	FIPS CODES		*************;	
		*convert character to numeric;
		IF substr(STFIPSc,1,1)="0" THEN ST_FIPS=substr(STFIPSc,2,1)*1;
		ELSE IF substr(STFIPSc,1,1) NE "0" THEN ST_FIPS=substr(STFIPSc,1,2)*1;

		STATE_AB=fipstate(ST_FIPS);
		*ST_FIPS=stfips (STATE_AB);

		
		IF CNTYFIPS = .  AND CNTYFIPSc = "999" THEN CNTYFIPS=.;
			ELSE IF CNTYFIPS = .  AND substr(CNTYFIPSc,1,2)="00" THEN CNTYFIPS=substr(CNTYFIPSc,3,1)*1;
			ELSE IF CNTYFIPS = .  AND substr(CNTYFIPSc,1,1)="0" THEN CNTYFIPS=substr(CNTYFIPSc,2,2)*1;
			ELSE IF CNTYFIPS = .  AND substr(CNTYFIPSc,1,1) NE "0" THEN CNTYFIPS=substr(CNTYFIPSc,1,3)*1;
	
		* Fixing FIPS codes  - change in FIPS code for some counties;
		IF ST_FIPS = 51 AND CNTYFIPS = 730 THEN CNTYFIPS = 53; * 51730 to 51053;



		LABEL EDU = "MOTHER'S EDUCATION: 0 - AT LEAST HS GRAD, 1 - < HS GRAD"
			  RACE = "MOTHER'S RACE: 0 - NON HISP WHITE,  1 - NON HISP BLACK, 2 - HISPANIC, 3 - NON HISP OTHER"
			  AGE = "MOTHER'S AGE CAT: 1 - 12 to <18 years, 2- 18 to <35 years, 3- 35+ years";


		DROP STFIPSc CNTYFIPSc MEDUCYRS GESTEST DBIRWT MAGE MRACE DPLURAL RESTATUS;
	RUN;



/* Denominator File 1998  */
%MACRO GET (PATH,FILE);
DATA &FILE._98D;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..dat" LRECL=1500 PAD;
   INPUT
	 	@7   	YEAR			4.
		@11  	RESTATUS        1.
		@19  	STFIPSc	    	$2.
		@21 	CNTYFIPSc		$3.
		@30		MAGE			2.
		@34 	MRACE			1.
		@39 	MEDUCYRS		2.
		@89 	DPLURAL			1.
		@74 	GESTEST			2.
		@81  	DBIRWT         	4.;	
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;
%GET (Linked_File_Birth_1998,DEN98P2);
run;



/* Denominator File 1999  */
%MACRO GET (PATH,FILE);
DATA &FILE._99D;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..dat" LRECL=1500 PAD;
   INPUT
	 	@7   	YEAR			4.
		@11  	RESTATUS        1.
		@19  	STFIPSc	    	$2.
		@21 	CNTYFIPSc		$3.
		@30		MAGE			2.
		@34 	MRACE			1.
		@39 	MEDUCYRS		2.
		@89 	DPLURAL			1.
		@74 	GESTEST			2.
		@81  	DBIRWT         	4.;	
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;
%GET (Linked_File_Birth_1999,Den99co2);
run;


/* Denominator Files 2000-2002 */
%MACRO GET (PATH,FILE);
DATA &FILE._D;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..dat" LRECL=1500 PAD;
   INPUT
	 	@7   	YEAR			4.
		@11  	RESTATUS        1.
		@19  	STFIPSc	    	$2.
		@21 	CNTYFIPSc		$3.
		@30		MAGE			2.
		@34 	MRACE			1.
		@39 	MEDUCYRS		2.
		@89 	DPLURAL			1.
		@74 	GESTEST			2.
		@81  	DBIRWT         	4.;	
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;
%GET (Linked_File_Birth_2000,Usden00);
%GET (Linked_File_Birth_2001,Usden01);
%GET (Linked_File_Birth_2002,Usden02);
run;




	DATA DENOMINATORd;
		SET DEN98P2_98d Den99co2_99d Usden00_d Usden01_d Usden02_d;

	************	RACE	*************;

		IF MRACE=6 THEN RACE=0; 			 *non hisp white;
		IF MRACE in (1,2,3,4,5) THEN RACE=2; *hispanic;
		IF MRACE=7 THEN RACE=1; 			 *non hisp black;
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


	************	AGE		*************;
		IF 12 le MAGE lt 18 then AGE=1;
		else if 18 le MAGE lt 35 then AGE=2;
		else if 35 le MAGE then AGE=3;


	************	BIRTH WEIGHT		*************;
		IF DBIRWT<9999 THEN DO;
			IF 500<DBIRWT<2500 THEN LBW=1; ELSE LBW=0;
			IF 500<DBIRWT<1500 THEN VLBW=1; ELSE VLBW=0;
		END;


	************	PRE TERM BIRTH		*************;
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


		************	MOTHER'S EDUCATION		*************;
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
		

		************	FIPS CODES		*************;	
		*convert character to numeric;
		IF substr(STFIPSc,1,1)="0" THEN ST_FIPS=substr(STFIPSc,2,1)*1;
		ELSE IF substr(STFIPSc,1,1) NE "0" THEN ST_FIPS=substr(STFIPSc,1,2)*1;

		STATE_AB=fipstate(ST_FIPS);
		*ST_FIPS=stfips (STATE_AB);

		
		IF CNTYFIPS = .  AND CNTYFIPSc = "999" THEN CNTYFIPS=.;
			ELSE IF CNTYFIPS = .  AND substr(CNTYFIPSc,1,2)="00" THEN CNTYFIPS=substr(CNTYFIPSc,3,1)*1;
			ELSE IF CNTYFIPS = .  AND substr(CNTYFIPSc,1,1)="0" THEN CNTYFIPS=substr(CNTYFIPSc,2,2)*1;
			ELSE IF CNTYFIPS = .  AND substr(CNTYFIPSc,1,1) NE "0" THEN CNTYFIPS=substr(CNTYFIPSc,1,3)*1;
	
		* Fixing FIPS codes  - change in FIPS code for some counties;
		IF ST_FIPS = 51 AND CNTYFIPS = 730 THEN CNTYFIPS = 53; * 51730 to 51053;



		LABEL EDU = "MOTHER'S EDUCATION: 0 - AT LEAST HS GRAD, 1 - < HS GRAD"
			  RACE = "MOTHER'S RACE: 0 - NON HISP WHITE,  1 - NON HISP BLACK, 2 - HISPANIC, 3 - NON HISP OTHER"
			  AGE = "MOTHER'S AGE CAT: 1 - 12 to <18 years, 2- 18 to <35 years, 3- 35+ years";


		DROP STFIPSc CNTYFIPSc MEDUCYRS GESTEST DBIRWT MAGE MRACE DPLURAL RESTATUS;
	RUN;









/* Denominator File 2003-2004 */
%MACRO GET (PATH,FILE);
DATA &FILE._D;
INFILE "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\&PATH.\&FILE..dat" LRECL=1500 PAD;
   INPUT
	 	@15   	YEAR			4.
	    @109   	STATE_AB    	$2.
		@138  	RESTATUS        1.
		@114 	CNTYFIPS		3.
		@89		MAGE			2.
		@149 	MRACE			1.
		@155	MEDUC			1.
		@156 	MEDUCYRS		2.
		@423 	DPLURAL			1.
		@451 	GESTEST			2.
		@467  	DBIRWT         	4.;	
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;
%GET (Linked_File_Birth_2003,Usden03);
%GET (Linked_File_Period_2004,Usden04);
run;






/* Denominator File 2005,2006, 2008  */
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
		@156	MEDUCYRS		2.
		@155	MEDUC			1.
		@423	DPLURAL			1.
		@451	GESTEST			2.
		@467  	DBIRWT         	4.;
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;

%GET (Linked_File_Birth_2005,VS05LKBC);
%GET (Linked_File_Birth_2006,VS06LINK);
%GET (Linked_File_Birth_2008,VS08LKBC);
RUN;




	DATA DENOMINATORe;
		SET Usden03_d Usden04_d Vs05lkbc_d Vs06link_d VS08LKBC_d;


	************	RACE	*************;
		IF MRACE=6 THEN RACE=0; 			 *non hisp white;
		IF MRACE in (1,2,3,4,5) THEN RACE=2; *hispanic;
		IF MRACE=7 THEN RACE=1; 			 *non hisp black;
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


	************	AGE	*************;
		IF 12 le MAGE lt 18 then AGE=1;
		else if 18 le MAGE lt 35 then AGE=2;
		else if 35 le MAGE then AGE=3;


	************	BIRTH WEIGHT		*************;
		IF DBIRWT<9999 THEN DO;
			IF 500<DBIRWT<2500 THEN LBW=1; ELSE LBW=0;
			IF 500<DBIRWT<1500 THEN VLBW=1; ELSE VLBW=0;
		END;

		************	PRE TERM BIRTH		*************;
		IF GESTEST<99 THEN DO;
			IF 22<GESTEST<37 THEN PTB=1; ELSE PTB=0;
			IF 22<GESTEST<32 THEN VPTB=1; ELSE VPTB=0;
			IF 32<=GESTEST<37 THEN MPTB=1; ELSE MPTB=0;
			IF GESTEST IN (32,33) THEN PTB32_33=1; ELSE PTB32_33=0;
			IF 34<=GESTEST<=36 THEN LPTB=1; ELSE LPTB=0;
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


		************	FIPS CODES		*************;	
		
		ST_FIPS=stfips (STATE_AB);


	
		* Fixing FIPS codes  - change in FIPS code for some counties;
		IF ST_FIPS = 51 AND CNTYFIPS = 730 THEN CNTYFIPS = 53; * 51730 to 51053;




		LABEL EDU = "MOTHER'S EDUCATION: 0 - AT LEAST HS GRAD, 1 - < HS GRAD"
			  RACE = "MOTHER'S RACE: 0 - NON HISP WHITE,  1 - NON HISP BLACK, 2 - HISPANIC, 3 - NON HISP OTHER"
			  AGE = "MOTHER'S AGE CAT: 1 - 12 to <18 years, 2- 18 to <35 years, 3- 35+ years";


		DROP STFIPSc CNTYFIPSc MEDUCYRS MEDUC GESTEST DBIRWT MAGE MRACE DPLURAL RESTATUS;
	RUN;



/* Denominator File 2007, 2009-2010 */
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
		@451	GESTEST			2.
		@467  	DBIRWT         	4.;
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;
%GET (Linked_File_Birth_2007,VS07LKBC);
%GET (Linked_File_Birth_2009,VS09LKBC);
%GET (Linked_File_Birth_2010,VS10LKBC);
RUN;



/* Denominator File 2011-2013  */
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
		@156	MEDUCYRS		2.
		@155	MEDUC			1.
		@423	DPLURAL			1.
		@451	GESTEST			2.
		@467  	DBIRWT         	4.;
					
IF RESTATUS EQ 4 THEN DELETE; *restatus is used to exclude births to foreign residents;
IF DPLURAL ge 2 THEN DELETE;  *singleton births only;
%MEND;

%GET (Linked_File_Period_2011,VS11LINK);
%GET (Linked_File_Period_2012,VS12LINK);
%GET (Linked_File_Period_2013,VS13LINK);
RUN;



	DATA DENOMINATORf;
		SET Vs07lkbc_d Vs09lkbc_d Vs10lkbc_d VS11LINK_d VS12LINK_d VS13LINK_d;

	************	RACE	*************;
		IF MRACE=6 THEN RACE=0; 			 *non hisp white;
		IF MRACE in (1,2,3,4,5) THEN RACE=2; *hispanic;
		IF MRACE=7 THEN RACE=1; 			 *non hisp black;
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


	************	AGE	*************;
		IF 12 le MAGE lt 18 then AGE=1;
		else if 18 le MAGE lt 35 then AGE=2;
		else if 35 le MAGE then AGE=3;


	************	BIRTH WEIGHT		*************;
		IF DBIRWT<9999 THEN DO;
			IF 500<DBIRWT<2500 THEN LBW=1; ELSE LBW=0;
			IF 500<DBIRWT<1500 THEN VLBW=1; ELSE VLBW=0;
		END;

		************	PRE TERM BIRTH		*************;
		IF GESTEST<99 THEN DO;
			IF 22<GESTEST<37 THEN PTB=1; ELSE PTB=0;
			IF 22<GESTEST<32 THEN VPTB=1; ELSE VPTB=0;
			IF 32<=GESTEST<37 THEN MPTB=1; ELSE MPTB=0;
			IF GESTEST IN (32,33) THEN PTB32_33=1; ELSE PTB32_33=0;
			IF 34<=GESTEST<=36 THEN LPTB=1; ELSE LPTB=0;
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


		************	FIPS CODES		*************;	
		
		ST_FIPS=stfips (STATE_AB);


	
		* Fixing FIPS codes  - change in FIPS code for some counties;
		IF ST_FIPS = 51 AND CNTYFIPS = 730 THEN CNTYFIPS = 53; * 51730 to 51053;




		LABEL EDU = "MOTHER'S EDUCATION: 0 - AT LEAST HS GRAD, 1 - < HS GRAD"
			  RACE = "MOTHER'S RACE: 0 - NON HISP WHITE,  1 - NON HISP BLACK, 2 - HISPANIC, 3 - NON HISP OTHER"
			  AGE = "MOTHER'S AGE CAT: 1 - 12 to <18 years, 2- 18 to <35 years, 3- 35+ years";


		DROP STFIPSc CNTYFIPSc MEDUCYRS MEDUC GESTEST DBIRWT MAGE MRACE DPLURAL RESTATUS;
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




* added this block of code on July 1, 2018 - for 2014 and 2015 data;
/* Denominator File 2014-2015  
	1. Run code in folders to extract data from txt file first as a sas dataset
	2. Pulling relevent variables here
*/

LIBNAME nat2014 "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\BIRTHS_2014";

DATA births2014;
	SET nat2014.natl2014;

	IF RESTATUS NE 4; *non foreign resident births;
	IF DPLURAL = 1; *singleton births only;

	YEAR = dob_yy;
	MAGE = mager;
	STATE_AB = MRTERR;
	CNTYFIPS = RCNTY;
	DBIRWT = dbwt;
	MRACE = MRACEHISP;

	DROP dob_yy mager MRTERR RCNTY dbwt MRACEHISP DPLURAL RESTATUS;

RUN;


LIBNAME nat2015 "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\BIRTHS_2015";

DATA births2015;
	SET nat2015.natl2015;

	IF RESTATUS NE 4; *non foreign resident births;
	IF DPLURAL = 1; *singleton births only;

	YEAR = dob_yy;
	MAGE = mager;
	STATE_AB = MRTERR;
	CNTYFIPS = RCNTY;
	DBIRWT = dbwt;
	MRACE = MRACEHISP;

	DROP dob_yy mager MRTERR RCNTY dbwt MRACEHISP DPLURAL RESTATUS;

RUN;



DATA DENOMINATORg;
	SET births2014 births2015; 

		************	RACE	*************;
		IF MRACE=1 THEN RACE=0; 			 *non hisp white;
		IF MRACE=7 THEN RACE=2; *hispanic;
		IF MRACE=2 THEN RACE=1; 			 *non hisp black;
		IF MRACE in (3,4,5,6) THEN RACE=3; 			 *non hisp other;
			/*
			1 Non-Hispanic White (only)
			2 Non-Hispanic Black (only)
			3 Non-Hispanic AIAN (only)
			4 Non-Hispanic Asian (only)
			5 Non-Hispanic NHOPI (only)
			6 Non-Hispanic more than one race
			7 Hispanic
			8 Origin unknown or not stated
			*/


	************	AGE	*************;
		IF 12 le MAGE lt 18 then AGE=1;
		else if 18 le MAGE lt 35 then AGE=2;
		else if 35 le MAGE then AGE=3;


	************	BIRTH WEIGHT		*************;
		IF DBIRWT<9999 THEN DO;
			IF 500<DBIRWT<2500 THEN LBW=1; ELSE LBW=0;
			IF 500<DBIRWT<1500 THEN VLBW=1; ELSE VLBW=0;
		END;

	
		* Education - use years if available, or degree/completion of school otherwise;
		IF MEDUC = . OR MEDUC = 9 THEN EDU = .;
		ELSE IF MEDUC in (1,2) THEN EDU = 1;
		ELSE IF MEDUC GE 3 AND MEDUC LE 8 THEN EDU = 0;
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


		************	FIPS CODES		*************;	
		
		ST_FIPS=stfips (STATE_AB);


	
		* Fixing FIPS codes  - change in FIPS code for some counties;
		IF ST_FIPS = 51 AND CNTYFIPS = 730 THEN CNTYFIPS = 53; * 51730 to 51053;




		LABEL EDU = "MOTHER'S EDUCATION: 0 - AT LEAST HS GRAD, 1 - < HS GRAD"
			  RACE = "MOTHER'S RACE: 0 - NON HISP WHITE,  1 - NON HISP BLACK, 2 - HISPANIC, 3 - NON HISP OTHER"
			  AGE = "MOTHER'S AGE CAT: 1 - 12 to <18 years, 2- 18 to <35 years, 3- 35+ years";


		DROP MEDUC DBIRWT MAGE MRACE ;
RUN;



DATA nchs.DENOMINATOR;
	SET DENOMINATORa DENOMINATORb DENOMINATORc DENOMINATORd DENOMINATORe DENOMINATORf DENOMINATORg;

	COUNT=1;
	COUNTY=ST_FIPS*1000+CNTYFIPS;

	IF ST_FIPS in (2,15) THEN DELETE; *drop alaska (02) and hawaii (15);

	IF AGE = 1 THEN COUNTAGE1=1; ELSE COUNTAGE1=0;
	IF AGE = 3 THEN COUNTAGE2=1; ELSE COUNTAGE2=0;
	IF AGE = 2 THEN DO; COUNTAGE1=0; COUNTAGE2=0; END;

RUN;




* Creating Count data;

PROC SUMMARY NWAY DATA=nchs.DENOMINATOR;
VAR VLBW;
CLASS COUNTY YEAR RACE ;
OUTPUT OUT=BIRTHW SUM=;
RUN;

PROC SUMMARY NWAY DATA=nchs.DENOMINATOR;
VAR LBW;
CLASS COUNTY RACE YEAR;
OUTPUT OUT=COUNT N=;
RUN;

*want ecologicovariates that are from entire population (not race specific);
PROC SUMMARY NWAY DATA=nchs.DENOMINATOR;
VAR EDU COUNTAGE1 COUNTAGE2;
CLASS COUNTY YEAR ;
OUTPUT OUT=ECOLOGIC_N SUM=;
RUN;

PROC SUMMARY NWAY DATA=nchs.DENOMINATOR;
VAR COUNT;
CLASS COUNTY YEAR;
OUTPUT OUT=ECOLOGIC_D N=;
RUN;




* Creating combined data set;
PROC SORT DATA= BIRTHw;
BY COUNTY YEAR RACE;
RUN;
PROC SORT DATA= COUNT;
BY COUNTY YEAR RACE ;
RUN;


PROC SORT DATA= ECOLOGIC_N;
BY COUNTY YEAR;
RUN;
PROC SORT DATA= ECOLOGIC_D;
BY COUNTY YEAR ;
RUN;



DATA ECOLOGIC;
MERGE ECOLOGIC_N (DROP=_TYPE_ _FREQ_) ECOLOGIC_D (DROP=_TYPE_ _FREQ_);
BY COUNTY YEAR;
  array vars{3} EDU COUNTAGE1 COUNTAGE2; * define the array, i.e., group the variables x y z into array vars; 
  do i =1  to 3;       * repeat an action for each element of the array; 
    if vars{i} = . then vars{i} = 0;  * select individual elements of the array; 
  end; 
DROP i;

*creating covariates based on population proportion;
PctNoHS = (EDU/COUNT)*100;
PctLT18YRS = (COUNTAGE1/COUNT)*100;
PctGT35YRS = (COUNTAGE2/COUNT)*100;

RUN;


DATA FULL;
MERGE BIRTHW (DROP=_TYPE_ _FREQ_) COUNT (DROP=_TYPE_ _FREQ_ RENAME=(LBW=D_LBW)) ;
BY COUNTY YEAR RACE ;
  array vars{2} VLBW D_LBW ; /* define the array, i.e., group the variables x y z into array vars */ 
  do i =1  to 2;       /* repeat an action for each element of the array */ 
    if vars{i} = . then vars{i} = 0;  /* select individual elements of the array */ 
  end; 
DROP i;

KEEP COUNTY RACE YEAR VLBW D_LBW ;
RUN;
/* The denominator is COUNT for infant mortality (death neonatal postneonatal)
   The denominator is D_LBW for LBW, VLBW
   The denominator is D_PTB for PTB, VPTB, MPTB, PTB32_33, LPTB */

PROC SORT DATA= FULL;
BY COUNTY YEAR ;
RUN;

PROC SORT DATA= ECOLOGIC;
BY COUNTY YEAR ;
RUN;

DATA FULL2;
	MERGE FULL ECOLOGIC;
	BY COUNTY YEAR;
RUN;

* Make permenant dataset;
DATA NCHS.ST_VLBW_LONG_89_15; 
	SET full2; 
RUN;


	* subset - non hispanic whites, blacks and hispanic;
	* long to wide;
	DATA ST_black; 
		SET NCHS.ST_VLBW_LONG_89_15; 

		IF RACE = 1;

		VLBW_BL=VLBW;
		DEN_BL=D_LBW;

		DROP VLBW D_LBW RACE;

	RUN;

	DATA ST_white; 
		SET NCHS.ST_VLBW_LONG_89_15; 

		IF RACE = 0;

		VLBW_WH=VLBW;
		DEN_WH=D_LBW;

		DROP VLBW D_LBW RACE;

	RUN;


	DATA ST_hispanic; 
		SET NCHS.ST_VLBW_LONG_89_15; 

		IF RACE = 2;

		VLBW_HI=VLBW;
		DEN_HI=D_LBW;

		DROP VLBW D_LBW RACE;

	RUN;


	PROC SORT data = ST_black;
		BY COUNTY YEAR;
	PROC SORT data = ST_white;
		BY COUNTY YEAR;
	RUN;
	PROC SORT data = ST_hispanic;
		BY COUNTY YEAR;
	RUN;

	DATA nchs.ST_Wide_89_15;
		MERGE ST_black ST_white ST_hispanic;
		BY COUNTY YEAR;

		Length COUNTYc $5;

		*Recode zero or missing denominators;
		IF DEN_BL in (0,.) THEN DEN_BL = 1;
		IF DEN_WH in (0,.) THEN DEN_WH = 1;
		IF DEN_HI in (0,.) THEN DEN_HI = 1;

		* create log offset;
		ln_BL = log(DEN_BL);
		ln_WH = log(DEN_WH);
		ln_HI = log(DEN_HI);

		*Recode missing numerators b/c of Arc issues;
		IF VLBW_BL = . THEN VLBW_BL = 9999;
		IF VLBW_WH = . THEN VLBW_WH = 9999;
		IF VLBW_HI = . THEN VLBW_HI = 9999;
	
		* county change
		Dade County, Florida (12-025):
		Renamed as Miami-Dade County (12-086) effective July 22, 1997.;
		IF COUNTY = 12025 THEN COUNTY=12086; 

		* create character version of COUNTY;
		IF COUNTY = . THEN COUNTYc = '';
		IF COUNTY < 10000 THEN COUNTYc = COMPRESS("0"||COUNTY);
		IF COUNTY >= 10000 THEN COUNTYc = COUNTY; 

	RUN;



*Import dummy counties/years to ensure there is one row per county-year;
PROC IMPORT OUT= WORK.DUMMY 
            DATAFILE= "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\Datasets\DummyCountyYear_2014-15.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;



DATA dummy2;
	SET dummy;

		Length COUNTYc $5;
		
		* county change
		Dade County, Florida (12-025):
		Renamed as Miami-Dade County (12-086) effective July 22, 1997.;
		IF COUNTY = 12025 THEN COUNTY=12086; 

		* create character version of COUNTY;
		IF COUNTY < 10000 THEN COUNTYc = COMPRESS("0"||COUNTY);
		IF COUNTY >= 10000 THEN COUNTYc = COUNTY; 


RUN;


PROC SORT DATA = nchs.ST_Wide_89_15;
	BY YEAR COUNTY ;
PROC SORT DATA = dummy2;
	BY YEAR COUNTY ;

DATA nchs.ST_Wide2_89_15;
	MERGE nchs.ST_Wide_89_15 (in=a) dummy2 (in=b);
	BY YEAR COUNTY ;
	*IF a=1 and b=0;

		*Recode zero or missing denominators;
		IF DEN_BL in (0,.) THEN DEN_BL = 1;
		IF DEN_WH in (0,.) THEN DEN_WH = 1;
		IF DEN_HI in (0,.) THEN DEN_HI = 1;
RUN;
* 2 counties not present in original dummy list: 08-014 (became new county in 2001) [added in dummy list] and 12086 ;




* Merge with shapefile (removed 4 neighborless counties) and restrict to only counties in shapefile;
PROC IMPORT OUT= WORK.ShpNB 
            DATAFILE= "H:\RSPH PhD\RA2-Spatial\VLBW_Spatial\CARBayes\USA_Counties_ContiguousNB.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;


DATA ShpNB2;
	SET ShpNB;

	COUNTYc = FIPS;

	KEEP COUNTYc;
RUN; 


PROC SORT DATA = ShpNB2;
	BY COUNTYc;
PROC SORT DATA = nchs.ST_Wide2_89_15;
	BY COUNTYc;

DATA nchs.ST_Wide_shp_89_15;
	MERGE ShpNB2 (in=a) nchs.ST_Wide2_89_15 (in=b);
	BY COUNTYc;
	if a=1;
RUN;


/* Export as csv and read into R (create shapefile in R) */
	PROC EXPORT DATA= nchs.ST_Wide_shp_89_15 
            OUTFILE= "T:\epiprojs\Kramer_projs\NCHS_Birth_Data\InfantMortality1980_2014_Monica\Datasets\ST_Wide_shp_89_15.csv" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;
		


/* Export as csv and read into R (create shapefile in R) */
	PROC EXPORT DATA= nchs.ST_Wide_shp_89_15 
            OUTFILE= "H:\RSPH PhD\RA2-Spatial\VLBW_Spatial\CARBayes\ST_Wide_shp_89_15.csv" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;
