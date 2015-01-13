/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, January 13, 2015     TIME: 11:51:33 AM
PROJECT: TanZ_SAS_project_011315
PROJECT PATH: P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp
---------------------------------------- */

/* Library assignment for Local.ZDATA */
Libname ZDATA V9 'P:\QAC\qac200\students\ztan01\Working Projects' ;
/* Library assignment for Local.ZDATA */
Libname ZDATA V9 'P:\QAC\qac200\students\ztan01\Working Projects' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (ZDATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (ZDATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ZDATA  "P:\QAC\qac200\students\ztan01\Working Projects" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: INFULLYR    */
LIBNAME EC100028 "P:\QAC\qac200\students\ztan01\Working Projects";


%LET _CLIENTTASKLABEL='INFULLYR ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_2012__CAT_);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_2012__CAT_ AS 
   SELECT t1.DUPERSID, 
          t1.SEX, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EMPST31, 
          t1.ACTDTY31, 
          t1.ACTDTY42, 
          t1.ACTDTY53, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.INSCOV12, 
          t1.PREVCOVR, 
          t1.PRVEV12, 
          t1.CANCERDX, 
          t1.ERTOT12, 
          t1.TYPEPE42, 
          t1.CHBRON31, 
          t1.CHBRON53, 
          t1.STPRAT12, 
          t1.OTPAAT12, 
          t1.PUBAT12X, 
          t1.MCDAT12X, 
          t1.PRIVAT12, 
          t1.TRIAT12X, 
          t1.ADHECR42, 
          t1.PMEDPP31, 
          t1.PMEDPP42, 
          t1.PMEDPP53, 
          t1.TTLP12X, 
          t1.ADAPPT42, 
          t1.ADEXPL42, 
          t1.ADLIST42, 
          t1.ADTLHW42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADILWW42, 
          t1.ADRTWW42, 
          t1.ADFFRM42, 
          t1.ADRTCR42, 
          t1.ADSPEC42, 
          t1.ADFHLP42, 
          t1.ADNSMK42, 
          t1.ADEGMC42, 
          t1.ADSPRF42, 
          t1.ADILCR42, 
          t1.ADNDCR42, 
          t1.ADDPRS42, 
          t1.ADINTR42, 
          t1.PHQ242, 
          t1.ADDRBP42, 
          t1.ADHOPE42, 
          t1.ADNERV42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADWRTH42, 
          t1.ADEFRT42, 
          t1.K6SUM42, 
          t1.ADCAPE42, 
          t1.ADDOWN42, 
          t1.ADNRGY42, 
          t1.ADSOCA42, 
          t1.ADMALS42, 
          t1.ADPALS42, 
          t1.ADPAIN42, 
          t1.ADMWLM42, 
          t1.ADPWLM42, 
          t1.ADOVER42, 
          t1.ADSMOK42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADGENH42, 
          t1.ADCLIM42, 
          t1.ADDAYA42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.EMPHDX, 
          t1.LFTDIF31, 
          t1.LFTDIF53, 
          t1.PUBP12X, 
          t1.WLKDIF31, 
          t1.WLKDIF53, 
          t1.WLKLIM31, 
          t1.WLKLIM53, 
          t1.ADLANG42, 
          t1.ADRISK42, 
          t1.SFFLAG42, 
          t1.ADPRX42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.HEALTH_IN_GENERAL, 
          t1.HLTH_LIMIT_MOD_ACTIVITIES, 
          t1.HLTH_LIMIT_CLIMBING_STAIRS, 
          t1.'4WKS:ACCMP_LESS_B/C_PHYS_PRBS'n, 
          t1.' 4WKS:WORK_LIMT_B/C_PHY_PROBS'n, 
          t1.'4WKS:ACCMP LESS B/C MNT PRBS'n, 
          t1.'4WKS:WORK LIMT B/C MNT PROBS'n, 
          t1.'4WKS:PAIN LIMITS NORMAL WORK'n, 
          t1.'4WKS: FELT CALM/PEACEFUL'n, 
          t1.'4WKS: HAD A LOT OF ENERGY'n, 
          t1.'4WKS: FELT DOWNHEARTED/DEPR'n, 
          t1.'4WKS: HLTH STOPPED SOC ACTIV'n, 
          t1.'MARITAL STATUS'n, 
          t1.'MORE LIKELY TO TAKE RISKS'n, 
          t1.'# OF VISITS TO MED OFF FOR CARE'n, 
          t1.'HOW OFT EVYTNG NEEDED EFFT'n, 
          t1.'EASY GETTING MED CARE'n, 
          t1.'RATING OF HEALTH CARE'n, 
          t1.'GOT CARE WHEN NEEDED ILL/INJ'n, 
          t1.'INJ/ILL NEEDING IMMD CARE'n, 
          t1.'DO NOT NEED HEALTH INSURANCE'n, 
          t1.'INSURANCE NOT WORTH COST'n, 
          t1.'NEED ANY CARE, TEST, TREATMENT'n, 
          t1.'CANCER DIAGNOSIS'n, 
          t1.'CHRONIC BRONCHITS LAST 12 MONTHS'n, 
          t1.'CHRONIC BRONCHITIS LAST 12 MTHS'n, 
          t1.'ANGINA DIAGNOSIS'n, 
          t1.'ARTHRITIS DIAGNOSIS'n, 
          t1.'EMPHYSEMA DIAGNOSIS'n, 
          t1.'DIFFICULTY LIFTING 10 POUNDS'n, 
          t1.'DIFFICULTY LIFTING 10 POUNDS1'n, 
          t1.'DIFFICULTY WALKING 3 BLOCKS'n, 
          t1.'DIFFICULTY WALKING 3 BLOCKS1'n, 
          t1.'EMPLOYMENT STATUS R3/1'n, 
          t1.'EMPLOYMENT STATUS R4/2'n, 
          t1.'EMPLOYMENT STATUS R5/3'n, 
          t1.'EDUCATION RECODE'n, 
          t1.HEALTH_IN_GENERAL_RVSSCORE, 
          t1.PAIN_LMTS_NRM_WORK_RVSSCRD, 
          t1.'FELT_CALM/PEACEFUL_RVSSCRD'n, 
          t1.HAD_A_LOT_OF_ENRG_RVSSCRD, 
          t1.'HOW OFTEN RESTLESS'n, 
          t1.'SF-12 AGGREGATE SCORE'n, 
          t1.'RISK/RESTLESSNESS/ENERGY AGG'n, 
          t1.'RISK/RESTLESS/NRGY CAT'n, 
          t1.'Married or not currently married'n, 
          t1.'EDUCATION CAT'n, 
          /* INFULLYR */
            (1) LABEL="ER visits in the full year of 2012" AS INFULLYR
      FROM EC100028.meps_fullyr_2012__cat t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:51:03 AM
   By task: Data Set Attributes

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__CAT_
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForQUERY_FOR_MEPS_FU);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=WORK.QUERY_FOR_MEPS_FULLYR_2012__CAT_ OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsForQUERY_FOR_MEPS_FU(LABEL="Contents Details for QUERY_FOR_MEPS_FULLYR_2012__CAT_");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsForQUERY_FOR_MEPS_FU
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='QUERY_FOR_MEPS_FULLYR_2012__CAT_';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsForQUERY_FOR_MEPS_FU OUT=WORK.CONTContentsForQUERY_FOR_MEPS_FU;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsForQUERY_FOR_MEPS_FU
		WHERE memname='QUERY_FOR_MEPS_FULLYR_2012__CAT_';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: INER   */
LIBNAME EC100030 "P:\QAC\qac200\Data\MEPS";


%LET _CLIENTTASKLABEL='INER';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          /* INER */
            (1) AS INER
      FROM EC100030.meps_er_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Full outer merge   */
%LET _CLIENTTASKLABEL='Full outer merge';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(ZDATA.MEPS_MERGED);

PROC SQL;
   CREATE TABLE ZDATA.MEPS_MERGED(label="MEPS_MERGED") AS 
   SELECT t1.DUPERSID, 
          t1.SEX, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EMPST31, 
          t1.ACTDTY31, 
          t1.ACTDTY42, 
          t1.ACTDTY53, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.INSCOV12, 
          t1.PREVCOVR, 
          t1.PRVEV12, 
          t1.CANCERDX, 
          t1.ERTOT12, 
          t1.TYPEPE42, 
          t1.CHBRON31, 
          t2.DUID, 
          t2.PID, 
          t2.DUPERSID AS DUPERSID1, 
          t2.EVNTIDX, 
          t2.EVENTRN, 
          t2.ERHEVIDX, 
          t2.FFEEIDX, 
          t2.PANEL, 
          t2.MPCDATA, 
          t2.ERDATEYR, 
          t2.ERDATEMM, 
          t2.ERDATEDD, 
          t2.SEEDOC, 
          t2.VSTCTGRY, 
          t2.VSTRELCN, 
          t2.LABTEST, 
          t2.SONOGRAM, 
          t2.XRAYS, 
          t2.MAMMOG, 
          t2.MRI, 
          t2.EKG, 
          t2.EEG, 
          t2.RCVVAC, 
          t2.ANESTH, 
          t2.THRTSWAB, 
          t2.OTHSVCE, 
          t2.SURGPROC, 
          t2.MEDPRESC, 
          t2.ERICD1X, 
          t2.ERICD2X, 
          t2.ERICD3X, 
          t2.ERPRO1X, 
          t2.ERCCC1X, 
          t2.ERCCC2X, 
          t2.ERCCC3X, 
          t2.FFERTYPE, 
          t2.FFBEF12, 
          t2.ERXP12X, 
          t2.ERTC12X, 
          t2.ERFSF12X, 
          t2.ERFMR12X, 
          t2.ERFMD12X, 
          t2.ERFPV12X, 
          t2.ERFVA12X, 
          t2.ERFTR12X, 
          t2.ERFOF12X, 
          t2.ERFSL12X, 
          t2.ERFWC12X, 
          t2.ERFOR12X, 
          t2.ERFOU12X, 
          t2.ERFOT12X, 
          t2.ERFXP12X, 
          t2.ERFTC12X, 
          t2.ERDSF12X, 
          t2.ERDMR12X, 
          t2.ERDMD12X, 
          t2.ERDPV12X, 
          t2.ERDVA12X, 
          t2.ERDTR12X, 
          t2.ERDOF12X, 
          t2.ERDSL12X, 
          t2.ERDWC12X, 
          t2.ERDOR12X, 
          t2.ERDOU12X, 
          t2.ERDOT12X, 
          t2.ERDXP12X, 
          t2.ERDTC12X, 
          t2.IMPFLAG, 
          t2.PERWT12F, 
          t2.VARSTR, 
          t2.VARPSU, 
          t2.INER, 
          t1.CHBRON53, 
          t1.STPRAT12, 
          t1.OTPAAT12, 
          t1.PUBAT12X, 
          t1.MCDAT12X, 
          t1.PRIVAT12, 
          t1.TRIAT12X, 
          t1.ADHECR42, 
          t1.PMEDPP31, 
          t1.PMEDPP42, 
          t1.PMEDPP53, 
          t1.TTLP12X, 
          t1.ADAPPT42, 
          t1.ADEXPL42, 
          t1.ADLIST42, 
          t1.ADTLHW42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADILWW42, 
          t1.ADRTWW42, 
          t1.ADFFRM42, 
          t1.ADRTCR42, 
          t1.ADSPEC42, 
          t1.ADFHLP42, 
          t1.ADNSMK42, 
          t1.ADEGMC42, 
          t1.ADSPRF42, 
          t1.ADILCR42, 
          t1.ADNDCR42, 
          t1.ADDPRS42, 
          t1.ADINTR42, 
          t1.PHQ242, 
          t1.ADDRBP42, 
          t1.ADHOPE42, 
          t1.ADNERV42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADWRTH42, 
          t1.ADEFRT42, 
          t1.K6SUM42, 
          t1.ADCAPE42, 
          t1.ADDOWN42, 
          t1.ADNRGY42, 
          t1.ADSOCA42, 
          t1.ADMALS42, 
          t1.ADPALS42, 
          t1.ADPAIN42, 
          t1.ADMWLM42, 
          t1.ADPWLM42, 
          t1.ADOVER42, 
          t1.ADSMOK42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADGENH42, 
          t1.ADCLIM42, 
          t1.ADDAYA42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.EMPHDX, 
          t1.LFTDIF31, 
          t1.LFTDIF53, 
          t1.PUBP12X, 
          t1.WLKDIF31, 
          t1.WLKDIF53, 
          t1.WLKLIM31, 
          t1.WLKLIM53, 
          t1.ADLANG42, 
          t1.ADRISK42, 
          t1.SFFLAG42, 
          t1.ADPRX42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.HEALTH_IN_GENERAL, 
          t1.HLTH_LIMIT_MOD_ACTIVITIES, 
          t1.HLTH_LIMIT_CLIMBING_STAIRS, 
          t1.'4WKS:ACCMP_LESS_B/C_PHYS_PRBS'n, 
          t1.' 4WKS:WORK_LIMT_B/C_PHY_PROBS'n, 
          t1.'4WKS:ACCMP LESS B/C MNT PRBS'n, 
          t1.'4WKS:WORK LIMT B/C MNT PROBS'n, 
          t1.'4WKS:PAIN LIMITS NORMAL WORK'n, 
          t1.'4WKS: FELT CALM/PEACEFUL'n, 
          t1.'4WKS: HAD A LOT OF ENERGY'n, 
          t1.'4WKS: FELT DOWNHEARTED/DEPR'n, 
          t1.'4WKS: HLTH STOPPED SOC ACTIV'n, 
          t1.'MARITAL STATUS'n, 
          t1.'MORE LIKELY TO TAKE RISKS'n, 
          t1.'# OF VISITS TO MED OFF FOR CARE'n, 
          t1.'HOW OFT EVYTNG NEEDED EFFT'n, 
          t1.'EASY GETTING MED CARE'n, 
          t1.'RATING OF HEALTH CARE'n, 
          t1.'GOT CARE WHEN NEEDED ILL/INJ'n, 
          t1.'INJ/ILL NEEDING IMMD CARE'n, 
          t1.'DO NOT NEED HEALTH INSURANCE'n, 
          t1.'INSURANCE NOT WORTH COST'n, 
          t1.'NEED ANY CARE, TEST, TREATMENT'n, 
          t1.'CANCER DIAGNOSIS'n, 
          t1.'CHRONIC BRONCHITS LAST 12 MONTHS'n, 
          t1.'CHRONIC BRONCHITIS LAST 12 MTHS'n, 
          t1.'ANGINA DIAGNOSIS'n, 
          t1.'ARTHRITIS DIAGNOSIS'n, 
          t1.'EMPHYSEMA DIAGNOSIS'n, 
          t1.'DIFFICULTY LIFTING 10 POUNDS'n, 
          t1.'DIFFICULTY LIFTING 10 POUNDS1'n, 
          t1.'DIFFICULTY WALKING 3 BLOCKS'n, 
          t1.'DIFFICULTY WALKING 3 BLOCKS1'n, 
          t1.'EMPLOYMENT STATUS R3/1'n, 
          t1.'EMPLOYMENT STATUS R4/2'n, 
          t1.'EMPLOYMENT STATUS R5/3'n, 
          t1.'EDUCATION RECODE'n, 
          t1.HEALTH_IN_GENERAL_RVSSCORE, 
          t1.PAIN_LMTS_NRM_WORK_RVSSCRD, 
          t1.'FELT_CALM/PEACEFUL_RVSSCRD'n, 
          t1.HAD_A_LOT_OF_ENRG_RVSSCRD, 
          t1.'HOW OFTEN RESTLESS'n, 
          t1.'SF-12 AGGREGATE SCORE'n, 
          t1.'RISK/RESTLESSNESS/ENERGY AGG'n, 
          t1.'RISK/RESTLESS/NRGY CAT'n, 
          t1.'Married or not currently married'n, 
          t1.'EDUCATION CAT'n, 
          t1.INFULLYR
      FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__CAT_ t1
           FULL JOIN WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT t2 ON (t1.DUPERSID = t2.DUPERSID)
      WHERE t2.INER = 1 AND t1.INFULLYR = 1;
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:51:04 AM
   By task: List Data

   Input Data: Local:ZDATA.MEPS_MERGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:ZDATA.MEPS_MERGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DUPERSID, T.DUPERSID1, T.INER
	FROM ZDATA.MEPS_MERGED as T
;
QUIT;
TITLE;
TITLE1 "Report Listing";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=100)
	OBS="Row number"
	LABEL
	;
	VAR DUPERSID DUPERSID1 INER;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes1   */
%LET _CLIENTTASKLABEL='Data Set Attributes1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:51:04 AM
   By task: Data Set Attributes1

   Input Data: Local:WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForQUERY_FOR_MEPS_ER);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsForQUERY_FOR_MEPS_ER(LABEL="Contents Details for QUERY_FOR_MEPS_ER_2012_SAS7BDAT");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsForQUERY_FOR_MEPS_ER
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='QUERY_FOR_MEPS_ER_2012_SAS7BDAT';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsForQUERY_FOR_MEPS_ER OUT=WORK.CONTContentsForQUERY_FOR_MEPS_ER;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsForQUERY_FOR_MEPS_ER
		WHERE memname='QUERY_FOR_MEPS_ER_2012_SAS7BDAT';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Count DUPERSID   */
%LET _CLIENTTASKLABEL='Count DUPERSID';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_ER_2012_SAS7_0000);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_ER_2012_SAS7_0000 AS 
   SELECT t1.DUPERSID, 
          /* COUNT_of_DUPERSID */
            (COUNT(t1.DUPERSID)) AS COUNT_of_DUPERSID
      FROM WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT t1
      GROUP BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Inner Join   */
%LET _CLIENTTASKLABEL='Inner Join';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(ZDATA.MEPS_MERGED_COUNT);

PROC SQL;
   CREATE TABLE ZDATA.MEPS_MERGED_COUNT(label="MEPS_MERGED_COUNT") AS 
   SELECT t1.DUPERSID, 
          t1.SEX, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EMPST31, 
          t1.ACTDTY31, 
          t1.ACTDTY42, 
          t1.ACTDTY53, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.INSCOV12, 
          t1.PREVCOVR, 
          t1.PRVEV12, 
          t1.CANCERDX, 
          t1.ERTOT12, 
          t1.TYPEPE42, 
          t1.CHBRON31, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t2.DUPERSID AS DUPERSID2, 
          t2.COUNT_of_DUPERSID, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
          t1.CHBRON53, 
          t1.STPRAT12, 
          t1.OTPAAT12, 
          t1.PUBAT12X, 
          t1.MCDAT12X, 
          t1.PRIVAT12, 
          t1.TRIAT12X, 
          t1.ADHECR42, 
          t1.PMEDPP31, 
          t1.PMEDPP42, 
          t1.PMEDPP53, 
          t1.TTLP12X, 
          t1.ADAPPT42, 
          t1.ADEXPL42, 
          t1.ADLIST42, 
          t1.ADTLHW42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADILWW42, 
          t1.ADRTWW42, 
          t1.ADFFRM42, 
          t1.ADRTCR42, 
          t1.ADSPEC42, 
          t1.ADFHLP42, 
          t1.ADNSMK42, 
          t1.ADEGMC42, 
          t1.ADSPRF42, 
          t1.ADILCR42, 
          t1.ADNDCR42, 
          t1.ADDPRS42, 
          t1.ADINTR42, 
          t1.PHQ242, 
          t1.ADDRBP42, 
          t1.ADHOPE42, 
          t1.ADNERV42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADWRTH42, 
          t1.ADEFRT42, 
          t1.K6SUM42, 
          t1.ADCAPE42, 
          t1.ADDOWN42, 
          t1.ADNRGY42, 
          t1.ADSOCA42, 
          t1.ADMALS42, 
          t1.ADPALS42, 
          t1.ADPAIN42, 
          t1.ADMWLM42, 
          t1.ADPWLM42, 
          t1.ADOVER42, 
          t1.ADSMOK42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADGENH42, 
          t1.ADCLIM42, 
          t1.ADDAYA42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.EMPHDX, 
          t1.LFTDIF31, 
          t1.LFTDIF53, 
          t1.PUBP12X, 
          t1.WLKDIF31, 
          t1.WLKDIF53, 
          t1.WLKLIM31, 
          t1.WLKLIM53, 
          t1.ADLANG42, 
          t1.ADRISK42, 
          t1.SFFLAG42, 
          t1.ADPRX42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.HEALTH_IN_GENERAL, 
          t1.HLTH_LIMIT_MOD_ACTIVITIES, 
          t1.HLTH_LIMIT_CLIMBING_STAIRS, 
          t1.'4WKS:ACCMP_LESS_B/C_PHYS_PRBS'n, 
          t1.' 4WKS:WORK_LIMT_B/C_PHY_PROBS'n, 
          t1.'4WKS:ACCMP LESS B/C MNT PRBS'n, 
          t1.'4WKS:WORK LIMT B/C MNT PROBS'n, 
          t1.'4WKS:PAIN LIMITS NORMAL WORK'n, 
          t1.'4WKS: FELT CALM/PEACEFUL'n, 
          t1.'4WKS: HAD A LOT OF ENERGY'n, 
          t1.'4WKS: FELT DOWNHEARTED/DEPR'n, 
          t1.'4WKS: HLTH STOPPED SOC ACTIV'n, 
          t1.'MARITAL STATUS'n, 
          t1.'MORE LIKELY TO TAKE RISKS'n, 
          t1.'# OF VISITS TO MED OFF FOR CARE'n, 
          t1.'HOW OFT EVYTNG NEEDED EFFT'n, 
          t1.'EASY GETTING MED CARE'n, 
          t1.'RATING OF HEALTH CARE'n, 
          t1.'GOT CARE WHEN NEEDED ILL/INJ'n, 
          t1.'INJ/ILL NEEDING IMMD CARE'n, 
          t1.'DO NOT NEED HEALTH INSURANCE'n, 
          t1.'INSURANCE NOT WORTH COST'n, 
          t1.'NEED ANY CARE, TEST, TREATMENT'n, 
          t1.'CANCER DIAGNOSIS'n, 
          t1.'CHRONIC BRONCHITS LAST 12 MONTHS'n, 
          t1.'CHRONIC BRONCHITIS LAST 12 MTHS'n, 
          t1.'ANGINA DIAGNOSIS'n, 
          t1.'ARTHRITIS DIAGNOSIS'n, 
          t1.'EMPHYSEMA DIAGNOSIS'n, 
          t1.'DIFFICULTY LIFTING 10 POUNDS'n, 
          t1.'DIFFICULTY LIFTING 10 POUNDS1'n, 
          t1.'DIFFICULTY WALKING 3 BLOCKS'n, 
          t1.'DIFFICULTY WALKING 3 BLOCKS1'n, 
          t1.'EMPLOYMENT STATUS R3/1'n, 
          t1.'EMPLOYMENT STATUS R4/2'n, 
          t1.'EMPLOYMENT STATUS R5/3'n, 
          t1.'EDUCATION RECODE'n, 
          t1.HEALTH_IN_GENERAL_RVSSCORE, 
          t1.PAIN_LMTS_NRM_WORK_RVSSCRD, 
          t1.'FELT_CALM/PEACEFUL_RVSSCRD'n, 
          t1.HAD_A_LOT_OF_ENRG_RVSSCRD, 
          t1.'HOW OFTEN RESTLESS'n, 
          t1.'SF-12 AGGREGATE SCORE'n, 
          t1.'RISK/RESTLESSNESS/ENERGY AGG'n, 
          t1.'RISK/RESTLESS/NRGY CAT'n, 
          t1.'Married or not currently married'n, 
          t1.'EDUCATION CAT'n, 
          t1.INFULLYR
      FROM ZDATA.MEPS_MERGED t1
           INNER JOIN WORK.QUERY_FOR_MEPS_ER_2012_SAS7_0000 t2 ON (t1.DUPERSID = t2.DUPERSID);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recode Xrays MRIs   */
%LET _CLIENTTASKLABEL='Recode Xrays MRIs';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(ZDATA.RECODED_MEPS_MERGED_COUNT);

PROC SQL;
   CREATE TABLE ZDATA.RECODED_MEPS_MERGED_COUNT(label="RECODED_MEPS_MERGED_COUNT") AS 
   SELECT t1.DUPERSID, 
          /* XRAY RECODED */
            (CASE 
               WHEN -7 = t1.XRAYS THEN .
               WHEN -8 = t1.XRAYS THEN .
               WHEN -9 = t1.XRAYS THEN .
               WHEN 95 = t1.XRAYS THEN 2
               ELSE t1.XRAYS
            END) LABEL="X Ray recoded" AS 'XRAY RECODED'n, 
          /* MRI RECODED */
            (CASE 
               WHEN -7 = t1.MRI THEN .
               WHEN -8 = t1.MRI THEN .
               WHEN -9 = t1.MRI THEN .
               WHEN 95 = t1.MRI THEN 2
               ELSE t1.MRI
            END) LABEL="MRI Recoded" AS 'MRI RECODED'n, 
          t1.SEX, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EMPST31, 
          t1.ACTDTY31, 
          t1.ACTDTY42, 
          t1.ACTDTY53, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.INSCOV12, 
          t1.PREVCOVR, 
          t1.PRVEV12, 
          t1.CANCERDX, 
          t1.ERTOT12, 
          t1.TYPEPE42, 
          t1.CHBRON31, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.DUPERSID2, 
          t1.COUNT_of_DUPERSID, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
          t1.CHBRON53, 
          t1.STPRAT12, 
          t1.OTPAAT12, 
          t1.PUBAT12X, 
          t1.MCDAT12X, 
          t1.PRIVAT12, 
          t1.TRIAT12X, 
          t1.ADHECR42, 
          t1.PMEDPP31, 
          t1.PMEDPP42, 
          t1.PMEDPP53, 
          t1.TTLP12X, 
          t1.ADAPPT42, 
          t1.ADEXPL42, 
          t1.ADLIST42, 
          t1.ADTLHW42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADILWW42, 
          t1.ADRTWW42, 
          t1.ADFFRM42, 
          t1.ADRTCR42, 
          t1.ADSPEC42, 
          t1.ADFHLP42, 
          t1.ADNSMK42, 
          t1.ADEGMC42, 
          t1.ADSPRF42, 
          t1.ADILCR42, 
          t1.ADNDCR42, 
          t1.ADDPRS42, 
          t1.ADINTR42, 
          t1.PHQ242, 
          t1.ADDRBP42, 
          t1.ADHOPE42, 
          t1.ADNERV42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADWRTH42, 
          t1.ADEFRT42, 
          t1.K6SUM42, 
          t1.ADCAPE42, 
          t1.ADDOWN42, 
          t1.ADNRGY42, 
          t1.ADSOCA42, 
          t1.ADMALS42, 
          t1.ADPALS42, 
          t1.ADPAIN42, 
          t1.ADMWLM42, 
          t1.ADPWLM42, 
          t1.ADOVER42, 
          t1.ADSMOK42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADGENH42, 
          t1.ADCLIM42, 
          t1.ADDAYA42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.EMPHDX, 
          t1.LFTDIF31, 
          t1.LFTDIF53, 
          t1.PUBP12X, 
          t1.WLKDIF31, 
          t1.WLKDIF53, 
          t1.WLKLIM31, 
          t1.WLKLIM53, 
          t1.ADLANG42, 
          t1.ADRISK42, 
          t1.SFFLAG42, 
          t1.ADPRX42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.HEALTH_IN_GENERAL, 
          t1.HLTH_LIMIT_MOD_ACTIVITIES, 
          t1.HLTH_LIMIT_CLIMBING_STAIRS, 
          t1.'4WKS:ACCMP_LESS_B/C_PHYS_PRBS'n, 
          t1.' 4WKS:WORK_LIMT_B/C_PHY_PROBS'n, 
          t1.'4WKS:ACCMP LESS B/C MNT PRBS'n, 
          t1.'4WKS:WORK LIMT B/C MNT PROBS'n, 
          t1.'4WKS:PAIN LIMITS NORMAL WORK'n, 
          t1.'4WKS: FELT CALM/PEACEFUL'n, 
          t1.'4WKS: HAD A LOT OF ENERGY'n, 
          t1.'4WKS: FELT DOWNHEARTED/DEPR'n, 
          t1.'4WKS: HLTH STOPPED SOC ACTIV'n, 
          t1.'MARITAL STATUS'n, 
          t1.'MORE LIKELY TO TAKE RISKS'n, 
          t1.'# OF VISITS TO MED OFF FOR CARE'n, 
          t1.'HOW OFT EVYTNG NEEDED EFFT'n, 
          t1.'EASY GETTING MED CARE'n, 
          t1.'RATING OF HEALTH CARE'n, 
          t1.'GOT CARE WHEN NEEDED ILL/INJ'n, 
          t1.'INJ/ILL NEEDING IMMD CARE'n, 
          t1.'DO NOT NEED HEALTH INSURANCE'n, 
          t1.'INSURANCE NOT WORTH COST'n, 
          t1.'NEED ANY CARE, TEST, TREATMENT'n, 
          t1.'CANCER DIAGNOSIS'n, 
          t1.'CHRONIC BRONCHITS LAST 12 MONTHS'n, 
          t1.'CHRONIC BRONCHITIS LAST 12 MTHS'n, 
          t1.'ANGINA DIAGNOSIS'n, 
          t1.'ARTHRITIS DIAGNOSIS'n, 
          t1.'EMPHYSEMA DIAGNOSIS'n, 
          t1.'DIFFICULTY LIFTING 10 POUNDS'n, 
          t1.'DIFFICULTY LIFTING 10 POUNDS1'n, 
          t1.'DIFFICULTY WALKING 3 BLOCKS'n, 
          t1.'DIFFICULTY WALKING 3 BLOCKS1'n, 
          t1.'EMPLOYMENT STATUS R3/1'n, 
          t1.'EMPLOYMENT STATUS R4/2'n, 
          t1.'EMPLOYMENT STATUS R5/3'n, 
          t1.'EDUCATION RECODE'n, 
          t1.HEALTH_IN_GENERAL_RVSSCORE, 
          t1.PAIN_LMTS_NRM_WORK_RVSSCRD, 
          t1.'FELT_CALM/PEACEFUL_RVSSCRD'n, 
          t1.HAD_A_LOT_OF_ENRG_RVSSCRD, 
          t1.'HOW OFTEN RESTLESS'n, 
          t1.'SF-12 AGGREGATE SCORE'n, 
          t1.'RISK/RESTLESSNESS/ENERGY AGG'n, 
          t1.'RISK/RESTLESS/NRGY CAT'n, 
          t1.'Married or not currently married'n, 
          t1.'EDUCATION CAT'n, 
          t1.INFULLYR
      FROM ZDATA.MEPS_MERGED_COUNT t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:51:05 AM
   By task: One-Way Frequencies1

   Input Data: Local:ZDATA.RECODED_MEPS_MERGED_COUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:ZDATA.RECODED_MEPS_MERGED_COUNT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T."XRAY RECODED"n, T."MRI RECODED"n, T.COUNT_of_DUPERSID
	FROM ZDATA.RECODED_MEPS_MERGED_COUNT as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES "XRAY RECODED"n /  SCORES=TABLE;
	TABLES "MRI RECODED"n /  SCORES=TABLE;
	TABLES COUNT_of_DUPERSID /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:51:06 AM
   By task: Table Analysis

   Input Data: Local:ZDATA.RECODED_MEPS_MERGED_COUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:ZDATA.RECODED_MEPS_MERGED_COUNT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.XRAYS, T."XRAY RECODED"n, T."MRI RECODED"n, T.MRI
	FROM ZDATA.RECODED_MEPS_MERGED_COUNT as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES XRAYS * "XRAY RECODED"n /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES MRI * "MRI RECODED"n /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis   */
%LET _CLIENTTASKLABEL='Distribution Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:51:06 AM
   By task: Distribution Analysis

   Input Data: Local:ZDATA.RECODED_MEPS_MERGED_COUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:ZDATA.RECODED_MEPS_MERGED_COUNT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID
	FROM ZDATA.RECODED_MEPS_MERGED_COUNT as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: COUNT_of_DUPERSID";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR COUNT_of_DUPERSID;
	HISTOGRAM   COUNT_of_DUPERSID / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Count Cat   */
%LET _CLIENTTASKLABEL='Count Cat';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.CAT_COUNT_RECODED_MEPS_MERGED_CO);

PROC SQL;
   CREATE TABLE WORK.CAT_COUNT_RECODED_MEPS_MERGED_CO(label="CAT_COUNT_RECODED_MEPS_MERGED_CO") AS 
   SELECT t1.DUPERSID, 
          t1.'XRAY RECODED'n, 
          t1.'MRI RECODED'n, 
          t1.SEX, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EMPST31, 
          t1.ACTDTY31, 
          t1.ACTDTY42, 
          t1.ACTDTY53, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.INSCOV12, 
          t1.PREVCOVR, 
          t1.PRVEV12, 
          t1.CANCERDX, 
          t1.ERTOT12, 
          t1.TYPEPE42, 
          t1.CHBRON31, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.DUPERSID2, 
          t1.COUNT_of_DUPERSID, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
          t1.CHBRON53, 
          t1.STPRAT12, 
          t1.OTPAAT12, 
          t1.PUBAT12X, 
          t1.MCDAT12X, 
          t1.PRIVAT12, 
          t1.TRIAT12X, 
          t1.ADHECR42, 
          t1.PMEDPP31, 
          t1.PMEDPP42, 
          t1.PMEDPP53, 
          t1.TTLP12X, 
          t1.ADAPPT42, 
          t1.ADEXPL42, 
          t1.ADLIST42, 
          t1.ADTLHW42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADILWW42, 
          t1.ADRTWW42, 
          t1.ADFFRM42, 
          t1.ADRTCR42, 
          t1.ADSPEC42, 
          t1.ADFHLP42, 
          t1.ADNSMK42, 
          t1.ADEGMC42, 
          t1.ADSPRF42, 
          t1.ADILCR42, 
          t1.ADNDCR42, 
          t1.ADDPRS42, 
          t1.ADINTR42, 
          t1.PHQ242, 
          t1.ADDRBP42, 
          t1.ADHOPE42, 
          t1.ADNERV42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADWRTH42, 
          t1.ADEFRT42, 
          t1.K6SUM42, 
          t1.ADCAPE42, 
          t1.ADDOWN42, 
          t1.ADNRGY42, 
          t1.ADSOCA42, 
          t1.ADMALS42, 
          t1.ADPALS42, 
          t1.ADPAIN42, 
          t1.ADMWLM42, 
          t1.ADPWLM42, 
          t1.ADOVER42, 
          t1.ADSMOK42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADGENH42, 
          t1.ADCLIM42, 
          t1.ADDAYA42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.EMPHDX, 
          t1.LFTDIF31, 
          t1.LFTDIF53, 
          t1.PUBP12X, 
          t1.WLKDIF31, 
          t1.WLKDIF53, 
          t1.WLKLIM31, 
          t1.WLKLIM53, 
          t1.ADLANG42, 
          t1.ADRISK42, 
          t1.SFFLAG42, 
          t1.ADPRX42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.HEALTH_IN_GENERAL, 
          t1.HLTH_LIMIT_MOD_ACTIVITIES, 
          t1.HLTH_LIMIT_CLIMBING_STAIRS, 
          t1.'4WKS:ACCMP_LESS_B/C_PHYS_PRBS'n, 
          t1.' 4WKS:WORK_LIMT_B/C_PHY_PROBS'n, 
          t1.'4WKS:ACCMP LESS B/C MNT PRBS'n, 
          t1.'4WKS:WORK LIMT B/C MNT PROBS'n, 
          t1.'4WKS:PAIN LIMITS NORMAL WORK'n, 
          t1.'4WKS: FELT CALM/PEACEFUL'n, 
          t1.'4WKS: HAD A LOT OF ENERGY'n, 
          t1.'4WKS: FELT DOWNHEARTED/DEPR'n, 
          t1.'4WKS: HLTH STOPPED SOC ACTIV'n, 
          t1.'MARITAL STATUS'n, 
          t1.'MORE LIKELY TO TAKE RISKS'n, 
          t1.'# OF VISITS TO MED OFF FOR CARE'n, 
          t1.'HOW OFT EVYTNG NEEDED EFFT'n, 
          t1.'EASY GETTING MED CARE'n, 
          t1.'RATING OF HEALTH CARE'n, 
          t1.'GOT CARE WHEN NEEDED ILL/INJ'n, 
          t1.'INJ/ILL NEEDING IMMD CARE'n, 
          t1.'DO NOT NEED HEALTH INSURANCE'n, 
          t1.'INSURANCE NOT WORTH COST'n, 
          t1.'NEED ANY CARE, TEST, TREATMENT'n, 
          t1.'CANCER DIAGNOSIS'n, 
          t1.'CHRONIC BRONCHITS LAST 12 MONTHS'n, 
          t1.'CHRONIC BRONCHITIS LAST 12 MTHS'n, 
          t1.'ANGINA DIAGNOSIS'n, 
          t1.'ARTHRITIS DIAGNOSIS'n, 
          t1.'EMPHYSEMA DIAGNOSIS'n, 
          t1.'DIFFICULTY LIFTING 10 POUNDS'n, 
          t1.'DIFFICULTY LIFTING 10 POUNDS1'n, 
          t1.'DIFFICULTY WALKING 3 BLOCKS'n, 
          t1.'DIFFICULTY WALKING 3 BLOCKS1'n, 
          t1.'EMPLOYMENT STATUS R3/1'n, 
          t1.'EMPLOYMENT STATUS R4/2'n, 
          t1.'EMPLOYMENT STATUS R5/3'n, 
          t1.'EDUCATION RECODE'n, 
          t1.HEALTH_IN_GENERAL_RVSSCORE, 
          t1.PAIN_LMTS_NRM_WORK_RVSSCRD, 
          t1.'FELT_CALM/PEACEFUL_RVSSCRD'n, 
          t1.HAD_A_LOT_OF_ENRG_RVSSCRD, 
          t1.'HOW OFTEN RESTLESS'n, 
          t1.'SF-12 AGGREGATE SCORE'n, 
          t1.'RISK/RESTLESSNESS/ENERGY AGG'n, 
          t1.'RISK/RESTLESS/NRGY CAT'n, 
          t1.'Married or not currently married'n, 
          t1.'EDUCATION CAT'n, 
          t1.INFULLYR, 
          /* Count DUPERSID CAT */
            (CASE  
               WHEN t1.COUNT_of_DUPERSID>=1 and t1.COUNT_of_DUPERSID <2
               THEN 1
               WHEN t1.COUNT_of_DUPERSID>=2 and t1.COUNT_of_DUPERSID <5
               THEN 2
               WHEN t1.COUNT_of_DUPERSID>=5
               THEN 3
            
            END) LABEL="Count DUPERSID CAT" AS 'Count DUPERSID CAT'n
      FROM ZDATA.RECODED_MEPS_MERGED_COUNT t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies2   */
%LET _CLIENTTASKLABEL='One-Way Frequencies2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:51:06 AM
   By task: One-Way Frequencies2

   Input Data: Local:WORK.CAT_COUNT_RECODED_MEPS_MERGED_CO
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.CAT_COUNT_RECODED_MEPS_MERGED_CO
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T."Count DUPERSID CAT"n
	FROM WORK.CAT_COUNT_RECODED_MEPS_MERGED_CO as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES "Count DUPERSID CAT"n /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes2   */
%LET _CLIENTTASKLABEL='Data Set Attributes2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:51:07 AM
   By task: Data Set Attributes2

   Input Data: Local:ZDATA.MEPS_MERGED_COUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTCONTENTSFORMEPS_MERGED__0000);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ZDATA.MEPS_MERGED_COUNT OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTCONTENTSFORMEPS_MERGED__0000(LABEL="Contents Details for MEPS_MERGED_COUNT");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTCONTENTSFORMEPS_MERGED__0000
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_MERGED_COUNT';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTCONTENTSFORMEPS_MERGED__0000 OUT=WORK.CONTCONTENTSFORMEPS_MERGED__0000;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTCONTENTSFORMEPS_MERGED__0000
		WHERE memname='MEPS_MERGED_COUNT';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Working Projects\TanZ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:51:07 AM
   By task: One-Way Frequencies

   Input Data: Local:WORK.QUERY_FOR_MEPS_ER_2012_SAS7_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_ER_2012_SAS7_0000
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.COUNT_of_DUPERSID
	FROM WORK.QUERY_FOR_MEPS_ER_2012_SAS7_0000 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
