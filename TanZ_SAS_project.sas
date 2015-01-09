/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Friday, January 09, 2015     TIME: 2:47:22 PM
PROJECT: TanZ_SAS_project_010915
PROJECT PATH: P:\QAC\qac200\students\ztan01\Completed Projects (Daily Backup)\010915\TanZ_SAS_project_010915.egp
---------------------------------------- */

/* Library assignment for Local.ZDATA */
Libname ZDATA BASE 'P:\QAC\qac200\students\ztan01\Working Projects' ;
/* Library assignment for Local.ZDATA */
Libname ZDATA BASE 'P:\QAC\qac200\students\ztan01\Working Projects' ;


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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Completed Projects (Daily Backup)\010915\TanZ_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ZDATA BASE "P:\QAC\qac200\students\ztan01\Working Projects" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Filter by age >_18   */
%LET _CLIENTTASKLABEL='Filter by age >_18';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Completed Projects (Daily Backup)\010915\TanZ_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(ZDATA.MEPS_FULLYR_2012_Subset);

PROC SQL;
   CREATE TABLE ZDATA.MEPS_FULLYR_2012_Subset(label="MEPS_FULLYR_2012_Subset") AS 
   SELECT t1.DUPERSID, 
          t1.ACTDTY31, 
          t1.ACTDTY42, 
          t1.ACTDTY53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.CANCERDX, 
          t1.CHBRON31, 
          t1.CHBRON53, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ERTOT12, 
          t1.INSCOV12, 
          t1.K6SUM42, 
          t1.MARRY12X, 
          t1.MCDAT12X, 
          t1.MCS42, 
          t1.OTPAAT12, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.PMEDPP31, 
          t1.PMEDPP42, 
          t1.PMEDPP53, 
          t1.PREVCOVR, 
          t1.PRIVAT12, 
          t1.PRVEV12, 
          t1.PUBAT12X, 
          t1.RACETHX, 
          t1.REGION12, 
          t1.SEX, 
          t1.SFFLAG42, 
          t1.STPRAT12, 
          t1.TRIAT12X, 
          t1.TTLP12X, 
          t1.TYPEPE42, 
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
          t1.EDRECODE
      FROM EC100024.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18
      ORDER BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Code For Data Set Attributes1   */
%LET SYSLAST=ZDATA.MEPS_FULLYR_2012_SUBSET;
%LET _CLIENTTASKLABEL='Code For Data Set Attributes1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Completed Projects (Daily Backup)\010915\TanZ_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_010915.egp';
%LET _SASPROGRAMFILE=;

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 9:39:04 AM
   By task: Data Set Attributes1

   Input Data: Local:ZDATA.MEPS_FULLYR_2012_SUBSET
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(ZDATA.CONTCONTENTSFORMEPS_FULLYR_2012_);
TITLE "Data set attributes for subset data set";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ZDATA.MEPS_FULLYR_2012_SUBSET ;

RUN;



GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies for 2012 Adult MEPS subset   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for 2012 Adult MEPS subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Completed Projects (Daily Backup)\010915\TanZ_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:46:49 PM
   By task: One-Way Frequencies for 2012 Adult MEPS subset

   Input Data: Local:ZDATA.MEPS_FULLYR_2012_SUBSET
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:ZDATA.MEPS_FULLYR_2012_SUBSET
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.ACTDTY31, T.ACTDTY42, T.ACTDTY53, T.ADAPPT42, T.ADCAPE42, T.ADCLIM42, T.ADCMPD42, T.ADCMPM42, T.ADCMPY42, T.ADDAYA42, T.ADDOWN42, T.ADDPRS42, T.ADDRBP42, T.ADEFRT42, T.ADEGMC42, T.ADEXPL42, T.ADEZUN42, T.ADFFRM42, T.ADFHLP42
		     , T.ADGENH42, T.ADHECR42, T.ADHOPE42, T.ADILCR42, T.ADILWW42, T.ADINSA42, T.ADINSB42, T.ADINST42, T.ADINTR42, T.ADLANG42, T.ADLIST42, T.ADMALS42, T.ADMWLM42, T.ADNDCR42, T.ADNERV42, T.ADNRGY42, T.ADNSMK42, T.ADOVER42, T.ADPAIN42
		     , T.ADPALS42, T.ADPRTM42, T.ADPRX42, T.ADPWLM42, T.ADRESP42, T.ADREST42, T.ADRISK42, T.ADRTCR42, T.ADRTWW42, T.ADSAD42, T.ADSMOK42, T.ADSOCA42, T.ADSPEC42, T.ADSPRF42, T.ADTLHW42, T.ADWRTH42, T.AGE12X, T.ANGIDX, T.ARTHDX
		     , T.CANCERDX, T.CHBRON31, T.CHBRON53, T.EDRECODE, T.EMPHDX, T.EMPST31, T.EMPST42, T.EMPST53, T.ERTOT12, T.INSCOV12, T.K6SUM42, T.LFTDIF31, T.LFTDIF53, T.MARRY12X, T.MCDAT12X, T.MCS42, T.OTPAAT12, T.PCS42, T.PHQ242, T.PMEDPP31
		     , T.PMEDPP42, T.PMEDPP53, T.PREVCOVR, T.PRIVAT12, T.PRVEV12, T.PUBAT12X, T.PUBP12X, T.RACETHX, T.REGION12, T.SEX, T.SFFLAG42, T.STPRAT12, T.TRIAT12X, T.TTLP12X, T.TYPEPE42, T.WLKDIF31, T.WLKDIF53, T.WLKLIM31, T.WLKLIM53
	FROM ZDATA.MEPS_FULLYR_2012_SUBSET(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for 2012 Adult MEPS Subset";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Zach Tan";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES ACTDTY31 / MISSPRINT  SCORES=TABLE;
	TABLES ACTDTY42 / MISSPRINT  SCORES=TABLE;
	TABLES ACTDTY53 / MISSPRINT  SCORES=TABLE;
	TABLES ADAPPT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCAPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCLIM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDAYA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDOWN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDPRS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDRBP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEFRT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEGMC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEXPL42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEZUN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFFRM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFHLP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADGENH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHECR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHOPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSB42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINTR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLANG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLIST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNDCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNERV42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNRGY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNSMK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADOVER42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPAIN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRTM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRX42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRESP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADREST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRISK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSAD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSMOK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSOCA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPEC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPRF42 / MISSPRINT  SCORES=TABLE;
	TABLES ADTLHW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADWRTH42 / MISSPRINT  SCORES=TABLE;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE;
	TABLES ANGIDX / MISSPRINT  SCORES=TABLE;
	TABLES ARTHDX / MISSPRINT  SCORES=TABLE;
	TABLES CANCERDX / MISSPRINT  SCORES=TABLE;
	TABLES CHBRON31 / MISSPRINT  SCORES=TABLE;
	TABLES CHBRON53 / MISSPRINT  SCORES=TABLE;
	TABLES EDRECODE / MISSPRINT  SCORES=TABLE;
	TABLES EMPHDX / MISSPRINT  SCORES=TABLE;
	TABLES EMPST31 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST42 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST53 / MISSPRINT  SCORES=TABLE;
	TABLES ERTOT12 / MISSPRINT  SCORES=TABLE;
	TABLES INSCOV12 / MISSPRINT  SCORES=TABLE;
	TABLES K6SUM42 / MISSPRINT  SCORES=TABLE;
	TABLES LFTDIF31 / MISSPRINT  SCORES=TABLE;
	TABLES LFTDIF53 / MISSPRINT  SCORES=TABLE;
	TABLES MARRY12X / MISSPRINT  SCORES=TABLE;
	TABLES MCDAT12X / MISSPRINT  SCORES=TABLE;
	TABLES MCS42 / MISSPRINT  SCORES=TABLE;
	TABLES OTPAAT12 / MISSPRINT  SCORES=TABLE;
	TABLES PCS42 / MISSPRINT  SCORES=TABLE;
	TABLES PHQ242 / MISSPRINT  SCORES=TABLE;
	TABLES PMEDPP31 / MISSPRINT  SCORES=TABLE;
	TABLES PMEDPP42 / MISSPRINT  SCORES=TABLE;
	TABLES PMEDPP53 / MISSPRINT  SCORES=TABLE;
	TABLES PREVCOVR / MISSPRINT  SCORES=TABLE;
	TABLES PRIVAT12 / MISSPRINT  SCORES=TABLE;
	TABLES PRVEV12 / MISSPRINT  SCORES=TABLE;
	TABLES PUBAT12X / MISSPRINT  SCORES=TABLE;
	TABLES PUBP12X / MISSPRINT  SCORES=TABLE;
	TABLES RACETHX / MISSPRINT  SCORES=TABLE;
	TABLES REGION12 / MISSPRINT  SCORES=TABLE;
	TABLES SEX / MISSPRINT  SCORES=TABLE;
	TABLES SFFLAG42 / MISSPRINT  SCORES=TABLE;
	TABLES STPRAT12 / MISSPRINT  SCORES=TABLE;
	TABLES TRIAT12X / MISSPRINT  SCORES=TABLE;
	TABLES TTLP12X / MISSPRINT  SCORES=TABLE;
	TABLES TYPEPE42 / MISSPRINT  SCORES=TABLE;
	TABLES WLKDIF31 / MISSPRINT  SCORES=TABLE;
	TABLES WLKDIF53 / MISSPRINT  SCORES=TABLE;
	TABLES WLKLIM31 / MISSPRINT  SCORES=TABLE;
	TABLES WLKLIM53 / MISSPRINT  SCORES=TABLE;
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


/*   START OF NODE: Recode Variables   */
%LET _CLIENTTASKLABEL='Recode Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Completed Projects (Daily Backup)\010915\TanZ_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
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
          /* HEALTH_IN_GENERAL */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="Health in general" AS HEALTH_IN_GENERAL, 
          /* HLTH_LIMIT_MOD_ACTIVITIES */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="Health limiting activities" AS HLTH_LIMIT_MOD_ACTIVITIES, 
          /* HLTH_LIMIT_CLIMBING_STAIRS */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="Health limiting the climbing of stairs" AS HLTH_LIMIT_CLIMBING_STAIRS, 
          /* 4WKS:ACCMP_LESS_B/C_PHYS_PRBS */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="Accomplished less because of physical problems" AS '4WKS:ACCMP_LESS_B/C_PHYS_PRBS'n, 
          /*  4WKS:WORK_LIMT_B/C_PHY_PROBS */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="Work limit because of physical problems" AS ' 4WKS:WORK_LIMT_B/C_PHY_PROBS'n, 
          /* 4WKS:ACCMP LESS B/C MNT PRBS */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="Accmp less because of mnt prbs" AS '4WKS:ACCMP LESS B/C MNT PRBS'n, 
          /* 4WKS:WORK LIMT B/C MNT PROBS */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="Work limit because of mnt problems" AS '4WKS:WORK LIMT B/C MNT PROBS'n, 
          /* 4WKS:PAIN LIMITS NORMAL WORK */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="Pain limits normal work" AS '4WKS:PAIN LIMITS NORMAL WORK'n, 
          /* 4WKS: FELT CALM/PEACEFUL */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="Felt calm/peaceful" AS '4WKS: FELT CALM/PEACEFUL'n, 
          /* 4WKS: HAD A LOT OF ENERGY */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="Had a lot of energy" AS '4WKS: HAD A LOT OF ENERGY'n, 
          /* 4WKS: FELT DOWNHEARTED/DEPR */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="Felt downhearted/depressed" AS '4WKS: FELT DOWNHEARTED/DEPR'n, 
          /* 4WKS: HLTH STOPPED SOC ACTIV */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="Health stopped social activities" AS '4WKS: HLTH STOPPED SOC ACTIV'n, 
          /* MARITAL STATUS */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Marital Status" AS 'MARITAL STATUS'n, 
          /* MORE LIKELY TO TAKE RISKS */
            (CASE 
               WHEN -1 = t1.ADRISK42 THEN .
               WHEN -7 = t1.ADRISK42 THEN .
               WHEN -8 = t1.ADRISK42 THEN .
               WHEN -9 = t1.ADRISK42 THEN .
               ELSE t1.ADRISK42
            END) LABEL="More likely to take risks" AS 'MORE LIKELY TO TAKE RISKS'n, 
          /* # OF VISITS TO MED OFF FOR CARE */
            (CASE 
               WHEN -1 = t1.ADAPPT42 THEN .
               WHEN -8 = t1.ADAPPT42 THEN .
               WHEN -9 = t1.ADAPPT42 THEN .
               ELSE t1.ADAPPT42
            END) LABEL="# Of visits to medical office for care" AS '# OF VISITS TO MED OFF FOR CARE'n, 
          /* HOW OFT EVYTNG NEEDED EFFT */
            (CASE 
               WHEN -1 = t1.ADEFRT42 THEN .
               WHEN -7 = t1.ADEFRT42 THEN .
               WHEN -8 = t1.ADEFRT42 THEN .
               WHEN -9 = t1.ADEFRT42 THEN .
               ELSE t1.ADEFRT42
            END) LABEL="How often everything needed effort" AS 'HOW OFT EVYTNG NEEDED EFFT'n, 
          /* EASY GETTING MED CARE */
            (CASE 
               WHEN -1 = t1.ADEGMC42 THEN .
               WHEN -9 = t1.ADEGMC42 THEN .
               ELSE t1.ADEGMC42
            END) LABEL="Easy getting medical care" AS 'EASY GETTING MED CARE'n, 
          /* RATING OF HEALTH CARE */
            (CASE 
               WHEN -1 = t1.ADHECR42 THEN .
               WHEN -9 = t1.ADHECR42 THEN .
               ELSE t1.ADHECR42
            END) LABEL="Rating of health care" AS 'RATING OF HEALTH CARE'n, 
          /* GOT CARE WHEN NEEDED ILL/INJ */
            (CASE 
               WHEN -1 = t1.ADILWW42 THEN .
               WHEN -9 = t1.ADILWW42 THEN .
               ELSE t1.ADILWW42
            END) LABEL="Got care when needed for illness/injury" AS 'GOT CARE WHEN NEEDED ILL/INJ'n, 
          /* INJ/ILL NEEDING IMMD CARE */
            (CASE 
               WHEN -1 = t1.ADILCR42 THEN .
               WHEN -9 = t1.ADILCR42 THEN .
               ELSE t1.ADILCR42
            END) LABEL="Injury/illness needing immediate care" AS 'INJ/ILL NEEDING IMMD CARE'n, 
          /* DO NOT NEED HEALTH INSURANCE */
            (CASE 
               WHEN -1 = t1.ADINSA42 THEN .
               WHEN -7 = t1.ADINSA42 THEN .
               WHEN -8 = t1.ADINSA42 THEN .
               WHEN -9 = t1.ADINSA42 THEN .
               ELSE t1.ADINSA42
            END) LABEL="Do not need health insurance" AS 'DO NOT NEED HEALTH INSURANCE'n, 
          /* INSURANCE NOT WORTH COST */
            (CASE 
               WHEN -1 = t1.ADINSB42 THEN .
               WHEN -7 = t1.ADINSB42 THEN .
               WHEN -8 = t1.ADINSB42 THEN .
               WHEN -9 = t1.ADINSB42 THEN .
               ELSE t1.ADINSB42
            END) LABEL="Insurance not worth cost" AS 'INSURANCE NOT WORTH COST'n, 
          /* NEED ANY CARE, TEST, TREATMENT */
            (CASE 
               WHEN -1 = t1.ADNDCR42 THEN .
               WHEN -8 = t1.ADNDCR42 THEN .
               WHEN -9 = t1.ADNDCR42 THEN .
               ELSE t1.ADNDCR42
            END) LABEL="Need any care, test, treatment" AS 'NEED ANY CARE, TEST, TREATMENT'n, 
          /* CANCER DIAGNOSIS */
            (CASE 
               WHEN -7 = t1.CANCERDX THEN .
               WHEN -8 = t1.CANCERDX THEN .
               WHEN -9 = t1.CANCERDX THEN .
               ELSE t1.CANCERDX
            END) LABEL="Cancer Diagnosis" AS 'CANCER DIAGNOSIS'n, 
          /* CHRONIC BRONCHITS LAST 12 MONTHS */
            (CASE 
               WHEN -1 = t1.CHBRON31 THEN .
               WHEN -7 = t1.CHBRON31 THEN .
               WHEN -8 = t1.CHBRON31 THEN .
               WHEN -9 = t1.CHBRON31 THEN .
               ELSE t1.CHBRON31
            END) LABEL="Chronic Bronchitis last 12 months" AS 'CHRONIC BRONCHITS LAST 12 MONTHS'n, 
          /* CHRONIC BRONCHITIS LAST 12 MTHS */
            (CASE 
               WHEN -1 = t1.CHBRON53 THEN .
               WHEN -7 = t1.CHBRON53 THEN .
               WHEN -8 = t1.CHBRON53 THEN .
               WHEN -9 = t1.CHBRON53 THEN .
               ELSE t1.CHBRON53
            END) LABEL="Chronic bronchitis last 12 months" AS 'CHRONIC BRONCHITIS LAST 12 MTHS'n, 
          /* ANGINA DIAGNOSIS */
            (CASE 
               WHEN -7 = t1.ANGIDX THEN .
               WHEN -8 = t1.ANGIDX THEN .
               WHEN -9 = t1.ANGIDX THEN .
               ELSE t1.ANGIDX
            END) LABEL="Angina Diagnosis" AS 'ANGINA DIAGNOSIS'n, 
          /* ARTHRITIS DIAGNOSIS */
            (CASE 
               WHEN -7 = t1.ARTHDX THEN .
               WHEN -8 = t1.ARTHDX THEN .
               WHEN -9 = t1.ARTHDX THEN .
               ELSE t1.ARTHDX
            END) LABEL="Arthritis Diagnosis" AS 'ARTHRITIS DIAGNOSIS'n, 
          /* EMPHYSEMA DIAGNOSIS */
            (CASE 
               WHEN -7 = t1.EMPHDX THEN .
               WHEN -8 = t1.EMPHDX THEN .
               WHEN -9 = t1.EMPHDX THEN .
               ELSE t1.EMPHDX
            END) LABEL="Emphysema diagnosis" AS 'EMPHYSEMA DIAGNOSIS'n, 
          /* DIFFICULTY LIFTING 10 POUNDS */
            (CASE 
               WHEN -1 = t1.LFTDIF31 THEN .
               WHEN -8 = t1.LFTDIF31 THEN .
               ELSE t1.LFTDIF31
            END) LABEL="Difficulty lifting 10 pounds" AS 'DIFFICULTY LIFTING 10 POUNDS'n, 
          /* DIFFICULTY LIFTING 10 POUNDS1 */
            (CASE 
               WHEN -1 = t1.LFTDIF53 THEN .
               WHEN -7 = t1.LFTDIF53 THEN .
               WHEN -8 = t1.LFTDIF53 THEN .
               WHEN -9 = t1.LFTDIF53 THEN .
               ELSE t1.LFTDIF53
            END) LABEL="Difficulty lifting 10 pounds" AS 'DIFFICULTY LIFTING 10 POUNDS1'n, 
          /* DIFFICULTY WALKING 3 BLOCKS */
            (CASE 
               WHEN -1 = t1.WLKDIF31 THEN .
               WHEN -8 = t1.WLKDIF31 THEN .
            END) LABEL="Difficulty walking 3 blocks" AS 'DIFFICULTY WALKING 3 BLOCKS'n, 
          /* DIFFICULTY WALKING 3 BLOCKS1 */
            (CASE 
               WHEN -1 = t1.WLKDIF53 THEN .
               WHEN -7 = t1.WLKDIF53 THEN .
               WHEN -8 = t1.WLKDIF53 THEN .
               WHEN -9 = t1.WLKDIF53 THEN .
               ELSE t1.WLKDIF53
            END) LABEL="Difficulty walking 3 blocks " AS 'DIFFICULTY WALKING 3 BLOCKS1'n, 
          /* EMPLOYMENT STATUS R3/1 */
            (CASE 
               WHEN -1 = t1.EMPST31 THEN .
               WHEN -7 = t1.EMPST31 THEN .
               WHEN -8 = t1.EMPST31 THEN .
               WHEN -9 = t1.EMPST31 THEN .
               ELSE t1.EMPST31
            END) LABEL="Employment status R3/1" AS 'EMPLOYMENT STATUS R3/1'n, 
          /* EMPLOYMENT STATUS R4/2 */
            (CASE 
               WHEN -1 = t1.EMPST42 THEN .
               WHEN -7 = t1.EMPST42 THEN .
               WHEN -8 = t1.EMPST42 THEN .
               WHEN -9 = t1.EMPST42 THEN .
               ELSE t1.EMPST42
            END) LABEL="Employment status R4/2" AS 'EMPLOYMENT STATUS R4/2'n, 
          /* EMPLOYMENT STATUS R5/3 */
            (CASE 
               WHEN -7 = t1.EMPST53 THEN .
               WHEN -8 = t1.EMPST53 THEN .
               WHEN -9 = t1.EMPST53 THEN .
               ELSE t1.EMPST53
            END) LABEL="Employment status R5/3" AS 'EMPLOYMENT STATUS R5/3'n, 
          /* EDUCATION RECODE */
            (CASE 
               WHEN -7 = t1.EDRECODE THEN .
               WHEN -8 = t1.EDRECODE THEN .
               WHEN -9 = t1.EDRECODE THEN .
               ELSE t1.EDRECODE
            END) LABEL="Education recode" AS 'EDUCATION RECODE'n
      FROM ZDATA.MEPS_FULLYR_2012_SUBSET t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Completed Projects (Daily Backup)\010915\TanZ_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:46:50 PM
   By task: Table Analysis

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADGENH42, T.HEALTH_IN_GENERAL
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE as T
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
	TABLES ADGENH42 * HEALTH_IN_GENERAL /
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


/*   START OF NODE: Table Analysis1   */
%LET _CLIENTTASKLABEL='Table Analysis1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Completed Projects (Daily Backup)\010915\TanZ_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:46:50 PM
   By task: Table Analysis1

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADDAYA42, T.HLTH_LIMIT_MOD_ACTIVITIES
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE as T
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
	TABLES ADDAYA42 * HLTH_LIMIT_MOD_ACTIVITIES /
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


/*   START OF NODE: Table Analysis2   */
%LET _CLIENTTASKLABEL='Table Analysis2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Completed Projects (Daily Backup)\010915\TanZ_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:46:50 PM
   By task: Table Analysis2

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CANCERDX, T."CANCER DIAGNOSIS"n
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE as T
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
	TABLES CANCERDX * "CANCER DIAGNOSIS"n /
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


/*   START OF NODE: Table Analysis3   */
%LET _CLIENTTASKLABEL='Table Analysis3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Completed Projects (Daily Backup)\010915\TanZ_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:46:50 PM
   By task: Table Analysis3

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ARTHDX, T."ARTHRITIS DIAGNOSIS"n
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE as T
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
	TABLES ARTHDX * "ARTHRITIS DIAGNOSIS"n /
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


/*   START OF NODE: Table Analysis4   */
%LET _CLIENTTASKLABEL='Table Analysis4';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Completed Projects (Daily Backup)\010915\TanZ_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:46:50 PM
   By task: Table Analysis4

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADRISK42, T."MORE LIKELY TO TAKE RISKS"n
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE as T
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
	TABLES ADRISK42 * "MORE LIKELY TO TAKE RISKS"n /
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


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ztan01\Completed Projects (Daily Backup)\010915\TanZ_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='TanZ_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:46:52 PM
   By task: Data Set Attributes

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFormeps_fullyr_2012);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsFormeps_fullyr_2012(LABEL="Contents Details for meps_fullyr_2012");
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
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsFormeps_fullyr_2012
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsFormeps_fullyr_2012 OUT=WORK.CONTContentsFormeps_fullyr_2012;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsFormeps_fullyr_2012
		WHERE memname='MEPS_FULLYR_2012';
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

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
