/* SAS codes for "Home Equity Loan Risk Prediction using Logistic Regression Model"  */

libname tt "C:\Users\Bmehdi\Desktop\SAS Project"; 
proc contents data=tt.Hmeq;
RUN;

/*
#Variables Description	
===============================
	#	Variable	Type	Len	Label

	1	bad			Num		8	Default or seriously delinquent
	10	clage		Num		8	Age of oldest trade line in months
	12	clno		Num		8	Number of trade (credit) lines
	13	debtinc		Num		8	Debt to income ratio
	9	delinq		Num		8	Number of delinquent trade lines
	8	derog		Num		8	Number of major derogatory reports
	6	job			Char	7	Prof/exec sales mngr office self other
	2	loan		Num		8	Amount of current loan request
	3	mortdue		Num		8	Amount due on existing mortgage
	11	ninq		Num		8	Number of recent credit inquiries
	5	reason		Char	7	Home improvement or debt consolidation
	4	value		Num		8	Value of current property
	7	yoj			Num		8	Years on current job
===============================
*/


/* proc freq of char variables */
proc freq data=tt.hmeq;
   tables bad reason job;
RUN;

/* Splitting data into train and test sets (Dev (train) and Val (test)) */
data MODEL_DEV MODEL_VAL;
  set tt.Hmeq;
  if ranuni(1234567)<=0.6 THEN OUTPUT MODEL_DEV;
  ELSE                         OUTPUT MODEL_VAL;
RUN;

/* Converting char (categorical) variables to numeric ones by creating dummy variables */
data MODEL_DEV1(drop=job reason);
  set MODEL_DEV;
  JOB_Mgr=(JOB='Mgr');
  JOB_Office=(JOB='Office');
  JOB_Other=(JOB='Other');
  JOB_ProfExe=(JOB='ProfExe');
  JOB_Sales=(JOB='Sales');
  JOB_Self=(JOB='Self');
  JOB_miss=(JOB=' ');
  REASON_DebtCon=(REASON='DebtCon');
  REASON_HomeImp=(REASON='HomeImp');
  REASON_Miss=(REASON=' ');
RUN;

proc contents data=MODEL_DEV1;
RUN;

/* Preprocessing for 10 numerical variables in 10 steps: */
/* Step 1: variable = clage */
PROC RANK DATA = MODEL_DEV1 (KEEP=BAD clage)
          GROUPS = 10
          OUT = JUNK1     ;
          RANKS NEWVBLE   ;
          VAR clage       ;
RUN;

PROC SUMMARY DATA = JUNK1 NWAY ;
            CLASS NEWVBLE      ;
            VAR BAD clage      ;
            OUTPUT OUT = JUNK2
            MEAN =
            MIN(clage)=MIN
            MAX(clage)=MAX
            N = NOBS         ;
RUN;

DATA JUNK2                                 ;
         SET JUNK2                          ;
         IF BAD NE 0 THEN
            LOGIT = LOG ( BAD / (1- BAD) )  ;
         ELSE IF BAD = 0 THEN LOGIT = .     ;
RUN;

PROC SQL NOPRINT;
        CREATE TABLE JUNK3 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE clage=. ;
RUN;    

DATA JUNK3;
        SET JUNK3;
        LOGIT=LOG( BAD / (1- BAD));
RUN; 

DATA JUNK4;
        SET JUNK2 JUNK3;
RUN;

PROC PRINT DATA = JUNK4;
RUN;

PROC PLOT DATA = JUNK4                                ;
            TITLE1 "Plot of Logit(Response) by clage" ;
            PLOT  LOGIT* clage                        ;
RUN;
title;

proc plot data = JUNK4;
        plot BAD*clage;
        plot _freq_*clage;
        TITLE2 "Plot of Response by clage" ;
RUN;
title;

PROC PRINT DATA = JUNK4 LABEL SPLIT = '*' NOOBS         ;
            TITLE3 "Table of Response by Grouped clage" ;
            VAR NEWVBLE NOBS clage MIN MAX BAD          ;
            LABEL NEWVBLE = "clage Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
RUN;
title;


/* Step 2: variable = clno */
PROC RANK DATA = MODEL_DEV1 (KEEP=BAD clno)
          GROUPS = 10
          OUT = JUNK5     ;
          RANKS NEWVBLE   ;
          VAR clno      ;
RUN;

PROC SUMMARY DATA = JUNK5 NWAY ;
            CLASS NEWVBLE      ;
            VAR BAD clno      ;
            OUTPUT OUT = JUNK6
            MEAN =
            MIN(clno)=MIN
            MAX(clno)=MAX
            N = NOBS         ;
RUN;

DATA JUNK6                                 ;
         SET JUNK6                          ;
         IF BAD NE 0 THEN
            LOGIT = LOG ( BAD / (1- BAD) )  ;
         ELSE IF BAD = 0 THEN LOGIT = .     ;
RUN;

PROC SQL NOPRINT;
        CREATE TABLE JUNK7 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE clno=. ;
RUN;    

DATA JUNK7;
        SET JUNK7;
        LOGIT=LOG( BAD / (1- BAD));
RUN; 

DATA JUNK8;
        SET JUNK6 JUNK7;
RUN;

PROC PRINT DATA = JUNK8;
RUN;

PROC PLOT DATA = JUNK8                                ;
            TITLE1 "Plot of Logit(Response) by clno" ;
            PLOT  LOGIT* clno                        ;
RUN;
title;

proc plot data = JUNK8;
        plot BAD*clno;
        plot _freq_*clno;
        TITLE2 "Plot of Response by clno" ;
RUN;
title;

PROC PRINT DATA = JUNK8 LABEL SPLIT = '*' NOOBS         ;
            TITLE3 "Table of Response by Grouped clno" ;
            VAR NEWVBLE NOBS clno MIN MAX BAD          ;
            LABEL NEWVBLE = "clno Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
RUN;
title;


/* Step 3: variable = debtinc */
PROC RANK DATA = MODEL_DEV1 (KEEP=BAD debtinc)
          GROUPS = 10
          OUT = JUNK9     ;
          RANKS NEWVBLE   ;
          VAR debtinc       ;
RUN;

PROC SUMMARY DATA = JUNK9 NWAY ;
            CLASS NEWVBLE      ;
            VAR BAD debtinc      ;
            OUTPUT OUT = JUNK10
            MEAN =
            MIN(debtinc)=MIN
            MAX(debtinc)=MAX
            N = NOBS         ;
RUN;

DATA JUNK10                                 ;
         SET JUNK10                          ;
         IF BAD NE 0 THEN
            LOGIT = LOG ( BAD / (1- BAD) )  ;
         ELSE IF BAD = 0 THEN LOGIT = .     ;
RUN;

PROC SQL NOPRINT;
        CREATE TABLE JUNK11 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE debtinc=. ;
RUN;    

DATA JUNK11;
        SET JUNK11;
        LOGIT=LOG( BAD / (1- BAD));
RUN; 

DATA JUNK12;
        SET JUNK10 JUNK11;
RUN;

PROC PRINT DATA = JUNK12;
RUN;

PROC PLOT DATA = JUNK12                                ;
            TITLE1 "Plot of Logit(Response) by debtinc" ;
            PLOT  LOGIT* debtinc                        ;
RUN;
title;

proc plot data = JUNK12;
        plot BAD*debtinc;
        plot _freq_*debtinc;
        TITLE2 "Plot of Response by debtinc" ;
RUN;
title;

PROC PRINT DATA = JUNK12 LABEL SPLIT = '*' NOOBS         ;
            TITLE3 "Table of Response by Grouped debtinc" ;
            VAR NEWVBLE NOBS debtinc MIN MAX BAD          ;
            LABEL NEWVBLE = "debtinc Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
RUN;
title;


/* Step 4: variable = delinq */
PROC RANK DATA = MODEL_DEV1 (KEEP=BAD delinq)
          GROUPS = 10
          OUT = JUNK13     ;
          RANKS NEWVBLE   ;
          VAR delinq       ;
RUN;

PROC SUMMARY DATA = JUNK13 NWAY ;
            CLASS NEWVBLE      ;
            VAR BAD delinq      ;
            OUTPUT OUT = JUNK14
               MEAN =
               MIN(delinq)=MIN
               MAX(delinq)=MAX
                N = NOBS         ;
RUN;

DATA JUNK14                                 ;
         SET JUNK14                          ;
         IF BAD NE 0 THEN
            LOGIT = LOG ( BAD / (1- BAD) )  ;
         ELSE IF BAD = 0 THEN LOGIT = .     ;
RUN;

PROC SQL NOPRINT;
        CREATE TABLE JUNK15 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE delinq=. ;
RUN;    

DATA JUNK15;
        SET JUNK15;
        LOGIT=LOG( BAD / (1- BAD));
RUN; 

DATA JUNK16;
        SET JUNK14 JUNK15;
RUN;

PROC PLOT DATA = JUNK16                                ;
            TITLE1 "Plot of Logit(Response) by delinq" ;
            PLOT  LOGIT* delinq                        ;
RUN;
title;

proc plot data = JUNK16;
        plot BAD*delinq;
        plot _freq_*delinq;
        TITLE2 "Plot of Response by delinq" ;
RUN;
title;

PROC PRINT DATA = JUNK16 LABEL SPLIT = '*' NOOBS         ;
            TITLE3 "Table of Response by Grouped delinq" ;
            VAR NEWVBLE NOBS delinq MIN MAX BAD          ;
            LABEL NEWVBLE = "delinq Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
RUN;
title;


/* Step 5: variable = derog */
PROC RANK DATA = MODEL_DEV1 (KEEP=BAD derog)
          GROUPS = 10
          OUT = JUNK17     ;
          RANKS NEWVBLE   ;
          VAR derog      ;
RUN;

PROC SUMMARY DATA = JUNK17 NWAY ;
            CLASS NEWVBLE      ;
            VAR BAD derog      ;
            OUTPUT OUT = JUNK18
            MEAN =
            MIN(derog)=MIN
            MAX(derog)=MAX
            N = NOBS         ;
RUN;

DATA JUNK18                                 ;
         SET JUNK18                          ;
         IF BAD NE 0 THEN
            LOGIT = LOG ( BAD / (1- BAD) )  ;
         ELSE IF BAD = 0 THEN LOGIT = .     ;
RUN;

PROC SQL NOPRINT;
        CREATE TABLE JUNK19 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE derog=. ;
RUN;    

DATA JUNK19;
        SET JUNK19;
        LOGIT=LOG( BAD / (1- BAD));
RUN; 

DATA JUNK20;
        SET JUNK18 JUNK19;
RUN;

PROC PLOT DATA = JUNK20                                ;
            TITLE1 "Plot of Logit(Response) by derog" ;
            PLOT  LOGIT* derog                        ;
RUN;
title;

proc plot data = JUNK20;
        plot BAD*derog;
        plot _freq_*derog;
        TITLE2 "Plot of Response by derog" ;
RUN;
title;

PROC PRINT DATA = JUNK20 LABEL SPLIT = '*' NOOBS         ;
            TITLE3 "Table of Response by Grouped derog" ;
            VAR NEWVBLE NOBS derog MIN MAX BAD          ;
            LABEL NEWVBLE = "derog Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
RUN;
title;


/* Step 6: variable = loan */
PROC RANK DATA = MODEL_DEV1 (KEEP=BAD loan)
          GROUPS = 10
          OUT = JUNK21     ;
          RANKS NEWVBLE   ;
          VAR loan       ;
RUN;

PROC SUMMARY DATA = JUNK21 NWAY ;
            CLASS NEWVBLE      ;
            VAR BAD loan      ;
            OUTPUT OUT = JUNK22
            MEAN =
            MIN(loan)=MIN
            MAX(loan)=MAX
            N = NOBS         ;
RUN;

DATA JUNK22                                 ;
         SET JUNK22                          ;
         IF BAD NE 0 THEN
            LOGIT = LOG ( BAD / (1- BAD) )  ;
         ELSE IF BAD = 0 THEN LOGIT = .     ;
RUN;

PROC SQL NOPRINT;
        CREATE TABLE JUNK23 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE loan=. ;
RUN;    

DATA JUNK23;
        SET JUNK23;
        LOGIT=LOG( BAD / (1- BAD));
RUN; 

DATA JUNK24;
        SET JUNK22 JUNK23;
RUN;

PROC PLOT DATA = JUNK24                                ;
            TITLE1 "Plot of Logit(Response) by loan" ;
            PLOT  LOGIT* loan                       ;
RUN;
title;

proc plot data = JUNK24;
        plot BAD*loan;
        plot _freq_*loan;
        TITLE2 "Plot of Response by loan" ;
RUN;
title;

PROC PRINT DATA = JUNK24 LABEL SPLIT = '*' NOOBS         ;
            TITLE3 "Table of Response by Grouped loan" ;
            VAR NEWVBLE NOBS loan MIN MAX BAD          ;
            LABEL NEWVBLE = "loan Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
RUN;
title;


/* Step 7: variable = mortdue */
PROC RANK DATA =MODEL_DEV1(KEEP=BAD MORTDUE)
          GROUPS = 10
          OUT = JUNK25     ;
          RANKS NEWVBLE         ;
          VAR MORTDUE        ;
RUN                        ;

PROC SUMMARY DATA = JUNK25 NWAY ;
             CLASS NEWVBLE             ;
             VAR BAD MORTDUE         ;
             OUTPUT OUT = JUNK26
                  MEAN =
                  MIN(MORTDUE)=MIN
                  MAX(MORTDUE)=MAX
                  N = NOBS         ;
RUN                            ;

DATA JUNK26                    ;
       SET JUNK26                 ;
       IF BAD NE 0 THEN
          LOGIT = LOG ( BAD / (1- BAD) ) ;
          ELSE IF BAD = 0 THEN LOGIT = .       ;
RUN                            ;

PROC SQL NOPRINT;
        CREATE TABLE JUNK27 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE MORTDUE=. ;
RUN      ;

DATA JUNK27;
      SET JUNK27;
      LOGIT=LOG(BAD/(1-BAD));
RUN;

DATA JUNK28;
      SET JUNK26 JUNK27;
RUN;

PROC PLOT DATA = JUNK28        ;
          TITLE1 "Plot of Logit(Response) by MORTDUE" ;
          PLOT  LOGIT* MORTDUE      ;
RUN                            ;
title;

PROC PLOT data=JUNK28;
          plot BAD*MORTDUE;
          plot _freq_*MORTDUE;
          TITLE2 "Plot of Response by MORTDUE" ;
RUN;
title;

PROC PRINT DATA = JUNK28 LABEL SPLIT = '*' NOOBS ;
           TITLE3 "Table of Response by Grouped MORTDUE" ;
           VAR NEWVBLE NOBS MORTDUE MIN MAX BAD ;
           LABEL NEWVBLE = "MORTDUE Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
RUN;
title;


/* Step 8: variable = ninq */
PROC RANK DATA =MODEL_DEV1(KEEP=BAD NINQ)
          GROUPS = 10
          OUT = JUNK29     ;
          RANKS NEWVBLE         ;
          VAR NINQ        ;
RUN                        ;

PROC SUMMARY DATA = JUNK29 NWAY ;
             CLASS NEWVBLE             ;
             VAR BAD NINQ         ;
             OUTPUT OUT = JUNK30
                  MEAN =
                  MIN(NINQ)=MIN
                  MAX(NINQ)=MAX
                  N = NOBS         ;
RUN                            ;

DATA JUNK30                     ;
      SET JUNK30                 ;
      IF BAD NE 0 THEN
         LOGIT = LOG ( BAD / (1- BAD) ) ;
         ELSE IF BAD = 0 THEN LOGIT = .       ;
RUN                            ;

PROC SQL NOPRINT;
         CREATE TABLE JUNK31 AS
         SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
         FROM MODEL_DEV1
         WHERE NINQ=.;
RUN      ;

DATA JUNK31;
        SET JUNK31;
        LOGIT=LOG(BAD/(1-BAD));
RUN;

DATA JUNK32;
       SET JUNK30 JUNK31;
RUN;

PROC PLOT DATA = JUNK32         ;
          TITLE1 "Plot of Logit(Response) by NINQ" ;
          PLOT  LOGIT* NINQ      ;
RUN                            ;
title;

PROC PLOT data=JUNK32;
          plot BAD*NINQ;
          plot _freq_*NINQ;
          TITLE2 "Plot of Response by NINQ" ;
RUN;
title;

PROC PRINT DATA = JUNK32 LABEL SPLIT = '*' NOOBS ;
           TITLE3 "Table of Response by Grouped NINQ" ;
           VAR NEWVBLE NOBS NINQ MIN MAX BAD ;
           LABEL NEWVBLE = "NINQ Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
RUN;
title;


/* Step 9: variable = value */
PROC RANK DATA =MODEL_DEV1(KEEP=BAD VALUE)
          GROUPS = 10
          OUT = JUNK33    ;
          RANKS NEWVBLE         ;
          VAR VALUE       ;
RUN;

PROC SUMMARY DATA = JUNK33 NWAY ;
             CLASS NEWVBLE             ;
             VAR BAD VALUE        ;
             OUTPUT OUT = JUNK34
                  MEAN =
                  MIN(VALUE)=MIN
                  MAX(VALUE)=MAX
                  N = NOBS         ;
RUN                            ;

DATA JUNK34                     ;
     SET JUNK34                ;
     IF BAD NE 0 THEN
        LOGIT = LOG ( BAD / (1- BAD) ) ;
        ELSE IF BAD = 0 THEN LOGIT = .       ;
RUN                            ;

PROC SQL NOPRINT;
        CREATE TABLE JUNK35 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE VALUE=.;
RUN;

DATA JUNK35;
     SET JUNK35;
     LOGIT=LOG(BAD/(1-BAD));
RUN;

DATA JUNK36;
     SET JUNK34 JUNK35;
RUN;

PROC PLOT DATA = JUNK36         ;
          TITLE1 "Plot of Logit(Response) by VALUE" ;
          PLOT  LOGIT* VALUE      ;
RUN                            ;
title;

PROC PLOT data=JUNK36;
          plot BAD*VALUE;
          plot _freq_*VALUE;
          TITLE2 "Plot of Response by VALUE" ;
RUN;
title;

PROC PRINT DATA = JUNK36 LABEL SPLIT = '*' NOOBS ;
           TITLE3 "Table of Response by Grouped VALUE" ;
           VAR NEWVBLE NOBS VALUE MIN MAX BAD ;
           LABEL NEWVBLE = "VALUE Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
RUN;
title;


/* Step 10: variable = yoj */
PROC RANK DATA =MODEL_DEV1(KEEP=BAD yoj)
          GROUPS = 10
          OUT = JUNK37     ;
          RANKS NEWVBLE         ;
          VAR yoj     ;
RUN;

PROC SUMMARY DATA = JUNK37 NWAY ;
             CLASS NEWVBLE             ;
             VAR BAD yoj      ;
             OUTPUT OUT = JUNK38
                  MEAN =
                  MIN(yoj)=MIN
                  MAX(yoj)=MAX
                  N = NOBS         ;
RUN;

DATA JUNK38                     ;
     SET JUNK38                 ;
     IF BAD NE 0 THEN
        LOGIT = LOG ( BAD / (1- BAD) ) ;
        ELSE IF BAD = 0 THEN LOGIT = .       ;
RUN;

PROC SQL NOPRINT;
         CREATE TABLE JUNK39 AS
         SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
         FROM MODEL_DEV1
         WHERE yoj=.;
RUN             ;

DATA JUNK39;
     SET JUNK39;
     LOGIT=LOG(BAD/(1-BAD));
RUN;

DATA JUNK40;
     SET JUNK38 JUNK39;
RUN;

PROC PLOT DATA = JUNK40         ;
          TITLE1 "Plot of Logit(Response) by yoj" ;
          PLOT  LOGIT*yoj     ;
RUN;
title;

PROC PLOT data=JUNK40;
        plot BAD*yoj;
        plot _freq_*yoj;
        TITLE2 "Plot of Response byyoj" ;
RUN;
title;

PROC PRINT DATA = JUNK40 LABEL SPLIT = '*' NOOBS ;
           TITLE3 "Table of Response by Grouped yoj" ;
           VAR NEWVBLE NOBS yoj MIN MAX BAD ;
           LABEL NEWVBLE = "yoj Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                  ;
RUN;
title;


/* DATA Preproccessing 2*/
/* Creating dummy variables, filling missing values and data transformation on the Dev set*/
data MODEL_DEV11;
  set MODEL_DEV;
  JOB_Mgr=(JOB='Mgr');
  JOB_Office=(JOB='Office');
  JOB_Other=(JOB='Other');
  JOB_ProfExe=(JOB='ProfExe');
  JOB_Sales=(JOB='Sales');
  JOB_Self=(JOB='Self');
  JOB_miss=(JOB=' ');
  REASON_DebtCon=(REASON='DebtCon');
  REASON_HomeImp=(REASON='HomeImp');
  REASON_Miss=(REASON=' ');
  
  if CLAGE=. then CLAGE=94.325;
  if CLAGE>295 then CLAGE=295;
  if CLNO<10 then CLNO=0;
  if CLNO=. then CLNO= 31.3878;
  if CLNO<15 then CLNO= 15;
     DEBTINC_MISS=(DEBTINC=.);
  if DELINQ=. then DELINQ=0;
  if DEROG=. then DEROG=0;
  if LOAN>30500 then LOAN=30500;
  if MORTDUE=. then MORTDUE= 54175.95;
  if NINQ=. then NINQ=0;
     VALUE_MISS=(VALUE=.);
  if YOJ=. then YOJ=25.1988;
RUN;

PROC CONTENTS DATA=MODEL_DEV11;
RUN;

/* input 2 */
proc logistic data=MODEL_DEV11 descending;
model bad= DEBTINC_MISS JOB_Mgr JOB_Office JOB_Other JOB_ProfExe JOB_Sales JOB_Self JOB_miss REASON_DebtCon REASON_HomeImp 
           REASON_Miss VALUE_MISS clage clno delinq derog loan mortdue ninq yoj
  /selection=stepwise fast lackfit rsquare corrb stb;
RUN;

/*model evaluation on evaluation (test) set */
data val;
  set MODEL_DEV11;
Logit=
-1.2706		
+2.6731		*	DEBTINC_MISS	/*Debt to income ratio IS MISSING */
-0.5499		*	JOB_Office
-0.3044     *   JOB_ProfExe
+0.8003		*	JOB_Sales
-2.0704		*	JOB_miss
-0.2603     *   REASON_DebtCon
+4.3879		*	VALUE_MISS		/*Value of current property IS MISSING */
-0.00727	*	clage			/*Age of oldest trade line in months*/
+0.6760		*	delinq			/*Number of delinquent trade lines*/
+0.5825		*	derog			/*Number of major derogatory reports*/
+0.1180 	*	ninq			/*Number of recent credit inquiries*/
-0.0266		*	yoj				/*Years on current job*/
;
prob=1/(1+exp(-logit)); 
RUN;

proc sort data=val out=val1;
   by descending prob;
RUN;

proc rank		data = val1
				out = val_ranked
				groups = 20
				descending;
		var		prob;
		ranks	rank;
RUN;

data val_ranked(drop=rank prob);
set val_ranked;
	model_rank=rank + 1;
	model_score=prob;
RUN;

ods csv body='rank.csv';
PROC TABULATE DATA = val_ranked MISSING NOSEPS;
            CLASS model_rank        ;
            VAR   model_score     bad  ;
	TABLES model_rank=' ' ALL, model_score*MEAN*F=5.3 
           bad='BAD'*(sum='# of Bad' n='# of Acct' mean*F=5.3)/box='Rank';
RUN;
ods csv close;

proc sort data=val out=val1;
   by descending prob;
RUN;



%let ds=val1;                   /*output dataset from proc logistic*/
%let response=bad;           /*response variable */

options mprint;
%macro charts(role=);

proc rank data = &ds out = gar;
 where prob^=.;;
 var prob;
 ranks rp;
RUN;

proc sql;
  select count(*) as tot_obs,
         sum(&response=1) as resp1,
         sum(&response=0) as resp0,
         mean(&response) as resprate
  into :tot_obs, :resp1, :resp0, :resprate
  from gar;
quit;

proc sort data = &ds out=preds1 (keep=prob &response);
 where prob^=.;
 by descending prob;
RUN;

                     /*** Lift chart and Moving Avg(Gains Chart)***/

data lft (keep=c_resp c_perf c_obs &response prob t_resp m_avg c_prob avg_resp);
 set preds1;

 if _n_ le &resp1 then c_perf = _n_ / &resp1;
 else                  c_perf = 1;

 if &response = 1 then do;
   t_resp+1;
   c_resp = t_resp/&resp1;
 end;
 c_obs = _n_ / &tot_obs;

 c_prob + prob;
 m_avg=c_prob/_n_;

 avg_resp = &resprate;

 attrib
        c_resp label = 'Cumulative Response'
        m_avg  label = 'Predicted Prob'
        c_prob label = 'Cumulative Predicted Prob'
        c_obs  label = 'Cumulative Population';
RUN;


proc plot data = lft ;                 /*** Lift Chart ***/
 plot (c_resp  c_obs)*c_obs='*' /overlay;
                              
 label c_obs='Cumulative Population'
       c_resp='Cumulative Response';
 
*title "Lift Chart - &role";
RUN;
title;

proc plot data = lft ;                 /*** Moving Avg.***/
 plot (m_avg avg_resp )*c_obs='*' / overlay ;
 format m_avg c_obs avg_resp percent6.;
*title "Gains Chart - &role";
RUN;
title;

proc datasets library=work;
  delete preds1 gar lft;
quit;

%mend;

%charts(role=model)




