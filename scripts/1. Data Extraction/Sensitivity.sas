LIBNAME INPUT 'H:\Projekt\Familiarity of mono-MTX\Data Extraction';
LIBNAME SRQ 'K:\Reuma\RASPA 2021\01. Data Warehouse\01. Processed Data\01. SRQ';
LIBNAME NPR 'K:\Reuma\RASPA 2021\01. Data Warehouse\02. Raw Data\04. NPR';
LIBNAME PDR 'K:\Reuma\RASPA 2021\01. Data Warehouse\01. Processed Data\05. PDR';
LIBNAME MGR 'K:\Reuma\RASPA 2021\01. Data Warehouse\02. Raw Data\02. MGR';
LIBNAME CACHE 'H:\Projekt\Familiarity of mono-MTX\Data Extraction\Sensitivity cohorts\Data cache';
LIBNAME OUTPUT 'H:\Projekt\Familiarity of mono-MTX\Data Extraction\Sensitivity cohorts';

*-- SECTION 1: A COHORT OF ONLY SIBLINGS -- SECTION 1: A COHORT OF ONLY SIBLINGS -- SECTION 1: A COHORT OF ONLY SIBLINGS -- SECTION 1: A COHORT OF ONLY SIBLINGS -- SECTION 1: A COH --;

PROC SQL;
	CREATE TABLE SENSITIVITY_SIB AS
	SELECT *
	FROM INPUT.COHORT
	WHERE reltyp = "Helsyskon"
	ORDER BY pid, pid_rel
;
quit;

DATA OUTPUT.SENSITIVITY_SIB;
	SET SENSITIVITY_SIB;
run;

*-- SECTION 2: PREDICTION COHORT OF ONLY SIBLINGS -- SECTION 2: PREDICTION COHORT OF ONLY SIBLINGS -- SECTION 2: PREDICTION COHORT OF ONLY SIBLINGS -- SECTION 2: PREDICTION COHORT  --;
*-- 1. Append the date of first MTX prescriptions for both relatives and index patients to the siblings. --;

PROC SQL;
	CREATE TABLE SENSITIVITY_SIB_INDEX AS
	SELECT a.*, b.order_date AS ind_first_MTX
	FROM OUTPUT.SENSITIVITY_SIB AS a
	LEFT JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid = b.pid
	WHERE preparat_kod = "MTX"
;
	CREATE TABLE SENSITIVITY_SIB_INDEX AS
	SELECT *
	FROM SENSITIVITY_SIB_INDEX
	GROUP BY pid
	HAVING ind_first_MTX = min(ind_first_MTX)
;
	CREATE TABLE SENSITIVITY_SIB_INDEX AS
	SELECT a.*, b.order_date AS rel_first_MTX
	FROM SENSITIVITY_SIB_INDEX AS a
	LEFT JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid_rel = b.pid
	WHERE preparat_kod = "MTX"
;
	CREATE TABLE SENSITIVITY_SIB_INDEX AS
	SELECT *
	FROM SENSITIVITY_SIB_INDEX
	GROUP BY pid_rel
	HAVING rel_first_MTX = min(rel_first_MTX)
	ORDER BY pid, pid_rel
;
quit;

*-- SECTION 2.1: INDEX PATIENTS FOR PERSISTENCE AT 1 YEAR -- SECTION 2.1: INDEX PATIENTS FOR PERSISTENCE AT 1 YEAR -- SECTION 2.1: INDEX PATIENTS FOR PERSISTENCE AT 1 YEAR -- SECTI --;
*-- 1. Exclude all those that did not have a relative starting MTX more than a year before themselves. --;

PROC SQL;
	CREATE TABLE SENSITIVITY_SIB_INDEX_1YR AS
	SELECT *
	FROM SENSITIVITY_SIB_INDEX
	WHERE ind_first_MTX > rel_first_MTX + 365
;
	CREATE TABLE SENSITIVITY_SIB_INDEX_1YR AS
	SELECT DISTINCT pid, persist_1yr,
		   CASE WHEN (SUM(rel_persist_1yr) > 0) THEN 1 ELSE 0 END AS any_rel_persist_1yr,
		   kon, age_at_MTX_start, dis_deb_year
	FROM SENSITIVITY_SIB_INDEX_1YR
	GROUP BY pid
;
quit;

DATA OUTPUT.SENSITIVITY_INDEX_1YR_SIB;
	SET SENSITIVITY_SIB_INDEX_1YR;
run;

*-- SECTION 2.2: INDEX PATIENTS FOR PERSISTENCE AT 3 YEARS -- SECTION 2.2: INDEX PATIENTS FOR PERSISTENCE AT 3 YEARS -- SECTION 2.2: INDEX PATIENTS FOR PERSISTENCE AT 3 YEARS -- SE --;
*-- 1. Exclude all those that did not have a relative starting MTX more than three years before themselves. --;

PROC SQL;
	CREATE TABLE SENSITIVITY_SIB_INDEX_3YR AS
	SELECT *
	FROM SENSITIVITY_SIB_INDEX
	WHERE ind_first_MTX > rel_first_MTX + 1096
;
	CREATE TABLE SENSITIVITY_SIB_INDEX_3YR AS
	SELECT DISTINCT pid, persist_3yr,
		   CASE WHEN (SUM(rel_persist_3yr) > 0) THEN 1 ELSE 0 END AS any_rel_persist_3yr,
		   kon, age_at_MTX_start, dis_deb_year
	FROM SENSITIVITY_SIB_INDEX_3YR
	GROUP BY pid
;
quit;

DATA OUTPUT.SENSITIVITY_INDEX_3YR_SIB;
	SET SENSITIVITY_SIB_INDEX_3YR;
run;

* --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ;
* --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ;
* --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ;

*-- SECTION 3: A 'ROBUST' COHORT -- SECTION 3: A 'ROBUST' COHORT -- SECTION 3: A 'ROBUST' COHORT -- SECTION 3: A 'ROBUST' COHORT -- SECTION 3: A 'ROBUST' COHORT -- SECTION 3: A RO --;
*-- STEP 1 -- REMOVE DUPLICATED IDS -- STEP 1 -- REMOVE DUPLICATED IDS -- STEP 1 -- REMOVE DUPLICATED IDS -- STEP 1 -- REMOVE DUPLICATED IDS -- STEP 1 -- REMOVE DUPLICATED IDS --  --;
PROC SQL;
	CREATE TABLE ROB_1 AS
	SELECT *, COUNT(pid) AS n_duplicates
	FROM SRQ.SRQ_BASDATA
	GROUP BY pid
	HAVING n_duplicates = 1
;
quit;

DATA CACHE.ROB_1;
	SET ROB_1;
run;

*-- STEP 2 -- FILTER OUT NON-RA PATIENTS -- STEP 2 -- FILTER OUT NON-RA PATIENTS -- STEP 2 -- FILTER OUT NON-RA PATIENTS -- STEP 2 -- FILTER OUT NON-RA PATIENTS -- STEP 2 -- FILT -- ;
PROC SQL;
	CREATE TABLE ROB_2 AS
	SELECT *
	FROM CACHE.ROB_1
	WHERE diagnosgrupp1 = "RA" OR diagnosgrupp2 = "RA"
;
quit;

DATA CACHE.ROB_2;
	SET ROB_2;
run;

*-- STEP 3 -- FILTER OUT LATE AND EARLY SRQ INCLUSIONS -- STEP 3 -- FILTER OUT LATE AND EARLY SRQ INCLUSIONS -- STEP 3 -- FILTER OUT LATE AND EARLY SRQ INCLUSIONS -- STEP 3 -- FI -- ;
PROC SQL;
	CREATE TABLE ROB_3 AS
	SELECT *
	FROM CACHE.ROB_2
	WHERE YEAR(inclusion_date) BETWEEN 2006 AND 2019
;
quit;

DATA CACHE.ROB_3;
	SET ROB_3;
run;

*-- STEP 4 -- FILTER OUT NON EARLY RA PATIENTS -- STEP 4 -- FILTER OUT NON EARLY RA PATIENTS -- STEP 4 -- FILTER OUT NON EARLY RA PATIENTS -- STEP 4 -- FILTER OUT NON EARLY RA PA -- ;
PROC SQL;
	CREATE TABLE ROB_4 AS
	SELECT *
	FROM CACHE.ROB_3
	WHERE tidig_ra = 1
;
quit;

DATA CACHE.ROB_4;
	SET ROB_4;
run;

*-- STEP 5 -- FILTER OUT INDIVIDUALS INCLUDED INTO SRQ A YEAR PRIOR TO OR AFTER FIRST RA VISIT -- STEP 5 -- FILTER OUT INDIVIDUALS INCLUDED INTO SRQ A YEAR PRIOR TO OR AFTER FIRS -- ;
PROC SQL;
	CREATE TABLE ROB_5 AS
	SELECT DISTINCT a.pid, a.inclusion_date, b.visit_date
	FROM CACHE.ROB_4 AS a
	INNER JOIN (SELECT pid, visit_date
			    FROM SRQ.SRQ_BESOKSDATA
				WHERE startbesok = 1) AS b
	ON a.pid = b.pid
	WHERE . < ABS(INTCK("days", b.visit_date, a.inclusion_date)) < 365.25
;
quit;

DATA CACHE.ROB_5;
	SET ROB_5;
run;

*-- STEP 6 -- FILTER OUT INDIVIDUALS INCLUDED INTO SRQ A YEAR PRIOR TO OR AFTER FIRST RA VISIT (NPR) -- STEP 6 -- FILTER OUT INDIVIDUALS INCLUDED INTO SRQ A YEAR PRIOR TOR OR AFT -- ;
*-- Note that differences from STEP 5 in the use of INNER/LEFT JOIN is intentional as individuals from SRQ may not exist within NPR and these should not be removed here. --;
*-- Note also that we do not filter out those with missing in the final step (i.e. no `. < ABS...`) for the same reason. --;
PROC SQL;
	CREATE TABLE ROB_6 AS
	SELECT *
	FROM (SELECT *
		  FROM NPR.IN
		  OUTER UNION CORR
		  SELECT *
		  FROM NPR.OUT) AS a
	LEFT JOIN (SELECT DISTINCT diagnosgrupp1 AS diagnosgrupp,
					  COMPRESS(diagnoskod_1, ".") AS diagnoskod
			   FROM SRQ.SRQ_BASDATA) AS b
	ON a.dia1 = b.diagnoskod OR a.dia2 = b.diagnoskod
	WHERE b.diagnosgrupp = "RA"
;
	CREATE TABLE ROB_6 AS
	SELECT *
	FROM ROB_6
	GROUP BY pid
	HAVING indatum = MIN(indatum)
;
	CREATE TABLE ROB_6 AS
	SELECT DISTINCT a.pid, a.inclusion_date, b.indatum
	FROM CACHE.ROB_5 AS a
	LEFT JOIN ROB_6 AS b
	ON a.pid = b.pid
	WHERE ABS(INTCK("days", b.indatum, a.inclusion_date)) < 365.25
;
quit;

DATA CACHE.ROB_6;
	SET ROB_6;
run;

*-- STEP 7 -- FILTER OUT INDIVIDUALS THAT DID NOT RECEIVE MONO-MTX AS THEIR FIRST TREATMENT -- STEP 7 -- FILTER OUT INDIVIDUALS THAT DID NOT RECEIVE MONO-MTX AS THEIR FIRST TREAT -- ;
PROC SQL;
	CREATE TABLE ROB_7 AS
	SELECT *
	FROM SRQ.SRQ_TERAPI
	WHERE pid IN (SELECT DISTINCT pid
				  FROM CACHE.ROB_6)
;
	CREATE TABLE ROB_7 AS
	SELECT a.*, b.first_MTX_date
	FROM ROB_7 AS a
	INNER JOIN (SELECT pid, min(order_date) AS first_MTX_date FORMAT YYMMDD10.
				FROM ROB_7
				WHERE preparat_kod = "MTX"
				GROUP BY pid) AS b
	ON a.pid = b.pid
;
	CREATE TABLE ROB_7 AS
	SELECT DISTINCT pid
	FROM ROB_7
	GROUP BY pid
	HAVING SUM(CASE WHEN (((prep_typ = "bioprep" OR prep_typ = "csdmard" OR prep_typ = "infusion_drugs" OR prep_typ = "tsdmard") AND NOT preparat_kod = "MTX")) AND
						   (. < order_date <= first_MTX_date + 30) THEN 1 ELSE 0 END) = 0
;
quit;

DATA CACHE.ROB_7;
	SET ROB_7;
run;

*-- STEP 8 -- FILTER OUT INDIVIDUALS THAT DID NOT RECEIVE MONO-MTX AS THEIR FIRST TREATMENT (PDR) -- STEP 8 -- FILTER OUT INDIVIDUALS THAT DID NOT RECEIVE MONO-MTX AS THEIR FIRS -- ;
PROC SQL;
	CREATE TABLE ROB_8 AS
	SELECT pid, atc, edatum
	FROM PDR.PDR
	WHERE pid IN (SELECT DISTINCT pid
				  FROM CACHE.ROB_7)
	ORDER BY pid, edatum
;
	CREATE TABLE ROB_8 AS
	SELECT a.*, b.first_MTX_date
	FROM ROB_8 AS a
	LEFT JOIN (SELECT pid, min(edatum) AS first_MTX_date FORMAT YYMMDD10.
			   FROM ROB_8
			   WHERE atc = "L04AX03"
			   GROUP BY pid) AS b
	ON a.pid = b.pid
;
	CREATE TABLE ROB_8 AS
	SELECT a.*, b.prep_typ
	FROM ROB_8 AS a
	LEFT JOIN (SELECT DISTINCT prep_typ, atc_kod
			   FROM SRQ.SRQ_TERAPI
			   WHERE NOT missing(atc_kod)) AS b
	ON a.atc = b.atc_kod
	ORDER BY pid, edatum
;
	CREATE TABLE ROB_8 AS
	SELECT DISTINCT pid
	FROM ROB_8
	GROUP BY pid
	HAVING SUM(CASE WHEN (((prep_typ = "bioprep" OR prep_typ = "csdmard" OR prep_typ = "infusion_drugs" OR prep_typ = "tsdmard") AND NOT atc = "L04AX03")) AND
						   (edatum <= first_MTX_date + 30) THEN 1 ELSE 0 END) = 0
;
quit;

DATA CACHE.ROB_8;
	SET ROB_8;
run;

*-- STEP 9 -- IDENTIFYING ALL FIRST-DEGREE RELATIVES -- STEP 9 -- IDENTIFYING ALL FIRST-DEGREE RELATIVES -- STEP 9 -- IDENTIFYING ALL FIRST-DEGREE RELATIVES -- STEP 9 -- IDENT --;
PROC SQL;
	CREATE TABLE ROB_9 AS
	(SELECT pid, pid_sibling AS pid_rel, syskontyp AS reltyp
	 FROM MGR.SIBLINGS
	 OUTER UNION CORR
	 SELECT pid, pid_child AS pid_rel, "Barn" AS reltyp
	 FROM MGR.CHILDREN)
	 	OUTER UNION CORR
	(SELECT pid, pid_father AS pid_rel, "Father" AS reltyp
	 FROM MGR.PARENTS
	 OUTER UNION CORR
	 SELECT pid, pid_mother AS pid_rel, "Mother" AS reltyp
	 FROM MGR.PARENTS)
	ORDER BY pid
;
	CREATE TABLE ROB_9 AS
	SELECT *
	FROM ROB_9
	WHERE pid IN (SELECT pid
				  FROM CACHE.ROB_8)
;
	CREATE TABLE ROB_9 AS
	SELECT *
	FROM ROB_9
	WHERE pid_rel IN (SELECT pid
					  FROM CACHE.ROB_8) AND
		  NOT missing(pid_rel) AND
		 (reltyp = "Barn" OR reltyp = "Mother" OR reltyp = "Father" OR reltyp = "Helsyskon")
;
quit;

DATA CACHE.ROB_9;
	SET ROB_9;
run;

*-- STEP 10 -- FILTER OUT THOSE THAT HAD MTX MORE THAN 90 DAYS PRIOR TO FIRST MTX SRQ -- STEP 10 -- FILTER OUT THOSE THAT HAD MTX MORE THAN 90 DAYS PRIOR TO FIRST MTX SRQ -- PART --;
PROC SQL;
	CREATE TABLE ROB_10 AS
	SELECT a.pid, a.pid_rel, b.order_date AS first_MTX_presc
	FROM CACHE.ROB_9 AS a
	LEFT JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid = b.pid
	WHERE preparat_kod = "MTX"
;
	CREATE TABLE ROB_10 AS
	SELECT *
	FROM ROB_10
	GROUP BY pid
	HAVING first_MTX_presc = min(first_MTX_presc)
;
	CREATE TABLE ROB_10 AS
	SELECT a.*, b.atc, b.edatum
	FROM ROB_10 AS a
	LEFT JOIN PDR.PDR AS b
	ON a.pid = b.pid
	ORDER BY pid, edatum
;
	CREATE TABLE ROB_10 AS
	SELECT *
	FROM ROB_10
	GROUP BY pid
	HAVING SUM(CASE WHEN (INTCK("days", first_MTX_presc, edatum) < -90) AND (ATC = "L04AX03") THEN 1 ELSE 0 END) = 0
;	
	CREATE TABLE ROB_10 AS
	SELECT DISTINCT pid, pid_rel, first_MTX_presc
	FROM ROB_10
;
quit;

DATA CACHE.ROB_10;
	SET ROB_10;
run;

*-- STEP 11 -- IDENTIFY ALL THOSE THAT HAD A NON-MTX DMARD PER PDR -- STEP 11 -- IDENTIFY ALL THOSE THAT HAD A NON-MTX DMARD PER PDR -- STEP 11 -- IDENTIFY ALL THOSE THAT HAD A NON --;
PROC SQL;
	CREATE TABLE ROB_11 AS
	SELECT a.pid, a.atc, a.edatum
	FROM PDR.PDR AS a
	LEFT JOIN (SELECT DISTINCT prep_typ, atc_kod
			   FROM SRQ.SRQ_TERAPI
			   WHERE NOT missing(atc_kod)) AS b
	ON a.atc = b.atc_kod
	WHERE prep_typ = "bioprep" OR
		  prep_typ = "csdmard" OR
		  prep_typ = "infusion_drugs" OR
		  prep_typ = "tsdmard"
;
	CREATE TABLE ROB_11 AS
	SELECT *
	FROM CACHE.ROB_10 AS a
	LEFT JOIN ROB_11 AS b
	ON a.pid = b.pid
	ORDER BY pid, edatum
;
	CREATE TABLE ROB_11 AS
	SELECT *, 
		   CASE WHEN SUM(((0 <= INTCK("days", first_MTX_presc, edatum) <= 365) AND (NOT ATC = "L04AX03"))) > 0 THEN 1 ELSE . END AS fail_monotherapy_1yr,
		   CASE WHEN SUM(((0 <= INTCK("days", first_MTX_presc, edatum) <= 1096) AND (NOT ATC = "L04AX03"))) > 0 THEN 1 ELSE . END AS fail_monotherapy_3yr
	FROM ROB_11
	GROUP BY pid
;
	CREATE TABLE ROB_11 AS
	SELECT DISTINCT pid, pid_rel, fail_monotherapy_1yr, fail_monotherapy_3yr
	FROM ROB_11
;
quit;

DATA CACHE.ROB_11;
	SET ROB_11;
run;

*-- PART 12 -- APPENDING THE ADDITIONAL VARIABLES -- PART 12 -- APPENDING THE ADDITIONAL VARIABLES -- PART 12 -- APPENDING THE ADDITIONAL VARIABLES -- PART 12 -- APPENDING THE ADDI --;
*-- Use INNER JOIN here to also exclude those relatives that did not meet the previous criteria (those checked after picking up relatives). --;
PROC SQL;
	CREATE TABLE ROB_12 AS
	SELECT a.*, b.fail_monotherapy_1yr AS rel_fail_1yr, b.fail_monotherapy_3yr AS rel_fail_3yr
	FROM CACHE.ROB_11 AS a
	INNER JOIN CACHE.ROB_11 AS b
	ON a.pid_rel = b.pid
	ORDER BY pid, pid_rel
;
	CREATE TABLE ROB_12 AS
	SELECT *
	FROM ROB_12 AS a
	LEFT JOIN INPUT.COHORT AS b
	ON CATS(a.pid, a.pid_rel) = CATS(b.pid, b.pid_rel)
	ORDER BY pid, pid_rel
;
	CREATE TABLE ROB_12 AS
	SELECT pid, pid_rel, reltyp,
		   CASE WHEN (fail_monotherapy_1yr = 1) THEN 0 ELSE persist_1yr END AS persist_1yr,
		   CASE WHEN (rel_fail_1yr = 1) THEN 0 ELSE rel_persist_1yr END AS rel_persist_1yr,
		   CASE WHEN (fail_monotherapy_3yr = 1) THEN 0 ELSE persist_3yr END AS persist_3yr,
		   CASE WHEN (rel_fail_3yr = 1) THEN 0 ELSE rel_persist_3yr END AS rel_persist_3yr,
		   kon, age_at_MTX_start, dis_deb_year, three_months_responder, six_months_responder, rel_three_months_responder, rel_six_months_responder
	FROM ROB_12
;
quit;

DATA CACHE.ROB_12;
	SET ROB_12;
run;

*-- SECTION 4: PREDICTION COHORT FROM THE 'ROBUST' EXTRACTION -- SECTION 4: PREDICTION COHORT FROM THE 'ROBUST' EXTRACTION -- SECTION 4: PREDICTION COHORT FROM THE 'ROBUST' EXTRACT --;

PROC SQL;
	CREATE TABLE SENSITIVITY_ROB_INDEX AS
	SELECT a.*, b.order_date AS ind_first_MTX
	FROM OUTPUT.SENSITIVITY_ROB AS a
	LEFT JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid = b.pid
	WHERE preparat_kod = "MTX"
;
	CREATE TABLE SENSITIVITY_ROB_INDEX AS
	SELECT *
	FROM SENSITIVITY_ROB_INDEX
	GROUP BY pid
	HAVING ind_first_MTX = min(ind_first_MTX)
;
	CREATE TABLE SENSITIVITY_ROB_INDEX AS
	SELECT a.*, b.order_date AS rel_first_MTX
	FROM SENSITIVITY_ROB_INDEX AS a
	LEFT JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid_rel = b.pid
	WHERE preparat_kod = "MTX"
;
	CREATE TABLE SENSITIVITY_ROB_INDEX AS
	SELECT *
	FROM SENSITIVITY_ROB_INDEX
	GROUP BY pid_rel
	HAVING rel_first_MTX = min(rel_first_MTX)
	ORDER BY pid, pid_rel
;
quit;

*-- SECTION 4.1: INDEX PATIENTS FOR PERSISTENCE AT 1 YEAR -- SECTION 4.1: INDEX PATIENTS FOR PERSISTENCE AT 1 YEAR -- SECTION 4.1: INDEX PATIENTS FOR PERSISTENCE AT 1 YEAR -- SECTI --;

PROC SQL;
	CREATE TABLE SENSITIVITY_ROB_INDEX_1YR AS
	SELECT *
	FROM SENSITIVITY_ROB_INDEX
	WHERE ind_first_MTX > rel_first_MTX + 365
;
	CREATE TABLE SENSITIVITY_ROB_INDEX_1YR AS
	SELECT DISTINCT pid, persist_1yr,
		   CASE WHEN (SUM(rel_persist_1yr) > 0) THEN 1 ELSE 0 END AS any_rel_persist_1yr,
		   kon, age_at_MTX_start, dis_deb_year, rel_three_months_responder, rel_six_months_responder
	FROM SENSITIVITY_ROB_INDEX_1YR
	GROUP BY pid
;
quit;

DATA OUTPUT.SENSITIVITY_INDEX_1YR_ROB;
	SET SENSITIVITY_ROB_INDEX_1YR;
run;

*-- SECTION 4.2: INDEX PATIENTS FOR PERSISTENCE AT 3 YEARS -- SECTION 4.2: INDEX PATIENTS FOR PERSISTENCE AT 3 YEARS -- SECTION 4.2: INDEX PATIENTS FOR PERSISTENCE AT 3 YEARS -- SE --;

PROC SQL;
	CREATE TABLE SENSITIVITY_ROB_INDEX_3YR AS
	SELECT *
	FROM SENSITIVITY_ROB_INDEX
	WHERE ind_first_MTX > rel_first_MTX + 1096
;
	CREATE TABLE SENSITIVITY_ROB_INDEX_3YR AS
	SELECT DISTINCT pid, persist_3yr,
		   CASE WHEN (SUM(rel_persist_3yr) > 0) THEN 1 ELSE 0 END AS any_rel_persist_3yr,
		   kon, age_at_MTX_start, dis_deb_year, rel_three_months_responder, rel_six_months_responder
	FROM SENSITIVITY_ROB_INDEX_3YR
	GROUP BY pid
;
quit;

DATA OUTPUT.SENSITIVITY_INDEX_3YR_ROB;
	SET SENSITIVITY_ROB_INDEX_3YR;
run;
