*-- READ ME: To use this script for a given data set, perform the following manipulations of code:
	1. Update the folder name for the INPUT data to the folder where the data that you want the TABLE_1 for. 
	2. Update the second line of the SELECT clause in Section 1.3. to be the variable that one wishes to stratify on. 
	3. Update the file to INNER JOINR to in Section 1.3. to be the cohort that you wish to use within INPUT. 
	4. Update the WHERE clause in both PROC SQL commands of Section 2. to be the the variable taken from 3. --;

LIBNAME COHORT 'H:\Projekt\Familiarity of mono-MTX\Data Extraction';
LIBNAME SRQ 'K:\Reuma\RASPA 2021\01. Data Warehouse\01. Processed Data\01. SRQ';
LIBNAME MGR 'K:\Reuma\RASPA 2021\01. Data Warehouse\02. Raw Data\02. MGR';
LIBNAME INPUT 'H:\Projekt\Familiarity of mono-MTX\Data Extraction\Sensitivity cohorts';

*-- SECTION 0: PREPARE THE INPUT DATA -- SECTION 0: PREPARE THE INPUT DATA -- SECTION 0: PREPARE THE INPUT DATA -- SECTION 0: PREPARE THE INPUT DATA -- SECTION 0: PREPARE THE INPU  --;
PROC SQL;
	CREATE TABLE DF_0 AS
	SELECT DISTINCT pid, kon, age_at_MTX_start, dis_deb_year
	FROM COHORT.COHORT
;
	CREATE TABLE DF_0 AS
	SELECT a.*, b.order_date AS first_MTX_presc
	FROM DF_0 AS a
	LEFT JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid = b.pid
	WHERE preparat_kod = "MTX"
;
	CREATE TABLE DF_0 AS
	SELECT *
	FROM DF_0
	GROUP BY pid
	HAVING first_MTX_presc = min(first_MTX_presc)
;
quit;

*-- SECTION 1: ACQUIRING THE RELEVANT VARIABLES -- SECTION 1: ACQUIRING THE RELEVANT VARIABLES -- SECTION 1: ACQUIRING THE RELEVANT VARIABLES -- SECTION 1: ACQUIRING THE RELEVANT V --;
*-- SECTION 1.1: ACQUIRING VARIABLE OF SERO+ AND AGE AT TREATMENT START -- SECTION 1.1: ACQUIRING VARIABLE OF SERO+ AND AGE AT TREATMENT START -- SECTION 1.1: ACQUIRING VARIABLE OF --;

PROC SQL;
	CREATE TABLE DF_1_1 AS
	SELECT a.*, YEAR(a.first_MTX_presc) AS year_of_MTX_start,
		   CATS(SUBSTR(b.diagnoskod_1, 1, 3), SUBSTR(b.diagnoskod_1, 5, 1)) AS icd_1,
		   CATS(SUBSTR(b.diagnoskod_2, 1, 3), SUBSTR(b.diagnoskod_2, 5, 1)) AS icd_2
	FROM DF_0 AS a
	LEFT JOIN SRQ.SRQ_BASDATA AS b
	ON a.pid = b.pid
;
	CREATE TABLE DF_1_1 AS
	SELECT *, CASE WHEN icd_1 = "M059" OR icd_1 = "M058" THEN 1
				   WHEN icd_1 = "M060" OR icd_1 = "M068" THEN 0
				   ELSE . END AS SERO_POS
	FROM DF_1_1
;
	CREATE TABLE DF_1_1 AS
	SELECT pid, kon, age_at_MTX_start, dis_deb_year, year_of_MTX_start,
		   CASE WHEN (SERO_POS = . AND (icd_2 = "M059" OR icd_2 = "MO58")) THEN 1
		   	    WHEN (SERO_POS = . AND (icd_2 = "M060" OR icd_2 = "M068")) THEN 0
				ELSE SERO_POS END AS SERO_POS
	FROM DF_1_1
;
quit;

*-- SECTION 1.2: ACQUIRING DATA ON ALL FIRST-DEGREE RELATIVES -- SECTION 1.2: ACQUIRING DATA ON ALL FIRST-DEGREE RELATIVES -- SECTION 1.2: ACQUIRING DATA ON ALL FIRST-DEGREE RELATI --;
*-- Note that this does not take into account settings where the parents were missing. They still had two parents of course but data would have been unavailable on them, no? --;

PROC SQL;
	CREATE TABLE MGR_LONG AS
	(SELECT pid, pid_child AS pid_rel, "Child" as reltyp
	 FROM MGR.CHILDREN
	 OUTER UNION CORR
	 SELECT pid, pid_sibling AS pid_rel, syskontyp AS reltyp
	 FROM MGR.SIBLINGS)
	OUTER UNION CORR
    (SELECT pid, pid_father AS pid_rel, "Father" AS reltyp
	 FROM MGR.PARENTS
	 OUTER UNION CORR
	 SELECT pid, pid_mother AS pid_rel, "Mother" AS reltyp
	 FROM MGR.PARENTS)
	ORDER BY pid
;
	CREATE TABLE DF_1_2 AS
	SELECT *
	FROM DF_1_1 AS a
	LEFT JOIN MGR_LONG AS b
	ON a.pid = b.pid
	WHERE reltyp = "Father" OR
		  reltyp = "Mother" OR
		  reltyp = "Children" OR
		  reltyp = "Helsyskon"
;
	CREATE TABLE DF_1_2 AS
	SELECT DISTINCT pid, kon, age_at_MTX_start, dis_deb_year, year_of_MTX_start, SERO_POS, COUNT(pid) AS n_first_degree_rel
	FROM DF_1_2
	GROUP BY pid
;
quit;

*-- SECTION 1.3: INNER JOIN TO THE TARGET COHORT -- SECTION 1.3: INNER JOIN TO THE TARGET COHORT -- SECTION 1.3: INNER JOIN TO THE TARGET COHORT -- SECTION 1.3: INNER JOIN TO THE  --;
PROC SQL;
	CREATE TABLE DF_1_3 AS
	SELECT DISTINCT a.pid, a.kon, a.age_at_MTX_start, a.year_of_MTX_start, a.SERO_POS, a.n_first_degree_rel,
		   b.persist_3yr
	FROM DF_1_2 AS a
	INNER JOIN INPUT.SENSITIVITY_ROB AS b
	ON a.pid = b.pid
	ORDER BY pid
;
quit;

*-- SECTION 2: CREATE THE TARGET TABLE -- -- SECTION 2: CREATE THE TARGET TABLE ---- SECTION 2: CREATE THE TARGET TABLE ---- SECTION 2: CREATE THE TARGET TABLE ---- SECTION 2: CRE --;
PROC SQL;
	CREATE TABLE TABLE_1_0 AS
	SELECT *, CASE WHEN (year_of_MTX_start <= MEDIAN(year_of_MTX_start)) THEN year_of_MTX_start ELSE . END AS year_of_MTX_start_Q1,
		      CASE WHEN (year_of_MTX_start >= MEDIAN(year_of_MTX_start)) THEN year_of_MTX_start ELSE . END AS year_of_MTX_start_Q3,
			  CASE WHEN (n_first_degree_rel <= MEDIAN(n_first_degree_rel)) THEN n_first_degree_rel ELSE . END AS n_first_degree_rel_Q1,
			  CASE WHEN (n_first_degree_rel >= MEDIAN(n_first_degree_rel)) THEN n_first_degree_rel ELSE . END AS n_first_degree_rel_Q3
	FROM DF_1_3
	WHERE persist_3yr = 0
;
	CREATE TABLE TABLE_1_0 AS
	SELECT COUNT(*) AS N, SUM(CASE WHEN (kon = "Kvinna") THEN 1 ELSE 0 END) AS Female, MEAN(CASE WHEN (kon = "Kvinna") THEN 1 ELSE 0 END) AS Female_percent,
		   SUM(CASE WHEN (SERO_POS = 1) THEN 1 ELSE 0 END) AS SERO_POS, MEAN(CASE WHEN (SERO_POS = 1) THEN 1 ELSE 0 END) AS SERO_POS_percent, SUM(CASE WHEN (SERO_POS = .) THEN 1 ELSE 0 END) AS SERO_POS_MISS,
		   MEAN(age_at_MTX_start) AS age_at_MTX_start, STD(age_at_MTX_start) AS age_at_MTX_start_SD,
		   MEDIAN(year_of_MTX_start) AS year_of_MTX_start, MEDIAN(year_of_MTX_start_Q1) AS year_of_MTX_start_Q1, MEDIAN(year_of_MTX_start_Q3) AS year_of_MTX_start_Q3,
		   MEDIAN(n_first_degree_rel) AS n_first_degree_rel, MEDIAN(n_first_degree_rel_Q1) AS n_first_degree_rel_Q1, MEDIAN(n_first_degree_rel_Q3) AS n_first_degree_rel_Q3
	FROM TABLE_1_0
;
	CREATE TABLE TABLE_1_0 AS
	SELECT N, CAT(Female, " (", 100 * ROUND(Female_percent, 0.01), "%)") AS Female,
			  CAT(SERO_POS, " (", 100 * ROUND(SERO_POS_percent, 0.01), "%), ", SERO_POS_MISS, " missing") AS SERO_POS,
			  CAT(ROUND(age_at_MTX_start, 1), " (", ROUND(age_at_MTX_start_SD, 1), ")") AS age_at_MTX_start,
			  CAT(ROUND(year_of_MTX_start, 1), " (", ROUND(year_of_MTX_start_Q1, 1), "-", ROUND(year_of_MTX_start_Q3, 1), ")") AS year_of_MTX_start,
			  CAT(ROUND(n_first_degree_rel, 1), " (", ROUND(n_first_degree_rel_Q1, 1), "-", ROUND(n_first_degree_rel_Q3, 1), ")") AS n_first_degree_rel
	FROM TABLE_1_0
;
quit;

PROC SQL;
	CREATE TABLE TABLE_1_1 AS
	SELECT *, CASE WHEN (year_of_MTX_start <= MEDIAN(year_of_MTX_start)) THEN year_of_MTX_start ELSE . END AS year_of_MTX_start_Q1,
		      CASE WHEN (year_of_MTX_start >= MEDIAN(year_of_MTX_start)) THEN year_of_MTX_start ELSE . END AS year_of_MTX_start_Q3,
			  CASE WHEN (n_first_degree_rel <= MEDIAN(n_first_degree_rel)) THEN n_first_degree_rel ELSE . END AS n_first_degree_rel_Q1,
			  CASE WHEN (n_first_degree_rel >= MEDIAN(n_first_degree_rel)) THEN n_first_degree_rel ELSE . END AS n_first_degree_rel_Q3
	FROM DF_1_3
	WHERE persist_3yr = 1
;
	CREATE TABLE TABLE_1_1 AS
	SELECT COUNT(*) AS N, SUM(CASE WHEN (kon = "Kvinna") THEN 1 ELSE 0 END) AS Female, MEAN(CASE WHEN (kon = "Kvinna") THEN 1 ELSE 0 END) AS Female_percent,
		   SUM(CASE WHEN (SERO_POS = 1) THEN 1 ELSE 0 END) AS SERO_POS, MEAN(CASE WHEN (SERO_POS = 1) THEN 1 ELSE 0 END) AS SERO_POS_percent, SUM(CASE WHEN (SERO_POS = .) THEN 1 ELSE 0 END) AS SERO_POS_MISS,
		   MEAN(age_at_MTX_start) AS age_at_MTX_start, STD(age_at_MTX_start) AS age_at_MTX_start_SD,
		   MEDIAN(year_of_MTX_start) AS year_of_MTX_start, MEDIAN(year_of_MTX_start_Q1) AS year_of_MTX_start_Q1, MEDIAN(year_of_MTX_start_Q3) AS year_of_MTX_start_Q3,
		   MEDIAN(n_first_degree_rel) AS n_first_degree_rel, MEDIAN(n_first_degree_rel_Q1) AS n_first_degree_rel_Q1, MEDIAN(n_first_degree_rel_Q3) AS n_first_degree_rel_Q3
	FROM TABLE_1_1
;
	CREATE TABLE TABLE_1_1 AS
	SELECT N, CAT(Female, " (", 100 * ROUND(Female_percent, 0.01), "%)") AS Female,
			  CAT(SERO_POS, " (", 100 * ROUND(SERO_POS_percent, 0.01), "%), ", SERO_POS_MISS, " missing") AS SERO_POS,
			  CAT(ROUND(age_at_MTX_start, 1), " (", ROUND(age_at_MTX_start_SD, 1), ")") AS age_at_MTX_start,
			  CAT(ROUND(year_of_MTX_start, 1), " (", ROUND(year_of_MTX_start_Q1, 1), "-", ROUND(year_of_MTX_start_Q3, 1), ")") AS year_of_MTX_start,
			  CAT(ROUND(n_first_degree_rel, 1), " (", ROUND(n_first_degree_rel_Q1, 1), "-", ROUND(n_first_degree_rel_Q3, 1), ")") AS n_first_degree_rel
	FROM TABLE_1_1
;
quit;
