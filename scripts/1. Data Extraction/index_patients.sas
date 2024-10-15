LIBNAME INPUT 'H:\Projekt\Familiarity of mono-MTX\Data Extraction';
LIBNAME OUTPUT 'H:\Projekt\Familiarity of mono-MTX\Data Extraction';
LIBNAME SRQ 'K:\Reuma\RASPA 2021\01. Data Warehouse\01. Processed Data\01. SRQ';

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 0 --- IDENTIFY THE MTX START DATES --- STEP 0 --- IDENTIFY THE MTX START DATES --- STEP 0 --- IDENTIFY THE MTX START DATES --- STEP 0 --- IDENTIFY THE MTX START DA --- ;

PROC SQL;
	CREATE TABLE INDEX_0 AS
	SELECT a.*, b.order_date AS ind_first_MTX
	FROM INPUT.COHORT AS a
	LEFT JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid = b.pid
	WHERE preparat_kod = "MTX"
;
	CREATE TABLE INDEX_0 AS
	SELECT *
	FROM INDEX_0
	GROUP BY pid
	HAVING ind_first_MTX = min(ind_first_MTX)
;
	CREATE TABLE INDEX_0 AS
	SELECT a.*, b.order_date AS rel_first_MTX
	FROM INDEX_0 AS a
	LEFT JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid_rel = b.pid
	WHERE preparat_kod = "MTX"
;
	CREATE TABLE INDEX_0 AS
	SELECT *
	FROM INDEX_0
	GROUP BY pid_rel
	HAVING rel_first_MTX = min(rel_first_MTX)
	ORDER BY pid, pid_rel
;
quit;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 1 --- IDENTIFY ALL INDIVIDUALS WITH A RELATIVE STARTING TREATMENT AT LEAST ONE YEAR PRIOR --- STEP 1 --- IDENTIFY ALL INDIVIDUALS WITH A RELATIVE STARTING TREATMEN --- ;
* -- 1. Removes all individuals without a relative starting treatment at least one year prior. 
	 2. Aggregate all individuals with multiple relatives. Use the binary, ANY family history of persistence as the explanatory variable. --;

PROC SQL;
	CREATE TABLE INDEX_1 AS
	SELECT *
	FROM INDEX_0
	WHERE ind_first_MTX > rel_first_MTX + 365
;
	CREATE TABLE INDEX_1 AS
	SELECT DISTINCT pid, p1yr,
		   CASE WHEN (SUM(p1yr_rel) > 0) THEN 1 ELSE 0 END AS any_rel_p1yr,
		   kon, age_at_MTX_start, dis_deb_year
	FROM INDEX_1
	GROUP BY pid
;
quit;

DATA OUTPUT.COHORT_INDEX_1YR;
	SET INDEX_1;
run;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 2 --- IDENTIFY ALL INDIVIDUALS WITH A RELATIVE STARTING TREATMENT AT LEAST THREE YEARS PRIOR --- STEP 2 --- IDENTIFY ALL INDIVIDUALS WITH A RELATIVE STARTING TREAT --- ;
* -- 1. Removes all individuals without a relative starting treatment at least three years prior. 
	 2. Aggregate all individuals with multiple relatives. Use the binary, ANY family history of persistence as the explanatory variable. --;

PROC SQL;
	CREATE TABLE INDEX_2 AS
	SELECT *
	FROM INDEX_0
	WHERE ind_first_MTX > rel_first_MTX + 1096
;
	CREATE TABLE INDEX_2 AS
	SELECT DISTINCT pid, p3yr,
		   CASE WHEN (SUM(p3yr_rel) > 0) THEN 1 ELSE 0 END AS p3yr_rel,
		   kon, age_at_MTX_start, dis_deb_year
	FROM INDEX_2
	GROUP BY pid
;
quit;

DATA OUTPUT.COHORT_INDEX_3YR;
	SET INDEX_2;
run;
