LIBNAME IN 'H:\Projekt\Familiarity of mono-MTX\Data Extraction';
LIBNAME SRQ 'K:\Reuma\RASPA 2021\01. Data Warehouse\01. Processed Data\01. SRQ';
LIBNAME OUT 'H:\Projekt\Familiarity of mono-MTX\Data Extraction\Misc';

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* -- APPEND ON THE CLINIC AND REGION --------------------------------------------------------------------------------------------------------------------------------------------- ;
PROC SQL;
	CREATE TABLE CL_REG_1 AS
	SELECT a.pid, a.pid_rel, a.persist_1yr, a.rel_persist_1yr, a.persist_3yr, a.rel_persist_3yr,
		   b.tillhor, b.lan, b.region
	FROM IN.COHORT AS a
	LEFT JOIN SRQ.SRQ_BASDATA AS b
	ON a.pid = b.pid
;
quit;

PROC SQL;
	CREATE TABLE CL_REG_2 AS
	SELECT a.*, b.tillhor AS tillhor_rel, b.lan AS lan_rel, b.region AS region_rel
	FROM CL_REG_1 AS a
	LEFT JOIN SRQ.SRQ_BASDATA AS b
	ON a.pid_rel = b.pid
;
quit;

DATA OUT.CLIN_REG;
	SET CL_REG_2;
run;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* -- MARGINAL CONCORDANCE -------------------------------------------------------------------------------------------------------------------------------------------------------- ;
PROC SQL;
	CREATE TABLE CL_REG_3 AS
	SELECT CASE WHEN (tillhor = tillhor_rel) THEN 1 ELSE 0 END AS tillhor_CONC, 
		   CASE WHEN (lan = lan_rel) THEN 1 ELSE 0 END AS lan_CONC,
		   CASE WHEN (region = region_rel) THEN 1 ELSE 0 END AS region_CONC, *
	FROM CL_REG_2
;
quit;

PROC SQL;
	CREATE TABLE TILLHOR_CONC_MARGINAL AS
	SELECT *
	FROM CL_REG_3
	WHERE tillhor_CONC = 1
;
quit;

PROC SQL;
	CREATE TABLE LAN_CONC_MARGINAL AS
	SELECT *
	FROM CL_REG_3
	WHERE lan_CONC = 1
;
quit;

PROC SQL;
	CREATE TABLE REGION_CONC_MARGINAL AS
	SELECT *
	FROM CL_REG_3
	WHERE region_CONC = 1
;
quit;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* -- CONCORDANCE AMONG PAIRS CONCORDANT FOR PERSISTENCE (ONE YEAR) --------------------------------------------------------------------------------------------------------------- ;
PROC SQL;
	CREATE TABLE CL_REG_4 AS
	SELECT CASE WHEN (persist_1yr = rel_persist_1yr) THEN 1 ELSE 0 END AS p1yr_CONC, *
	FROM CL_REG_3
;
quit;

PROC SQL;
	CREATE TABLE TILLHOR_CONC_P1YR AS
	SELECT *
	FROM CL_REG_4
	WHERE tillhor_CONC = 1 AND p1yr_CONC = 1
;
quit;

PROC SQL;
	CREATE TABLE LAN_CONC_P1YR AS
	SELECT *
	FROM CL_REG_4
	WHERE lan_CONC = 1 AND p1yr_CONC = 1
;
quit;

PROC SQL;
	CREATE TABLE REGION_CONC_P1YR AS
	SELECT *
	FROM CL_REG_4
	WHERE region_CONC = 1 AND p1yr_CONC = 1
;
quit;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* -- CONCORDANCE AMONG PAIRS CONCORDANT FOR PERSISTENCE (THREE YEARS) ------------------------------------------------------------------------------------------------------------ ;
PROC SQL;
	CREATE TABLE CL_REG_5 AS
	SELECT CASE WHEN (persist_3yr = rel_persist_3yr) THEN 1 ELSE 0 END AS p3yr_CONC, *
	FROM CL_REG_4
;
quit;

PROC SQL;
	CREATE TABLE TILLHOR_CONC_P3YR AS
	SELECT *
	FROM CL_REG_5
	WHERE tillhor_CONC = 1 AND p3yr_CONC = 1
;
quit;

PROC SQL;
	CREATE TABLE LAN_CONC_P3YR AS
	SELECT *
	FROM CL_REG_5
	WHERE lan_CONC = 1 AND p3yr_CONC = 1
;
quit;

PROC SQL;
	CREATE TABLE REGION_CONC_P3YR AS
	SELECT *
	FROM CL_REG_5
	WHERE region_CONC = 1 AND p3yr_CONC = 1
;
quit;
