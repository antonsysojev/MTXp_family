* In this updated version I've included information on whether the patient exited monotherapy, which helps remove the missing data. ;

LIBNAME INPUT 'H:\Projekt\Familiarity of mono-MTX\Data Extraction';
LIBNAME SRQ 'K:\Reuma\RASPA 2021\01. Data Warehouse\01. Processed Data\01. SRQ';

PROC SQL;
	CREATE TABLE DF_1 AS
	SELECT pid, order_date AS MTX_order_date
	FROM SRQ.SRQ_TERAPI
	WHERE preparat_kod = "MTX"
;
	CREATE TABLE DF_1 AS
	SELECT pid, MIN(MTX_order_date) AS first_MTX_date FORMAT yymmdd10.
	FROM DF_1
	GROUP BY pid
;
	CREATE TABLE DF_1 AS
	SELECT DISTINCT a.pid, b.first_MTX_date, a.p1yr, a.p3yr
	FROM INPUT.COHORT AS a
	INNER JOIN DF_1 AS b
	ON a.pid = b.pid
;
quit;

PROC SQL;
	CREATE TABLE DF_2 AS
	SELECT a.*, b.preparat_kod, b.prep_typ, b.order_date,
		   CASE WHEN (first_MTX_date <= order_date <= first_MTX_date + 365) AND 
					 (prep_typ = "bioprep" OR prep_typ = "csdmard" OR prep_typ = "infusion_drugs" OR prep_typ = "tsdmard") AND 
					 (NOT preparat_kod = "MTX") THEN 1 ELSE 0 END AS not_DMARD_monotherapy_1yr,
		   CASE WHEN (first_MTX_date <= order_date <= first_MTX_date + 1096) AND
					 (prep_typ = "bioprep" OR prep_typ = "csdmard" OR prep_typ = "infusion_drugs" OR prep_typ = "tsdmard") AND 
					 (NOT preparat_kod = "MTX") THEN 1 ELSE 0 END AS not_DMARD_monotherapy_3yr
	FROM DF_1 AS a
	INNER JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid = b.pid
;
	CREATE TABLE DF_2 AS
	SELECT DISTINCT pid, p1yr, p3yr, first_MTX_date,
		   CASE WHEN SUM(not_DMARD_monotherapy_1yr) > 0 THEN 1 ELSE 0 END AS fail_DMARD_monotherapy_1yr,
		   CASE WHEN SUM(not_DMARD_monotherapy_3yr) > 0 THEN 1 ELSE 0 END AS fail_DMARD_monotherapy_3yr
	FROM DF_2
	GROUP BY pid
;
quit;

PROC SQL;
	CREATE TABLE DF_3 AS
	SELECT a.*, CASE WHEN (first_MTX_date <= order_date <= first_MTX_date + 365) AND
			  			  (preparat_kod = "MTX") AND
			  			  (missing(end_date) OR end_date > first_MTX_date + 365) THEN 1 ELSE 0 END AS still_on_MTX_at_1yr,
				CASE WHEN (first_MTX_date <= order_date <= first_MTX_date + 1096) AND
			  			  (preparat_kod = "MTX") AND
						  (missing(end_date) OR end_date > first_MTX_date + 1096) THEN 1 ELSE 0 END AS still_on_MTX_at_3yr
	FROM DF_2 AS a
	INNER JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid = b.pid
;
	CREATE TABLE DF_3 AS
	SELECT DISTINCT pid, first_MTX_date, p1yr, p3yr, fail_DMARD_monotherapy_1yr, fail_DMARD_monotherapy_3yr,
		   CASE WHEN (SUM(still_on_MTX_at_1yr) > 0) THEN 1 ELSE 0 END AS still_on_any_MTX_at_1yr,
		   CASE WHEN (SUM(still_on_MTX_at_3yr) > 0) THEN 1 ELSE 0 END AS still_on_any_MTX_at_3yr
	FROM DF_3
	GROUP BY pid
;
quit;

* --- ;

PROC SQL;
	CREATE TABLE NP1YR AS
	SELECT DISTINCT pid, first_MTX_date, fail_DMARD_monotherapy_1yr, still_on_any_MTX_at_1yr
	FROM DF_3
	WHERE p1yr = 0
;
	CREATE TABLE NP1YR AS
	SELECT a.*, b.orsak, b.order_date
	FROM NP1YR AS a
	LEFT JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid = b.pid
	WHERE preparat_kod = "MTX" AND first_MTX_date = order_date
;
	CREATE TABLE NP1YR AS
	SELECT *, CASE WHEN (orsak = "" AND still_on_any_MTX_at_1yr = 1 AND fail_DMARD_monotherapy_1yr = 1) THEN "Brutit mot DMARD-monoterapi" ELSE ORSAK END AS orsak_alt
	FROM NP1YR
;
quit;

PROC FREQ DATA = NP1YR; TABLE orsak_alt; run;

* --- ;

PROC SQL;
	CREATE TABLE NP3YR AS
	SELECT DISTINCT pid, first_MTX_date, fail_DMARD_monotherapy_3yr, still_on_any_MTX_at_3yr
	FROM DF_3
	WHERE p3yr = 0
;
	CREATE TABLE NP3YR AS
	SELECT a.*, b.orsak, b.order_date
	FROM NP3YR AS a
	LEFT JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid = b.pid
	WHERE preparat_kod = "MTX" AND first_MTX_date = order_date
;
	CREATE TABLE NP3YR AS
	SELECT *, CASE WHEN (orsak = "" AND still_on_any_MTX_at_3yr = 1 AND fail_DMARD_monotherapy_3yr = 1) THEN "Brutit mot DMARD-monoterapi" ELSE ORSAK END AS orsak_alt
	FROM NP3YR
;
quit;

PROC FREQ DATA = NP3YR; TABLE orsak_alt; run;
