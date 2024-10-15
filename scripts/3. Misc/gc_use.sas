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
	SELECT DISTINCT a.pid, b.first_MTX_date, a.p1yr, a.p3yr, a.M3_EULAR, a.M6_EULAR
	FROM INPUT.COHORT AS a
	INNER JOIN DF_1 AS b
	ON a.pid = b.pid
;
quit;

PROC SQL;
	CREATE TABLE DF_2 AS
	SELECT a.*, b.prep_typ
	FROM DF_1 AS a
	INNER JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid = b.pid
	WHERE a.first_MTX_date = b.order_date
;
quit;

PROC SQL;
	CREATE TABLE DF_3 AS
	SELECT *, CASE WHEN prep_typ = "cortisone" THEN 1 ELSE 0 END AS gc
	FROM DF_2
;
	CREATE TABLE DF_3 AS
	SELECT DISTINCT pid, p1yr, p3yr, M3_EULAR, M6_EULAR, SUM(gc) AS n_gc_at_baseline
	FROM DF_3
	GROUP BY pid
;
	CREATE TABLE DF_3 AS
	SELECT pid, p1yr, p3yr, M3_EULAR, M6_EULAR, CASE WHEN n_gc_at_baseline > 0 THEN 1 ELSE 0 END AS gc_at_baseline
	FROM DF_3
	GROUP BY pid
	ORDER BY p1yr
;
quit;

PROC FREQ DATA = DF_3; TABLE gc_at_baseline; BY p1yr; run;

PROC SQL;
	CREATE TABLE DF_3 AS
	SELECT *
	FROM DF_3
	ORDER BY p3yr
;
quit;

PROC FREQ DATA = DF_3; TABLE gc_at_baseline; BY p3yr; run;

PROC SQL;
	CREATE TABLE DF_3 AS
	SELECT *
	FROM DF_3
	ORDER BY M3_EULAR
;
quit;

PROC FREQ DATA = DF_3; TABLE gc_at_baseline; BY M3_EULAR; run;

PROC SQL;
	CREATE TABLE DF_3 AS
	SELECT *
	FROM DF_3
	ORDER BY M6_EULAR
;
quit;

PROC FREQ DATA = DF_3; TABLE gc_at_baseline; BY M6_EULAR; run;

