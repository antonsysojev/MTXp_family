LIBNAME OUT 'H:\Projekt\Familiarity of mono-MTX\Data Extraction\Data cache';
LIBNAME SRQ 'K:\Reuma\RASPA 2021\01. Data Warehouse\01. Processed Data\01. SRQ';
LIBNAME MGR 'K:\Reuma\RASPA 2021\01. Data Warehouse\02. Raw Data\02. MGR';

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 1 --- REMOVE DUPLICATED IDS --- STEP 1 --- REMOVE DUPLICATED IDS --- STEP 1 --- REMOVE DUPLICATED IDS --- STEP 1 --- REMOVE DUPLICATED IDS --- STEP 1 --- REMOVE DU --- ;
PROC SQL;
	CREATE TABLE SRQ_1 AS
	SELECT *, COUNT(pid) AS n_duplicates
	FROM SRQ.SRQ_basdata
	GROUP BY pid
	HAVING n_duplicates = 1
;
quit;

DATA OUT.SRQ_1;
	SET SRQ_1;
run;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 2 --- FILTER OUT NON-RA PATIENTS --- STEP 2 --- FILTER OUT NON-RA PATIENTS --- STEP 2 --- FILTER OUT NON-RA PATIENTS --- STEP 2 --- FILTER OUT NON-RA PATIENTS ---  --- ;
PROC SQL;
	CREATE TABLE SRQ_2 AS
	SELECT *
	FROM OUT.SRQ_1
	WHERE diagnosgrupp1 = "RA" OR diagnosgrupp2 = "RA"
;
quit;

DATA OUT.SRQ_2;
	SET SRQ_2;
run;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 3 --- FILTER OUT LATE AND EARLY SRQ INCLUSIONS --- STEP 3 --- FILTER OUT LATE AND EARLY SRQ INCLUSIONS --- STEP 3 --- FILTER OUT LATE AND EARLY SRQ INCLUSIONS ---  --- ;
PROC SQL;
	CREATE TABLE SRQ_3 AS
	SELECT *
	FROM OUT.SRQ_2
	WHERE YEAR(inclusion_date) BETWEEN 1999 AND 2019
;
quit;

DATA OUT.SRQ_3;
	SET SRQ_3;
run;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 4 --- FILTER OUT NON EARLY RA PATIENTS --- STEP 4 --- FILTER OUT NON EARLY RA PATIENTS --- STEP 4 --- FILTER OUT NON EARLY RA PATIENTS --- STEP 4 --- FILTER OUT NO --- ;
PROC SQL;
	CREATE TABLE SRQ_4 AS
	SELECT *
	FROM OUT.SRQ_3
	WHERE tidig_ra = 1
;
quit;

DATA OUT.SRQ_4;
	SET SRQ_4;
run;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 5 --- FILTER OUT INDIVIDUALS INCLUDED INTO SRQ A YEAR PRIOR TO OR AFTER FIRST RA VISIT --- STEP 5 --- FILTER OUT INDIVIDUALS INCLUDED INTO SRQ A YEAR PRIOR TO OR A --- ;
PROC SQL;
	CREATE TABLE SRQ_5 AS
	SELECT DISTINCT a.pid, a.inclusion_date, b.visit_date
	FROM OUT.SRQ_4 AS a
	INNER JOIN (SELECT pid, visit_date
				FROM SRQ.SRQ_besoksdata
				WHERE startbesok = 1) AS b
	ON a.pid = b.pid
	WHERE . < ABS(INTCK("days", b.visit_date, a.inclusion_date)) < 365.25
;
quit;

* -- NOTE: Use DISTINCT since people may have multiple visit on the day of first visit. This makes sure no line is duplicated which gives a more accurate number of individuals in the end.--;
* -- NOTE: Use INNER JOIN to immediately remove all individuals not within `SRQ_besoksdata` (or without a starting visit). If you lack visit data then you can't be defined as persistent/non-persistent anyways. --;
* -- NOTE: The `INTCK()` counts the number of days between the first visit and the date of inlusion (i.e., inclusion_date - visit_date). The use of `ABS()` makes sure we get those included more than a year prior to, as well as after, their first visit. --;
* -- NOTE: A given year is approximately 365.25 days long (accounting for leap year). Differences between exclusive and inclusive inequalities, as well as upper limits of 365 and 365.25 are negligible. --;
* -- NOTE: Using the lower limit of missing (`.`) means we exclude everybody who misses either a starting visit date or an inclusion date. --;
* -- COMMENT TO YOURSELF: The code looks tricky but is pretty intuitive once you look at it. It gives the same count when running each step separately (you've double-checked this a million times...) --;

DATA OUT.SRQ_5;
	SET SRQ_5;
run;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 6 --- FILTER OUT INDIVIDUALS THAT DID NOT RECEIVE MONO-MTX AS THEIR FIRST TREATMENT --- STEP 6 --- FILTER OUT INDIVIDUALS THAT DID NOT RECEIVE MONO-MTX AS THEIR FI --- ;
PROC SQL;
	CREATE TABLE SRQ_6 AS
	SELECT *
	FROM SRQ.SRQ_TERAPI
	WHERE preparat_kod = "MTX"
;
	CREATE TABLE SRQ_6 AS
	SELECT DISTINCT pid, MIN(order_date) AS first_MTX_date FORMAT yymmdd10.
	FROM SRQ_6
	GROUP BY pid
;
	CREATE TABLE SRQ_6 AS
	SELECT a.pid, b.first_MTX_date
	FROM OUT.SRQ_5 AS a
	INNER JOIN SRQ_6 AS b
	ON a.pid = b.pid
;
	CREATE TABLE SRQ_6 AS
	SELECT a.*, b.order_date, b.preparat_kod, b.prep_typ
	FROM SRQ_6 AS a
	INNER JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid = b.pid
;
	CREATE TABLE SRQ_6 AS
	SELECT *, CASE WHEN ((order_date <= first_MTX_date + 30) AND 
						 (prep_typ = "bioprep" OR prep_typ = "csdmard" OR prep_typ = "infusion_drugs" OR prep_typ = "tsdmard") AND 
						 (NOT preparat_kod = "MTX")) THEN 1 ELSE 0 END AS not_DMARD_monotherapy
	FROM SRQ_6
;
	CREATE TABLE SRQ_6 AS
	SELECT DISTINCT pid
	FROM SRQ_6
	GROUP BY pid
	HAVING SUM(not_DMARD_monotherapy) = 0
;
quit;

* -- NOTE: In (1-2) I obtain the date of the first MTX prescription for all individuals in SRQ_terapi. --;
* -- NOTE: The use of DISTINCT in (2) removes all copies of patients with multiple prescriptions of MTX on their first date. These should be few and should be taken care of in (6) but can be dealt with here to get more correct number of observations. --;
* -- NOTE: The use of INNER JOIN in (3) works to remove all individuals who never had an MTX prescription from our cohort (these would have been lost in (1)). --; 
* -- NOTE: The use of INNER JOIN in (4) removes all individuals that were not in SRQ_terapi, though these should have been cut at the previous join already. --;
* -- NOTE: In (5), the problematic observations are identified as a variable for easier inspection of its correctness. All prescriptions of non-MTX DMARDs, made up and until 30 days of starting the first MTX treatment, are identifed for removal. --;
* -- NOTE: In (6), I cut all individuals who had at least one prescription identified in (5). The use of DISTINCT makes sure each line corresponds to one individual. --;

DATA OUT.SRQ_6;
	SET SRQ_6;
run;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 7 --- IDENTIFYING ALL FIRST-DEGREE RELATIVES --- STEP 7 --- IDENTIFYING ALL FIRST-DEGREE RELATIVES --- STEP 7 --- IDENTIFYING ALL FIRST-DEGREE RELATIVES --- STEP 7 --- ;
PROC SQL;
	CREATE TABLE SRQ_7 AS
	(SELECT pid, pid_sibling AS pid_rel, syskontyp AS reltyp
	 FROM MGR.SIBLINGS
	 OUTER UNION CORR
	 SELECT pid, pid_child AS pid_rel, "Barn" as reltyp
	 FROM MGR.CHILDREN)
	 	OUTER UNION CORR
	(SELECT pid, pid_father AS pid_rel, "Father" as reltyp
	 FROM MGR.PARENTS
	 OUTER UNION CORR
	 SELECT pid, pid_mother AS pid_rel, "Mother" as reltyp
	 FROM MGR.PARENTS)
	ORDER BY pid
;
	CREATE TABLE SRQ_7 AS
	SELECT *
	FROM SRQ_7
	WHERE pid IN (SELECT pid
				  FROM OUT.SRQ_6)
;
	CREATE TABLE SRQ_7 AS
	SELECT *
	FROM SRQ_7
	WHERE pid_rel IN (SELECT pid
					  FROM OUT.SRQ_6) AND
		  NOT missing(pid_rel) AND
		  (reltyp = "Barn" OR reltyp = "Mother" OR reltyp = "Father" OR reltyp = "Helsyskon")
;
quit;

DATA OUT.SRQ_7;
	SET SRQ_7;
run;

* -- NOTE: In (1) I simply create a long, combined MGR containing each individual and all their siblings, children and parents. --;
* -- NOTE: In (2), the WHERE clause filters to only the people who were in the cohort up and until then. This is equivalent to an INNER JOIN of OUT.SRQ_6 AS a on (1) AS b. I think this is faster though? --;
* -- NOTE: In (3) I use WHERE to cut out all lines where the pid of the relative is not among the pid (i.e. they do not meet the inclusion/exclusion criteria already applied), keeping only those with non-missing relative pid as well as first-degree relatives. --;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
