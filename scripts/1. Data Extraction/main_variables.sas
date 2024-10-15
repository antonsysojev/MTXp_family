* WARNING: I changed the name of the persistence variables (from `persist_1yr` and similar) to `p1yr` and similar. This means that running this from top to bottom MAY CRASH.
		   This is OK though, and any crash should be SOLELY due to the changing of variable names. Be careful when running this. I will fix it someday in the future...;

LIBNAME IN 'H:\Projekt\Familiarity of mono-MTX\Data Extraction\Data cache';
LIBNAME CACHE 'H:\Projekt\Familiarity of mono-MTX\Data Extraction\Data cache\variables';
LIBNAME OUT 'H:\Projekt\Familiarity of mono-MTX\Data Extraction';
LIBNAME SRQ 'K:\Reuma\RASPA 2021\01. Data Warehouse\01. Processed Data\01. SRQ';
LIBNAME DEATH 'K:\Reuma\RASPA 2021\01. Data Warehouse\02. Raw Data\08. Death';

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 0 --- IDENTIFY THE MTX START DATES --- STEP 0 --- IDENTIFY THE MTX START DATES --- STEP 0 --- IDENTIFY THE MTX START DATES --- STEP 0 --- IDENTIFY THE MTX START DA --- ;

PROC SQL;
	CREATE TABLE VARIABLES_0 AS
	SELECT pid, order_date AS MTX_order_date
	FROM SRQ.SRQ_TERAPI
	WHERE preparat_kod = "MTX"
;
	CREATE TABLE VARIABLES_0 AS
	SELECT pid, MIN(MTX_order_date) AS first_MTX_date FORMAT yymmdd10.
	FROM VARIABLES_0
	GROUP BY pid
;
	CREATE TABLE VARIABLES_0 AS
	SELECT DISTINCT a.pid, b.first_MTX_date
	FROM IN.SRQ_7 AS a
	INNER JOIN VARIABLES_0 AS b
	ON a.pid = b.pid
;
quit;

* -- NOTE: Need to do the filtering of first_MTX_date in two steps. Can possibly be done in fewer lines but this gets you all MTX prescriptions, and then the first occurring date per individual. --;
* -- NOTE: We toss all columns but `pid` since all relatives are among the `pid` so it's enough to do these and then link this information back up on the input data. --;
* -- NOTE: The use of DISTINCT allows us to cut all the duplicates from the `pid` column (i.e. all individuals with multiple relatives). --;

DATA CACHE.VARIABLES_0;
	SET VARIABLES_0;
run;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 1 --- IDENTIFY ALL LIVING INDIVIDUALS --- STEP 1 --- IDENTIFY ALL LIVING INDIVIDUALS --- STEP 1 --- IDENTIFY ALL LIVING INDIVIDUALS --- STEP 1 --- IDENTIFY ALL LIV --- ;

PROC SQL;
	CREATE TABLE VARIABLES_1 AS
	SELECT DISTINCT a.*, CASE WHEN (. < INTCK("days", a.first_MTX_date, b.deathdate) < 365) THEN 1 ELSE 0 END AS dead_within1yr,
			  			 CASE WHEN (. < INTCK("days", a.first_MTX_date, b.deathdate) < 1096) THEN 1 ELSE 0 END AS dead_within3yr
	FROM VARIABLES_0 AS a
	LEFT JOIN DEATH.DEATH AS b
	ON a.pid = b.pid
;
quit;

* -- NOTE: Using DISTINCT removes the lines that are duplicated. For some reason, DEATH.DEATH contains certain repetitions in the variables used here. --;

DATA CACHE.VARIABLES_1;
	SET VARIABLES_1;
run;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 2 --- REMOVE ALL TREATED WITH ADDITIONAL DMARD (SRQ) --- STEP 2 --- REMOVE ALL TREATED WITH ADDITIONAL DMARD (SRQ) --- STEP 2 --- REMOVE ALL TREATED WITH ADDITIONA --- ;

PROC SQL;
	CREATE TABLE VARIABLES_2 AS
	SELECT a.*, b.preparat_kod, b.prep_typ, b.order_date,
		   CASE WHEN (first_MTX_date <= order_date <= first_MTX_date + 365) AND 
					 (prep_typ = "bioprep" OR prep_typ = "csdmard" OR prep_typ = "infusion_drugs" OR prep_typ = "tsdmard") AND 
					 (NOT preparat_kod = "MTX") THEN 1 ELSE 0 END AS not_DMARD_monotherapy_1yr,
		   CASE WHEN (first_MTX_date <= order_date <= first_MTX_date + 1096) AND
					 (prep_typ = "bioprep" OR prep_typ = "csdmard" OR prep_typ = "infusion_drugs" OR prep_typ = "tsdmard") AND 
					 (NOT preparat_kod = "MTX") THEN 1 ELSE 0 END AS not_DMARD_monotherapy_3yr
	FROM VARIABLES_1 AS a
	INNER JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid = b.pid
;
	CREATE TABLE VARIABLES_2 AS
	SELECT DISTINCT pid, first_MTX_date, dead_within1yr, dead_within3yr,
		   CASE WHEN SUM(not_DMARD_monotherapy_1yr) > 0 THEN 1 ELSE 0 END AS fail_DMARD_monotherapy_1yr,
		   CASE WHEN SUM(not_DMARD_monotherapy_3yr) > 0 THEN 1 ELSE 0 END AS fail_DMARD_monotherapy_3yr
	FROM VARIABLES_2
	GROUP BY pid
;
quit;

* -- NOTE: I split this into two chunks to make it more clear exactly what we are doing.--;
* -- NOTE: The first chunk identifies all who break away from MTX in DMARD-monotherapy (happens if a treatment was prescribed within the period, if it was a DMARD and if that DMARD was not MTX). --;
* -- NOTE: The second chunk identifies all who broke away from MTX in DMARD-monotherapy at least once as those failing monotherapy. --;

DATA CACHE.VARIABLES_2;
	SET VARIABLES_2;
run;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 3 --- REMOVE ANY NOT REMAINING ON MTX --- STEP 3 --- REMOVE ANY NOT REMAINING ON MTX --- STEP 3 --- REMOVE ANY NOT REMAINING ON MTX --- STEP 3 --- REMOVE ANY NOT R --- ;

PROC SQL;
	CREATE TABLE VARIABLES_3 AS
	SELECT a.*, CASE WHEN (first_MTX_date <= order_date <= first_MTX_date + 365) AND
			  			  (preparat_kod = "MTX") AND
			  			  (missing(end_date) OR end_date > first_MTX_date + 365) THEN 1 ELSE 0 END AS still_on_MTX_at_1yr,
				CASE WHEN (first_MTX_date <= order_date <= first_MTX_date + 1096) AND
			  			  (preparat_kod = "MTX") AND
						  (missing(end_date) OR end_date > first_MTX_date + 1096) THEN 1 ELSE 0 END AS still_on_MTX_at_3yr
	FROM VARIABLES_2 AS a
	INNER JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid = b.pid
;
	CREATE TABLE VARIABLES_3 AS
	SELECT DISTINCT pid, first_MTX_date, dead_within1yr, dead_within3yr, fail_DMARD_monotherapy_1yr, fail_DMARD_monotherapy_3yr,
		   CASE WHEN (SUM(still_on_MTX_at_1yr) > 0) THEN 1 ELSE 0 END AS still_on_any_MTX_at_1yr,
		   CASE WHEN (SUM(still_on_MTX_at_3yr) > 0) THEN 1 ELSE 0 END AS still_on_any_MTX_at_3yr
	FROM VARIABLES_3
	GROUP BY pid
;
quit;

* -- NOTE: Splitting into two to improve readability. --;
* -- NOTE: First chunk identifies all treatments that were MTX and still ongoing where the treatment needed to be started within the study period, be of MTX and not having ended within the period. --;
* -- NOTE: Second chunk counts the above, identifiying each individual that had at least one such ongoing MTX treatment. There should be no one with multiple though. --;

DATA CACHE.VARIABLES_3;
	SET VARIABLES_3;
run;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 4 --- OUTPUT FINAL DATA SET --- STEP 4 --- OUTPUT FINAL DATA SET --- STEP 4 --- OUTPUT FINAL DATA SET --- STEP 4 --- OUTPUT FINAL DATA SET --- STEP 4 --- OUTPUT FI --- ;

PROC SQL;
	CREATE TABLE VARIABLES_4 AS
	SELECT pid, first_MTX_date, 
		   CASE WHEN (dead_within1yr = 0) AND
					 (fail_DMARD_monotherapy_1yr = 0) AND
					 (still_on_any_MTX_at_1yr = 1) THEN 1 ELSE 0 END AS p1yr,
		   CASE WHEN (dead_within3yr = 0) AND
				     (fail_DMARD_monotherapy_3yr = 0) AND
				     (still_on_any_MTX_at_3yr) THEN 1 ELSE 0 END AS p3yr
	FROM VARIABLES_3
;
	CREATE TABLE COHORT AS
	SELECT *
	FROM IN.SRQ_7 AS a
	INNER JOIN VARIABLES_4 AS b
	ON a.pid = b.pid
;
	CREATE TABLE COHORT AS
	SELECT a.*, b.p1yr AS p1yr_rel, b.p3yr AS p3yr_rel
	FROM COHORT AS a
	INNER JOIN VARIABLES_4 AS b
	ON a.pid_rel = b.pid
;
quit;

* -- NOTE: If an individual is not dead, did not break monotherapy, and were still on any MTX by the end of the time period, then they are considered persistent. --;

DATA OUT.COHORT;
	SET COHORT;
run;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
*-- STEP 5 --- APPENDING COVARIATES --- STEP 5 --- APPENDING COVARIATES --- STEP 5 --- APPENDING COVARIATES --- STEP 5 --- APPENDING COVARIATES --- STEP 5 --- APPENDING COVARIA --;

PROC SQL;
	CREATE TABLE COHORT_COVARIATES AS
	SELECT a.pid, first_MTX_date, b.kon, FLOOR(INTCK("days", b.birthday, a.first_MTX_date) / 365.25) AS age_at_MTX_start, YEAR(b.disease_debut_1) AS dis_deb_year, p1yr, p3yr,
		   pid_rel, reltyp, p1yr_rel, p3yr_rel
	FROM COHORT AS a
	INNER JOIN SRQ.SRQ_BASDATA AS b
	ON a.pid = b.pid
;
quit;

* -- NOTE: I take this opportunity to shift the order of the columns. Here, the first six columns detail the index patients, the remaining concerning the relatives. --;
* -- NOTE: Debut of disease is taken as the year of the variable `disease_debut_1`. This may be wrong for some patients though, who had RA as their second disease (variable of interest is `disease_debut_2`. --;
* -- NOTE: Keep `first_MTX_date` as we need it for identifying responders/non-responders per EULAR criteria in the next step. --;

DATA OUT.COHORT;
	SET COHORT_COVARIATES;
run;

* -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ;
* --- STEP 6 --- IDENTIFYING REPONDERS AT 3 AND 6 MONTHS --- STEP 6 --- IDENTIFYING REPONDERS AT 3 AND 6 MONTHS  --- STEP 6 --- IDENTIFYING REPONDERS AT 3 AND 6 MONTHS  --- S --- ;

PROC SQL;
	CREATE TABLE VARIABLES_6_1 AS
	SELECT DISTINCT a.pid, a.first_MTX_date, b.das28 AS base_DAS28ESR, b.das28CRP AS base_DAS28CRP, b.visit_date AS base_date
	FROM COHORT_COVARIATES AS a
	INNER JOIN SRQ.SRQ_BESOKSDATA AS b
	ON a.pid = b.pid
	WHERE (a.first_MTX_date - 90 <= b.visit_date <= a.first_MTX_date + 5) AND
		   NOT (MISSING(b.das28) AND MISSING(b.das28CRP))
;
	CREATE TABLE VARIABLES_6_1 AS
	SELECT *
	FROM VARIABLES_6_1
	GROUP BY pid
	HAVING (first_MTX_date - base_date + 5) = MIN(first_MTX_date - base_date + 5)
;
quit;

* -- NOTE: I grab only the `pid` and `first_MTX_date` columns since the relatives are the same set of individuals and this information suffices for now. --;
* -- NOTE: The use of DISTINCT allows me to remove any `pid` appearing twice due to multiple first-degree relatives. This should make the new table free of duplicate lines. --;
* -- NOTE: I use INNER JOIN which here cuts out all individuals who were not in SRQ_BESOKSDATA. These individuals will be MISSING in response status. --;
* -- NOTE: The WHERE call allows us to filter to only the visits occurring around the date of first MTX prescription (in the interval of [-90, 5]) as well as only the visits where information on DAS28ESR or DAS28CRP is available. Those without this data are removed and will be MISSING in response status. --;
* -- NOTE: Patients may have multiple visits within this interval (a small set does), so each line DOES NOT necessarily correspond to a single patient, this is why we need the second chunk. --;
* -- NOTE: The second chunk extracts the visit closest to the first_MTX_date. As the interval is not centered around zero, a patient could have a visit occurring both n days prior and n days after, in which case days-since-MTX-visit is inconclusive. Adding five shifts the zero so that the minimum is always distinct (unless there were multiple visists at one days which we assume there isn't). --;
* -- NOTE: The approach of adding five leads to a minor change in that visits occurring AFTER the MTX visit is prioritized over visits occurring prior to. This means that if a visit occurred four days prior and a second visit occurred five days efter, then the five-days-after-visit would be the counted one. However, later visits should be more accurrate as patients need to visit a pharmacy to get their MTX, meaning treatment start is slightly later than the actual visit. --;
* -- NOTE: A better solution is to use the closest visit (in absolute value) and if there are two with the same proximity, picking the one occuring after the visit. I didn't know how to easily code this though, and this works fine here since there are no such visits anyways. --;

PROC SQL;
	CREATE TABLE VARIABLES_6_2 AS
	SELECT DISTINCT a.pid, a.first_MTX_date, b.das28 AS base_DAS28ESR, b.das28CRP AS base_DAS28CRP, b.visit_date AS base_date
	FROM COHORT_COVARIATES AS a
	INNER JOIN SRQ.SRQ_BESOKSDATA AS b
	ON a.pid = b.pid
	WHERE (a.first_MTX_date + 5 < b.visit_date <= a.first_MTX_date + 30) AND
		  NOT (MISSING(b.das28) AND MISSING(b.das28CRP)) AND
		  NOT (a.pid IN (SELECT DISTINCT pid FROM VARIABLES_6_1))
;
	CREATE TABLE VARIABLES_6_2 AS
	SELECT *
	FROM VARIABLES_6_2
	GROUP BY pid
	HAVING (first_MTX_date - base_date) = MIN(first_MTX_date - base_date)
;
	CREATE TABLE VARIABLES_6_2 AS
	SELECT pid, first_MTX_date, base_DAS28ESR, base_DAS28CRP
	FROM VARIABLES_6_1
	OUTER UNION CORR
	SELECT pid, first_MTX_date, base_DAS28ESR, base_DAS28CRP
	FROM VARIABLES_6_2
	ORDER BY pid
;
quit;

* -- NOTE: We repeat the above, now searching within the interval of (5, 30] days since first_MTX_visit and ONLY among those that were not already in the data. The use of this criteria (final line of the WHERE call) makes it possibly to simply row bind the two together (what we do in the third chunk). --;
* -- NOTE: The second chunk extracts only the visit closest to the date of first_MTX_visit which is redundant here but kept for its generality. --;

PROC SQL;
	CREATE TABLE VARIABLES_6_3 AS
	SELECT a.*, b.das28 AS M3_DAS28ESR, b.das28CRP AS M3_DAS28CRP, b.visit_date AS M3_date
	FROM COHORT_COVARIATES AS a
	INNER JOIN SRQ.SRQ_BESOKSDATA AS b
	ON a.pid = b.pid
	WHERE (a.first_MTX_date + 31 <= b.visit_date <= a.first_MTX_date + 149) AND
		  NOT (MISSING(b.das28) AND MISSING(b.das28CRP))
	ORDER BY pid, M3_date
;
	CREATE TABLE VARIABLES_6_3 AS
	SELECT pid, first_MTX_date, M3_DAS28ESR, M3_DAS28CRP
	FROM VARIABLES_6_3
	GROUP BY pid
	HAVING (ABS(M3_date - (first_MTX_date + 90)) = MIN(ABS(M3_date - (first_MTX_date + 90))))
;
quit;

* -- NOTE: In the second chunk, I filter to the visit closest to 90 days after the date of first MTX prescription (first_MTX_date + 90). Note that this could lead to multiple equidistant visits (one visist n days prior to the midpoint, one visit n days after). Unlike for the above chunk, I skip this check here hoping for no such cases. --; 
* -- NOTE: A better approach would make a choice here but unlike the previous part, which visit to prioritize is not obvious. Instead, I make no choice, which is fine since there are no such visits anyways. --;

PROC SQL;
	CREATE TABLE VARIABLES_6_4 AS
	SELECT a.*, b.das28 AS M6_DAS28ESR, b.das28CRP AS M6_DAS28CRP, b.visit_date AS M6_date
	FROM COHORT_COVARIATES AS a
	INNER JOIN SRQ.SRQ_BESOKSDATA AS b
	ON a.pid = b.pid
	WHERE (a.first_MTX_date + 150 <= b.visit_date <= a.first_MTX_date + 269) AND
		  NOT (MISSING(b.das28) AND MISSING(b.das28CRP))
	ORDER BY pid, M6_date
;
	CREATE TABLE VARIABLES_6_4 AS
	SELECT pid, first_MTX_date, M6_DAS28ESR, M6_DAS28CRP
	FROM VARIABLES_6_4
	GROUP BY pid
	HAVING (ABS(M6_date - (first_MTX_date + 180)) = MIN(ABS(M6_date - (first_MTX_date + 180))))
;
quit;

PROC SQL;
	CREATE TABLE VARIABLES_6_5 AS
	SELECT DISTINCT a.pid, a.first_MTX_date, b.base_DAS28ESR, b.base_DAS28CRP
	FROM COHORT_COVARIATES AS a
	LEFT JOIN VARIABLES_6_2 AS b
	ON a.pid = b.pid
;
	CREATE TABLE VARIABLES_6_5 AS
	SELECT DISTINCT a.*, b.M3_DAS28ESR, b.M3_DAS28CRP
	FROM VARIABLES_6_5 AS a
	LEFT JOIN VARIABLES_6_3 AS b
	ON a.pid = b.pid
;
	CREATE TABLE VARIABLES_6_5 AS
	SELECT DISTINCT a.*, b.M6_DAS28ESR, b.M6_DAS28CRP
	FROM VARIABLES_6_5 AS a
	LEFT JOIN VARIABLES_6_4 AS b
	ON a.pid = b.pid
;
	CREATE TABLE VARIABLES_6_5 AS
	SELECT *, CASE WHEN (MISSING(base_DAS28ESR) OR MISSING(M3_DAS28ESR)) AND 
						(MISSING(base_DAS28CRP) OR MISSING(M3_DAS28CRP)) THEN ''
				   WHEN ((base_DAS28ESR - M3_DAS28ESR >= 1.2) AND (. < M3_DAS28ESR <= 3.2) OR
						 (base_DAS28CRP - M3_DAS28CRP >= 1.2) AND (. < M3_DAS28CRP <= 3.2)) THEN 'good'
				   WHEN (((base_DAS28ESR - M3_DAS28ESR >= 1.2) AND (M3_DAS28ESR > 3.2)) OR ((0.6 < base_DAS28ESR - M3_DAS28ESR <= 1.2) AND (. < M3_DAS28ESR <= 5.1))) OR
				   		 (((base_DAS28CRP - M3_DAS28CRP >= 1.2) AND (M3_DAS28CRP > 3.2)) OR ((0.6 < base_DAS28CRP - M3_DAS28CRP <= 1.2) AND (. < M3_DAS28CRP <= 5.1))) THEN 'moderate' 
				   ELSE 'poor' END AS M3_EULAR,
			  CASE WHEN (MISSING(base_DAS28ESR) OR MISSING(M6_DAS28ESR)) AND
			  			(MISSING(base_DAS28CRP) OR MISSING(M6_DAS28CRP)) THEN ''
				   WHEN ((base_DAS28ESR - M6_DAS28ESR >= 1.2) AND (. < M6_DAS28ESR <= 3.2) OR
				   		 (base_DAS28CRP - M6_DAS28CRP >= 1.2) AND (. < M6_DAS28CRP <= 3.2)) THEN 'good'
				   WHEN (((base_DAS28ESR - M6_DAS28ESR >= 1.2) AND (M6_DAS28ESR > 3.2)) OR ((0.6 < base_DAS28ESR - M6_DAS28ESR <= 1.2) AND (. < M6_DAS28ESR <= 5.1))) OR
				   		 (((base_DAS28CRP - M6_DAS28CRP >= 1.2) AND (M6_DAS28CRP > 3.2)) OR ((0.6 < base_DAS28CRP - M6_DAS28CRP <= 1.2) AND (. < M6_DAS28CRP <= 5.1))) THEN 'moderate'
				   ELSE 'poor' END AS M6_EULAR
	FROM VARIABLES_6_5
;
	CREATE TABLE VARIABLES_6_5 AS
	SELECT pid, first_MTX_date, CASE WHEN (M3_EULAR = 'good' OR M3_EULAR = 'moderate') THEN 1
					 				 WHEN (M3_EULAR = 'poor') THEN 0
					 				 ELSE . END AS M3_EULAR,
								CASE WHEN (M6_EULAR = 'good' OR M6_EULAR = 'moderate') THEN 1
					 				 WHEN (M6_EULAR = 'poor') THEN 0
					 				 ELSE . END AS M6_EULAR
	FROM VARIABLES_6_5
;
quit;

* -- NOTE: Good response is defined as having a SIGNIFICANT CHANGE AND LOW DISEASE ACTIVITY (van Gestal et al.). The former is defined as a change of 1.2 (but I here assume that this means a decrease of at least 1.2 (increase would mean worse disease activity) (a decrease larger than 1.2 would be even better than just 1.2)). --;
* -- NOTE: I'm currently using, and have been previously, the same criteria when measuring via CRP. This must be somewhat flawed, as I know ESR and CRP calculated DAS28 to not be comparable (hence why we do not compare across measures). Differences should be stable, but the scalar inequality can not be right as DAS28CRP should be on a different scale. --;
* -- NOTE: To be moderate, you need to have SIGNIFICANT CHANGE AND HIGH-MODERATE DISEASE ACTIVITY, or, MEDIUM CHANGE AND MODERATE-LOW DISEASE ACTIVITY. This is defined according to van Gestal et al. where LOW is (-infty, 3.2], MEDIUM is (3.2, 5.1] and HIGH is (5.1, infty). Furthermore, MEDIUM CHANGE is (0.6, 1.2]. --;
* -- NOTE: Remember that the IF-call is sequential. It looks first to the missing step, catching all those which are then finished regardless of the following steps. This means that we don't need to care about how missing values are interpreted for the next calls since all rows with insufficient data are finished and taken away. Still, just to be safe, all one-sided inequalities were changed to have the missing value as a non-inclusive lower limit. --;

PROC SQL;
	CREATE TABLE VARIABLES_6_6 AS
	SELECT a.*, CASE WHEN (first_MTX_date <= order_date <= first_MTX_date + 90) AND
						  (preparat_kod = "MTX") AND
						  (MISSING(end_date) OR end_date > first_MTX_date + 90) THEN 1 ELSE 0 END AS still_on_MTX_at_90,
				CASE WHEN (first_MTX_date <= order_date <= first_MTX_date + 180) AND
						  (preparat_kod = "MTX") AND
						  (MISSING(end_date) OR end_date > first_MTX_date + 180) THEN 1 ELSE 0 END AS still_on_MTX_at_180
	FROM VARIABLES_6_5 AS a
	INNER JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid = b.pid
;
	CREATE TABLE VARIABLES_6_6 AS
	SELECT DISTINCT pid, first_MTX_date, 
		   CASE WHEN (SUM(still_on_MTX_at_90) > 0) THEN M3_EULAR ELSE 0 END AS M3_EULAR,
		   CASE WHEN (SUM(still_on_MTX_at_180) > 0) THEN M6_EULAR ELSE 0 END AS M6_EULAR
	FROM VARIABLES_6_6
	GROUP BY pid
;
	CREATE TABLE VARIABLES_6_6 AS
	SELECT a.*, CASE WHEN (first_MTX_date <= b.order_date <= first_MTX_date + 90) AND
					 	  (b.prep_typ = "bioprep" OR b.prep_typ = "csdmard" OR b.prep_typ = "infusion_drugs" OR b.prep_typ = "tsdmard") AND 
						  (NOT b.preparat_kod = "MTX") THEN 1 ELSE 0 END AS not_DMARD_monotherapy_90,
				CASE WHEN (first_MTX_date <= b.order_date <= first_MTX_date + 180) AND
					 	  (b.prep_typ = "bioprep" OR b.prep_typ = "csdmard" OR b.prep_typ = "infusion_drugs" OR b.prep_typ = "tsdmard") AND 
						  (NOT b.preparat_kod = "MTX") THEN 1 ELSE 0 END AS not_DMARD_monotherapy_180
	FROM VARIABLES_6_6 AS a
	INNER JOIN SRQ.SRQ_TERAPI AS b
	ON a.pid = b.pid
;
	CREATE TABLE VARIABLES_6_6 AS
	SELECT DISTINCT pid, CASE WHEN (SUM(not_DMARD_monotherapy_90) = 0) THEN M3_EULAR ELSE 0 END AS M3_EULAR, 
		   CASE WHEN (SUM(not_DMARD_monotherapy_180) = 0) THEN M6_EULAR ELSE 0 END AS M6_EULAR
	FROM VARIABLES_6_6
	GROUP BY pid
;
quit;

* -- NOTE: Above, we repeat the code when checking for people stopping treatment to find any stops made during this period. This should improve upon missingness, since if someone stopped early they are clearly non-responders though they would have previously been coded as missing due to no more visits measuring DAS28. --;
* -- NOTE: It also helps 'fix' the incorrect responders. Say a patient stops MTX due to inefficacy per the DAS28 at three months, starting instead a TNF-inhibitor. If this novel treatment now works well, then the patient would be a responder by six months, though for a different treatment. --;

PROC SQL;
	CREATE TABLE COHORT_FINAL AS
	SELECT a.pid, a.kon, a.age_at_MTX_start, a.dis_deb_year, a.p1yr, a.p3yr, b.M3_EULAR, b.M6_EULAR,
		   a.pid_rel, a.reltyp, a.p1yr_rel, a.p3yr_rel
	FROM COHORT_COVARIATES AS a
	LEFT JOIN VARIABLES_6_6 AS b
	ON a.pid = b.pid
;
	CREATE TABLE COHORT_FINAL AS
	SELECT a.*, b.M3_EULAR AS M3_EULAR_rel, b.M6_EULAR AS M6_EULAR_rel
	FROM COHORT_FINAL AS a
	LEFT JOIN VARIABLES_6_6 AS b
	ON a.pid_rel = b.pid
;
quit;

DATA OUT.COHORT;
	SET COHORT_FINAL;
run;
