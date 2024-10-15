LIBNAME IN 'H:\Projekt\Familiarity of mono-MTX\Data Extraction\Data cache';
LIBNAME SRQ 'K:\Reuma\RASPA 2021\01. Data Warehouse\01. Processed Data\01. SRQ';
LIBNAME MGR 'K:\Reuma\RASPA 2021\01. Data Warehouse\02. Raw Data\02. MGR';

PROC SQL;
	CREATE TABLE DF_1 AS
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
	CREATE TABLE DF_1 AS
	SELECT *
	FROM DF_1
	WHERE pid IN (SELECT pid
				  FROM IN.SRQ_5)
;
	CREATE TABLE DF_1 AS
	SELECT *
	FROM DF_1
	WHERE pid_rel IN (SELECT pid
					  FROM IN.SRQ_5) AND
		  NOT missing(pid_rel) AND
		  (reltyp = "Barn" OR reltyp = "Mother" OR reltyp = "Father" OR reltyp = "Helsyskon")
;
quit;
