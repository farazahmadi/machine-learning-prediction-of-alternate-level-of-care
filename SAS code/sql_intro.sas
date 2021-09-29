libname mydas base "P:\2019 0970 151 000\DAS Data\";

proc sql;
	create table myvars as
	SELECT * FROM dictionary.columns
	WHERE libname = "mydas"
	AND memname = "P970_151_NACRS_NEW"
	;
quit;


proc sql noprint;
	select prov, triage 
	from mydas.P970_151_NACRS_NEW
	where indexyear = 2015;
quit;

proc sql;
	select prov, triage 
	from mydas.P970_151_NACRS_NEW;
quit;



/* chih date*/

proc sql;
	create table want as
	select id, max(erlefttime) as dt
	from MYDAS.P970_151_CIHI; 
quit;


/* Somayeh code after */
proc sql;
	select count(*) from MYDAS.P970_151_NACRS_NEW as A
	where A.edvisit='1';
quit;
