libname mydas base "P:\2019 0970 151 000\DAS Data\DAS Data";

proc sql;
create table ED_NACRS as
select ID,days_to_triagedate,triage_time,days_to_regdate,reg_time,days_to_lefteddate,lefted_time,triage
from mydas.P970_151_NACRS_NEW as A where A.edvisit='1'; 
quit;

/* Creating the merged Cihi data (bigger file) */


proc sql;
create table NACRSDAD as
select * from WORK.ED_NACRS as A inner join mydas.P970_151_CIHI as B   
on A.ID=B.id and A.days_to_lefteddate=B.days_to_admdate; 
quit;

proc export data=WORK.NACRSDAD
outfile='P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders/datasets/New/NACRSDAD_ver2.csv'
dbms=csv replace;
quit;



proc contents data = mydas.P970_151_MASTER_CIHI;
quit;

proc contents data = WORK.ED_NACRS;
quit;


proc sql;
select count(*)
from mydas.P970_151_NACRS_NEW;
quit;

proc sql;
select count(*)
from mydas.P970_151_NACRS_NEW as A where A.edvisit='1'; 
quit;
