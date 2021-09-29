libname mydas base "P:\2019 0970 151 000\DAS Data\DAS Data";

proc sql;
create table ED_NACRS as
select ID,days_to_arrivaldate, days_to_ambarrdate,days_to_ambtransdate,arrival_time,ambarr_time,ambtrans_time,
triage,days_to_triagedate,triage_time,days_to_regdate,reg_time,days_to_assessdate,assess_time,
days_to_npassessdate,npassess_time,days_to_indate1,days_to_indate2,days_to_indate3,days_to_indate4,
days_to_indate5,days_to_indate6,days_to_indate7,days_to_indate8,days_to_indate9,days_to_indate10,
instart_time1,instart_time2,instart_time3,instart_time4,instart_time5,instart_time6,instart_time7,instart_time8,instart_time9,instart_time10,
days_to_consultarrdate1,days_to_consultarrdate2,days_to_consultarrdate3,consultarr_time1,consultarr_time2,consultarr_time3,
days_to_consultdate1,days_to_consultdate2,days_to_consultdate3,consult_time1,consult_time2,consult_time3,
clidecunitflag,days_to_clidecunitindate,days_to_clidecunitoutdate,clidecunitin_time,clidecunitout_time,
days_to_referraldate,days_to_lefteddate,lefted_time,days_to_ddate,d_time,
aminst_enc,prvnum1_enc,prvnum2_enc,prvnum3_enc,prvnum4_enc,prvnum5_enc,days_to_bdate,
btany,cacsanetech,cacsbranch,cacscode,cacsintervention,
cacsit1,cacsit2,cacsit3,cacsit4,cacsit5,cacsit6,cacsit7,cacsit8,cacsitcnt,cacsitcnt1,cacsitcnt2,cacsitcnt3,
cacsitcnt4,cacsitcnt5,cacsitcnt6,cacsitcnt7,cacslogictype,cacspartition,
incode1,incode2,incode3,incode4,incode5,incode6,incode7,incode8,incode9,incode10,inatloc1,inatloc2,inatloc3,inatloc4,inatloc5,inatloc6,inatloc7,inatloc8,inatloc9,inatloc10,
dx10code1,dx10code2,dx10code3,dx10code4,dx10code5,dx10code6,dx10code7,dx10code8,dx10code9,dx10code10,
eddischdx1,eddischdx2,eddischdx3,glscoma,complaint1,complaint2,complaint3,consultserv1,consultserv2,consultserv3,
admambul, prvserv1, prvserv2, prvserv3, prvserv4, prvserv5, prvserv6, prvserv7, prvserv8, prvserv9, prvserv10, from_type, to_type
from mydas.P970_151_NACRS_NEW as A where A.edvisit='1'; 
quit;

proc export data=WORK.ED_NACRS
outfile='P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders/datasets/New/ED_NACRS.csv'
dbms=csv replace;
quit;


proc sql;
create table NACRASCIHI as
select * from WORK.ED_NACRS as A inner join mydas.P970_151_MASTER_CIHI as B   
on A.ID=B.id and A.days_to_lefteddate=B.days_to_admdate; 
quit;

proc export data=WORK.NACRASCIHI
outfile='P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders/datasets/New/NACRASCIHI_2.csv'
dbms=csv replace;
quit;



proc contents data = mydas.P970_151_MASTER_CIHI;
quit;

proc contents data = WORK.ED_NACRS;
quit;


/* Creating the merged Cihi data (bigger file) */


proc sql;
create table NACRSDAD as
select * from WORK.ED_NACRS as A inner join mydas.P970_151_CIHI as B   
on A.ID=B.id and A.days_to_lefteddate=B.days_to_admdate; 
quit;

proc export data=WORK.NACRSDAD
outfile='P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders/datasets/New/NACRSDAD.csv'
dbms=csv replace;
quit;
