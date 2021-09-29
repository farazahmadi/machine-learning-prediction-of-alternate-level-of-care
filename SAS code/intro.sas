libname mydas base "P:\2019 0970 151 000\DAS Data\";

*proc print data=nacrs.p970_151_nacrs_new;

/* See variables in order */
proc contents data=mydas.p970_151_nacrs_new out=test;
run;

proc sort data=test;
	by Varnum;
run;

/* See first n observation */
proc print data=mydas.p970_151_nacrs_new (obs=10);
run;

/*
proc means data = mydas.p970_151_nacrs_new;
	var indexyear;
run;
*/

/*DAD data set - ALC var*/
proc contents data=mydas.p970_151_master_cihi out=test_cihi;
run;

proc contents data=mydas.p970_151_cihi out=test_cihi;
run;

proc univariate data=mydas.p970_151_cihi;
	var erlefttime;
run;
