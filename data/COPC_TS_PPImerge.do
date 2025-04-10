** COPC CSRs & PPI Data Project **

* Created by: Molly Melin
* Last Updated: 12-Dec 2024

***************************
***************************

*** PREPARATION ***

* Set working directory (location where your project files are)
cd "\Users\mmelin\Desktop\PEP Stata Sample"


*************************************
*** STEP 1: Check COPC diagnostics ***
*************************************

use "COPC_TS.dta", clear

tab year
* -- 1997-2018
tab ccode
* -- 45 unique countries 
summarize
* --  20,328 total observations


*****************************
*** STEP 2: Merging data ***
*****************************
use "COPC_TS.dta" , clear

**Note the PPI is only from 2009-2020, but keeping all years for now since ciri cover 1997-2018

drop _merge

joinby ccode year using "/Users/mmelin/Desktop/PEP Stata Sample/ppidata.dta", unmatched(both)_merge(_m)


*************************************
*** STEP 3: Checking missingness ***
*************************************

tab _m
drop if _m==2

** check how many countries are missing from the PPI data
distinct ccode if missing(PPIscore) & year > 2009

duplicates report id year
duplicates list id year
duplicates drop id year, force

save "COPCCRSPPI_TS.dta", replace

***************************
*** STEP 4: Merging CIRI & Farris data *** 
***************************

joinby ccode year using "C:\Users\mmelin\Desktop\PEP Stata Sample\CIRIGHTS_COPC_Farissmerge.dta", unmatched(both)_merge(_m2)

tab _m2
*for now, we will only keep countries with large companies in them
keep if _m2==3

* Check missingness
sum 
* 20,328 full match 

save "CIRIFarrisCOPCCRSPPI_TS.dta", replace

**********************************
*** STEP 5: Think about models ***
**********************************
xtset id year

gen gc_value_dum=l.globalcompactmember*l.valuedummy
label variable gc_value_dum "gc_prop_l1*valuedummy_l1"

gen gc_neg_dum=l.globalcompactmember*l.negdummy
label variable gc_neg_dum "l.globalcompactmember*l.negdummy"


***Positive Peace***
reg PPIscore gc_value_dum l.globalcompactmember  l.valuedummy  pop_ln_l1 democ_l1 rgdppc_ln_l1 companycount violence_l1 l.PPIscore, robust cluster(id)

xtreg PPIscore l.globalcompactmember  l.valuedummy l.negdummy pop_ln_l1 l.companycount l.ep_end l.PPIscore,re 

xtmixed PPIscore year companycount_l1 fdi_ln oda_ln rgdppc_11_ln l.PPIscore|| _all:ccode || _all:id
xtmixed PPIscore l.valuedummy#l.globalcompactmember l.foundation2  l.employees_ln l.DJSI l.PPIscore|| _all:ccode companycount|| _all:id

xtmixed PPIscore l.valuedummy l.globalcompactmember l.foundation2  l.employees_ln l.DJSI l.extractive|| _all:ccode l.polity_diff l.rgdppc|| _all:id

xtmixed PPIscore l.value_prop#l.globalcompactmember l.foundation2  l.employees_ln l.DJSI l.PPIscore|| _all:ccode companycount|| _all:id

xtmixed PPIscore l.valuedummy#l.globalcompactmember l.foundation2  l.employees_ln l.DJSI l.extractive|| ccode: ||id:,mle

*Model 1 & 2
gen PPIscore_inv=-1*PPIscore

xtmixed PPIscore_inv l.value_prop l.neg_prop l.PPIscore year|| ccode: ||id:,mle
estimates store small

xtmixed PPIscore_inv l.value_prop l.neg_prop l.globalcompactmember l.foundation2  l.revenue_ln l.employees_ln l.DJSI l.extractive l.PPIscore year|| ccode: ||id:,mle
estimates store full

xtmixed PPIscore_inv l.valuedummy l.negdummy l.PPIscore year|| ccode: ||id:,mle
estimates store smalldummy

xtmixed PPIscore_inv l.valuedummy l.negdummy l.globalcompactmember l.foundation2  l.revenue_ln l.employees_ln l.DJSI l.extractive l.PPIscore year|| ccode: ||id:,mle
estimates store fulldummy

etable, showstars estimates(small full smalldummy fulldummy) stars(.05 "*" .01 "**" .001 "***", attach(_r_b)) export(PPIresults.docx, replace)

***Human Rights***
*Model 1: all rights
quietly xtmixed human_rights_score l.value_prop  l.human_rights_score year || ccode: ||id:,mle
estimates store smallprop

quietly xtmixed human_rights_score l.value_prop  l.human_rights_score year l.rgdppc l.n_peace2|| ccode: ||id:,mle
estimates store lgprop

quietly xtmixed human_rights_score l.value_prop  l.fdi l.human_rights_score year l.rgdppc l.n_peace2|| ccode: ||id:,mle
estimates store lgpropfdi

quietly xtmixed human_rights_score l.valuedummy  l.human_rights_score year || ccode: ||id:,mle
estimates store smalldummy

quietly xtmixed human_rights_score l.valuedummy  l.human_rights_score year l.rgdppc l.n_peace2|| ccode: ||id:,mle
estimates store lgdummy

quietly xtmixed human_rights_score l.valuedummy l.fdi l.human_rights_score year l.rgdppc l.n_peace2|| ccode: ||id:,mle
estimates store lgdummyfdi

etable, showstars estimates(smallprop lgprop lgpropfdi smalldummy lgdummy lgdummyfdi)stars(.05 "*" .01 "**" .001 "***", attach(_r_b)) export(CIRIhrs.docx, replace)

*Model 2: physical
xtmixed physint_sum l.value_prop l.physint_sum year|| ccode: ||id:,mle
estimates store smallpropphys

xtmixed physint_sum l.value_prop l.physint_sum year l.rgdppc l.n_peace2 l.pop_ln l.cinc|| ccode: ||id:,mle
estimates store lgpropphys

xtmixed physint_sum l.value_prop l.physint_sum year|| ccode: ||id:,mle
estimates store smalldummyphys

xtmixed physint_sum l.value_prop l.physint_sum year l.rgdppc l.n_peace2 l.pop_ln l.cinc|| ccode: ||id:,mle
estimates store lgdummyphys

etable, showstars estimates(smallpropphys lgpropphys smalldummyphys lgdummyphys)stars(.05 "*" .01 "**" .001 "***", attach(_r_b)) export(CIRIhrs.docx, replace)

*Model 3: empowerment
xtmixed speech l.value_prop l.speech year|| ccode: ||id:,mle
estimates store smallpropspeech

xtmixed speech l.value_prop l.speech year l.rgdppc l.n_peace2 l.pop_ln l.cinc|| ccode: ||id:,mle
estimates store lgpropspeech

xtmixed speech l.valuedummy l.speech year|| ccode: ||id:,mle
estimates store smalldummyspeech

xtmixed speech l.valuedummy l.speech year l.rgdppc l.n_peace2 l.pop_ln l.cinc|| ccode: ||id:,mle
estimates store lgdummyspeech

etable, showstars estimates(smallpropspeech lgpropspeech smalldummyspeech lgdummyspeech)stars(.05 "*" .01 "**" .001 "***", attach(_r_b)) export(CIRIhrs.docx, replace)

*Model 4: justice
xtmixed trial_l l.value_prop l.trial_l year|| ccode: ||id:,mle
estimates store smallpropjustice

xtmixed trial_l l.value_prop l.trial_l year l.rgdppc l.n_peace2 l.pop_ln l.cinc|| ccode: ||id:,mle
estimates store lgpropjustice

xtmixed trial_l l.valuedummy l.trial_l year|| ccode: ||id:,mle
estimates store smalldummyjustice

xtmixed trial_l l.valuedummy l.trial_l year l.rgdppc l.n_peace2 l.pop_ln l.cinc|| ccode: ||id:,mle
estimates store lgdummyjustice

etable, showstars estimates(smallpropjustice lgpropjustice smalldummyjustice lgdummyjustice)stars(.05 "*" .01 "**" .001 "***", attach(_r_b)) export(CIRIhrs.docx, replace)

*Model 5: workers law
xtmixed workerrights_laws_sum l.value_prop l.workerrights_laws_sum year|| ccode: ||id:,mle
estimates store smallpropwork

xtmixed workerrights_laws_sum l.value_prop l.workerrights_laws_sum year l.rgdppc l.n_peace2 l.pop_ln l.cinc|| ccode: ||id:,mle
estimates store lgpropwork

xtmixed workerrights_laws_sum l.valuedummy l.workerrights_laws_sum year|| ccode: ||id:,mle
estimates store smalldummywork

xtmixed workerrights_laws_sum l.valuedummy l.workerrights_laws_sum year l.rgdppc l.n_peace2 l.pop_ln l.cinc|| ccode: ||id:,mle
estimates store lgdummywork

etable, showstars estimates(smallpropwork lgpropwork smalldummywork lgdummywork)stars(.05 "*" .01 "**" .001 "***", attach(_r_b)) export(CIRIhrs.docx, replace)

*Model 6: workers practice
xtmixed workerrights_practices_sum l.value_prop l.workerrights_practices_sum year|| ccode: ||id:,mle
estimates store smallpropworkp

xtmixed workerrights_practices_sum l.value_prop l.workerrights_practices_sum year l.rgdppc l.n_peace2 l.pop_ln l.cinc|| ccode: ||id:,mle
estimates store lgpropworkp

xtmixed workerrights_practices_sum l.valuedummy l.workerrights_practices_sum year|| ccode: ||id:,mle
estimates store smalldummyworkp

xtmixed workerrights_practices_sum l.valuedummy l.workerrights_practices_sum year l.rgdppc l.n_peace2 l.pop_ln l.cinc|| ccode: ||id:,mle
estimates store lgdummyworkp

etable, showstars estimates(smallpropworkp lgpropworkp smalldummyworkp lgdummyworkp)stars(.05 "*" .01 "**" .001 "***", attach(_r_b)) export(CIRIhrs.docx, replace)

*Model 7: domestic movement *HERE
xtmixed dommov l.value_prop l.dommov year|| ccode: ||id:,mle
estimates store smallpropdom

xtmixed dommov l.value_prop l.dommov year l.rgdppc l.n_peace2 l.pop_ln l.cinc|| ccode: ||id:,mle
estimates store lgpropdom

xtmixed dommov l.valuedummy l.dommov year l.rgdppc || ccode: ||id:,mle
estimates store smalldummydom

xtmixed dommov l.valuedummy l.dommov year l.rgdppc l.n_peace2 l.pop_ln l.cinc|| ccode: ||id:,mle
estimates store lgdummydom

etable, showstars estimates(smallpropdom lgpropdom smalldummydom lgdummydom)stars(.05 "*" .01 "**" .001 "***", attach(_r_b)) export(CIRIdom.docx, replace)

*Model 8
xtmixed wecon l.value_prop l.wecon year|| ccode: ||id:,mle
estimates store smallpropwec

xtmixed wecon l.value_prop l.wecon year l.rgdppc l.n_peace2 l.pop_ln l.cinc|| ccode: ||id:,mle
estimates store lgpropwec

xtmixed wecon l.valuedummy l.wecon year|| ccode: ||id:,mle
estimates store smalldummywec

xtmixed wecon l.valuedummy l.wecon year l.rgdppc l.n_peace2 l.pop_ln l.cinc|| ccode: ||id:,mle
estimates store lgdummywec

etable, showstars estimates(smallpropwec lgpropwec smalldummywec lgdummywec)stars(.05 "*" .01 "**" .001 "***", attach(_r_b)) export(CIRIsmalldum.docx, replace)


xtmixed  Fariss_mean L1_Fariss_mean year neg_prop_l1 value_prop_l1 || _all:ccode || _all:id
xtmixed  human_rights_score year companycount_l1 fdi_ln oda_ln rgdppc_11_ln|| _all:ccode || _all:id
xtmixed  physint_sum year companycount_l1 fdi_ln oda_ln rgdppc_11_ln|| _all:ccode || _all:id


***control variables: year companycount_l1 (or lagged DV) fdi_ln oda_ln rgdppc_11_ln
*ep_end rgdppc2 democ cinc2
* GDP, pop, polity see https://chicagounbound.uchicago.edu/cgi/viewcontent.cgi?article=13822&context=journal_articles
