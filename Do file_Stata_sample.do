* Generic code for event studies
* data(string) - path of data
* lags(int 4) - number of lags for event study (default is 4)
* leads(int 4) - number of leads for event study
* outcome(string) - outcome of interest
* eventVar(string) - variable containing event occurrences
* timeVar(string) - variable pertaining to time periods
* idVar(string) - variable pertaining to units 
* controls(string) - any additional controls to be included in eventstudy

*===========================================================================================================================

*BUILD DATA!


* This dofile builds the dataset containing all the variables. Please use the command "keep" before running anything with this dataset for simplicity.

*===========================================================================================================================

*------------------------------------------------------------------------------------
*Bring in AFIP Employment data: This includes exporters, importers and domestic firms
use "Draft2023/DataRaw/AFIP_employment_2001-2015/AFIP_empleo2001-2019_it.dta",clear
merge m:1 ID using "Draft2023/DataRaw/AFIP_employment_2001-2015/base_CLAE.dta"
label var numempl "Employment"
label var clae "AFIP: Sector actividad 6 digitos"

*Lets only keep firms for which we observe positive employment and CLAE.
*This are all firms that existed at some year. Maybe 2007. We should ask Fede.
*This is not important. Most of the firms we are deleting are very small and do not import or export.

keep if _merge==3
keep if Year>=2003 & Year<=2011

*Lets keep firms that are active for at least X years.
gen a=1 if numempl>0
bysort ID: egen yearsactive=sum(a)
table yearsactive
*Lets keep only firms that are active for at least 3 years in the period 2003-2011.
keep if yearsactive>=3
keep ID Year numempl clae promedio_salario_empleado

*Check that there are not duplicates in temrs of ID Year
duplicates tag ID Year,gen(a)
sum a
assert `r(mean)'==0
drop a
gen alwaysactive_sample=1
save "Draft2023/DataStata/AFIP_SampleIDs_Employment.dta",replace

*------------------------------------------------------------
*LIST OF FIRMS THAT WE WILL USE (to use later)
bysort ID: keep if _n==1
keep ID
save "Draft2023/DataStata/AFIP_SampleIDs_active.dta",replace
*------------------------------------------------------------

use "Draft2023/DataStata/AFIP_SampleIDs_Employment.dta",clear

*--------------------------------------------------------------------
*Bring in Imports:
merge 1:1 ID Year using "Draft2023/dataSTATA/Customs/imports_it.dta"
*only using data are firms that are not active many years. drop them
drop if _merge==2
*replace impo=0 for firms that are active but does not import.
replace impo=0 if impo==.
drop _merge
label var impo "Customs: Imports (US$)"
label var impointer "Customs: Imports intermediates (US$)"
*--------------------------------------------------------------------

*--------------------------------------------------------------------
*Bring in Exports
merge 1:1 ID Year using "Draft2023/dataSTATA/Customs/exports_it.dta",
*assert(3) nogen
label var exports "Exports: Exports (US$)"
drop if _merge==2
replace exports=0 if exports==.
drop _merge
table Year
*--------------------------------------------------------------------

*--------------------------------------------------------------------
*Bring in LNA Exposure at the firm-level:
*Constructed with do file: "LNA\Draft2023\CodeStata\LNA\LNA_exposure_8dig_new.do"
merge 1:1 ID Year using "Draft2023/DataStata/Customs/lna_exposurealternatives_it.dta",
keep if Year>=2003
drop if Year==.
drop _merge
*--------------------------------------------------------------------


**Does it makes sense to fillin here? This will put ceros to non-active firms in some years.
**But it will create a balance panel. Adding this ceros make effect on imports too high.
fillin ID Year


*--------------------------------------------------------------------
*Fill in zeros, construct key variables:

*Allocate 0 imports to those years that the firm did not import
replace impo=0 if impo==.
replace impointer=0 if impointer==.
replace exports=0 if exports==.
replace numempl=0 if numempl==.



*Sector of activity for firms in missing years:
destring clae,replace
bysort ID: egen clae_aux=max(clae)
replace clae=clae_aux if clae==.

*Sector of activity at 4 and 2 digits:
nsplit clae,gen(clae4 aux) d(4 2)
drop aux
nsplit clae,gen(clae2 aux) d(2 4)
drop aux
drop if clae==.
order ID clae clae4 clae2 Year exports impo

*Variables in logs
g lpromedio_salario_empleado=log(promedio_salario_empleado)
gen lnexpo=ln(1+exports)
gen lnimpo=ln(1+impo)
gen lnL=ln(1+numempl)
gen expo_indicator=0
replace expo_indicator=1 if exports>0


*merge m:1 ID using "Draft2023/DataStata/AFIP_SampleIDs_active.dta"

*Active firm-year combination binary: 
gen active=0
replace active=1 if numempl>0

*Fill in zeros to Exposure measures
forvalues i=0(1)6{
replace Exposurev`i'=0 if Exposurev`i'==. 
}

save "Draft2023/dataSTATA/L_FinalDataset.dta",replace
*=============================================================================*
* Events
*=============================================================================*

sort cuit anio

replace shimp=0 if shimp==.
gen lshimp=l.shimp
replace lshimp=0 if lshimp==.
gen event=0
replace event=1 if lshimp==0 & shimp!=0
gen event2=0
replace event2=1 if lshimp<0.10 & shimp>=0.10
gen event3=0
replace event3=1 if lshimp<0.25 & shimp>=0.25
gen lexp=ln(fob+(fob^2+1)^(1/2))

egen max=max(shimp), by(cuit)
*drop if max==0
gen event4=event
replace event4=0 if anio==2011
nsplit clae, d(4 2) gen(clae4 clae42)
nsplit clae6, d(2 4) gen(clae2 clae24)
drop lshimp fob clae42 clae24

****************************************************************************************************************************************************
*                         Event Study                                                                                                              *
****************************************************************************************************************************************************
global work "C:\Users\rodri\Dropbox\LNA\Draft2023\Results\Figures"

capture program drop eventStudy
program define eventStudy,
	syntax,[ data(string) lags(int 4) leads(int 4) smplename(string)  outcome(string) eventVar(string) timeVar(string) idVar(string) smple(string) controls(string)]
	set scheme s2mono

	use `data', clear

	xtset `idVar' `timeVar'

	forvalues x=1/`leads'{
		gen F`x'`eventVar' = F`x'.`eventVar'
		replace F`x'`eventVar'=0 if F`x'==.
	}

	forvalues x=0/`lags'{
		gen L`x'`eventVar' = L`x'.`eventVar'
	replace L`x'`eventVar'=0 if L`x'==.
	}
	
	
	reghdfe `outcome' L*`eventVar' F*`eventVar' i.`timeVar' if  `timeVar'>2002 & `timeVar'<=2011 & `smple', absorb(i.`controls'##c.`timeVar' `idVar') 
	*vce(cluster `idVar')
	*Normalize year t-1
	forval f = 1/`leads'{
		local nlc "`nlc' (B`f': _b[F`f'`eventVar'] - _b[F1`eventVar'])"
	}
	forval l = 0/ `lags'{
		local nlc "`nlc' (A`l': _b[L`l'`eventVar'] - _b[F1`eve111ntVar'])"
	}
	nlcom `nlc', post

	preserve 
		parmest, norestore 
		gen t = substr(parm,2,1)
		destring t, replace
		replace t = -t if substr(parm,1,1) =="B"
		sort t
		twoway (sc estimate t, mcolor(red) mlcolor(black) lcolor(red) connect(direct)) (rcap min95 max95 t , lcolor(gs10) ), xline(-1, lpattern(dash) lcolor(black)) yline(0, lcolor(black)) xtitle("Years since product requires non-automatic import license") xlabel(-`leads'(1)`lags') ytitle("log(exports)")
		graph display Graph, ysize(10) xsize(15) margin(tiny) scheme(s1mono) 
		graph export $work\G1`smplename'.png, replace
		graph export $work\G1`smplename'.pdf, replace
	restore 

end




* Cuando lo toca la primera LNA - Controlando por clae4
local data "$work\Base.dta"
local lags = 2
local leads = 5
local outcome "lexp"
local eventVar "event"
local smple clae6!=. 
local smplename "Cutoff_0_clae_1"
local timeVar "anio"
local idVar "cuit"
local controls "clae4"

eventStudy, data(`data') smplename(`smplename') lags(`lags') smple(`smple') leads(`leads') outcome(`outcome') eventVar(`eventVar') timeVar(`timeVar') idVar(`idVar') controls(`controls')



* Cuando lo toca la primera LNA - Sin controlar por clae4
local data "$work\Base.dta"
local lags = 2
local leads = 5
local outcome "lexp"
local eventVar "event"
local smple clae6!=. 
local smplename "Cutoff_0_clae_0"
local timeVar "anio"
local idVar "cuit"
local controls "anio"

eventStudy, data(`data') smplename(`smplename') lags(`lags') smple(`smple') leads(`leads') outcome(`outcome') eventVar(`eventVar') timeVar(`timeVar') idVar(`idVar') controls(`controls')



* Cuando LNA=10% de las impo - Sin controlar por clae4
local data "$work\Base.dta"
local lags = 2
local leads = 5
local outcome "lexp"
local eventVar "event2"
local smple clae6!=. 
local smplename "Cutoff_10_clae_0"
local timeVar "anio"
local idVar "cuit"
local controls "anio"

eventStudy, data(`data') smplename(`smplename') lags(`lags') smple(`smple') leads(`leads') outcome(`outcome') eventVar(`eventVar') timeVar(`timeVar') idVar(`idVar') controls(`controls')



* Cuando LNA=10% de las impo - Controlando por clae4
local data "$work\Base.dta"
local lags = 2
local leads = 5
local outcome "lexp"
local eventVar "event2"
local smple clae6!=. 
local smplename "Cutoff_10_clae_1"
local timeVar "anio"
local idVar "cuit"
local controls "clae4"

eventStudy, data(`data') smplename(`smplename') lags(`lags') smple(`smple') leads(`leads') outcome(`outcome') eventVar(`eventVar') timeVar(`timeVar') idVar(`idVar') controls(`controls')


* Cuando LNA=25% de las impo - Sin controlar por clae4
local data "$work\Base.dta"
local lags = 2
local leads = 5
local outcome "lexp"
local eventVar "event2"
local smple clae6!=. 
local smplename "Cutoff_25_clae_0"
local timeVar "anio"
local idVar "cuit"
local controls "anio"

eventStudy, data(`data') smplename(`smplename') lags(`lags') smple(`smple') leads(`leads') outcome(`outcome') eventVar(`eventVar') timeVar(`timeVar') idVar(`idVar') controls(`controls')



* Cuando LNA=25% de las impo - Controlando por clae4
local data "$work\Base.dta"
local lags = 2
local leads = 5
local outcome "lexp"
local eventVar "event2"
local smple clae6!=. 
local smplename "Cutoff_25_clae_1"
local timeVar "anio"
local idVar "cuit"
local controls "clae4"

eventStudy, data(`data') smplename(`smplename') lags(`lags') smple(`smple') leads(`leads') outcome(`outcome') eventVar(`eventVar') timeVar(`timeVar') idVar(`idVar') controls(`controls')




****************************************************************************************************************************************************
*Ahora le pongo valor 0 al event en el año 2011 para que las empresas que empiezan a ser afectadas ese año funcionen como control*
****************************************************************************************************************************************************


capture program drop eventStudy
program define eventStudy,
	syntax,[ data(string) lags(int 4) leads(int 4) smplename(string)  outcome(string) eventVar(string) timeVar(string) idVar(string) smple(string) controls(string)]
	set scheme s2mono

	use `data', clear

	xtset `idVar' `timeVar'

	replace `eventVar'=0 if `timeVar'==2011
	
	forvalues x=1/`leads'{
		gen F`x'`eventVar' = F`x'.`eventVar'
		replace F`x'`eventVar'=0 if F`x'==.
	}

	forvalues x=0/`lags'{
		gen L`x'`eventVar' = L`x'.`eventVar'
	replace L`x'`eventVar'=0 if L`x'==.
	}
	
	
	reghdfe `outcome' L*`eventVar' F*`eventVar' i.`timeVar' if  `timeVar'>2002 & `timeVar'<=2011 & `smple', absorb(i.`controls'##c.`timeVar' `idVar') 
	*vce(cluster `idVar')
	*Normalize year t-1
	forval f = 1/`leads'{
		local nlc "`nlc' (B`f': _b[F`f'`eventVar'] - _b[F1`eventVar'])"
	}
	forval l = 0/ `lags'{
		local nlc "`nlc' (A`l': _b[L`l'`eventVar'] - _b[F1`eve111ntVar'])"
	}
	nlcom `nlc', post

	preserve 
		parmest, norestore 
		gen t = substr(parm,2,1)
		destring t, replace
		replace t = -t if substr(parm,1,1) =="B"
		sort t
		twoway (sc estimate t, mcolor(red) mlcolor(black) lcolor(red) connect(direct)) (rcap min95 max95 t , lcolor(gs10) ), xline(-1, lpattern(dash) lcolor(black)) yline(0, lcolor(black)) xtitle("Years since product requires non-automatic import license") xlabel(-`leads'(1)`lags') ytitle("log(exports)")
		graph display Graph, ysize(10) xsize(15) margin(tiny) scheme(s1mono) 
		graph export $work\G1`smplename'.png, replace
		graph export $work\G1`smplename'.pdf, replace
	restore 

end




* Cuando lo toca la primera LNA - Controlando por clae4
local data "$work\Base.dta"
local lags = 2
local leads = 5
local outcome "lexp"
local eventVar "event"
local smple clae6!=. 
local smplename "Control_firms_2011_clae"
local timeVar "anio"
local idVar "cuit"
local controls "clae4"

eventStudy, data(`data') smplename(`smplename') lags(`lags') smple(`smple') leads(`leads') outcome(`outcome') eventVar(`eventVar') timeVar(`timeVar') idVar(`idVar') controls(`controls')



* Cuando lo toca la primera LNA - Sin controlar por clae4
local data "$work\Base.dta"
local lags = 2
local leads = 5
local outcome "lexp"
local eventVar "event"
local smple clae6!=. 
local smplename "Control_firms_2011"
local timeVar "anio"
local idVar "cuit"
local controls "anio"

eventStudy, data(`data') smplename(`smplename') lags(`lags') smple(`smple') leads(`leads') outcome(`outcome') eventVar(`eventVar') timeVar(`timeVar') idVar(`idVar') controls(`controls')


