


clear
use finaldata_ChineseListedCompanies_GEA&CP.dta
*0.Variable setting
*excluding special companies
drop if Sicmen_str=="J"|Sicmen_str=="K"
keep if LISTINGSTATE_11=="正常上市"
*dependent variable:donation
*explanatory variable
gen time=(year>=2016)
gen treat=0
replace treat=1 if Sicda_str=="B06"|Sicda_str=="B07"|Sicda_str=="B08"|Sicda_str=="B09" ///
|Sicda_str=="C17"|Sicda_str=="C19"|Sicda_str=="C22"|Sicda_str=="C25"|Sicda_str=="C26" ///
|Sicda_str=="C27"|Sicda_str=="C28"|Sicda_str=="C29"|Sicda_str=="C30"|Sicda_str=="C31" ///
|Sicda_str=="C32"|Sicda_str=="C33"|Sicda_str=="D44" 
//defines 16 industries as heavily polluting industries, 
//including thermal power, steel, cement, aluminum electrolysis, coal, metallurgy, the chemical industry, petrochemicals, building materials, papermaking, brewing, pharmaceuticals, fermentation, textiles, leather making, and mining
gen did=time*treat
*control variables
*firm_age
*Nature of Equity:govcon1_p
*IndependentDirectorNumber1
*Herfindahl Index:HHI_A
*Shareholding Ratio of the Largest Shareholder:top1
*roa
*Debt-to-Assets Ratio:F011201A
*Assets Size:size
*Logarithm of Monetary Cash
gen cash_ln=ln(cash*资产总计)
*other variables
gen bigfirm=(Y0601b>=2000)
gen time_pre1=(year==2015)
gen time_pre2=(year==2014)
gen time_pre3=(year==2013)
gen time_pre4=(year==2012)
gen time_pre5=(year==2011)
gen time_pre6=(year==2010)
gen time_post0=(year==2016)
gen time_post1=(year==2017)
gen time_post2=(year==2018)
gen time_post3=(year==2019)
gen time_post4=(year==2020)
gen time_post5=(year==2021)
gen post_0=(time_post0==1&treat==1)
forvalues i=1/5{
	gen pre_`i'=(time_pre`i'==1&treat==1)
	gen post_`i'=(time_post`i'==1&treat==1)
}
gen pre_6=(time_pre6==1&treat==1)
*details of CIEP
sum rectifying_cases,detail
return list
gen cases_middle=r(p50)
gen rectifying_cases_high=(rectifying_cases>=cases_middle)
gen backforward=(back_first==1|back_second==1)
*SO2 Emissions and Pollution Control Expenditures
gen so2=废气中二氧化硫排放量
sum so2,detail
return list
gen so2_middle=r(p50)
gen so2_high=(so2>=so2_middle)
gen ep_cost_ln=ln(ep_cost)
replace ep_cost_ln=0 if ep_cost==0
sum ep_cost,detail
return list
gen ep_cost_middle=r(p50)
gen ep_cost_high=(ep_cost>=ep_cost_middle)
*green credit
gen green_credit=F011301A   //Proportion of Long-term Loans in Total Assets
replace green_credit=0 if F011301A==.
gen green_credit2=D320801_33/TD  // Proportion of Interest Expenditure in Total Liabilities
replace green_credit2=0 if D320801_33==.
*competition
sum HHI_A,detail
return list
gen HHI_A_p50=r(p50)
gen high_competition=(HHI_A>=HHI_A_p50)
tab prov_code,gen(dummyprov_)
tab city_code,gen(dummycity_)
tab year,gen(dummyyear_)
tab Sicmen,gen(dummyindus_)
*winsorizing
winsor2 roa F011201A  ,replace cuts(1 99)


duplicates drop id year,force
xtset id year
global con1 "firm_age govcon1_p IndependentDirectorNumber1 HHI_A top1 roa F011201A size cash_ln"
global fix1 "dummyprov_* dummyyear_* dummyindus_*"
qui:xtreg donation_ln i.time##i.treat $con1 $fix1 ,  r
keep if e(sample)
save data_ChineseListedCompanies_GEA&CP_0.dta,replace

*1.Descriptive statistics
logout,save(Descriptive statistics) word replace: tabstat   ///
donation_ln donation_dum donation_asset firm_age govcon1_p IndependentDirectorNumber1 HHI_A top1 roa F011201A size cash_ln  ///
 ,s( mean sd min max  ) f(%12.2f) c(s)
*2.Baseline regression
xtreg donation_ln i.time##i.treat   ,  r
outreg2 using reg1.doc ,replace tstat ///
bdec(3) tdec(2) ctitle(CP)
xtreg donation_ln i.time##i.treat $con1  ,  r
outreg2 using reg1.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
xtreg donation_ln i.time##i.treat $con1 $fix1 ,  r
outreg2 using reg1.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
*3.Parallel trend test
*3.1 event study
xtreg donation_ln did pre_2 pre_3 pre_4 pre_5 pre_6 $con1 $fix1 , r
outreg2 using reg2.doc ,replace tstat ///
bdec(3) tdec(2) ctitle(CP)
xtreg donation_ln  pre_2 pre_3 pre_4 pre_5 pre_6  post_0 post_1 post_2 post_3 post_4  $con1 $fix1 ,  r
outreg2 using reg2.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
*3.2Figure:95% Confidence interval of policy effect
preserve
gen time_1=pre_6
gen time_2=pre_5
gen time_3=pre_4
gen time_4=pre_3
gen time_5=pre_2
gen time_6=pre_1
xtreg donation_ln time_1 time_2 time_3 time_4 time_5 time_6 post_*   $con1 $fix1 ,  r
coefplot, baselevels ///
keep(time_1 time_2 time_3 time_4 time_5 post_*) ///
vertical ///
coeflabels( time_1=2010 time_2=2011 time_3=2012 time_4=2013 time_5=2014 ///
post_0=2016 post_1=2017 post_2=2018 post_3=2019 post_4=2020 post_5=2021 ) ///
yline(0,lcolor(edkblue*0.8)) ///
xline(6, lwidth(vthin) lpattern(dash) lcolor(teal)) ///
ylabel(,labsize(*0.75)) xlabel(,labsize(*0.75)) ///
ytitle("Policy Dynamics", size(small)) ///
xtitle("Timing", size(small)) ///
addplot(line @b @at) ///
ciopts(lpattern(dash) recast(rcap) msize(medium)) ///
msymbol(circle_hollow) ///
scheme(s1mono)
graph export graph2.png ,replace
restore
*3.3Placebo test
set seed 24680
forvalues i=1/1000 {
	use data_ChineseListedCompanies_GEA&CP_0.dta,clear
	global con1 "firm_age2 govcon1_p IndependentDirectorNumber1 HHI_A top1 roa F011201A size cash_ln"
	global fix1 "dummyprov_* dummyyear_* dummyindus_*"
	gen treat_num0=runiform()
	gen treat_wrong0=0
	replace treat_wrong0=1 if treat_num0>=0.5
	gen treat_wrong=time*treat_wrong0
	qui xtreg donation_ln treat_wrong time treat_wrong0 $con1  $fix1 ,r  
	ereturn list
	gen b_treat=_b[treat_wrong]
	gen se_treat=_se[treat_wrong]
	keep b_treat se_treat
	duplicates drop b_treat,force
	save treat_wrong_`i'.dta,replace
}
use treat_wrong_1.dta,clear
forvalues j=2/1000 {
	append using treat_wrong_`j'.dta
	erase treat_wrong_`j'.dta
}
save test.dta,replace
erase treat_wrong_1.dta
gen tvalue=b_treat/se_treat
scalar df_r=e(N)-e(df_m)-1
gen Pvalue=2*ttail(df_r,abs(tvalue))
save test.dta,replace
sum b_treat,detail
twoway(kdensity b_treat,                                                                   ///
             xline(0.507, lpattern(dash)  lcolor(black))                                 ///
             mcolor(black)   lpattern(solid)                                                            ///
             xtitle("Coefficient"                        , size(medlarge))                 ///
             yaxis(1) ytitle("", size(medlarge))  ///
             xlabel(-0.4(0.2)0.6 , labsize(small) )                                                         ///
             ylabel(, labsize(medlarge) format(%01.0f))  ///
		     graphregion(fcolor(white)  lcolor(white))) ///
	  (scatter Pvalue b_treat,  ///
			xlabel(,grid glcolor(white)) ///
			yaxis(2) yline(0, lcolor(black) lp(shortdash) ) ///
			ylabel(,grid glcolor(white)) ///
			msymbol(smcircle_hollow) mcolor(black) ///
			ytitle("Density Distribution",size(medlarge)) ///
			legend(off) graphregion(color(white))) 
graph export "test_Coefficient.png", replace
*3.4PSM-DID
*method-1:transform the panel data into cross-sectional data year by year for matching
clear
use data_ChineseListedCompanies_GEA&CP_0.dta
xtset id year
global con1 "firm_age govcon1_p IndependentDirectorNumber1 HHI_A top1 roa F011201A size cash_ln"
global fix1 "dummyprov_* dummyyear_* dummyindus_*"
set seed 24680
gen norvar_1=rnormal()
sort norvar_1
psmatch2 treat $con1 , out(donation_ln) logit ate neighbor(2) comm  ties 
sum _pscore if treat==1,detail
*before mathcing
sum _pscore if treat == 0, detail
twoway(kdensity _pscore if treat == 1, lpattern(solid)                     ///
              lcolor(black)                                                  ///
              lwidth(thin)                                                   ///
              scheme(qleanmono)                                              ///
              ytitle("Kernel Density",                ///
                     size(medlarge) )                          ///
              xtitle("Propensity Score",                          ///
                     size(medlarge))                                         ///
              xline(0.2276   , lpattern(solid) lcolor(black))                ///
              xline(`r(mean)', lpattern(dash)  lcolor(black))                ///
              saving(kensity_cs_before, replace))                            ///
      (kdensity _pscore if treat == 0, lpattern(dash) lcolor(black)),                    ///
      xlabel(     , labsize(medlarge) format(%02.1f))                        ///
      ylabel(0(1)4, labsize(medlarge))                                       ///
      legend(label(1 "{stSans:Treat Group}")                                      ///
             label(2 "{stSans:Control Group}")                                      ///
             size(medlarge) position(1) symxsize(10)) graphregion(fcolor(white)  lcolor(white))
graph export "kensity_cs_before.emf", replace
*after matching
sum _pscore if treat == 0 & _weight != ., detail
twoway(kdensity _pscore if treat == 1, lpattern(solid)                     ///
              lcolor(black)                                                  ///
              lwidth(thin)                                                   ///
              scheme(qleanmono)                                              ///
              ytitle("Kernel Density",                ///
                     size(medlarge) )                          ///
              xtitle("Propensity Score",                          ///
                     size(medlarge))                                         ///
              xline(0.2778   , lpattern(solid) lcolor(black))                ///
              xline(`r(mean)', lpattern(dash)  lcolor(black))                ///
              saving(kensity_cs_after, replace))                             ///
      (kdensity _pscore if treat == 0 & _weight != ., lpattern(dash)lcolor(black) ),     ///
      xlabel(     , labsize(medlarge) format(%02.1f))                        ///
      ylabel(0(1)4, labsize(medlarge))                                       ///
      legend(label(1 "{stSans:Treat Group}")                                      ///
             label(2 "{stSans:Control Group}")                                      ///
             size(medlarge) position(1) symxsize(10)) graphregion(fcolor(white)  lcolor(white))
graph export "kensity_cs_after.emf", replace
*the comparison of the regression results of the treatment and control groups before and after PSM
gen     weight = _weight *2
replace weight = 1 if treat == 1 & _weight != .
global ab1 "absorb(prov_code Sicmen year)"
reg donation_ln treat $con1 $fix1 ,r   
outreg2 using reg3.doc ,replace tstat ///
bdec(3) tdec(2) ctitle(CP)
reghdfe donation_ln treat $con1  , $ab1   
outreg2 using reg3.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
reghdfe donation_ln treat $con1  if _weight!=., $ab1   
outreg2 using reg3.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
reghdfe donation_ln treat $con1  if _support==1, $ab1   
outreg2 using reg3.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
reghdfe donation_ln treat $con1  [fweight=weight], $ab1   
outreg2 using reg3.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
*method-2: directly match the panel data period by period
forvalue i = 2011/2021{
      preserve
          capture {
              keep if year == `i'
              set seed 24680
              gen  norvar_2 = rnormal()
              sort norvar_2
			  psmatch2 treat $con1 , out(donation_ln) logit ate neighbor(2) comm  ties 
              save `i'.dta, replace
              }
      restore
      }
clear all
use 2011.dta, clear
forvalue k =2012/2021 {
      capture {
          append using `k'.dta
          }
      }
save yby.dta, replace
sum _pscore if treat == 1, detail 
gen     weight = _weight *2
replace weight = 1 if treat == 1 & _weight != .
global ab1 "absorb(prov_code Sicmen year)"
reg donation_ln treat $con1 $fix1 ,r   
outreg2 using reg6.doc ,replace tstat ///
bdec(3) tdec(2) ctitle(CP)
reghdfe donation_ln treat $con1  , $ab1   
outreg2 using reg6.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
reghdfe donation_ln treat $con1  if _weight!=., $ab1   
outreg2 using reg6.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
reghdfe donation_ln treat $con1  if _support==1, $ab1   
outreg2 using reg6.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
reghdfe donation_ln treat $con1  [fweight=weight], $ab1  
outreg2 using reg6.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
*3.5 robustness tests
* The “de-capacity” policy
preserve
drop if Sicda_str=="B05"|Sicda_str=="C61"|Sicda_str=="C33"|Sicda_str=="C31"|Sicda_str=="C37" 
xtreg donation_ln i.time##i.treat $con1 $fix1 , r
outreg2 using reg7.doc ,replace tstat ///
bdec(3) tdec(2) ctitle(CP)
restore
*The green credit policy
xtreg donation_ln i.time##i.treat green_credit green_credit2 $con1 $fix1 , r
outreg2 using reg7.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
*Incorporating Firm Fixed Effects
xtreg donation_ln i.time##i.treat $con1 $fix1 ,fe vce(robust)
outreg2 using reg7.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
*Accounting for Interaction of Fixed Effects
xtreg donation_ln i.time##i.treat $con1 $fix1 i.year#i.Sicmen i.year#i.prov_code ,r
outreg2 using reg7.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
* Utilizing Regions with Both Treatment and Control Groups
bys city_reg:egen citymark=mean(treat)
count if citymark==0
count if citymark==1
preserve
drop if citymark==0|citymark==1
xtreg donation_ln i.time##i.treat $con1 $fix1 ,  r
outreg2 using reg8.doc ,replace tstat ///
bdec(3) tdec(2) ctitle(CP)
restore
*Changing Measurement Approaches
xtprobit donation_dum i.time##i.treat $con1 $fix1 ,  vce(robust)
outreg2 using reg8.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP_dummy)
xtreg donation_asset i.time##i.treat $con1 $fix1 ,  r
outreg2 using reg8.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP_ratio)
*Implementing a Triple-Difference Model
xtreg donation_ln i.time##i.treat##i.done_2016 $con1 $fix1 ,r
outreg2 using reg8.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)

*4.Mechanism
xtreg donation_ln (i.time##i.treat)#c.rectifying_cases  $con1 $fix1   ,r
outreg2 using reg9.doc ,replace tstat ///
bdec(3) tdec(2) ctitle(CP)
xtreg donation_ln (i.time##i.treat)#i.backforward $con1 $fix1   ,r
outreg2 using reg9.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
xtreg donation_ln (i.time##i.treat)#i.govcon1_p $con1 $fix1   ,r
outreg2 using reg9.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
xtreg donation_ln (i.time##i.treat)#c.ESG $con1 $fix1   ,r
outreg2 using reg9.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)

*5.Heterogeneity
*firm size
xtreg donation_ln i.time##i.treat $con1 $fix1   if bigfirm==1 ,r
outreg2 using reg10.doc ,replace tstat ///
bdec(3) tdec(2) ctitle(CP)
xtreg donation_ln i.time##i.treat $con1 $fix1   if bigfirm==0 ,r
outreg2 using reg10.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
*Industry competition
xtreg donation_ln i.time##i.treat $con1 $fix1   if high_competition==1 ,r
outreg2 using reg10.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
xtreg donation_ln i.time##i.treat $con1 $fix1   if high_competition==0 ,r
outreg2 using reg10.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
*Regional pollution and environmental expenditure
xtreg donation_ln i.time##i.treat $con1 $fix1   if so2_high==1 ,r
outreg2 using reg11.doc ,replace tstat ///
bdec(3) tdec(2) ctitle(CP)
xtreg donation_ln i.time##i.treat $con1 $fix1   if so2_high==0 ,r
outreg2 using reg11.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
xtreg donation_ln i.time##i.treat $con1 $fix1   if ep_cost_high==1 ,r
outreg2 using reg11.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)
xtreg donation_ln i.time##i.treat $con1 $fix1   if ep_cost_high==0 ,r
outreg2 using reg11.doc ,append tstat ///
bdec(3) tdec(2) ctitle(CP)


