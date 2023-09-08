clear all

cd /Users/chaoyuewang/Downloads

use /Users/chaoyuewang/Downloads/LEMAS2016/37323-0001-Data.dta

do /Users/chaoyuewang/Downloads/LEMAS2016/37323-0001-Supplemental_syntax.do

rename *, lower

rename county countyname

rename fips county

clonevar full_all = totftemp  
clonevar sworn_all = ftsauth

foreach race in white black hisp {
	
	clonevar sworn_`race'_male = pers_`race'_male
	clonevar sworn_`race'_female = pers_`race'_fem
	gen sworn_`race' = sworn_`race'_male +  sworn_`race'_female
}

tab pers_edu_min
sort county

tab pers_chf_race

rename city city0
destring zipcode, replace

merge m:n zipcode using /Users/chaoyuewang/Desktop/Racing-Police/Data/zip-to-city-crosswalk.dta

keep if _merge == 3
drop _merge

*** Descriptive Visualization

global dv "ftsauth" 
global indv "totftemp"
global thres "100000"

scatter $dv $indv if $indv < $thres ,  msymbol(oh) mcolor(midblue%40) msize(small) ///
|| pci 0 0 $thres $thres, lcolor(black%60) lpattern(dash) lwidth(medium) /// 
 scheme(s1mono)  ///
xlabel(, labsize(small) grid glpattern(shortdash) glcolor(gray%15) glwidth(thin))  /// 
ylabel(#5, labsize(small) grid glpattern(shortdash) glcolor(gray%15) glwidth(thin)) ///
 legend(off order(1 "White % in Police > in Pop." /// 
 2 "White % in Police < in Pop." ) row(1) ) ///
xtitle(# of Full-Time Employee) /// 
ytitle(# of Sworn Officer) /// 
 scale(0.9) ysize(*1.12) 


global dv "hispan_in_sworn" 
global indv "popserved"
global thres "40000"

scatter $dv $indv if $indv < $thres,  msymbol(oh) mcolor(ebblue%30) msize(small) ///
/// || pci 0 0 $thres $thres, lcolor(black%60) lpattern(dash) lwidth(medium) /// 
 || lowess $dv $indv if $indv < $thres, lcolor(red%75) lwidth(medthick) /// ciplot(rline) blpattern(dash) blcolor(black%70) blwidth(medium) ///
 scheme(s1mono)  ///
xlabel(, labsize(small) grid glpattern(shortdash) glcolor(gray%15) glwidth(thin))  /// 
ylabel(#5, labsize(small) grid glpattern(shortdash) glcolor(gray%15) glwidth(thin)) ///
 legend(off order(1 "White % in Police > in Pop." /// 
 2 "White % in Police < in Pop." ) row(1) ) ///
xtitle(# Population Served) /// 
ytitle(# White Sworn Officers) /// 
 scale(0.9) ysize(*1.12) 

 
*** Aggeragate County-Level Data

tab pers_chf_race

clonevar sworn_hispan  = sworn_hisp 

collapse (sum) full sworn_* popserved , by(city)

foreach race in white black hispan {
	gen `race'_in_sworn =  sworn_`race' / full_all
}

drop if white_in_sworn == 0 & black_in_sworn == 0
drop if white_in_sworn == .


// keep zipcode *_sworn sworn_* full_all
// gen zipcode2 = substr(zipcode, 1, 3)
// duplicates drop zipcode2, force

// rename state state2

duplicates drop city, force

drop if white_in_sworn > 1

sort white_in_sworn

save /Users/chaoyuewang/desktop/Racing-Police/Data/lemas-16-county.dta, replace





