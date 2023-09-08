clear all

*** Load CCES data

use "/Users/chaoyuewang/Downloads/Surveys/CCES/CCES22.dta"

*** Political and Demographic variables


*** Political and Demographic variables


tostring caseid, replace


replace pid7 = . if pid7 > 7
replace ideo5 = . if ideo5 > 5

recode faminc 97 = .
rename faminc income
tab income

tab gender4

gen age = 2020 - birthyr

gen white = (race == 1)
gen black = (race == 2)
gen hispan = (race == 3)
gen asian = (race == 4)

tab white

rename commonweight weight

*** Georgrahpic Info

decode inputstate,gen(state)
rename inputstate statefips

rename countyfips fips

rename countyname county

rename  lookupzip zipcode

*** Policing Attitudes

clonevar police_safe = CC22_307
clonevar police_increase = CC22_334c
clonevar police_decrease = CC22_334d

rename CC22_334f police_investigate
rename CC22_334b police_camera
rename CC22_334g police_endarms
rename CC22_334e police_banchoke
rename CC22_334h police_sue

** Output

keep weight pid7 ideo5 income gender4 educ birthyr age ///
race white black hispan asian ///
fips county state statefips zipcode caseid ///
police_*

outsheet using "/Users/chaoyuewang/Documents/Projects/Racialized Police/input/CCES22 attitudes/cces22 attitudes.csv", replace comma nolabel


