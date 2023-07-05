use "bwght50.dta",clear

label var faminc " 1988 family income, $1000s"
label var cigtax " cig. tax in home state, 1988"
label var cigprice "cig. price in home state, 1988"
label var bwght " birth weight, ounces"
label var fatheduc "father's yrs of educ"
label var motheduc "mother's yrs of educ"
label var parity "birth order of child"
label var male "=1 if male child"
labe  var white "=1 if white"
label var cigs "cigs smked per day while preg"
label var lbwght "log of bwght"
label var bwghtlbs "birth weight, pounds"
label var packs "packs smked per day while preg"
label var lfaminc "log(faminc)"
save bwght50,replace

use bwght50,clear
summarize

//generate logcigs = log(cigs)
generate motheduc2 = motheduc*motheduc
twoway(scatter bwght cigs)(lfit bwght cigs)
twoway(scatter lbwght cigs)(lfit lbwght cigs)
//twoway(scatter bwght logcigs)(lfit bwght logcigs)
//twoway(scatter lbwght logcigs)(lfit lbwght logcigs)

//first OLS regression on family condition
regress bwght cigs fatheduc motheduc faminc c.motheduc#c.motheduc, vce(robust)
test (fatheduc=0) (motheduc=0)(faminc=0)(motheduc2=0)

//second OLS regression on cig tax and price
regress lbwght cigs cigtax cigprice faminc, vce(robust)

//third OLS regression on 
generate malewhite = male*white
regress lbwght cigs male white malewhite parity ,vce(robust)

//fourth OLS regression 
generate fatheduc2 = fatheduc*fatheduc
regress lbwght cigs white parity  fatheduc fatheduc2,vce(robust)

//fifthe OLS regression
regress lbwght cigs white parity  ,vce(robust)
test (parity = 0)

generate logparity = log(parity)
generate lnparity = ln(parity)
twoway(scatter bwght logparity)(lfit bwght logparity)
twoway(scatter bwght lnparity)(lfit bwght lnparity)

regress bwght parity c.parity#c.parity,vce(robust)
twoway(scatter bwght parity)(function y = _b[_cons] + _b[parity]*x + _b[c.parity#c.parity]*x^2, range(0 10)) 

//linear probability model
//median of birth weight bwght is 120 ounces
summarize bwght,detail
generate bwght1 = 1 if bwght>120
replace bwght1 =0 if bwght <=120

probit bwght1 cigs white parity, vce(robust)
margins, dydx(cigs) at (cigs = (0 1 5 10 15 20))
test(cigs = 0)(white = 0)(parity = 0)
test(cigs = 0)

// logit
logit bwght1 cigs white parity, vce(robust)
margins, dydx(cigs) at (cigs = (0 1 5 10 15 20))
