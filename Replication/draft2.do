* What: Example do-file for draft 2
* Who:  Fran�ois Briatte and Ivaylo Petev
* When: 2012-02-23

* Topic:  Social Determinants of Obesity in the USA
* Data:   National Health Interview Survey
* Sample: N = 24,291 for the 2009 survey year

* Note: this file updates the previous draft with a few significance tests and 
* some preliminary regression modelling with the main variables of interest.


* =========
* = SETUP =
* =========


* Data.
use "Datasets/nhis2009.dta", clear

* Create a folder to export all files.
global pwd=c(pwd)
global wd "Replication/BriattePetev" // !note: edit to fill in your own names
cap mkdir "$wd"
cd "$wd"

* Log.
cap log using "draft2.log", name(draft2) replace

* Subsetting to data from most recent year.
drop if year!=2009

* Subsetting to variables used in the analysis.
keep serial psu strata perweight age sex raceb educrec1 ///
	health height weight uninsured vig10fwk yrsinus

* Set NHIS individual weights (used only for illustrative purposes).
svyset psu [pw=perweight], strata(strata) vce(linearized) singleunit(missing)


* ================
* = DESCRIPTIONS =
* ================


* List.
codebook, c


* DV: Body Mass Index (BMI)
* -------------------------

* Creating the BMI variable.
gen bmi = weight*703/height^2
la var bmi "Body Mass Index"

* Recoding into simpler categories.
gen bmi7 = .
la var bmi7 "BMI categories"
replace bmi7 = 1 if bmi < 16.5
replace bmi7 = 2 if bmi >= 16.5 & bmi < 18.5
replace bmi7 = 3 if bmi >= 18.5 & bmi < 25
replace bmi7 = 4 if bmi >= 25 & bmi < 30
replace bmi7 = 5 if bmi >= 30 & bmi < 35
replace bmi7 = 6 if bmi >= 35 & bmi < 40
replace bmi7 = 7 if bmi >= 40
la de bmi7 ///
	1 "Severely underweight" 2 "Underweight" 3 "Normal" ///
	4 "Overweight" 5 "Obese" 6 "Severely obese" 7 "Morbidly obese"
la val bmi7 bmi7

* Note: this is the slow, risky way of recoding continuous variables. Check the
* code used to construct bmi6 below for a quicker and less error-prone command.

* Breakdown of mean BMI by categories.
d bmi bmi7
tab bmi7, summ(bmi) // show mean BMI in each BMI category

* Inspecting DV for normality.
hist bmi, normal name(bmi_hist, replace)

* Transformations (use gladder for the graphical checks).
ladder bmi

* Log-BMI transformation.
gen logbmi=ln(bmi)
la var logbmi "log(BMI)"

* Inspect improvement in normality.
tabstat bmi logbmi, s(skewness kurtosis) c(s)

* Note: in what follows, significance tests are shown on the untransformed BMI
* variable for legibility. However, to make sure that you are interpreting the
* tests on your data as close as possible to their linearity assumptions, you
* should double-check all tests that you report by also running them on the 
* transformed version of your dependent variable, if one applies.


* IV: Age
* -------

su age, d

* Correlation.
pwcorr bmi age, obs sig

* Recode to 4 age groups.
recode age ///
	(18/44=1 "18-44") ///
	(45/64=2 "45-64") ///
	(65/74=3 "65-74") ///
	(75/max=4 "75+"), gen(age4)
la var age4 "Age groups"

* Exploration:
tab age4, summ(bmi) // mean BMI in each age group
bys age4: ci bmi    // confidence bands

* Crosstabulation of BMI categories with age groups
tab bmi7 age4

* Note: the counts of severaly underweight cells create large confidence bounds
* and render the Chi-squared test fairly useless here. Fisher's exact test will
* fail on such dimensions, so you have exhausted the association tests mentioned
* in the do-file from Week 6. You should recode into less categories to have, in
* this example, bmi7 recoded to bmi6 with both underweight groups coded as one.

* Recoding BMI to 6 groups.
gen bmi6:bmi6 = irecode(bmi, 0, 18.5, 25, 30, 35, 40, .)
la var bmi6 "BMI categories"

* Category labels.
la de bmi6 ///
	1 "Underweight" 2 "Normal" 3 "Overweight" ///
	4 "Obese" 5 "Severely obese" 6 "Morbidly obese"

* Crosstabulations:
tab bmi6 age4
tab bmi6 age4, nof cell // percents over the whole sample
tab bmi6 age4, nof row  // percents of age groups in BMI categories (rows)
tab bmi6 age4, nof col  // percents of BMI categories in age groups (columns)

* Chi-squared test:
tab bmi6 age4, chi2
tab bmi6 age4, chi2 V // with Cram�r's V

* Residuals (requires tab_chi, install if needed)
* ssc install tab_chi, replace
tabchi bmi6 age4, r noo noe // raw residuals
tabchi bmi6 age4, p noo noe // Pearson residuals

* Odds ratios provide an additional way to explore variations in obesity across
* age groups. Students interested in pushing their model to logistic regression
* form are particularly encouraged to follow, as they will be using odds ratios
* and log-odds at a later stage of analysis.

* Create dummies for specific binary states of the dependent variable.
gen obese = (bmi7 > 4)
gen normal = (bmi7==3)

* Obesity in two age groups.
tab obese age4 if inlist(age4,1,2), col exact matcell(odds)

* Probability of being obese.
tabodds obese age4 if inlist(age4,1,2)     // as odds
tabodds obese age4 if inlist(age4,1,2), or // as odds ratios

* Explanatory statement.
disp _n as txt "A person aged 45-64 is about " ///
	as res round((odds[2,2]*odds[1,1])/(odds[1,2]*odds[2,1]),.01) as txt ///
	" times more likely to be obese " _n "than a person aged 18-44."

* Normal weight across age groups.
tab normal age4, col matcell(odds) // exact test ommitted because waiting sucks

* Probability of being normal weight.
tabodds normal age4     // as odds
tabodds normal age4, or // as odds ratios against the odds of the first category

* Explanatory statement.
disp _n as txt "A person aged 75+ is about " ///
	as res 100*round((odds[2,4]*odds[1,1])/(odds[1,4]*odds[2,1]),.01) as txt ///
	"% as likely to be normal weight " _n "than a person aged 18-44."

* Visualization of the odds of being normal weight.
tabodds normal age, ciplot ///
	yti("Pr(normal weight)") xti("Age") ///
	name(normal_age, replace)

* Visualization of the odds of being obese.
tabodds obese age, ciplot ///
	yti("Pr(obese)") xti("Age") ///
	name(obese_age, replace)

* Create age groups every five years to average the data over small age groups.
gen age5f=5*floor(age/5)

* Smoother visualization of the odds of being normal weight.
tabodds normal age5f, ciplot ///
	yti("Pr(normal weight)") xti("Age") ///
	name(normal_age, replace)

* Smoother visualization of the odds of being obese with age.
tabodds obese age5f, ciplot  ///
	yti("Pr(obese)") xti("Age") ///
	name(obese_age, replace)


* IV: Gender
* ----------

fre sex

* Recode as dummy.
recode sex (1=0 "Male") (2=1 "Female"), gen(female)
la var female "Gender (1=female)"

* Exploration:
tab female, summ(bmi) // mean BMI in each gender group
bys female: ci bmi    // confidence bands

* t-test whether the mean estimates of BMI for males and females overlap or not.
ttest bmi, by(female)

* Test how the effect maintains within each age group.
bys age4: ttest bmi, by(female)

* Note: the interactions between age and sex might take over their respective
* interaction with the dependent variable. In presence of several covariates, 
* you can turn to ANOVA, as explained at the end of this do-file. Part III of
* this course covers linear regression, which builds on ANOVA.


* IV: Educational attainment
* --------------------------

fre educrec1

* Recode to 3 groups.
recode educrec1 ///
	(13=1 "Grade 12") ///
	(14=2 "Undergrad.") ///
	(15/16=3 "Postgrad."), gen(edu3)
la var edu3 "Educational attainment"

* Exploration:
tab edu3, summ(bmi) // mean BMI at each education level
bys edu3: ci bmi    // confidence bands

* Exclude middle group to t-test the Grade 12 and the Postgraduate categories.
ttest bmi if edu3!=2, by(edu3)

* Crosstabulations:
tab bmi6 edu3
tab bmi6 edu3, nof cell // percents over the whole sample
tab bmi6 edu3, nof col  // percents of BMI categories in age groups (columns)
tab bmi6 edu3, chi2 V   // Chi-squared test with Cram�r's V
* ssc install tab_chi, replace
tabchi bmi6 edu3, p noo noe // Pearson residuals

* Crosstabulation of age, gender and education groups.
table age4 female edu3
table age4 female edu3, c(mean bmi) f(%8.2f) // show mean BMI in each table cell

* Visualization:
sc bmi age if edu3==1, mc(dkgreen) || sc bmi age if edu3==3, mc(dkorange) ///
	legend(lab(1 "Grade 12") lab(2 "Postgraduate")) ///
	name(bmi_edu, replace)


* IV: Health status
* -----------------

fre health

* Exploration:
tab health, summ(bmi) // mean BMI at each health level
bys health: ci bmi    // confidence bands

* t-test of BMI for excellent v. poor health:
ttest bmi if inlist(health,1,5), by(health)

* Note: if you want to use a t-test on two categories within a variable that has
* more than two categories, as health status in this example, then the 'inlist'
* trick used above is probably the quickest option available.

* Plotting BMI and age for excellent vs. poor health:
sc bmi age if health==1, mc(dkgreen) || sc bmi age if health==5, mc(dkorange) ///
	legend(lab(1 "Excellent health") lab(2 "Poor health")) ///
	name(health, replace)

* Crosstabulations:
tab bmi6 health
tab bmi6 health, nof cell // percents over the whole sample
tab bmi6 health, nof col  // percents of BMI categories in age groups (columns)
tab bmi6 health, chi2 V   // Chi-squared test with Cram�r's V
* ssc install tab_chi, replace
tabchi bmi6 health, p noo noe // Pearson residuals

* Visualization:
gr bar bmi, over(health) asyvars over(female) ///
	by(age4, note("")) legend(rows(1)) ///
	name(age_health, replace)


* IV: Physical exercise
* ---------------------

fre vig10fwk

* Recode.
recode vig10fwk (94/95=0 "Little to no exercise") (96/99=.), gen(phy)

tab phy, m plot // the US has a pretty sedentary population
                // also, this is a really ugly distribution with huge issues,
                // so we will add some jitter to the scatterplot to help the
                // plot look more informative (type 'h sc' for details)

* Visualization.
sc bmi phy if phy > 0, jitter(3) name(bmi_phy, replace)

* Correlation.
pwcorr bmi phy, obs sig

* Visualization as boxplots.
gr box phy, noout over(female) asyvars over(age4) medl(lc(red)) ///
	by(health, total note("")) note("") yti("Mean physical activity") ///
	name(phy_box, replace) // note usage of the 'total' option, among others


* IV: Race
* --------

ren raceb race
fre race

* Exploration:
tab race, summ(bmi) // mean BMI at each health level
bys race: ci bmi    // confidence bands

* Plotting BMI and race categories:
spineplot bmi7 race, scale(.7) name(bmi7, replace)

* Slightly more code-consuming visualization, with stacked bars.
tab race, gen(race_)
gr bar race_*, stack over(bmi7) scale(.7) ///
	legend(row(1) lab(1 "NH White") lab(2 "NH Black") lab(3 "Hispanic") lab(4 "Asian")) ///
	name(bmi7_race, replace)

* Crosstabulations:
tab bmi6 race
tab bmi6 race, nof cell // percents over the whole sample
tab bmi6 race, nof col  // percents of BMI categories in age groups (columns)
tab bmi6 race, chi2 V   // Chi-squared test with Cram�r's V
* ssc install tab_chi, replace
tabchi bmi6 race, p noo noe // Pearson residuals

* Visualization:
spineplot bmi7 race, scale(.7) name(hins, replace)

* Visualization of the odds of being normal weight across race.
tabodds normal race, ciplot ///
	xlab(1 "NH White" 2 "NH Black" 3 "Hispanic" 4 "Asian") yline(1) ///
	name(normal_race, replace)


* IV: Health insurance
* --------------------

fre uninsured

* Recode to dummy.
recode uninsured (1=0 "Not covered") (2=1 "Covered") (else=.), gen(hins)
la var hins "Health insurance (1=covered)."

* Exploration:
tab hins, summ(bmi) // mean BMI at each health level
bys hins: ci bmi    // confidence bands

* Crosstabulations:
tab bmi6 hins
tab bmi6 hins, nof cell // percents over the whole sample
tab bmi6 hins, nof col  // percents of BMI categories in age groups (columns)
tab bmi6 hins, chi2 V   // Chi-squared test with Cram�r's V

* Plotting:
spineplot hins race, scale(.7) name(hins, replace)


* Summary statistics
* ------------------

* Export with tsst command.
tsst using draft2-stats.txt, su(bmi age) fre(female edu3 health phy race hins) replace


* =========
* = MODEL =
* =========


* Scatterplots of BMI and age by education level and race category.
sc bmi age if age < 50 & hins, ms(oh) mc(ltkhaki) ///
	by(edu3 race, legend(off) note("Showing respondents aged 18-49 with health insurance.")) || ///
	lfit bmi age if age < 50 & hins, yline(27.27, lc(gs8)) name(sc, replace)

* Note: we 'control' for health insurance and old age by removing these groups
* from the visualization, but we will learn to actually control for them soon.

* Simple linear regression of BMI against age.
reg bmi age

* Test whether the effect is consistent across race.
bys race: reg bmi age

* Test whether the effect is consistent across education.
bys edu3: reg bmi age

* The next and last part of the course will add multiple regressions here, using
* both continuous and categorical data. Stay tuned, and turn to the bonus for an
* introduction to to the logic of linear regression through ANOVA.


* =======
* = END =
* =======


* Close log (if opened).
cap log close draft2

* Reset working directory.
cd "$pwd"


* ==========
* = BONUS! =
* ==========


* A few things about ANOVA
* ------------------------

* You might find it limiting that ttest works only on two groups. If you hold
* the same assumptions across several groups, you might indeed want to extend
* the logic of the t-test over more categories of randomly selected units.

* Crosstabulation of gender and age:
tab age4 female

* Mean value in BMI in each cell:
table age4 female, col row c(mean bmi)

* Introduce ANOVA, which we will run on the untransformed BMI for clarity, but
* which relies on normality assumptions and thus should be run on log-BMI in a
* full research project. ANOVA means 'analysis of variance' and works like the
* t-test, but on any number of dimensions. This allows you to use ANOVA on any
* number of categorical variables, instead of just one dummy (binary) variable.
* The F-test used in ANOVA is the probability that the means of the dependent 
* variable (BMI) are equal across IV categories (age by sex). We then use the
* margins of that test, i.e. the estimated mean of the DV when one or more IV
* is set to a fixed value, to show the confidence intervals of each estimate.


* ANOVA tests
* -----------

* One-way (univariate) ANOVA:
oneway bmi age4
oneway bmi age4, b // Bonferroni test, i.e. p-values for each IV category

* Plot mean estimates of BMI by age and gender groups:
anova bmi age4
margins age4
marginsplot, name(anova1, replace)


* ANOVA plots
* -----------

* Analysis of variance over all age and gender groups:
anova bmi age4 female

* Plot mean estimates of BMI by age and gender groups:
margins age4#female
marginsplot, name(anova2, replace)

* Crosstabulation of gender and age within each race category:
bys race: tab age4 female

* Mean value in BMI in each cell:
table age4 female, by(race) col row c(mean bmi) f(%8.2f)

* Analysis of variance:
anova bmi age4 female race

* Plot mean estimates of BMI by age, sex and race groups:
margins age4#race#female
marginsplot, by(female) legend(row(1)) name(anova3, replace)


* Going further
* -------------

* Crosstabulation of gender, age and race, by health insurance coverage:
table age4 female hins, by(race) col row scol

* Mean values in BMI in each cell:
table age4 female hins, by(race) col row scol c(mean bmi) f(%8.2f)

* Analysis of variance:
anova bmi age4 female hins race

* ... which is just like ...
reg bmi i.age4 female hins i.race // ... multiple linear regression!

* At that stage, you have reached the point where a linear regression model is
* at least as informative than what you are trying to code. See you next week,
* and best of luck with your work, we know what overload means and sympathize!

* We are done. Thanks for following!
* exit
