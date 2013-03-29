
/* ------------------------------------------ SRQM Session 9 -------------------

   F. Briatte and I. Petev

 - TOPIC:  Satisfaction with Health Services in Britain and France
 
 - DATA:   European Social Survey Round 4 (2008)
    
   We explore patterns of satisfaction with the state of health services in
   the UK and France, two countries with extensive public healthcare systems
   and where health services play different roles in political competition.

 - (H1): We expect to observe high satisfaction on average, except among those
   in ill health, who we expect to report lower satisfaction regardless of age,
   sex, income or political views.

 - (H2): We also expect respondents in political opposition to the government to
   report less satisfaction with the state of health services in the country,
   independently of all other characteristics.

 - (H3): We finally expect to find lower patterns of satisfaction among those
   who report financial difficulties, as evidence of an income effect that
   we expect to exist in isolation of all others.

   We use data from the European Social Survey (ESS) Round 4. The sample used in
   the analysis contains N = 1,942 French and N = 2,079 UK individuals selected
   through stratified probability sampling and interviewed face-to-face in 2008.

   We run linear regressions for each country to assess whether satisfaction
   with health services can be predicted from political views, independently
   of age, sex, health status and financial situation.
   
   Last updated 2012-11-22.

----------------------------------------------------------------------------- */


* Install required commands.
foreach p in estout fre spineplot {
	cap which `p'
	if _rc == 111 cap noi ssc install `p'
}

* Log results.
cap log using code/week9.log, replace


* ====================
* = DATA DESCRIPTION =
* ====================


use data/ess2008, clear


* Dependent variable
* ------------------

d stf*

* Rename DV and a bunch of covariates
ren (stfhlth stfedu stfgov) (hsat esat gsat)

* Cross-country comparison.
tab cntry, summ(hsat)

* Detailed summary statistics.
su hsat, d


* Cross-country comparisons
* -------------------------

* Cross-country visualization (mean).
gr dot hsat, over(cntry, sort(1)des) yla(0 10) ///
    yti("Satisfaction in health services") ///
    name(dv_country, replace)

* Generate category dummies for the full 11-pt scale DV.
cap drop hsat11_*
tab hsat, gen(hsat11_)

* Cross-country visualization (full 11-pt scale).
gr hbar hsat11_*, over(cntry, sort(1)des) stack legend(off) ///
    yti("Satisfaction in health services") ///
    scheme(burd11) name(dv_country11, replace)


* Independent variables
* ---------------------

fre agea gndr health hincfel lrscale, r(10)

* Recode sex to dummy.
gen female:female = (gndr == 2) if !mi(gndr)
la def female 0 "Male" 1 "Female", replace
la var female "Gender"

* Fix age variable name.
ren agea age

* Generate six age groups (15-24, 25-34, ..., 65+).
gen age6:age6 = irecode(age,24,34,44,54,64,.)
replace age6 = 10*age6 + 15
la def age6 15 "15-24" 25 "25-34" 35 "35-44" 45 "45-54" 55 "55-64" 65 "65+", replace
la var age6 "Age groups"

* Subjective low income dummy.
gen lowinc = (hincfel > 2) if !mi(hincfel)
la var lowinc "Subjective low income"

* Recode left-right scale.
recode lrscale (0/4 = 1 "Left") (5 = 2 "Centre") (6/10 = 3 "Right"), gen(pol3)
la var pol3 "Political views (1 = left 3 = right)"


* Subsetting
* ----------

* Check missing values.
misstable pat hsat age6 female health pol3 lowinc if cntry == "FR"
misstable pat hsat age6 female health pol3 lowinc if cntry == "GB"

* Select case studies.
keep if inlist(cntry,"FR","GB")

* Delete incomplete observations.
drop if mi(hsat, age6, female, health, pol3, lowinc)

* Final sample sizes.
bys cntry: count


* Normality
* ---------

* Distribution of the DV in the case studies.
hist hsat, discrete normal xla(1 11) by(cntry, legend(off) note("")) ///
    name(dv_histograms, replace)

* Generate strictly positive DV recode.
gen hsat1 = hsat + 1

* Visual check of common transformations.
gladder hsat1, bin(11) ///
   name(gladder, replace)

/* Notes:

 - There are more missing observations for Britain than for France, and this
   might distort the results if the non-respondents come, for example, from the
   same end of the political spectrum. We'll be careful.

 - The distribution of the DV is skewed to the right in both case studies, which
   is consistent with the hypothesis that extensive healthcare states like the
   ones found in Britain France enjoy higher popular support.

 - To allow for a log-transformation, the variable should be strictly positive
   since the function f: y = log(x) is undefined for x = 0. We use a recode of
   the DV of strictly positive range to test for transformations.

 - The square root comes only marginally closer to a normal distribution. With
   little improvement in normality, transforming the DV would be overkill. It is
   reasonable to carry on with the untransformed DV. */


* Export summary statistics
* -------------------------

* The next command is part of the SRQM folder. If Stata returns an error when
* you run it, set the folder as your working directory and type -run profile-
* to run the course setup, then try the command again. If you still experience
* problems with the -stab- command, please send a detailed email on the issue.

stab using week9, replace ///
    su(hsat) ///
    fr(female age6 health lowinc pol3) ///
    by(cntry)

/* Basic syntax of -stab- command:

- argument: -using NAME-  adds the NAME prefix to the exported file(s)
- argument: -su()-        summarizes a list of continuous variables (mean, sd, min-max)
- argument: -fre()-       summarizes a list of categorical variables (frequencies)

- option:   -by-          produces several tables over a given categorical variable
- option:   -replace-     overwrite any previously existing tables
- option:   [aw, fw]      use survey weights (use only if you know how they work)

  In the example above, the -stab- command will export two files to the working
  directory, containing summary statistics for France (week9_stats_FR.txt) and
  Britain (week9_stats_GB.txt). If the research examines continuous variables,
  add the -corr- option to export a correlation matrix (shown next week). */


* =====================
* = ASSOCIATION TESTS =
* =====================


* Relationships with socio-demographics
* -------------------------------------

* Line graph using DV means computed for each age and gender group.
cap drop msat_?
bys cntry age6: egen msat_1 = mean(hsat) if female
bys cntry age6: egen msat_2 = mean(hsat) if !female
tw conn msat_? age6, by(cntry, note("")) ///
    xti("Age") yti("Mean level of satisfaction") ///
    legend(row(1) order(1 "Female" 2 "Male")) ///
    name(hsat_age_sex, replace)

* Association between DV and gender.
by cntry: ttest hsat, by(female)

* Correlation between DV and age (using the continuous measurements).
by cntry: pwcorr hsat age, obs sig

* Plot DV histograms over small multiples (6 age groups, 2 countries).
hist hsat, normal discrete by(cntry age6, col(3) note("") legend(off)) ///
    name(dv_age6, replace)

* Generate a dummy from extreme categories of age.
cap drop agex
gen agex:agex = .
replace agex = 0 if age6 == 15
replace agex = 1 if age6 == 65
la def agex 0 "15-24" 1 "65+", replace

* Difference between age extremes.
bys cntry: ttest hsat, by(agex)


* Relationship to health status
* -----------------------------

* DV by health.
gr dot hsat [aw = dweight], over(health) over(cntry) ///
    yti("Satisfaction in health services") ///
    name(dv_health, replace)

* Generate a dummy from health status (bad/very bad = 0, good/very good = 1).
cap drop health01
recode health (1/2 = 1 "Good") (4/5 = 0 "Poor") (else = .), gen(health01)

* Association between DV and health status.
bys cntry: ttest hsat, by(health01)


* Relationship to low income status
* ---------------------------------

* DV by income.
bys cntry: ttest hsat, by(lowinc)

* Association between IV and political attitude.
bys cntry: tab lowinc pol3, col chi2 nokey

* Proportions test (since the lowinc dummy is a proportion of the sample).
bys cntry: prtest lowinc if pol3 != 2, by(pol3)


* Relationship to left-right attitude
* -----------------------------------

* Correlation between DV and political attitude (left 1-10 right).
by cntry: pwcorr hsat lrscale, obs sig

* Association between DV and political attitude (left, centre, right).
gr box hsat, over(pol3) asyvars over(cntry) legend(row(1)) ///
    scheme(burd4) name(dv_pol3, replace)


* Comparison with covariates
* --------------------------

d hsat esat gsat

* DV and other ESS satisfaction items (edu = education, gov = government).
cap drop msat*
bys cntry lrscale: egen msat1 = mean(hsat)
bys cntry lrscale: egen msat2 = mean(esat)
bys cntry lrscale: egen msat3 = mean(gsat)

* Line graph, using the means computed above for each left-right group.
tw conn msat1-msat3 lrscale, by(cntry, note("")) ///
    xla(0 "Left" 10 "Right") xti("") yti("Mean level of satisfaction") ///
    legend(row(1) order(1 "Health services" 2 "Education" 3 "Government")) ///
    name(stf_lrscale, replace)

/* Notes:

 - The significance tests are expectedly highly positive due to the large N.
   The risk here is to make Type I errors, even though the variations between
   age groups in each country seem statistically robust.

 - Health status seems important in France but not in Britain, whereas old age
   seems important in Britain but not in France. It will be interesting to see
   if any of these effects persist after controlling for income.

 - The relationship between financial difficulties and political leaning shows
   how your independent variables are interacting with each other.
         
 - Other measures of satisfaction (which are not part of the model itself) show
   how health services correlate to other measures of public sector performance
   when the measures are examined by left-right positioning. Politics matter. */
 

* =====================
* = REGRESSION MODELS =
* =====================


* Multiple linear regression model for each country case.
bys cntry: reg hsat ib45.age6 female i.health lowinc ib2.pol3

* Cleaner output with the -leanout- command.
leanout: reg hsat ib45.age6 female i.health lowinc ib2.pol3 if cntry == "FR"
leanout: reg hsat ib45.age6 female i.health lowinc ib2.pol3 if cntry == "GB"

/* Notes:

 - This model is specified as a multiple linear regression. It captures linear
   relationships by computing the partial derivative of each variable, which is
   its effect on the DV when all other variables are held constant.

   We will therefore read the coefficient of an IV as its net effect on the DV,
   independently of all other variables in the model. This interpretation gives
   its meaning to the idiom of 'all other things being equal' (ceteris paribus).

 - The baseline age category is set to the category that contains the average
   population age (45-54 years-old) and is coded 'ib45' because the categories
   of 'age6' are coded 15, 25, 35 etc.

 - The baseline health status is set to default reference category 1 = very good.
   Categories 2-5 code for 2 = good to 5 = poor health.

 - The baseline political attitude is the modal (and central) category 2 = centre
   so that 1 = leftwing and 3-rightwing.
   
   The baseline model, given by the constant, is therefore the predicted mean of
   the DV for respondents who are males, aged 45-54, in very good health, at the
   centre politically and who did not report financial difficulties ('lowinc').
   
 - Let's manually check whether the model does a good job at predicting the
   constant (the baseline model) in the second country case:
   
   su hsat if age6 == 45 & !female & health == 1 & !lowinc & pol3 == 2 & cntry == "GB"
   
 - For the same country case, the model predicts a higher value for respondents
   aged 65+, keeping all other variables equal. Let's check that too:
   
   su hsat if age6 == 65 & !female & health == 1 & !lowinc & pol3 == 2 & cntry == "GB"
   
   Not so bad for a model predicting only 7% of the variance, but remember that
   the predicted values are only means, that they are significant only for some
   coefficients, and that they apply only to a fraction of all observations.
   
 - To assess the overall quality of the models, you should rather read the RMSE.
   The Root Mean Squared Error is the standard error of the regression: it shows
   by how much we mispredict the DV on average.

   We later turn to regression diagnostics to explore the error term. */


* Using the -estout- command
* --------------------------

* Store model estimates.
eststo clear
bys cntry: eststo: reg hsat ib45.age6 female i.health lowinc ib2.pol3

* View stored model estimates.
eststo dir

* View standardized coefficients.
esttab, wide nogaps beta(2) se(2) sca(rmse) mti("FR" "GB")

* Export unstandardized coefficients.
esttab using week12_regressions.txt, replace ///
    nolines wide nogaps b(1) se(1) sca(rmse) mti("FR" "GB")


* Models with covariates
* ----------------------

* Store model estimates (again).
eststo clear
bys cntry: eststo: reg hsat ib45.age6 female i.health lowinc ib2.pol3

* Run identical model on satisfaction with education.
bys cntry: eststo: reg esat ib45.age6 female i.health lowinc ib2.pol3

* Run identical model on satisfaction with government.
bys cntry: eststo: reg gsat ib45.age6 female i.health lowinc ib2.pol3

* View updated list of model estimates.
eststo dir

* Compare DV and covariates in each country, using standardized coefficients and
* R-squared to compare predicted variance across the models.
esttab est1 est3 est5, lab nogaps beta(2) se(2) r2 ///
	mti("Health" "Education" "Government") ti("France")

esttab est2 est4 est6, lab nogaps beta(2) se(2) r2 ///
	mti("Health" "Education" "Government") ti("UK")

/* Basic usage of -estout- commands:
  
 - The -estout- commands work by storing model estimates with -eststo- and then
   putting them into tables with -esttab-. Use these commands at the end of your
   models: start with -reg- and -leanout-, then use -eststo- and -esttab-.
   
 - The -estout- command is especially practical when you run many models, as
   shown here when we compare the model between country cases and then check
   how the DV model compares to other satisfaction measures (covariates).

 - Check the -estout- online documentation for more examples. */


* ==========================
* = REGRESSION DIAGNOSTICS =
* ==========================


* Note: what we call 'diagnostics' at that stage actually covers a broader range
* of postestimation commands like -margins- and -marginsplot- (marginal effects)
* or seemingly unrelated regression (SUREG). The overall logic of these commands
* is to help with the detection of patterns that are not taken into account by
* our 'front-end' linear regression model.


* (1) France: Residuals
* ---------------------

reg hsat ib45.age6 female i.health lowinc ib2.pol3 if cntry == "FR"

* Variance inflation.
vif

* Residuals-versus-fitted values plot.
rvfplot, yline(0) ///
	name(rvf_fr, replace)

* Store the standardized residuals for the estimation sample (France only).
cap drop rst_fr
predict rst_fr if e(sample), rsta

* Distribution of the residuals.
hist rst_fr, normal ///
	name(rst_fr_1, replace)

* Store the predicted values for the estimation sample (France only).
cap drop yhat_fr
predict yhat_fr if e(sample)

* Plot the distribution of the standardized residuals over socio-demographics.
hist rst_fr, normal by(female age6, legend(off)) bin(10) xline(0) ///
	name(rst_fr_2, replace)

* Plot the residuals-versus-fitted values by income and political views.
sc rst_fr yhat_fr, by(pol3 lowinc, col(2) legend(off)) yline(0) ///
	name(rst_fr_3, replace)


* (2) France: Marginal effects
* ----------------------------

* Briefly recall the model by calling -reg- without any new specification.
reg

* What is observable above is the (positive) linear effect of one predictor onto
* the DV: all other things kept equal, rightwing views lead to a higher level of
* satisfaction with health services, independently of age, gender, income and so
* on. You can show the same thing by predicting the marginal effect of the IV on
* the DV with the -margins- command.
margins pol3
marginsplot, ///
	name(margins_pol3_fr, replace)

* Let's plot a more complex interaction where we observe the effect of political
* views and health status combined. The linear effect of political views remains
* observable at good health but becomes indistinguishable when health degrades.
margins health#pol3
marginsplot, recast(line) recastci(rarea) ciopts(fi(25)) legend(row(1)) ///
	name(margins_health_pol3_fr, replace)


* (3) Britain: Exercise
* ---------------------

reg hsat ib45.age6 female i.health lowinc ib2.pol3 if cntry == "GB"

* As an exercise, run your own selection of regression diagnostics and marginal
* effects for the British model. Compare the predictors in each country and see,
* for instance, if age and political views have the same effects in Britain.


* ==============
* = EXTENSIONS =
* ==============


* Note: this section showcases some methods that are related to the content of
* the course, but go beyond its scope. Extension (1), bootstrapping, has to do
* with simulation; Extension (2), corrected standard errors, is explorable at
* much greater length with panel data analysis; and Extension (3), multilevel
* analysis, can go into much more subtle refinements than the raw model shown
* here. These topics require much broader theoretical support to operate, and
* are shown here for demonstration purposes.


* (1) Bootstrapping
* -----------------

reg hsat ib45.age6 female i.health lowinc ib2.pol3 if cntry == "FR", vce(bootstrap, r(100))
reg hsat ib45.age6 female i.health lowinc ib2.pol3 if cntry == "GB", vce(bootstrap, r(100))

/* What happened here:

 - Bootstrapping is a simulation technique that resamples the data as many times
   as you ask it (here we ran 1,000 replications) and then computes the standard
   error from the standard deviation of these simulations.

 - Resampling means that the data used in each simulation is randomly selected
   from the original dataset, with replacement: one value may appear many times.
   The result is 1,000 simulations of the data with slightly different values.
 
 - Bootstrapping is particularly efficient at lower sample sizes, for which it
   provides more reliable standard errors than the 'square root of N' formula.
   It applies to parametric estimation commands like -su-, -reg-, etc. */


* (2) Clustered standard errors
* -----------------------------

* Remember that we saved the initial models as 'est1' (FR) and 'est2' (GB).
eststo dir

* Store robust models.
eststo FRr: reg hsat ib45.age6 female i.health lowinc ib2.pol3 if cntry == "FR", vce(cluster regionfr)
eststo GBr: reg hsat ib45.age6 female i.health lowinc ib2.pol3 if cntry == "GB", vce(cluster regiongb)

* Compare both versions for a more realistic assessment of the standard errors.
esttab est1 FRr est2 GBr, nogaps b(2) se(2) sca(rmse) compress ///
	mti("FR" "FR robust" "GB" "GB robust")

/* What happened here:

 - We clustered the data by geographical region in each regression, which means
   that the standard errors of the coefficients will increase if the variance of
   the data differs between regions, indicating some macro-level effect.
 
 - In this example, we assume that poorer and/or less populated regions will not
   benefit from the same health care facilities than others, which will create
   differences between predicted means of the DV clustered by region.

 - The results show that the clustered models lose some significant coefficients
   in comparison to the original ones, which should invite us to correct some of
   our initial interpretations, or consider more advanced modelling.
   
 - Robust (corrected) standard errors become crucial when the data form a panel,
   as with cross-sectional time-series (CSTS) data, because the observations are
   then country-years and variance will exist between and within them.
   
 - More complex methods exist to analyze panel data and are implemented in Stata
   through the -xt- and -ts- commands. These methods require to handle different
   data structures and model equations, and are not covered here. */


* (3) Multilevel analysis
* -----------------------

* Reload the original dataset to get all countries.
use data/ess2008, clear

* Delete incomplete observations.
keep if !mi(agea,gndr,health,hincfel,lrscale)

/* What is going to happen here:

 - We start by adding a country-level predictor to the data by looking at the
   institutional characteristics of health systems. The classification used
   below is taken from Maurizio Ferrera, "The Boundaries of Welfare", 2005.
 
 - We then merge ESS data with QOG data to obtain the average national health
   expenditure per capita in each country case. Multilevel data for European
   countries and regions is also available from the ESS website.
 
 - Finally, we estimate a multi-level model with individual-level predictors and
   country-level predictors. The first step is to estimate a null model, then to
   add 'Level 1' (individual) predictors, then add 'Level 2' predictors.
 
 - The interpretation would be too long to cover here. For a full example with
   explanations, see Kristen Ringdal, "Learning Multilevel Analysis", 2012, at
   ESS EduNet: http://essedunet.nsd.uib.no/cms/topics/multilevel/ */

* Country-level predictor (1): health system.
gen hsys:hsys = .
replace hsys = 1 if inlist(cntry,"GB","IE") // NHS = National Health Services
replace hsys = 2 if inlist(cntry,"DK","SE","FI","IT","ES")
replace hsys = 3 if inlist(cntry,"FR","BE","LU") // SHI = Social Health Insurances
replace hsys = 4 if inlist(cntry,"DE","AT","NL")
la var hsys "Public health system"
la def hsys 1 "Centralized NHS" 2 "Decentralized NHS" 3 "Indirect SHI" 4 "Direct SHI", replace
tab cntry hsys

* Country-level predictor (2): health expenditure per capita.
gen ccodewb = ""
replace ccodewb = "BEL" if cntry == "BE"
replace ccodewb = "DEU" if cntry == "DE"
replace ccodewb = "DNK" if cntry == "DK"
replace ccodewb = "EST" if cntry == "ES"
replace ccodewb = "FIN" if cntry == "FI"
replace ccodewb = "FRA" if cntry == "FR"
replace ccodewb = "GBR" if cntry == "GB"
replace ccodewb = "IRL" if cntry == "IE"
replace ccodewb = "NLD" if cntry == "NL"
replace ccodewb = "SWE" if cntry == "SE"
merge m:1 ccodewb using data/qog2011, keep(3) keepusing(wdi_hec)

* Null model.
xtmixed stfhlth || cntry: , variance

/* Notes:
 
 - The constant of the null model is the weighted mean of the DV across all
   country cases, which is significantly lower than in the FR-GB comparison.
   Other results are readable in the bottom half of the -xtmixed- output.

 - The fraction of the variance that is found at the country-level is called the
   intra-class correlation (ICC) and is equal to the variance of the constant
   divided by the total variance of the null model (constant plus residuals).
 
 - In this example, ICC = .74/(.74+4.36) = 14.5% of total variance is at the
   country level, and the value of the constant is 2.2 times larger than its
   standard error. This confirms the usefulness of a multilevel model.

 - The p-value of the Chi-squared test tests whether the multilevel null model
   is more efficient than its one-level equivalent. The low p-value confirms
   that the two-level model is likely to be more useful here. */

* Adding individual-level predictors.
xtmixed stfhlth agea i.gndr i.health i.hincfel lrscale || cntry: , variance

* Adding country-level predictors.
xtmixed stfhlth agea i.gndr i.health i.hincfel lrscale i.hsys wdi_hec || cntry: , variance
	
* Store the model.
eststo MLm: xtmixed


* =======
* = END =
* =======


* Close log (if opened).
cap log close

* We are done, and we have covered tons of stuff. Thanks for following!
* exit, clear
