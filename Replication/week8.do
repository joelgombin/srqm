
/* ------------------------------------------ SRQM Session 8 -------------------

   F. Briatte and I. Petev

 - TOPIC:  Fertility and Education

 - DATA:   Quality of Government (2011)
 
   Last updated 2012-11-13.

----------------------------------------------------------------------------- */


* Install required commands.
foreach p in mkcorr {
	cap which `p'
	if _rc==111 cap noi ssc install `p' // install from online if missing
}

* Log results.
cap log using "Replication/week8.log", replace


* ===========
* = DATASET =
* ===========


* Data: Quality of Government (2011).
use "Datasets/qog2011.dta", clear

* Rename variables to short handles.
ren (wdi_fr bl_asyt25 undp_hdi ti_cpi gid_fgm) (births schooling hdi corruption femgov)

* Compute GDP per capita.
gen gdpc = unna_gdp/unna_pop
la var gdpc "Real GDP per capita (constant USD)"

* Geographical regions.
gen region:region = ht_region //! new: moved to this file
la var region "Geographical region"

* Recode to less, shorter labels.
recode region (6/10=6)
la de region 1 "E. Europe and PSU" 2 "Lat. America" ///
	3 "N. Africa and M. East" 4 "Sub-Sah. Africa" ///
	5 "W. Europe and N. America" 6 "Asia, Pacific and Carribean" ///
	, replace


* Finalized sample
* ----------------

* Have a quick look.
codebook births schooling gdpc hdi corruption femgov region, c

* Check missing values.
misstable pat births schooling gdpc hdi corruption femgov region ccodewb

* You would usually delete incomplete observations at that stage, but we will
* exceptionally keep them here to illustrate how pairwise correlation works.


* ===============
* = CORRELATION =
* ===============


* IV: Fertility Rates and Schooling Years
* ---------------------------------------

scatter births schooling

pwcorr births schooling, obs sig


* IV: Schooling Years and (Log) Gross Domestic Product
* ----------------------------------------------------

sc gdpc schooling

* A first look at the scatterplot shows no clear linear pattern, but we know
* from a previous session that the logarithmic variable transformation can be
* used to visualize exponential relationships differently. Consequently, we
* try to visualise the same variables with a logarithmic scale for GDP per capita.
sc gdpc schooling, ysc(log)

* In this classical case, log units are more informative than metric ones to
* identify the relationship between the dependent and independent variables.
gen log_gdpc = ln(gdpc)
la var log_gdpc "Real GDP per capita (log)"

* Verify the transformation.
sc log_gdpc schooling

* Obtain summary statistics.
su log_gdpc schooling

* Visual inspection of the relationship within the mean-mean quadrants.
sc log_gdpc schooling, yline(7.5) xline(6) ///
	name(log_gdpc_schooling, replace)

* Verify inspection computationally.
pwcorr gdpc log_gdpc schooling, obs sig


* IV: Corruption and Human Development
* ------------------------------------

* Before graphing the variables, we need to pass a few graph options, because
* the Corruption Perception Index is reverse-coded (0 marks high corruption,
* and 10 marks very low corruption). To enhance visual interpretation, we
* therefore use an inverted axis scale, and add horizontal axis labels to it.
sc corruption hdi, ysc(rev) ///
	xla(0 "Low" 1 "High") yla(0 "Highly corrupt" 10 "Lowly corrupt", angle(h)) ///
	name(corruption_hdi, replace)

* The pattern that appears graphically is not linear: corruption is stationary
* for low to medium values of HDI, and then rapidly drops towards high values.
* Given its shape, this relationship is thus more likely to be quadratic, i.e.
* of the form y = x^n where y is corruption, x is HDI and n > 1 is a power.
* If the correlation coefficient is statistically significant, we might treat
* the relationship between corruption and HDI as approximately linear, but we
* will lose some of the information observed visually by doing so.
pwcorr corruption hdi, obs sig


* IV: Female Government Ministers and Corruption
* ----------------------------------------------

* Obtain summary statistics.
su femgov corruption

* Visual inspection of the relationship within the mean-mean quadrants.
sc femgov corruption, yline(15) xline(4) ///
	name(femgov_corruption, replace)

* No clear pattern emerges from the scatterplot above. Never force a pattern
* onto the data: relationships should be apparent, not constructed. If there is
* no straightforward relationship, disregard it. Identically, never include a
* graph in your work if the relationship that it intends to show will not
* strike the reader between the eyes (i.e. run an intra-ocular trauma test).
* Inconclusive visual inspection can come with significant correlations, as is
* the case here if you actually compute the coefficient, but visual inspection
* and theoretical elaboration provide no substantive justification for it.


* Correlation and scatterplot matrixes
* ------------------------------------

* Start with visual inspection of the data organized as a scatterplot matrix.
* A scatterplot matrix contains all possible bivariate relationships between
* any number of variables. Building a matrix of your DV and IVs allows to spot
* relationships between IVs, which will be useful later on in your analysis.
* Note that the example below shows the untransformed measure of GDP per capita.
gr mat births schooling log_gdpc corruption femgov, ///
	name(gr_matrix, replace)

* You could also look at a sparser version of the matrix that shows only half of
* all plots for a subset of geographical regions.
gr mat births schooling log_gdpc corruption femgov if inlist(region,4,5), half ///
	name(gr_matrix, replace)

* The most practical way to consider all possible correlations in a list of
* predictors (or independent variables) is to build a correlation matrix out
* of their respective pairwise correlations. "Pair-wise" indicates that the
* correlation coefficient uses only pairs of valid, nonmissing observations,
* and disregards all observations where any of the variables is missing.
pwcorr births schooling log_gdpc corruption femgov

* The most common way to indicate statistically significant correlations in
* a correlation matrix is to use asterisks (stars) to mark them when their
* p-value is below the level of statistical significance.
pwcorr births schooling log_gdpc corruption femgov, star(.05)

* For explorative purposes, another option can be used to print out only the
* statistically significant correlations, which comes in handy especially in
* very large matrixes with majorily insignificant correlation coefficients.
pwcorr births schooling log_gdpc corruption femgov, print(.05)

* Export a correlation matrix.
mkcorr births schooling gdpc corruption femgov, ///
	lab num sig log("week8_correlations.txt") replace


* Marker labels for scatterplots
* ------------------------------

* Stata requires passing a lot of options to produce informative graphs. If you
* are using a set of consistent options on several graphs, you can store these
* in a global macro and apply them by calling the macro with a dollar sign ($).
* The following global macro is a list of graph options to make scatterplots
* more informative by showing country codes instead of anonymous data points:
global ccode "ms(i) mlabpos(0) mlab(ccodewb) legend(off)"

* The options contained in the global macro make the marker symbol invisible,
* then center the marker label and fill it with the ccodewb variable (holding
* country codes from the World Bank) in replacement of the usual dot markers.
* In the following plots, passing the $ccode option will result in actually
* passing these graph options, stored in the ccode ("country codes") macro.
* Note that this is a hack, and that you would not normally fiddle with global
* macros if you were programming Stata at a more advanced level: you would use
* local macros, which are more complex in usage and therefore avoided here.

* Improve previous example.
sc births schooling, $ccode ///
	name(fert_edu1, replace)

* Add a color difference to Western states by overlaying multiple scatterplots.
sc births schooling, $ccode || ///
	sc births schooling if region==5, $ccode ///
	name(fert_edu2, replace)

* Add a tone and color difference to subsaharan African states (more options!).
sc births schooling, mlabc(gs10) $ccode || ///
	sc births schooling if region==4, $ccode ///
	name(fert_edu3, replace)

* There are binders full of Stata graph options like these. Have a look at the
* help pages for two-way graphs (h tw) for a list that applies to scatterplots.


* ========
* = EXIT =
* ========


* Close log (if opened).
cap log close

* We are done. Just quit the application, have a nice week, and see you soon :)
* exit, clear
