* This do-file produces the teaching datasets distributed as part of the course.
* Use only to repair your Teaching Pack if you accidentally overwrite the files.
* Note that the code requires running Stata with admin privileges.

cap pr drop tdprep
pr tdprep
	syntax, lab(string asis) zip(name)
	// Teaching annotation.
	notes drop _dta
	la data "`lab'"
	note _dta: `lab'
	note _dta: Teaching dataset slightly altered from source.
	note _dta: Please do not redistribute.
	note _dta: This version: TS
	// Compress and uncompress.
	cd data
	saveold `zip', replace
	cap rm `zip'.zip
	zipfile `zip'*, saving(`zip'.zip, replace)
	unzipfile `zip'.zip, replace
	ls `zip'*
	cd ..
end

// -------------------------------------------------------------- ESS 2008 -----

// Get the data (downloaded from the website).

use data/ess2008.dta, clear

drop if essround != 4 // used in the initial subset from the cumulative dataset

foreach v of varlist * {
    qui count if !mi(`v')
    if r(N)==0 {
        di "Dropping: " "`v'"
        drop `v'
    }
}

tdprep, lab(European Social Survey 2008) zip(ess2008)

// -------------------------------------------------------------- GSS 2010 -----

// Get the data.

cap use "~/Documents/Research/Data/GSS/gss7210_r2b.dta"

// Download the 1972-2010 cumulative file if needed.
if _rc==601 {
	qui conf f gss7210_r2b.dta
	if _rc==601 {
		copy "http://publicdata.norc.org/GSS/DOCUMENTS/OTHR/GSS_stata.zip" temp.zip, replace
		unzipfile temp.zip
	}
	use gss7210_r2b.dta, clear
}

// The teaching dataset uses years 2000-2010.
drop if year < 2000

// Remove empty variables.

foreach v of varlist * {
qui su `v'
if r(N)==0 {
	local l: var l `v'
    di as txt "Removing","`v':","`l'"
    drop `v'
    }
}

// Remove low-N variables.

foreach v of varlist * {
	qui count if !mi(`v')
	if r(N) < 500 {
	local l: var l `v'
	di as txt "Removing","`v':","`l'"
    drop `v'
    }
}

// Export mock codebook.

log using data/gss2010_codebook.log, name(gss2010) replace
d
codebook
log close gss2010

tdprep, lab(General Social Survey 2000-2010) zip(gss2010)

// ------------------------------------------------------------- NHIS 2009 -----

// Get the data (downloaded from the website).

use data/nhis2009.dta, clear

// Export mock codebook.

log using data/nhis2009_codebook.log, name(nhis2009) replace
d
codebook
log close nhis2009

tdprep, lab(National Health Interview Survey 2009) zip(nhis2009)

// -------------------------------------------------------------- QOG 2011 -----

// Get the data.

use "http://www.qogdata.pol.gu.se/data/qog_std_cs.dta", clear

// Get the codebook.

copy "http://www.qog.pol.gu.se/digitalAssets/1390/1390246_the-qog-codebook--c-.pdf" data/qog2011_codebook.pdf, replace

tdprep, lab(Quality of Government 2011) zip(qog2011)

// -------------------------------------------------------------- WVS 2000 -----

// Get the data.

copy "http://www.asep-sa.org/wvs/wvs2000/wvs2000_v20090914_stata.zip" temp.zip, replace

unzipfile temp.zip
use wvs2000_v20090914.dta, clear

rm temp.zip
rm wvs2000_v20090914.dta

// Capitalize country names.
// Thanks to William A. Huber: http://stackoverflow.com/q/12591056/635806

local sLabelName: value l v2
di "`sLabelName'"

qui levelsof v2, local(xValues)
foreach x of local xValues {
    local sLabel: label (v2) `x', strict
    local sLabelNew =proper("`sLabel'")
    di as txt "`x': `sLabel' ==> `sLabelNew'"
    label define `sLabelName' `x' "`sLabelNew'", modify
}

// Export mock codebook.

log using data/wvs2000_codebook.log, name(wvs2000) replace
fre v2
d
codebook
log close wvs2000

tdprep, lab(World Values Survey 2000) zip(wvs2000)

// ttyl
