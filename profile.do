* What: SRQM profile
* Who:  F. Briatte
* When: 2012-10-07

// Backup log.
cap log using "Replication/backup.log", name("backlog") replace
if _rc==0 {
	noi di as inp _n "Backup log:" _n as txt r(filename)
}
else if _rc==604 {
	noi di as err "  The backup log was already open. Carry on."
}
else {
	noi di as err "  The backup log returned an error. Too bad."
}

// Load utilities.
cap adopath + "`c(pwd)'/Programs"
cap noi srqm check folder, nolog
if _rc != 0 {
	noi di as err "Please run this course from the original SRQM 'Teaching Pack' folder."
	exit -1
}

// Check settings.
if c(update_query)=="on" | c(more)=="on" {
	noi di as txt _n ///
		"  (It looks like you need to adjust some of your Stata system options.)"
	cap noi srqm setup, nolog
}

// Check packages.
cap which fre
if _rc==111 {
	noi di as txt _n ///
		"  (It looks like you need to install the additional packages for the course.)"
	cap noi srqm setup packages, nolog
}

// Check redirect.
if "$srqm_wd" != c(pwd) {
	noi di as txt _n ///
		"  (It looks like you need to (re)set the link to the SRQM working directory.)"
	cap noi srqm setup folder, nolog
}

// All set.
noi di as inp _n "Welcome!"
noi di as txt "  You are running Stata with the SRQM profile."

// Enjoy.
