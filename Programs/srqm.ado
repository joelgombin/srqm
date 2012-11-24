// --- SRQM --------------------------------------------------------------------

// This file creates the utilities called at startup by the profile.do file of
// the SRQM folder. See the README file of the Programs folder for details.

cap pr drop srqm
program srqm
	syntax anything [, noLOG forced]

	// parse syntax

	tokenize `anything'
	local setup =  ("`1'"=="setup")
	local check =  ("`1'"=="check")
	local fetch =  ("`1'"=="fetch") // now includes an update function
	local clean =  ("`1'"=="clean")
	local folder = ("`2'"=="folder")
	local packages = ("`2'"=="packages")
	local course = ("`2'"=="course")
	local log =    ("`log'"=="")
	local forced = ("`forced'"!="")
	
	// package list

	local install = "lookfor_all fre spineplot tab_chi mkcorr tabout estout leanout gstd01 clarify"

		* catplot ciplot log2do2 outreg2 revrs
		* kountry qog wbopendata
		* tufte lean2 schemes from http://leuven.economists.nl/stata/
	
	// dataset list

	local datasets = "ess2008 gss2010 nhis2009 qog2011 wvs2000"

	// interrupt backup log if any

	if `log' cap log off backlog

	// check syntax

	if `setup' | `check' | `clean' | `fetch' {
		if `log' {
			if "`2'"!="" local x = "-`2'"
			cap qui log using Programs/`1'`x'.log, name(SRQM) replace
			if !_rc local logged 1

			di as txt "Running SRQM utilities..."

			di as inp _n "User profile:"
			di as txt "Date:", as res c(current_date), c(current_time)
			di as txt "Software: Stata", as res c(stata_version)
			di as txt "System:", as res c(os), c(osdtl)
			di as txt "Computer:", as res c(machine_type)
			di as txt "Command:", as res "srqm `1' `2'"

			di as inp _n "Working directory:", as res c(pwd)

	 		cap qui log query SRQM
	 		if !_rc di as inp "Log path:", as res r(filename) _n	
			if _rc di as err "Log creation failed." _n
		
			di as inp "System directories:"
			adopath
		}
	}
	else {
		di as txt "Unrecognized subcommand. See the {browse Programs/README.pdf:README} file for syntax help."
		exit 0
	}

	// =========
	// = SETUP =
	// =========

	if `setup' {

		if `folder' {
			//
			// SRQM LINK
			//
			di _n as inp "Folder settings:"

			//
			// PROFILE LINK
			//
			tempname fh
			di as txt "Writing SRQM link to the Stata application folder:" _n as res "`c(sysdir_stata)'profile.do"
			cap file open fh using "`c(sysdir_stata)'profile.do", write replace
			if _rc == 0 {
				file write fh _n "*! This do-file automatically sets the working directory to the SRQM folder:" _n
				file write fh "*! `c(pwd)'" _n _n
				file write fh "global srqm_wd " _char(34) "`c(pwd)'" _char(34) _n
				file write fh "cap confirm file " _char(34) _char(36) "srqm_wd`c(dirsep)'Programs`c(dirsep)'srqm.ado" _char(34) _n _n
				file write fh "if _rc { // cannot load utilities" _n _tab "noi di as err _n ///" _n
				file write fh _tab _tab _char(34) "ERROR: The SRQM folder is no longer available at its former location:" _char(34) " as txt _n ///" _n
				file write fh _tab _tab _char(34) _char(36) "srqm_wd" _char(34) " _n(2) ///" _n
				file write fh _tab _tab _char(34) "This error occurs when you rename or relocate the SRQM folder." _char(34) " _n ///" _n
				file write fh _tab _tab _char(34) "Use the 'File : Change Working Directory...' menu to manually" _char(34) " _n ///" _n
				file write fh _tab _tab _char(34) "select the SRQM folder, then execute the {stata run profile} command." _char(34) " _n ///" _n
				file write fh _tab _tab _char(34) "For more help, see the README file of the SRQM folder." _char(34) _n
				file write fh _tab "exit -1" _n "}" _n "else {" _n
				file write fh _tab "cap cd " _char(34) _char(36) "srqm_wd" _char(34) _n _n
				file write fh _tab "cap noi run profile" _n
				file write fh _tab "if !_rc noi type profile.do, starbang" _n _n
				file write fh _tab "if _rc | " _char(34) _char(36) "srqm_wd" _char(34) "==" _char(34) _char(34) " { // folder check failed" _n
				file write fh _tab _tab "noi di as txt ///" _n
				file write fh _tab _tab _tab _char(34) "Some essential course material is not available in your working directory." _char(34) " _n(2) ///" _n
				file write fh _tab _tab _tab _char(34) "This error occurs when you modify the folders or files of the SRQM folder." _char(34) " _n ///" _n
				file write fh _tab _tab _tab _char(34) "Restore the SRQM folder from a backup copy or from http://f.briatte.org/srqm" _char(34) " _n ///" _n
				file write fh _tab _tab _tab _char(34) "Then set it as the working directory and execute the {stata run profile} command." _char(34) " _n ///" _n
				file write fh _tab _tab _tab _char(34) "For further help, see the README file of the SRQM folder." _char(34) _n
				file write fh _tab _tab "exit -1" _n
				file write fh _tab "}" _n
				file write fh "}" _n
				file close fh
				di as txt _n "Setting the redirect link to the current working directory:" _n as res c(pwd)
				di as inp ///
					_n "IMPORTANT: do not modify this folder path!", as txt "If you move or rename its elements," ///
					_n "Stata will not find the course material when you open it, and you will have to" ///
					_n "setup your computer again (see the README file of the SRQM folder for help)."
			}
			else {
				//
				// Windows Vista and 7 machines require the user to right-click
				// the application and run it as admin for this bit to work.
				//
				di as err ///
					_n "ERROR: The Stata application folder is not writable on your system." as txt _n(2) ///
					_n "Try again while running Stata with admin privileges. If the problem persists," ///
					_n "you will have to manually select the SRQM folder from the 'File : Change" ///
					_n "Working Directory...' menu and then execute the {stata run profile} command."
					_n "at the beginning of every course session. All apologies to Windows users."
				exit 0
			}
		}	
		else if `packages' {
			//
			// INSTALL PACKAGES
			//
			local i=0
			foreach t of local install {
				local i=`i'+1

				cap which `t'
				
				// tab_chi and tabchi
				if "`t'"=="tab_chi" cap which tabchi

				if _rc==111 | `forced' {
					if "`t'"=="clarify" {
						// note: keep clarify and next at end of install macro
						cap which simqi
						if (_rc==111 | `forced') cap noi net install clarify, from("http://gking.harvard.edu/clarify")
					}
					else if "`t'"=="gstd01" {
						// note: keep the underscore out of install macro
						cap which _gstd01
						if (_rc==111 | `forced') cap noi net install _gstd01, from("http://web.missouri.edu/~kolenikovs/stata")
					}
					else {
						cap noi ssc install `t', replace						
					}
					if _rc==699 {
						// issue: admin privileges required to modify stata.trk
						// workaround: install to personal folder (create if necessary)
						// on Sciences Po computers, path will be c:\ado\personal\
						// iterative (do that for every package that does not work)
						// so probably slow and desperate, but actually works
						local here = c(pwd)
						qui cd "`c(sysdir_plus)'"
						qui cd ..
						cap mkdir personal
						if !_rc noi di as txt "Could not install to the PLUS folder:" ///
							_n "`c(sysdir_plus)'" _n ///
							"Installing to the PERSONAL folder instead:" _n ///
							"`c(pwd)'/personal"
						cap cd personal
						cap sysdir set PLUS "`c(pwd)'" // shouldn't ever fail
						qui cd "`here'"
						// shoot again
						cap qui ssc install `t', replace
					}
				}
				else {
				 // di "`t' is already installed"
				}
				if _rc di as err "Installation failed with error code", _rc
			}
		}
		else {
			//
			// SYSTEM OPTIONS
			//
			di _n as inp "System options:"

			* Memory.
			if c(version) < 12 {
				cap set mem 500m, perm
				if !_rc di as txt "Memory set to 500MB (running older Stata)."
			}
			
			* Screen breaks.
			cap set more off, perm
			if !_rc di as txt "Screen breaks set to", c(more)
			
			* Maximum variables.
			clear all
			cap set maxvar 7500, perm
			if !_rc di as txt "Maximum variables set to", c(maxvar)
			
			* Scrollback buffer size.
			cap set scrollbufsize 500000
			if !_rc di as txt "Scrollback buffer size set to", c(scrollbufsize)
				
			* Software updates.
			cap set update_query off
	        if !_rc di as txt "Software updates set to", c(update_query)

			* Variable abbreviations.
			set varabbrev off, perm
	        if !_rc di as txt "Variable abbreviations set to", c(varabbrev)
			
			* Course themes.
			cap set scheme burd, perm
			if !_rc di as txt "Graphics scheme set to", c(scheme)
		}

	}

	// =========
	// = CHECK =
	// =========

	if `check' {

		if `folder' {
			// 
			// FOLDER INTEGRITY
			//
			di as inp _n "Working directory:" as txt _n "{browse `c(pwd)'}"
			ls, w

			foreach f in  "Datasets" "Replication" {
				cap cd "`f'"
				if _rc {
					di as err _n "ERROR: The `f' folder could not be located in the working directory."
					exit -1
				}
				di as txt _n "{browse `f'}" " folder:" _n c(pwd)
				if "`f'" == "Datasets" {
					// exhaustive check
					foreach d in `datasets' {
						cap confirm file `d'.dta
						if _rc {
							di as txt "Unzipping " as inp "`d'.dta" as txt "..."
							cap unzipfile "`d'", replace
						}
						if _rc==601 {
							di as err _n "ERROR: Neither `d'.dta or `d'.zip", ///
								"could be located" _n "in the `f' folder."
							qui cd ..
							exit -1
						}
					}
					cap noi ls *.dta, w
				}
				if "`f'" == "Replication" cap noi ls *.do, w	
				qui cd ..
			}
		}
		else if `packages' {
			//
			// PACKAGE UPDATES
			//
			di _n as inp "Package updates:"
			
			cap qui ssc hot
			if !_rc cap noi adoupdate, update all
			if _rc di as err "Could not go online to check for updates."
		}
		else if `course' {
			//
			// CHECK COURSE
			//
			di as inp _n "Course demo:"

			gr drop _all
			win man close viewer _all
			clear all
		
			local start = c(current_time)

			if "`3'"=="" local 3 = 1
			if "`4'"=="" local 4 = 12
			forvalues y=`3'/`4' {

				gr drop _all
				win man close viewer _all
				clear all

				// to test the package installation loops
				// and/or get plots in s2color default scheme:
				// srqm clean packages
				// set scheme s2color

				do Replication/week`y'.do
				repl week`y'
			
			gr drop _all
			win man close viewer _all
			clear all

			di as txt _n "Done! Routine launched at `start' and finished at", c(current_time) "."
			}
			else {
				di as txt "Updating `3'"
				strpos("`3'",".do") > 0
			}
		}
		else if "`3'" != "" {
			//
			// PLAIN CHECK
			//
			query
			ado dir
		}
		
	}

	// =========
	// = CLEAN =
	// =========

	if `clean' {

		if `folder' {
			//
			// UNLINK COURSE
			//
			di as inp _n "Remove SRQM link:"

			cap rm "`c(sysdir_stata)'profile.do"
			if _rc ==0 di as txt _n "Successfully removed", "`c(sysdir_stata)'profile.do." _n "Farewell, enjoy life and Stata!"
			if _rc !=0 di as err _n "Nothing to remove at", "`c(sysdir_stata)'profile.do." _n "You have already left. Be well!"
			cd "`c(sysdir_stata)'" // to avoid profile.do re-setting up on Macs
		}
		else if `packages' {
			//
			// PACKAGE ANNIHILATION
			//
			di as inp _n "Uninstalling packages..."
	
			foreach t of local install {
				cap noi ssc uninstall `t'
			}
			
			di as txt _n "Uninstalling a few more things..."
			cap ssc uninstall clarify
			cap ssc uninstall _gstd01

			set scheme s2color // set scheme back to default
		}
		else {
			//
			// CLEANUP WORKFILES
			//
			di as inp _n "Cleaning work files..." // requires X Window System

			local expr = "week*.log" // pretty destructive
			cap !rm `expr'

			local expr = "Programs/*.log"
			cap !rm `expr'

			local expr = "Replication/*.log"
			cap !rm `expr'

			forval i=1/12 {
				// erase replication folders created by repl
				local expr = "Replication/week`i'"
				cap !rm -R `expr'
			}
		}
	}

	// =========
	// = FETCH =
	// =========

	if `fetch' {

		cap cd "$srqm_wd"

		cap qui net
		if _rc == 631 {
			di as err "You do not seem to be online. Check your Internet connection."
			exit 0
		}

		local bk = strtoname("`2' backup `c(current_date)'")
		
		if "`3'" == "dofile" {
			cap qui copy "Replication/`2'.do" "Replication/`bk'.do", replace
			if !_rc di as txt "Do-file `2' updated."
			cap qui copy "http://briatte.org/srqm-updates/`2'.do" "Replication/`2'.do", replace
			if !_rc di as txt "Do-file `2' updated."
		}
		
		if "`3'" == "slides" {
			cap qui copy "Course/`2'.pdf" "Course/`bk'.pdf", replace
			cap qui copy "http://briatte.org/srqm-updates/`2'.pdf" "Course/`2'.pdf", replace
			if !_rc di as txt "Slides `2' updated."
		}
	}

	if `log' {
		di _n as inp "Done.", as txt "Have a nice day."
		cap log close SRQM
		cap log on backlog
	}

end
