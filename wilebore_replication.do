***************************************************************
* Unconditional Transfers and Tropical Forest Conservation.
* Evidence from a Randomized Control Trial in Sierra Leone 
* Authors: Wilebore, Voors, Bulte, Coomes and Kontoleon
* AMERICAN JOURNAL OF AGRICULTURAL ECONOMICS
* Date: 5 November 2018
* Contents
	* Table 1. Description of land cover classes used in RapidEye classification 
	* Table 2. Village characteristics at baseline and balance test between experimental groups 
	* Table 3. Village - Polygon level models
	* Table 4. Pixel level models
	* Table 5. Stated farming and labour intentions from treatment group.
	* Table 6. Table 6. Stated harvest amount at EL (household level)
	* Table A1. reanalyse dropping block 5 (10 of 11 missing data drom here) -- village level
	* Table A2. reanalyse dropping block 5 (10 of 11 missing data drom here) -- pixel level
	* Table A3. Robustness checks on impact on land use- proportions and log transformation of outcome variable
	* Table A4. Reasons for farming more/less/same due to aid
	* Table A5. Reasons for working more/less/same due to aid
	* Table A6. Other outcome measures at EL (household level)
	* Table A7. Heterogeneous treatment effects - village level 
* The do-file creates all tables used in the paper. 
* Set up folder structure: 
	* create a main folder
	* place "landuse.dta", "landusepixel.dta" and "landusehh.dta" in main folder 
	* within main folder for output create a folder called "tables"
	* set your directory to "main folder"
		*cd "[your path]/analysis/"

		set more off

***************************************************************
	
****************************
	* Load data and manipulations, resave
****************************
	 
	use "landuse.dta", clear	

**** 
* impute missing values at treatment arm mean
* standardize controls
****

	* impute missing values at treatment group mean
	foreach var in  bl_is3__age bl_is4__sex bl_is5__roof bl_is9__bushels bl_is20_ch_good ///
		standforchief vs_yearsinpower vs_12_edulvl vs_13_ldfrm vs_14_nrwife ///
		dgrnpkm droadkm dlibkm vs_24_draught vs_25_crpdis  ///
		area villarea_all_vegetation_2011 villarea_mature_forest_2011 slope altitude ///
		pop vs_157_nr_vilbyl vs_164_bylwlog 	vs_165_bylwhunt 	vs_166_bylwming vs_188_logcomsale vs_197_hntvil	vs_211_mininfrst {
			su `var' if Twfg==1
			replace `var' = r(mean) if `var' == . & Twfg==1
			su `var' if Twfg==0
			replace `var' = r(mean) if `var' ==. & Twfg==0
			}	
	
	* standardize controls	
	foreach var in bl_is20_ch_good bl_is3__age droadkm {
			egen S`var'= std(`var')
			}

	la var Sbl_is3__age "Average age (std)"
	la var Sbl_is20_ch_good "Chief is good (std)"
	la var Sdroadkm "Distance to road (std)"
	
	save "landuse1.dta", replace	


***********************
*** Table 1. Description of landcover classes used in RapidEye classification 
***********************
* Text

***********************
*** Table 2. Village characteristics at baseline and balance test between experimental groups 
***********************

	use "landuse1.dta", clear	
		
	
	* set up table and run comparison
	foreach var in bl_is3__age bl_is4__sex bl_is5__roof bl_is9__bushels bl_is20_ch_good ///
		standforchief vs_yearsinpower vs_12_edulvl vs_13_ldfrm vs_14_nrwife ///
		dgrnpkm droadkm dlibkm vs_24_draught vs_25_crpdis ///
		area villarea_all_vegetation_2011 villarea_mature_forest_2011 slope altitude ///
		pop vs_157_nr_vilbyl vs_164_bylwlog 	vs_165_bylwhunt 	vs_166_bylwming vs_188_logcomsale vs_197_hntvil	vs_211_mininfrst {
		gen te_`var'=.
		gen se_`var'=.
		gen p_`var'=.
		gen N_`var'=.
		gen cm_`var'=.
		reg `var' Twfg i.chf, robust
		replace te_`var'=_b[Twfg]
		replace se_`var'=_se[Twfg]
		replace p_`var'=2*(1-normal(abs(te_`var'/ se_`var')))
		replace N_`var'=e(N)
		ttest `var', by(Twfg)
		replace cm_`var'=r(mu_1)
		}
	gen n=_n
	keep if n==1

	* reshape and tidy
	keep n te_* se_* p_* N_* cm_*
	reshape long te_ se_ p_ N_ cm_, i(n) j(x) string
	drop n 
	order  x cm_ te_ se_ p_ N_
	format  %9.2f te se cm

	rename x variable
	rename  te_ T_C_base
	rename  se_ T_C_stderr_base
	rename  cm_ mean_control_base
	rename  p_ pvalue
	rename  N_ N_base

	* add labels
	generate str1 label = "."
	replace label = "Average age of household heads (years)" if variable == "bl_is3__age"
	replace label = "% male" if variable == "bl_is4__sex"
	replace label = "% of households with tin roof"	if variable == "bl_is5__roof"
	replace label = "Average amount of rice harvested (in bushels)"	if variable == "bl_is9__bushels"
	replace label = "Chief quality index. Village average to household question “Is your chief a good chief? (1,0)"	if variable == "bl_is20_ch_good"
	replace label = "Altitude (m above sea level)"	if variable == "altitude"
	replace label = "Average slope (degrees)"	if variable == "slope"
	replace label = "Vegetation cover in 2011 (hectares)"	if variable == "villarea_all_vegetation_2011"
	replace label = "Mature forest cover in 2011 (hectares)"	if variable == "villarea_mature_forest_2011"
	replace label = "Total village-polygon area (hectares)"	if variable == "area"
	replace label = "Number of families that can stand for chief"	if variable == "standforchief"
	replace label = "Chief years in power"	if variable == "vs_yearsinpower"
	replace label = "Education level of chief (years of formal education)"	if variable == "vs_12_edulvl"
	replace label = "Size of land farmed by chief (hectares)"	if variable == "vs_13_ldfrm"
	replace label = "Number of wives chief has"	if variable == "vs_14_nrwife"
	replace label = "Number of village forest management bylaws"	if variable == "vs_157_nr_vilbyl"
	replace label = "Bylaw for logging (=1 if yes, =0 if no)"	if variable == "vs_164_bylwlog"
	replace label = "Bylaw for hunting (=1 if yes, =0 if no)"	if variable == "vs_165_bylwhunt"
	replace label = "Bylaw for mining (=1 if yes, =0 if no)"	if variable == "vs_166_bylwming"
	replace label = "Distance to Gola Rainforest National Park (km)"	if variable == "dgrnpkm"
	replace label = "Distance to Liberia (km)"	if variable == "dlibkm"
	replace label = "Distance to nearest major road (km)"	if variable == "droadkm"
	replace label = "Population in 2010"	if variable == "pop"
	replace label = "Villagers log commercially outside GRNP (=1 if yes, =0 if no)"	if variable == "vs_188_logcomsale"
	replace label = "Villagers hunt outside GRNP (=1 if yes, =0 if no)"	if variable == "vs_197_hntvil"
	replace label = "Villagers mine in forest outside GRNP? (=1 if yes, =0 if no)"	if variable == "vs_211_mininfrst"
	replace label = "Village experienced drought within five years from baseline (=1 yes)"	if variable == "vs_24_draught"
	replace label = "Village experienced crop disease within five years from baseline (=1 yes)"	if variable == "vs_25_crpdis"

	* output
	*** Table 2 (outcomes in alphabetical order) ***
	order variable label mean_control_base T_C_base T_C_stderr_base pvalue N_base
	list variable label mean_control_base T_C_base T_C_stderr_base pvalue N_base
	
	outsheet using "tables/Table2", replace
	* notes
		** Note: Number of villages in control n = 17, treatment = 62, column (5) presents robust p-values from OLS regression controlling for randomization blocks (chiefdom). Missing values imputed at treatment arm mean.

***********************
****  Table 3 Village - Polygon level models
***********************

use "landuse1.dta", clear

	global BL_controls "Sbl_is20_ch_good Sbl_is3__age Sdroadkm "

	eststo clear
	eststo: xi:reg gb_all_v Twfg i.chf,  robust
	eststo: xi:reg gb_all_v Twfg i.chf $BL_controls, robust

	eststo: xi:reg gb_nm_v Twfg i.chf, robust
	eststo: xi:reg gb_nm_v Twfg i.chf $BL_controls,  robust
	
	eststo: xi:reg gb_m_v Twfg i.chf,  robust
	eststo: xi:reg gb_m_v Twfg i.chf $BL_controls, robust

	esttab using "tables/Table3.rtf", replace star(* 0.10 ** 0.05 *** 0.01) b(3) se ar2 label title("Table 3. Village Level Analysis of Program Impacts on Land Use Change") ///
	stats(N , fmt(a2) labels("Observations")) ///
	nonotes keep (_cons Twfg $BL_controls ) compress nogaps ///
		addnotes("Note: OLS regressions including randomization blocks (chiefdom). The dependent variables are the hectares that transition from any type of vegetated land in 2011 to ‘bare’ soil in 2013; ‘farmbush’ land to ‘bare’ soil and ‘mature forest’ land to ‘bare’ soil. Missing values for controls are imputed at treatment arm mean and standardized. Standard errors in parentheses. * p < 0.10, ** p < 0.05, *** p < 0.01") 


******************************************
****  Table 4 Pixel level models
******************************************

use "landusepixel.dta", clear

	* impute missing values at treatment group mean
	foreach var in bl_is20_ch_good bl_is3__age dist_road_pixelkm area near_grnp_pixelkm slope_pixel {
			su `var' if Twfg==1
			replace `var' = r(mean) if `var' == . & Twfg==1
			su `var' if Twfg==0
			replace `var' = r(mean) if `var' ==. & Twfg==0
			}	
	
	* standardize controls	
	foreach var in bl_is20_ch_good bl_is3__age dist_road_pixelkm area near_grnp_pixelkm slope_pixel {
			egen S`var'= std(`var')
			}

	la var Sbl_is3__age "Average age (std)"
	la var Sbl_is20_ch_good "Chief is good (std)"	
	la var Sdist_road_pixelkm "Distance to road (std)"
	la var Sarea "Total village land area (std)"
	la var Snear_grnp_pixelkm "Distance to GRNP (std)"
	la var Sslope_pixel "Slope"
		
save "landusepixel1.dta", replace

	*set controls global 
	global BL_controls "Sbl_is20_ch_good Sbl_is3__age Sdist_road_pixelkm"
	global Other_controls "Snear_grnp_pixelkm Sslope_pixel"

	* dep var any cleared (green to brown, farmbush to bare = 102. Forest to bare = 103) == gb_all 
	* dep var non forest (non mature green to brown) == gb_nm
	* dep var forest (mature green to brown) ==  gb_m

	eststo clear
	eststo: xi:reg gb_all Twfg i.chf,  cluster(vcode11)
	eststo: xi:reg gb_all Twfg i.chf Sarea,  cluster(vcode11)
	eststo: xi:reg gb_all Twfg i.chf $BL_controls $Other_controls Sarea, cluster(vcode11)

	eststo: xi:reg gb_nm Twfg i.chf,  cluster(vcode11)
	eststo: xi:reg gb_nm Twfg i.chf Sarea,  cluster(vcode11)
	eststo: xi:reg gb_nm Twfg i.chf $BL_controls $Other_controls Sarea, cluster(vcode11)

	eststo: xi:reg gb_m Twfg i.chf,  cluster(vcode11)
	eststo: xi:reg gb_m Twfg i.chf 	Sarea,  cluster(vcode11)
	eststo: xi:reg gb_m Twfg i.chf $BL_controls $Other_controls Sarea, cluster(vcode11)	

	esttab using "tables/Table4.rtf", replace star(* 0.10 ** 0.05 *** 0.01) b(3) se ar2 label title("Table 4. Pixel level analysis of programme impacts on land use change") ///
	stats(N N_clust, fmt(a2) labels( "Observations" "# Clusters")) ///
	nonotes keep (_cons Twfg $BL_controls $Other_controls  Sarea) compress nogaps ///
		addnotes("Note: Regressions include chiefdom level fixed effects. Dependent variable are pixels that transition from any type of vegetated land in 2011 to ‘bare’ soil in 2013; ‘farmbush’ land to ‘bare’ soil and ‘mature forest’ land to ‘bare’ soil. Missing values for controls are imputed at treatment arm mean and standardized. Standard errors in parentheses clustered at village level. * p < 0.10, ** p < 0.05, *** p < 0.01") 
				

***************************************	
*Table 5. Stated farming and labour intentions from treatment group.
***************************************
use "landusehh.dta", clear

	* Table
	 ta is_91_frmbhvr 
	 ta is_93_wrkbhvr 
	*cut and paste into text

***************************************
*Table 6. Stated harvest amount at EL (household level)
***************************************
	
	* impute missing values at treatment group mean
	foreach var in  bl_is20_ch_good bl_is3__age droadkm bl_is9__bushels {
			su `var' if Twfg==1
			replace `var' = r(mean) if `var' == . & Twfg==1
			su `var' if Twfg==0
			replace `var' = r(mean) if `var' ==. & Twfg==0
			}	

	* standardize controls	
	foreach var in bl_is20_ch_good bl_is3__age droadkm bl_is9__bushels {
			egen S`var'= std(`var')
			}

	la var Sbl_is3__age "Average age (std)"
	la var Sbl_is20_ch_good "Good Chief (std)"
	la var Sdroadkm "Distance to road (std)"

	*set controls global 
	global BL_controls "Sbl_is20_ch_good Sbl_is3__age Sdroadkm"

	* gen interaction term
	gen tis9__bushels = Twfg*bl_is9__bushels
	la var tis9__bushels "T*Bushels harvested at BL"
	eststo clear

	*Table 6
	eststo: xi:reg is_11_bshlhrvst Twfg tis9__bushels  bl_is9__bushels  i.chf, cluster(vcode11)
	eststo: xi:reg is_11_bshlhrvst Twfg tis9__bushels  bl_is9__bushels  i.chf $BL_controls, cluster(vcode11)
			
	esttab using "tables/Table6.rtf", replace star(* 0.10 ** 0.05 *** 0.01) b(3) se ar2 label title("Table 6. Stated harvest amount at EL") ///
		nonotes keep (_cons Twfg tis9__bushels  bl_is9__bushels  $BL_controls ) compress nogaps ///
			addnotes("Note: Regressions include chiefdom level fixed effects. Missing values for controls are imputed at treatment arm mean and standardized. Standard errors are clustered by village. The dependent variables is bushels of rice harvested at end line. Standard errors in parentheses. * p < 0.10, ** p < 0.05, *** p < 0.01") 
 

***********************************************************************************************************************
											**** CLAIMS IN TEXT ******
***********************************************************************************************************************

* set of claims using AHTS
* data source: MAFFS, SSL, and IPA-SL. 2012. “Agricultural Household Tracking Survey, Sierra Leone, V2.” Harvard Dataverse. DOI hdl/1902.1/18577.

* source 1: AHTS_Comm_SINGLE.tab
* convert to csv and import
	
	* subset to gola districts
		* Gola districts = Kailahun (code = 11, 82 HH); Kenema (code = 12, 91 HH); Pujehun (code = 34, 58 HH). Total = 231 HH. 
	gen gola_district = 1 if district_code== 11| district_code== 12| district_code== 34
	
	foreach var in g1 g3a g4e g5e g6e g7e g8e g9e{
		replace `var' = "." if `var' == "NA"
		destring `var', replace
		}
		
 * "... most households (80%) in eastern Sierra Leone hire external labor at some point in the agricultural cycle,"

	ta g1 if gola_district == 1
	
 * "and for all agricultural activities more than half of households reported a shortage of labor." 

	 ** At any time in the past 12 months, was there a shortage of people to be hired to perform this task for this price? ---------
	foreach var in g4e g5e g6e g7e g8e g9e{
		recode `var'  (2=0)
		}
	gen shortage_labour = 0 
	replace shortage_labour = g4e+g5e+g6e+g7e+g8e+g9e
	
	ta shortage_labour if gola_district == 1
		la var shortage_labour "Labour shortage by agric activity"
		
		* or all agricultural == 6

 * "The mean reported wage for general agricultural labor in the AHTS for eastern Sierra Leone is 6822 Leones per day (1.7 USD/day), "including the value of in-kind payments and meals."

	** G3a What is the standard wage per person per day to hire people for one day of general agricultural labor? (cost per person per day, including estimated value of in-kind payments) ----------
	sum g3a if gola_district == 1

* source 2: HTS_HH_F10_UPLAND_LABOUR_ACTIVITY.tab
* convert to csv and import

 * "for upland farms, approximately 1/3rd of annual labor requirements on farms in Sierra Leone come from hired labor, 
 * "with the rest from reciprocal labor agreements within the community, and from household labor."

	* section F10 (Upland Labour Farm)
	foreach var in f10d_a f10d_b f10f_a f10f_b f10g_a f10g_b {
		replace `var' = "." if `var' == "NA"
		destring `var', replace
		}
		
	gen HH_labour = f10d_a * f10d_b
	gen exchange_labour = f10f_a*f10f_b
	gen hired_labour = f10g_a*f10g_b
	gen all_lab = HH_labour + exchange_labour + hired_labour
	gen prop_hired = hired_labour/all_lab

	la var prop_hired "Hired Labour"
	su prop_hired 
 
* other claims
 
 * "The total value of payments to treatment villages therefore varied between villages, dependent on the number of households in the village (mean household number in our study villages was just below 40). "
 		
	use "landuse1.dta", clear

	su hhlds
	
*  The base model suggests that on average 19.1 ha more land is cleared in treated villages than in controls, (approx. 3.5% of the mean area size of the typical village in our study). 

	su area
	di 19/r(mean) 
	

***********************************************************************************************************************
											**** ANNEX ******
***********************************************************************************************************************


***************************************	 
* Table A1. reanalyse dropping block 5 (10 of 11 missing data drom here) -- village level
***************************************
		
	use "landuse1.dta", clear

	global BL_controls "Sbl_is20_ch_good Sbl_is3__age Sdroadkm "

	eststo clear
	eststo: xi:reg gb_all_v Twfg i.chf if chf!=5,  cluster(vcode11)
	eststo: xi:reg gb_all_v Twfg i.chf $BL_controls if chf!=5 ,  cluster(vcode11)

	eststo: xi:reg gb_nm_v Twfg i.chf if chf!=5,  cluster(vcode11)
	eststo: xi:reg gb_nm_v Twfg i.chf $BL_controls if chf!=5,  cluster(vcode11)

	eststo: xi:reg gb_m_v Twfg i.chf if chf!=5,  cluster(vcode11)
	eststo: xi:reg gb_m_v Twfg i.chf $BL_controls if chf!=5,  cluster(vcode11)

	esttab using "tables/TableA1.rtf", replace star(* 0.10 ** 0.05 *** 0.01) b(3) se ar2 label title("Table A1. Program Impact on land change village level- Dropping Block 5") ///
	stats(N , fmt(a2) labels("# Clusters")) ///
	nonotes keep (_cons Twfg $BL_controls  ) compress nogaps ///
		addnotes("Note: Table A1 provides a village level robustness analysis exploring the impact of dropping the block with most missing data; results remain unaltered. OLS regressions including chiefdom level fixed effects. The dependent variables are hectares that transition from any type of vegetation in 2011 to bare ground in 2013; farmbush to bare and mature forest to bare. Standard errors in parentheses. * p < 0.10, ** p < 0.05, *** p < 0.01. ") 

***************************************
* Table A2. reanalyse dropping block 5 (10 of 11 missing data drom here) -- pixel level
***************************************

use "landusepixel1.dta", clear

	*set controls global 
	global BL_controls "Sbl_is20_ch_good Sbl_is3__age Sdist_road_pixelkm "
	global Other_controls "Snear_grnp_pixelkm Sslope_pixel"

	* dep var any cleared (green to brown, farmbush to bare = 102. Forest to bare = 103) == gb_all 
	* dep var non forest (non mature green to brown) == gb_nm
	* dep var forest (mature green to brown) ==  gb_m

	eststo clear
	eststo: xi:reg gb_all Twfg i.chf if chf!=5,  cluster(vcode11)
	eststo: xi:reg gb_all Twfg i.chf Sarea if chf!=5,  cluster(vcode11)
	eststo: xi:reg gb_all Twfg i.chf $BL_controls $Other_controls Sarea if chf!=5,  cluster(vcode11)

	eststo: xi:reg gb_nm Twfg i.chf if chf!=5,  cluster(vcode11)
	eststo: xi:reg gb_nm Twfg i.chf Sarea if chf!=5,  cluster(vcode11)
	eststo: xi:reg gb_nm Twfg i.chf $BL_controls $Other_controls Sarea if chf!=5,  cluster(vcode11)

	eststo: xi:reg gb_m Twfg i.chf if chf!=5,  cluster(vcode11)
	eststo: xi:reg gb_m Twfg i.chf 	Sarea if chf!=5,  cluster(vcode11)
	eststo: xi:reg gb_m Twfg i.chf $BL_controls $Other_controls Sarea if chf!=5,  cluster(vcode11)	

	esttab using "tables/TableA2.rtf", replace star(* 0.10 ** 0.05 *** 0.01) b(3) se ar2 label title("Table A2. Program Impact on land change pixel level - Dropping Block 5") ///
	stats(N N_clust, fmt(a2) labels( "Observations" "# Clusters")) ///
	nonotes keep (_cons Twfg $BL_controls $Other_controls  Sarea) compress nogaps ///
		addnotes("Note: Table A2 provides a pixel level robustness analysis exploring the impact of dropping the block with most missing data; results remain unaltered. Regressions include chiefdom level fixed effects. Standard errors in parentheses clustered at village level. * p < 0.10, ** p < 0.05, *** p < 0.01. ") 


***********************************************************************************************************************
** Table A3. Robustness checks on impact on land use- proportions and log transformation of outcome variable
***********************************************************************************************************************
		
	use "landuse1.dta", clear

	global BL_controls "Sbl_is20_ch_good Sbl_is3__age Sdroadkm "

	* gen logs
	gen lgb_all_v = ln(gb_all_v)
		la var lgb_all_v "Any vegetation to bare (logs)"	
	* gen proportion
	gen prop_gb_all_v = gb_all_v/area
		la var prop_gb_all_v "Proportion any vegetation to bare"

	eststo clear
	eststo: xi:reg lgb_all_v Twfg i.chf,  robust
	eststo: xi:reg lgb_all_v Twfg i.chf $BL_controls , robust
	eststo: xi:reg prop_gb_all_v Twfg i.chf,  robust
	eststo: xi:reg prop_gb_all_v Twfg i.chf $BL_controls , robust
			
	esttab using "tables/Table_A3.rtf", replace star(* 0.10 ** 0.05 *** 0.01) b(3) se ar2 label title("Table A3. Village level analysis of programme impacts on land use change – transformation of dependent variable") ///
	nonotes keep (_cons Twfg $BL_controls) compress nogaps ///
		addnotes("Note: Table A3 provides a village level robustness analysis exploring the impact of transforming the dependent variable in logs and proportions; results remain unaltered. OLS regressions including chiefdom level fixed effects. Robust standard errors in parentheses. * p < 0.10, ** p < 0.05, *** p < 0.01. ")
	
***************************************	 
* Table A4. and  Table A5. 
***************************************
	*Table A4a. Reasons for farming less as a result of aid (10% of treated villages)
	*Table A4b. Reasons for farming the same as a result of aid (60% of treated responses)
	*Table A4c. Reasons for farming more as a result of aid (30% of treated responses)
	*Table A5a. Reasons for working less as a result of aid (12% of treated responses)
	*Table A5b. Reasons for working the same as a result of aid (56% of treated responses)
	*Table A5c. Reasons for working more as a result of aid (32% of treated responses)

use "tableA4andA5.dta", clear

	** Data Description**

	/*
	All variables obtained from main HH data file: landusehh.dta
	 
	is_91_frmbhvr: farm less, farm more, no change if treated (Twfg==1)
	is_92_frmbhvrchng: reasons for "is_91_frmbhvr"
	is_92_coded: reasons hand coded into categories; see Annex for description.

	is_93_wrkbhvr: no change; work less; work more if treated (Twfg==1)
	is_94_wrkbhvrchng: reasons for "is_93_wrkbhvr"
	is_94_coded:  reasons hand coded into categories; see Annex for description
	Twfg: treatment=1 
	*/

	*convert string variables
	encode is_91_frmbhvr, generate(is_91_coded)
	encode is_93_wrkbhvr, generate(is_93_coded)


	*Results for Table A4
	by is_91_coded, sort : tab1 is_92_coded 

	*Results for Table A5
	by is_93_coded, sort : tab1 is_94_coded


***************************************
* Table A6. Other outcome measures at EL (household level)
***************************************
	use "landusehh.dta", clear
	
	* income (from agriculture and logging), 
	* expenditures (including on land clearance equipment such as machetes) as well as 
	* loans and savings
	
	* impute missing values at treatment group mean
	foreach var in  bl_is20_ch_good bl_is3__age droadkm bl_is9__bushels {
			su `var' if Twfg==1
			replace `var' = r(mean) if `var' == . & Twfg==1
			su `var' if Twfg==0
			replace `var' = r(mean) if `var' ==. & Twfg==0
			}	

	* standardize controls	
	foreach var in bl_is20_ch_good bl_is3__age droadkm bl_is9__bushels {
			egen S`var'= std(`var')
			}

	la var Sbl_is3__age "Average age (std)"
	la var Sbl_is20_ch_good "Good Chief (std)"
	la var Sdroadkm "Distance to road (std)"


	*set controls global 
	global BL_controls "Sbl_is20_ch_good Sbl_is3__age Sdroadkm"
		
	eststo clear 
	foreach var in is_6_mphone is_13_lbrfrmelse is_14_selfrmprdct is_17a_incmearnings_o is_34_savemny  {
	eststo: xi:reg `var' Twfg  i.chf $BL_controls, cluster(vcode11)
	}
	
	esttab using "tables/Table_A6.rtf", replace star(* 0.10 ** 0.05 *** 0.01) b(3) se ar2 label title("Table A6. Other outcome measures at EL (household level)") ///
	mtitles("Owns Phone" "Labour income working on someone else's farm" "Income selling farm products" "Other Income" "Save money")	///
	nonotes keep (_cons Twfg  $BL_controls) compress nogaps ///
		addnotes("Notes: Table A6 presents the impact of treatment on outcome variables measured at end-line that capture measures of income/wealth. Regressions include chiefdom level fixed effects. Coefficients represent standard deviation difference from control. Standard errors in parentheses clustered at village level. * p < 0.10, ** p < 0.05, *** p < 0.01. ") 

***************************************
* Table A7.  Heterogeneous treatment effects - village level 
***************************************
use "landuse1.dta" , clear
	
	* standardize
	foreach var in dgrnpkm bl_is9__bushels villarea_all_vegetation_2011 area pop{
			egen S`var'= std(`var')
			}

	* gen interactions	
	gen Tdgrnpkm=Twfg*Sdgrnpkm
		la var Tdgrnpkm "T*Distance to GRNP"
		la var Sdgrnpkm "Distance to GRNP (std)"
	gen Tdroadkm =Twfg*Sdroadkm 
		la var Tdroadkm "T*Distance to road"
		la var Sdroadkm "Distance to road (std)"
	gen Tbl_is9__bushels=Twfg*Sbl_is9__bushels
		la var Tbl_is9__bushels "T*Bushels harvested BL"
		la var Sbl_is9__bushels "Bushels harvested BL (std)"
	gen Tbl_is20_ch_good=Twfg*bl_is20_ch_good
		la var Tbl_is20_ch_good "T*Chief is good"
		la var Sbl_is20_ch_good "Chief is good (std)"
	gen Tcover = Twfg*Svillarea_all_vegetation_2011 
		la var Tcover "T*Vegetation cover 2011"	
		la var Svillarea_all_vegetation_2011 "Vegetation cover in 2011 (std)"
	gen Tland=Twfg*Sarea
		la var Tland "T*Total village area (std)"
		la var Sarea "Total village area(std)"
	gen Tpop = Twfg*Spop
		la var Tpop "T*population"
		la var Spop "Population (std)"
	
	eststo clear
	eststo: xi:reg gb_all_v Twfg i.chf Tdgrnpkm Sdgrnpkm, robust
	eststo: xi:reg gb_all_v Twfg i.chf Tdroadkm Sdroadkm, robust
	eststo: xi:reg gb_all_v Twfg i.chf Tbl_is9__bushels bl_is9__bushels, robust
	eststo: xi:reg gb_all_v Twfg i.chf Tbl_is20_ch_good bl_is20_ch_good, robust
	eststo: xi:reg gb_all_v Twfg i.chf Tcover Svillarea_all_vegetation_2011, robust
	eststo: xi:reg gb_all_v Twfg i.chf Tland Sarea, robust
	eststo: xi:reg gb_all_v Twfg i.chf Tpop Spop, robust
	
	esttab using "tables/Table_A7.rtf", replace star(* 0.10 ** 0.05 *** 0.01) b(3) se ar2 label title("Table A7. Heterogeneous treatment effects - village level") ///
	nonotes drop (_Ichf_2 _Ichf_3 _Ichf_5 _Ichf_6 _Ichf_7) compress nogaps ///
		addnotes("Notes: Table A7 reports regression results reporting how the treatment effect varies across different key covariates. Importantly we find that in larger treatment villages more land was cleared. OLS regressions include chiefdom level fixed effects. The dependent variables are the square meters that go from any type of ‘green’ land in 2011 to ’bare’ soil in 2013. Robust standard errors in parentheses. * p < 0.10, ** p < 0.05, *** p < 0.01.") 
						
	
										*** END ****
