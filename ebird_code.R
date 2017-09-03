# This script was written to help identify entrie in ebird 
# entries which seems unreasonable for further inspection. 
# 
# Author: Tianchang Yang (tianchang.yang@richmond.edu)
# Jun 16, 2017

# Uncomment if haven't downloaded packages before
#install.packages("lubridate")
#install.packages("readxl")
#install.packages("stringr")



library(lubridate)
library(readxl)
library(stringr)
setwd("/Users/ty3gx/Desktop/vabba2/Atlas_Data_Season1")

ebird_file <- "eBird_ATL_VA_0916_Sorted_w_ecoregion.csv"
c_date_file <- "CoastalPlain.csv"
m_date_file <- "MountainsValleys.csv"
p_date_file <- "Piedmont.csv"
filter_file <- "Species_BreedingCodes_FilterGuidelines.csv"

Pflag <- "X"
Sflag <- "R"
okay <- "O"


############# Function to make judgment ##############
makeJudgment <- function(line, c_date, m_date, p_date) {
	species <- line[ ,"COMMON_NAME"]
	species <- toString(species)
	record_date <- mdy_hm(line[ ,"OBSERVATION_DATE"])
	code <- line[ ,"BREEDING_BIRD_ATLAS_CODE"]
	code <- toString(code)
	code <- str_replace_all(string = code, pattern = " ", repl = "")
	region <- line[ , "Ecoregion"]
	guideline_date <- NA
	str <- NA

	if (code == "") {
		str <- okay
	} else {
		if (region == "Atlantic Coastal Plain") {	
			guideline_date <- c_date[species, ]
		} else if (region == "Appalachian Ranges") {
			guideline_date <- m_date[species, ]
		} else if (region == "Piedmont") {	
			guideline_date <- p_date[species, ]
		} 


		N_1_start <- mdy(guideline_date[ ,1])
		N_1_end <- mdy(guideline_date[ ,2])
		M_1_start <- mdy(guideline_date[ ,3])
		M_1_end <- mdy(guideline_date[ ,4])
		T_1_start <- mdy(guideline_date[ ,5])
		T_1_end <- mdy(guideline_date[ ,6])
		B_start <- mdy(guideline_date[ ,7])
		B_end <- mdy(guideline_date[ ,8])
		T_2_start <- mdy(guideline_date[ ,9])
		T_2_end <- mdy(guideline_date[ ,10])
		M_2_start <- mdy(guideline_date[ ,11])
		M_2_end <- mdy(guideline_date[ ,12])
		N_2_start <- mdy(guideline_date[ ,13])
		N_2_end <- mdy(guideline_date[ ,14])

		N_1_interval <- interval(N_1_start,  N_1_end)
		M_1_interval <- interval(M_1_start,  M_1_end)
		T_1_interval <- interval(T_1_start,  T_1_end)
		B_interval <- interval(B_start,  B_end)
		T_2_interval <- interval(T_2_start,  T_2_end)
		M_2_interval <- interval(M_2_start,  M_2_end)
		N_2_interval <- interval(N_2_start,  N_2_end)

		in_N <- FALSE
		in_M <- FALSE
		in_T <- FALSE
		in_B <- FALSE

		if (!is.na(N_1_interval)) {
			if (record_date %within% N_1_interval) {
				in_N <- TRUE
			}
		}

		if (!is.na(M_1_interval)) {
			if (record_date %within% M_1_interval)  {
			in_M <- TRUE
			}
		}

		if (!is.na(T_1_interval)) {
			if (record_date %within% T_1_interval) {
				in_T <- TRUE
			}
		}

		if (!is.na(N_2_interval)) {
			if (record_date %within% N_2_interval) {
				in_N <- TRUE
			}
		}

		if (!is.na(M_2_interval)) {
			if (record_date %within% M_2_interval)  {
				in_M <- TRUE
			}
		}

		if (!is.na(T_2_interval)) {
			if (record_date %within% T_2_interval) {
				in_T <- TRUE
			}
		}

		if (!is.na(B_interval)) {
			if (record_date %within% B_interval) {
				in_B <- TRUE
			}
		}

		if (is.na(B_start)) {
			str <- Pflag
		} else {
			if (in_N) {
				str <- Pflag
			} else if (in_T || in_M || in_B) {
				str <- okay
			}
		}


		if ((code == "F") || (code == "H") || (code == "S") 
			|| (code == "S7") || (code == "M")) {
			if (!in_B) {
				str <- Pflag
			}
		}

		if (code == "PE") {
			str <- Pflag
		}

	}

	if (filter[species, code] == "X" || filter[species, code] == "R") {
		str <- filter[species, code]
	}

	str
	
}



################### Data Read-in ######################
ebird <- read.csv(ebird_file)
ebird <- as.data.frame(ebird)
c_date <- read.csv(c_date_file, row.names = 1)
m_date <- read.csv(m_date_file, row.names = 1)
p_date <- read.csv(p_date_file, row.names = 1)

temp_filter <- read.csv(filter_file)

temp_filter[] <- lapply(temp_filter, gsub, pattern = '[*]', replacement = '')
# TODO: still need to remove some weird symbols

temp_filter <- temp_filter[, !(colnames(temp_filter) %in% c("X","X.1","X.2"))]
filter <- temp_filter[,-1]
rownames(filter) <- temp_filter[,1]


size <- dim(ebird)[1]

################### Preparation #######################

result <- matrix(NA, nrow = size, ncol = 2)
colnames(result) <- c("GLOBAL UNIQUE IDENTIFIER", "JUDGEMENT")
result <- as.data.frame(result)
result[,1] <- ebird[,1]


pb = txtProgressBar(min = 0, max = size, initial = 0,
 char = "█", width = 80, style = 3) 
#count <- 1
stepi <- 1
for (i in 1:size) {
	stepi <- stepi + 1
	setTxtProgressBar(pb,stepi)

	#if (i == (size %/% 100) * count) {
	#	msg <- paste(count, "% finished.")
	#	print(msg)
	#	count <- count + 1
	#}
	str <- makeJudgment(ebird[i, ], c_date, m_date, p_date)
	result[i,2] <- str
}


# TODO: allocate all flagged entries into one dataframe

# TODO: write dataframe to file