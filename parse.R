# This script was written to help with parsing the bar chart date
# information in "BreedingGuidelinesBarChart.xlsx" to usable information
# for further use.
#
# Note this program makes the assumption that the species'
# breeding time follows the following order:
# | N | M | T | B | T | M | N |
# Note one or more season can be missing , for example, this program can 
# succesfully parse a breeding time like follows:
# | N | T | B | M | N |
# So this works for most species. However, information like follows will
# be given incorrect result, thus manual entry would be needed:
# | B | M | N | B |
# As of the information used, only one line of information will be failed to 
# parse, which is the "Great Horned Owl" entry in the CostalPlain sheet. It has
# an information of | B | N | B |
#
# Author: Tianchang Yang (tianchang.yang@richmond.edu)
# Jun 9, 2017




# Uncomment if haven't downloaded these packages before
# install.packages("lubridate")
# install.packages("readxl")


###################### setup ##########################

library(lubridate)
library(readxl)

# Set working directory to location where input files are located and
# output file will be written
setwd("/Users/ty3gx/Desktop/vabba2")

# filename
file <- "BreedingGuidelinesBarChart.xlsx"

# year for the data
year <- "2016"





### Function to determine date based on the cell number ####

findStartDate <- function(n, year) {
	temp <- n %% 4
	day <- 0
	if (temp == 1) {
		day <- 24
	} else if (temp == 2) {
		day <- 1
	} else if (temp == 3) {
		day <- 8
	} else if (temp == 0) {
		day <- 15
	}

	month <- (n - 2)%/%4 + 1

	result <- paste(month, day, sep = "/")
	result <- paste(result, year, sep = "/")

	result
}


findEndDate <- function(n, year) {
	temp <- n %% 4
	day <- 0
	if (temp == 1) {
		day <- 24
	} else if (temp == 2) {
		day <- 1
	} else if (temp == 3) {
		day <- 8
	} else if (temp == 0) {
		day <- 15
	}

	month <- (n - 2)%/%4 + 1

	result <- paste(month, day, sep = "/")
	result <- paste(result, year, sep = "/")
	
	date <- mdy(result)
    date <- date - days(1)

    result <- paste(month(date), day(date), year(date), sep = "/")
    result
}

################### Data Read-in ######################
data = read_excel(file, sheet = "CoastalPlain", skip = 9, col_names = FALSE)
size <- dim(data)[1]

#chr = data[65, 1]


#Remove all rows start with <NA> or "Species"
for (i in 1:size)
{
	if (is.na(data[i,1])) {
		data <- data[-c(i), ]
	}
}

data <- data[data$X__1 != "Species", ]
data <- data[data$X__1 != "ºWayne's Warbler is a distinctive sub-species of the Black-throated Green Warbler.", ]


#Remove the '*' symbol from the data
data[] <- lapply(data, gsub, pattern = '[*]', replacement = '')

#size <- dim(data)[1]
#for (i in 1:size)
#{
#	if (identical(data[i,1], chr)) {
#		data <- data[-c(i), ]
#	}
#}

data <- as.data.frame(data)

##################### Data Processing ####################

#Construct the matrix for the result
size <- dim(data)[1]
result <- matrix(NA, nrow = size, ncol = 15)
colnames(result) <- c("Species", "N_1_start", "N_1_end", "M_1_start", "M_1_end",
	 "T_1_start", "T_1_end", "B_start", "B_end", "T_2_start", "T_2_end", "M_2_start"
	 , "M_2_end", "N_2_start", "N_2_end")
result <- as.data.frame(result)

for (i in 1:size) {
	result[i,1] <- data[i,1]
}


cols <- dim(data)[2]

# loop through guideline data and record the date into more parserable dates
for (i in 1:size) {
	if (is.na(data[i,2])) {
		next
	} else {
		for (j in 3:cols) {
			if (is.na(data[i, j])) {
				data[i,j] <- "T"
			}
		}
	}
}


for (i in 1:size) {
	if (is.na(data[i,2])) {	
		next
	}

	N_1_start <- NA
	N_1_end <- NA
	M_1_start <- NA
	M_1_end <- NA
	T_1_start <- NA
	T_1_end <- NA
	B_start <- NA
	B_end <- NA
	T_2_start <- NA
	T_2_end <- NA
	M_2_start <- NA
	M_2_end <- NA
	N_2_start <- NA
	N_2_end <- NA

	second <- FALSE
	start <- TRUE


	for (j in 2:cols) {
		if(!second) {
			if ( identical(data[i,j], "N") ) {
				if(start) {
					N_1_start <- findStartDate(j, year)
					start <- FALSE
				} 
				if (!identical(data[i, j+1], "N")) {
					N_1_end <- findEndDate(j+1, year)
					start <- TRUE
				}
			}
			if ( identical(data[i,j], "M") ) {
				if(start) {
					M_1_start <- findStartDate(j, year)
					start <- FALSE
				}
				if (!identical(data[i, j+1], "M")) {
					M_1_end <- findEndDate(j+1, year)
					start <- TRUE
				}
			}
			if ( identical(data[i,j], "T") ) {
				if(start) {
					T_1_start <- findStartDate(j, year)
					start <- FALSE
				}
				if (!identical(data[i, j+1], "T")) {
					T_1_end <- findEndDate(j+1, year)
					start <- TRUE
				}
			}
			if ( identical(data[i,j], "B") ) {
				if(start) {
					B_start <- findStartDate(j, year)
					start <- FALSE
				}
				if (!identical(data[i, j+1], "B")) {
					B_end <- findEndDate(j+1, year)
					start <- TRUE
					second <- TRUE
				}
			}
		}

	if(second) {
			if ( identical(data[i,j], "N") ) {
				if(start) {
					N_2_start <- findStartDate(j, year)
					start <- FALSE
					temp <- paste("12/31", year, sep = "/")
					N_2_end <- temp
					start <- TRUE
					break
				}
			}
			if ( identical(data[i,j], "M") ) {
				if(start) {
					M_2_start <- findStartDate(j, year)
					start <- FALSE
				} 
				if (!identical(data[i, j+1], "M")) {
					M_2_end <- findEndDate(j+1, year)
					start <- TRUE
				}
			}
			if ( identical(data[i,j], "T") ) {
				if(start) {
					T_2_start <- findStartDate(j, year)
					start <- FALSE
				} 
				if (!identical(data[i, j+1], "T")) {
					T_2_end <- findEndDate(j+1, year)
					start <- TRUE
				}
			}
			if ( identical(data[i,j], "B") ) {
				next
			}
		}

	}

	result[i,2] <- N_1_start
	result[i,3] <- N_1_end
	result[i,4] <- M_1_start
	result[i,5] <- M_1_end
	result[i,6] <- T_1_start
	result[i,7] <- T_1_end
	result[i,8] <- B_start
	result[i,9] <- B_end
	result[i,10] <- T_2_start
	result[i,11] <- T_2_end
	result[i,12] <- M_2_start
	result[i,13] <- M_2_end
	result[i,14] <- N_2_start
	result[i,15] <- N_2_end

}





write.csv(result, file = "CoastalPlain.csv", row.names=FALSE)

###################################################################
###################################################################


################### Data Read-in ######################
data = read_excel(file, sheet = "Piedmont", skip = 9, col_names = FALSE)
size <- dim(data)[1]

#chr = data[65, 1]


#Remove all rows start with <NA> or "Species"
for (i in 1:size)
{
	if (is.na(data[i,1])) {
		data <- data[-c(i), ]
	}
}

data <- data[data$X__1 != "Species", ]

#Remove the '*' symbol from the data
data[] <- lapply(data, gsub, pattern = '[*]', replacement = '')

#size <- dim(data)[1]
#for (i in 1:size)
#{
#	if (identical(data[i,1], chr)) {
#		data <- data[-c(i), ]
#	}
#}

data <- as.data.frame(data)

##################### Data Processing ####################

#Construct the matrix for the result
size <- dim(data)[1]
result <- matrix(NA, nrow = size, ncol = 15)
colnames(result) <- c("Species", "N_1_start", "N_1_end", "M_1_start", "M_1_end",
	 "T_1_start", "T_1_end", "B_start", "B_end", "T_2_start", "T_2_end", "M_2_start"
	 , "M_2_end", "N_2_start", "N_2_end")
result <- as.data.frame(result)

for (i in 1:size) {
	result[i,1] <- data[i,1]
}


cols <- dim(data)[2]


for (i in 1:size) {
	if (is.na(data[i,2])) {
		next
	} else {
		for (j in 3:cols) {
			if (is.na(data[i, j])) {
				data[i,j] <- "T"
			}
		}
	}
}


for (i in 1:size) {
	if (is.na(data[i,2])) {	
		next
	}

	N_1_start <- NA
	N_1_end <- NA
	M_1_start <- NA
	M_1_end <- NA
	T_1_start <- NA
	T_1_end <- NA
	B_start <- NA
	B_end <- NA
	T_2_start <- NA
	T_2_end <- NA
	M_2_start <- NA
	M_2_end <- NA
	N_2_start <- NA
	N_2_end <- NA

	second <- FALSE
	start <- TRUE


	for (j in 2:cols) {
		if(!second) {
			if ( identical(data[i,j], "N") ) {
				if(start) {
					N_1_start <- findStartDate(j, year)
					start <- FALSE
				} 
				if (!identical(data[i, j+1], "N")) {
					N_1_end <- findEndDate(j+1, year)
					start <- TRUE
				}
			}
			if ( identical(data[i,j], "M") ) {
				if(start) {
					M_1_start <- findStartDate(j, year)
					start <- FALSE
				} 
				if (!identical(data[i, j+1], "M")) {
					M_1_end <- findEndDate(j+1, year)
					start <- TRUE
				}
			}
			if ( identical(data[i,j], "T") ) {
				if(start) {
					T_1_start <- findStartDate(j, year)
					start <- FALSE
				} 
				if (!identical(data[i, j+1], "T")) {
					T_1_end <- findEndDate(j+1, year)
					start <- TRUE
				}
			}
			if ( identical(data[i,j], "B") ) {
				if(start) {
					B_start <- findStartDate(j, year)
					start <- FALSE
				} 
				if (!identical(data[i, j+1], "B")) {
					B_end <- findEndDate(j+1, year)
					start <- TRUE
					second <- TRUE
				}
			}
		}

	if(second) {
			if ( identical(data[i,j], "N") ) {
				if(start) {
					N_2_start <- findStartDate(j, year)
					start <- FALSE
					temp <- paste("12/31", year, sep = "/")
					N_2_end <- temp
					start <- TRUE
					break
				}
			}
			if ( identical(data[i,j], "M") ) {
				if(start) {
					M_2_start <- findStartDate(j, year)
					start <- FALSE
				} 
				if (!identical(data[i, j+1], "M")) {
					M_2_end <- findEndDate(j+1, year)
					start <- TRUE
				}
			}
			if ( identical(data[i,j], "T") ) {
				if(start) {
					T_2_start <- findStartDate(j, year)
					start <- FALSE
				} 
				if (!identical(data[i, j+1], "T")) {
					T_2_end <- findEndDate(j+1, year)
					start <- TRUE
				}
			}
			if ( identical(data[i,j], "B") ) {
				next
			}
		}

	}

	result[i,2] <- N_1_start
	result[i,3] <- N_1_end
	result[i,4] <- M_1_start
	result[i,5] <- M_1_end
	result[i,6] <- T_1_start
	result[i,7] <- T_1_end
	result[i,8] <- B_start
	result[i,9] <- B_end
	result[i,10] <- T_2_start
	result[i,11] <- T_2_end
	result[i,12] <- M_2_start
	result[i,13] <- M_2_end
	result[i,14] <- N_2_start
	result[i,15] <- N_2_end

}





write.csv(result, file = "Piedmont.csv", row.names=FALSE)


###################################################################
###################################################################

################### Data Read-in ######################
data = read_excel(file, sheet = "MountainsValleys", skip = 9, col_names = FALSE)
size <- dim(data)[1]

#chr = data[65, 1]


#Remove all rows start with <NA> or "Species"
for (i in 1:size)
{
	if (is.na(data[i,1])) {
		data <- data[-c(i), ]
	}
}

data <- data[data$X__1 != "Species", ]

#Remove the '*' symbol from the data
data[] <- lapply(data, gsub, pattern = '[*]', replacement = '')

#size <- dim(data)[1]
#for (i in 1:size)
#{
#	if (identical(data[i,1], chr)) {
#		data <- data[-c(i), ]
#	}
#}

data <- as.data.frame(data)

##################### Data Processing ####################

#Construct the matrix for the result
size <- dim(data)[1]
result <- matrix(NA, nrow = size, ncol = 15)
colnames(result) <- c("Species", "N_1_start", "N_1_end", "M_1_start", "M_1_end",
	 "T_1_start", "T_1_end", "B_start", "B_end", "T_2_start", "T_2_end", "M_2_start"
	 , "M_2_end", "N_2_start", "N_2_end")
result <- as.data.frame(result)

for (i in 1:size) {
	result[i,1] <- data[i,1]
}


cols <- dim(data)[2]


for (i in 1:size) {
	if (is.na(data[i,2])) {
		next
	} else {
		for (j in 3:cols) {
			if (is.na(data[i, j])) {
				data[i,j] <- "T"
			}
		}
	}
}


for (i in 1:size) {
	if (is.na(data[i,2])) {	
		next
	}

	N_1_start <- NA
	N_1_end <- NA
	M_1_start <- NA
	M_1_end <- NA
	T_1_start <- NA
	T_1_end <- NA
	B_start <- NA
	B_end <- NA
	T_2_start <- NA
	T_2_end <- NA
	M_2_start <- NA
	M_2_end <- NA
	N_2_start <- NA
	N_2_end <- NA

	second <- FALSE
	start <- TRUE


	for (j in 2:cols) {
		if(!second) {
			if ( identical(data[i,j], "N") ) {
				if(start) {
					N_1_start <- findStartDate(j, year)
					start <- FALSE
				} 
				if (!identical(data[i, j+1], "N")) {
					N_1_end <- findEndDate(j+1, year)
					start <- TRUE
				}
			}
			if ( identical(data[i,j], "M") ) {
				if(start) {
					M_1_start <- findStartDate(j, year)
					start <- FALSE
				} 
				if (!identical(data[i, j+1], "M")) {
					M_1_end <- findEndDate(j+1, year)
					start <- TRUE
				}
			}
			if ( identical(data[i,j], "T") ) {
				if(start) {
					T_1_start <- findStartDate(j, year)
					start <- FALSE
				} 
				if (!identical(data[i, j+1], "T")) {
					T_1_end <- findEndDate(j+1, year)
					start <- TRUE
				}
			}
			if ( identical(data[i,j], "B") ) {
				if(start) {
					B_start <- findStartDate(j, year)
					start <- FALSE
				} 
				if (!identical(data[i, j+1], "B")) {
					B_end <- findEndDate(j+1, year)
					start <- TRUE
					second <- TRUE
				}
			}
		}

	if(second) {
			if ( identical(data[i,j], "N") ) {
				if(start) {
					N_2_start <- findStartDate(j, year)
					start <- FALSE
					temp <- paste("12/31", year, sep = "/")
					N_2_end <- temp
					start <- TRUE
					break
				}
			}
			if ( identical(data[i,j], "M") ) {
				if(start) {
					M_2_start <- findStartDate(j, year)
					start <- FALSE
				} 
				if (!identical(data[i, j+1], "M")) {
					M_2_end <- findEndDate(j+1, year)
					start <- TRUE
				}
			}
			if ( identical(data[i,j], "T") ) {
				if(start) {
					T_2_start <- findStartDate(j, year)
					start <- FALSE
				} 
				if (!identical(data[i, j+1], "T")) {
					T_2_end <- findEndDate(j+1, year)
					start <- TRUE
				}
			}
			if ( identical(data[i,j], "B") ) {
				next
			}
		}

	}

	result[i,2] <- N_1_start
	result[i,3] <- N_1_end
	result[i,4] <- M_1_start
	result[i,5] <- M_1_end
	result[i,6] <- T_1_start
	result[i,7] <- T_1_end
	result[i,8] <- B_start
	result[i,9] <- B_end
	result[i,10] <- T_2_start
	result[i,11] <- T_2_end
	result[i,12] <- M_2_start
	result[i,13] <- M_2_end
	result[i,14] <- N_2_start
	result[i,15] <- N_2_end

}





write.csv(result, file = "MountainsValleys.csv", row.names=FALSE)
