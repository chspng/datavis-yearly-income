# This is an exploratory visualization based on CANSIM 111-0008: Neighbourhood income and demographics, taxfilers and dependents with income by total income, sex and age group
# 2016-12-28
# Christine P'ng

library(ggplot2);

# read in datafile
income_data <- read.csv("01110008-eng.csv");

# subset for all ages and both sexes across all of Canada
age_ind <- ifelse (income_data$SEX == "Both sexes", TRUE, FALSE);
sex_ind <- ifelse (income_data$AGE == "All age groups", TRUE, FALSE);
geo_ind <- ifelse (income_data$GEO == "Canada", TRUE, FALSE);

# remove data that will not be plotted
group_ind <- ifelse (income_data$INC == "5-year percent change of median income", FALSE, TRUE);
group2_ind <- ifelse (income_data$INC == "Median total income (dollars)", FALSE, TRUE);
total_ind <- ifelse (income_data$INC == "Total persons with income", FALSE, TRUE);

ind <- c();
total_counts <- data.frame(
	"year" = seq(1:length(levels(factor(income_data$Ref_Date)))),
	"total" = seq(1:length(levels(factor(income_data$Ref_Date))))
	);

len <- seq(1:length(age_ind));
for (count in len){
	if (age_ind[count] && sex_ind[count] && geo_ind[count] && group_ind[count] && group2_ind[count]){
		# track the total number of people per year
		if(!total_ind[count]){
			total_counts$year[count] = income_data$Ref_Date[count]; # these values look too low
			total_counts$total[count] = income_data$Value[count]; 
			ind[count] <- FALSE;
		} else{
			ind[count] <- TRUE;		
		}
	} else{
		ind[count] <- FALSE;
	}
}

income_by_year <- data.frame(
	"year" = factor(income_data$Ref_Date[ind]), 
	"count" = as.numeric(as.character(income_data$Value[ind])),
	"group" = income_data$INC[ind]
	);

# normalize data by total persons
len_dat <- seq(1:dim(income_by_year)[1]);
for(counter in len_dat){
	# for each matching year, adjust the count by the total_counts value
	len_yrs <- seq(1:length(total_counts$year));
	for (year_ind in len_yrs){
		if (income_by_year$year[counter] == total_counts$year[year_ind]){
			income_by_year$count[counter] <- income_by_year$count[counter]/total_counts$total[year_ind];
		}	
	}
}

# adjust data to plot ranges instead of cumulative counts

# generate plot
qplot(data = income_by_year, x = year, y = count, group = group, colour = group, geom = "line")
 
