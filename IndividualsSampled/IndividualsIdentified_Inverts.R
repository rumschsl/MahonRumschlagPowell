library(tidyverse)

##Read in raw dataset
invertsites = read.csv(unzip("C:/Users/mikem/Documents/Research/USGS Stream Macros/MahonRumschlagPowell/20201217.0749.InvertResults.zip"),
                       colClasses = c("SiteNumber" = "character"))

##Change first column name, just in case it is weird (for Mike)
colnames(invertsites)[1] = "SIDNO"

##Filter the SampleTypeCode to only "IRTH", because this is what we use in the function
##Group by SIDNO (represents unique samples)
##Sum the "RawCount" within each SIDNO; this produces the TOTAL number of individuals
## that were identified to lowest Taxonomic resolution
counts = invertsites %>%
  filter(SampleTypeCode == "IRTH") %>%
  group_by(SIDNO) %>%
  summarize(IndCount = sum(RawCount, na.rm = TRUE))

##Plot histogram with binwidths of 25
ggplot(counts, aes(x = IndCount)) +
  geom_histogram(binwidth = 25)

##Filter out all counts greater than 1000 individuals
##Plot histogram with binwidths of 25
counts %>%
  filter(IndCount < 1000) %>%
  ggplot(aes(x = IndCount)) +
  scale_x_continuous(breaks = seq(0,1000,100))+
  geom_histogram(binwidth = 25)

##Average individuals per sample is ~400
mean(counts$IndCount)

##Proportion of samples with >300 individuals (0.828)
sum(counts$IndCount > 299) / nrow(counts)

##Proportion of samples with >200 individuals (0.901)
sum(counts$IndCount > 199) / nrow(counts)
