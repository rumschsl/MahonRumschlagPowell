library(tidyverse)
library(StreamData)

#
# MAKE PLOTS OF OCCURENCES OF GENERA THORUGH TIME
# 

##Got an error when abunMeasure = "abundance", Mahon updated the 'StreamData'
##R package on 3/30/21; you'll need to update it from Github for this function
##to work.
##Don't worry about this function if you're just looking at the taxa names
##on line 70.
inverts <- getInvertData(dataType = "occur",
                         taxonLevel = "Genus",
                         abunMeasure = "abundance")

colnames(inverts)
invertslong = inverts %>% 
  ##Remove columns that we don't need
  dplyr::select(-ProjectLabel, -ProjectAssignedSampleLabel, -NAWQA.SMCOD,
                -NAWQAStudyUnitCode, -CollectionDate, -StartTime, -TimeDatum,
                -CollectionMonth, -CollectionDayOfYear, -ProvisionalData,
                -SiteName, -StudyReachName, -SiteTypeName, -StateFIPSCode,
                -CountyFIPSCode, -Latitude_dd, -Longitude_dd, -CoordinateDatum,
                -HUCCode, -DrainageArea_mi2, -GeomorphicChannelUnit, 
                -ChannelBoundaries, -ChannelFeatures, -ReplicateType,
                -SampleTypeCode, -IdentificationEntity, -AreaSampTot_m2) %>%
  ##Pivot longer, so that we have a column for year, site, genus, and occurance
  pivot_longer(cols = Antocha:Utaperla,
               names_to = "Genus",
               values_to = "Occur") %>%
  ##If any occurance is >0, change to 1, so that it is a hit or not
  mutate(Occur = ifelse(Occur > 0,
                        1,
                        0)) %>%
  ##Account for multiple samples per site within a year
  group_by(CollectionYear, SiteNumber, Genus) %>%
  summarize(Occur = sum(Occur)) %>%
  ungroup() %>%
  ##If any occurance is >0, change to 1, so that it is a hit or not
  mutate(Occur = ifelse(Occur > 0,
                        1,
                        0)) %>%
  ##Get a count of the number of sites a genus appears in a given year
  group_by(CollectionYear, Genus) %>%
  summarize(Occur = sum(Occur)) %>%
  ungroup() %>%
  ##Compelte the data frame with missing combinations to make sure that
  ##Each genus shows up in each year (given a 0 if so)
  complete(CollectionYear, nesting(Genus), fill = list(Occur = 0))
  
##684 unique genera

##Split the genera list into 28 groups of 25 (all plots will be 25, except the last one, which is 9)
genlist = split(unique(invertslong$Genus), ceiling(seq_along(unique(invertslong$Genus)) / 25))

for(i in 1:28) {
  temp_plot = invertslong %>%
    filter(Genus %in% genlist[[i]]) %>%
    ggplot(aes(x = CollectionYear, y = Occur))+
    facet_wrap(~Genus, scales = "free_y")+
    geom_point()+
    stat_smooth(se = FALSE)+
    theme_classic()
  
  ggsave(temp_plot,
         file = paste("C:/Users/mikem/Documents/Research/USGS Stream Macros/MahonRumschlagPowell/plot_",
                      i,
                      ".jpg",
                      sep = ""),
         width = 10, height = 6, units = "in")
}

#
# INSPECT CHANGES IN TAXONOMY THROUGH TIME
#

##Import taxonomic names dataset
setwd("/Users/samantharumschlag/Documents/PowellCenter/Code/MahonRumschlagPowell")
taxnames = read.csv("20201028.1839.taxon_INVERT.csv",
                    fileEncoding="UTF-8-BOM")

##Make a dataset of taxa that include genus, species, or subspecies-level information 
gen_taxnames = taxnames %>%
  filter(Genus != "") %>% ##remove info at the subfamily or greater, keep genus, species, subspecies
  filter(BiologicalCommunity == "Invertebrates") %>% ##select only intertebrates, just in case
  dplyr::select(BenchTaxonName, TaxonConceptState, ##select columns with pertinent info
                PublishedTaxonName, Genus) ##Genus here is genus from PublishedTaxonName

##Note: TaxonConceptState doesn't seem to be very helpful. Observations exist in which
## TaxonConceptState is "current" but BenchTaxonName is not the same as PublishedTaxonName

taxnames %>%
  filter(BenchTaxonName == "Probezzia")

##Generate a list that is all the possibly problematic genera names
## The list is the unique genus-level BenchTaxonNames from gen_taxnames
BenchGenera = unique(
  (gen_taxnames %>% 
  mutate(BenchGenus = gsub( " .*$", "", gen_taxnames$BenchTaxonName )) %>%
  filter(BenchGenus != Genus)) #keep rows in which BenchGenus does not match Genus from PublishedTaxonName
  $BenchGenus #keep only BenchGenus column
  ) 

##Generate a list of the outdated Genera names (from BenchTaxonName) and the 
##matching updated Genera names (from PublishedTaxonName)

##Remove all outdated Genera names that only have a single updated genus name
##These genera are not problematic, because we will rely on genus ID's from
##PublishedTaxonName (i.e. "Old Genus Name" (from BenchTaxonName) is 
## always "New Genus Name" (from PublishedTaxonName))

##The problem genera are those that are associated with >1 updated genera names 
##  For example, Baetis was updated and was moved into 13 (!) genera:
##  Acentrella, Acerpenna, Baetisca, Callibaetis, Diphetor, Drunella, 
##  Ephemerella, Fallceon, Heterocloeon, Hexagenia, Labiobaetis, Plauditus,
##  AND Baetis!!! So, all observations that are at the genus level and are
##  "Baetis", need to be dealt with. There are 48 genera of these problem taxa
##    that we will need to deal with. BUT, not all of these are found in the 
##    final dataset and it may not be as problematic as once thought.

GeneraListProblems <- gen_taxnames %>% 
  mutate(BenchGenus = gsub( " .*$", "", gen_taxnames$BenchTaxonName )) %>%
  filter(BenchGenus %in% BenchGenera) %>%
  dplyr::select(BenchGenus, Genus) %>%
  group_by(BenchGenus, Genus) %>%
  slice(1) %>%
  group_by(BenchGenus) %>%
  mutate(BenchGenusToNewGenera = n()) %>%
  filter(BenchGenusToNewGenera > 1) %>%
  mutate(BenchGenusInNawqa = ifelse(BenchGenus %in% unique(invertslong$Genus),
                                    "Y",
                                    "N"),
         PublishedGenusInNawqa = ifelse(Genus %in% unique(invertslong$Genus),
                                    "Y",
                                    "N")) 

#generate table for use in package
GeneraListProblems <- GeneraListProblems[,1:2]


#get list of unique BenchGenus & Genus, sort alphabetically
unqAllGn = sort(unique(c(GeneraListProblems$BenchGenus,
                         as.character(GeneralListProblems$Genus))))  

#create a column that is the pairwise combos of BenchGenus and PublishedTaxon Genus
GeneraListProblems$BenchGenus_Genus = paste(GeneraListProblems$BenchGenus, GeneraListProblems$Genus, sep= "_")

#make output df to store all pairwise relationships among all genera
pairwiseDF <- data.frame(matrix(ncol = length(unqAllGn), nrow = length(unqAllGn)))
colnames(pairwiseDF) <- unqAllGn
rownames(pairwiseDF) <- unqAllGn

#loop through unqBnchGn
for( i in 1:length(unqAllGn)){
  #web is the web of genera names that correspond to given genus
  #Search for each unique BenchGenus & match to BenchGenus_Genus. 
  #For each match, get the BenchGenus & Genus and put into a single vector
  web <- c(GeneraListProblems$BenchGenus[grep(unqAllGn[i], GeneraListProblems$BenchGenus_Genus)],
         as.character(GeneraListProblems$Genus)[grep(unqAllGn[i], GeneraListProblems$BenchGenus_Genus)])

  #get unique observation of web
  webo <- sort(unique(web))

  #store pairwise relationships, "1" is a match
  pairwiseDF[i,] <- ifelse(colnames(pairwiseDF) %in% webo, 1, 0)
  }

#use graph theory to generate non-overlapping groups based on pairwiseDF
n = nrow(pairwiseDF)

#install.packages("igraph")
library(igraph)

## Make graph of how genera are connected
same <- which(pairwiseDF==1)
topology <- data.frame(N1=((same-1) %% n) + 1, N2=((same-1) %/% n) + 1)
topology <- topology[order(topology[[1]]),] # Get rid of loops and ensure right naming of vertices
g3 <- simplify(graph.data.frame(topology,directed = FALSE))
get.data.frame(g3)

# Plot graph
plot(g3)

#get clusters and membership information for each cluster
clust <- clusters(g3, mode="weak")$membership

#make a df with appropriate names for groups
clust <- data.frame(num = 1:98, group = clust, genus = unqAllGn)

#create label for "lumping"
#split list
clust_list <- split(clust, clust$group)
 
for( i in 1:length(clust_list)){
  clust_list[[i]]$lump <- paste(clust_list[[i]]$genus, collapse = "/")
 }
 
clust_labels <- do.call(rbind,clust_list)

setwd("/Users/samantharumschlag/Documents/PowellCenter/Code/MahonRumschlagPowell")
write.csv(clust_labels, "clust_labels.csv")





