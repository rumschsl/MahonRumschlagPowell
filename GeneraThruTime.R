library(tidyverse)
library(StreamData)

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

####Taxa names
taxnames = read.csv("C:/Users/mikem/Documents/Research/USGS Stream Macros/MahonRumschlagPowell/20201028.1839.taxon_INVERT.csv",
                    fileEncoding="UTF-8-BOM")

gen_taxnames = taxnames %>%
  filter(Genus != "") %>%
  filter(BiologicalCommunity == "Invertebrates") %>%
  dplyr::select(BenchTaxonName, TaxonConceptState,
                BiodataTaxonName, PublishedTaxonName, Genus)

taxnames %>%
  filter(BenchTaxonName == "Probezzia")

##Generate the unique Bench Genera names (outdated) that do not match the updated Genera names
BenchGenera = unique((gen_taxnames %>% 
  mutate(BenchGenus = gsub( " .*$", "", gen_taxnames$BenchTaxonName )) %>%
  filter(BenchGenus != Genus))$BenchGenus)

##Generate a list of the outdated Genera names and the matching updated Genera names
##Remove all outdated Genera names that only have a single updated genus name
##  These genera are not problematic, because individually, these outdated genera
##  will be completely renamed with a single updated genus.
##The problem genera are those that have 1+ updated genera names AND
##  their outdated genera names are included in the updated genera names
##  For example, Baetis was updated and was moved into 13 (!) genera:
##    Acentrella, Acerpenna, Baetisca, Callibaetis, Diphetor, Drunella, 
##    Ephemerella, Fallceon, Heterocloeon, Hexagenia, Labiobaetis, Plauditus,
##    AND Baetis!!! So, all observations that are at the genus level and are
##    "Baetis", need to be dealt with. There are 48 genera of these problem taxa
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
  filter(BenchGenusToNewGenera > 1)

View(GeneraListProblems)

length(unique(c(GeneraListProblems$Genus, GeneraListProblems$BenchGenus)))

length(unique(GeneraListProblems$Genus))
length(unique(GeneraListProblems$BenchGenus))


