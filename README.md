# Ligustrum phenology
Study about the phenology of an invasive species that became a problem at Araucaria Forest, an endangered area in Brazil.

It also comprises the analysis of the environmental variables related to the found phenology pattern and a comparison among the invasive species and two native areas, one located in an urban area (UAF - Urban Araucaria Forest) and the other in the countryside (NAF - Native Araucaria Forest).   



The study was based on the monthly observation of 49 trees of Ligustrum lucidum (Oleaceae) in the city of Curitiba, Paraná, South Brazil. On each observation, the ocurrence of vegetative and reproductive phenophases was observed. Leaf flush, flower budding, flowering, unripe fruits and ripe fruits were the phenophases observed. The intensity of each phenological event for each individual was scored on a scale from 0 to 4, which represents the percentage of crown coverage (from 0 to 100%). The scores were used to calculate the Fournier Index, which estimates the intensity of each phenophase during an observation from the sum of the scores for each individual during that observation.


Important notes:
* The table "phenophases.txt" show the frequencies of each phenophase through the year. It is used just for Circular analyses.
* The tables "leaf_flushing.txt", "flower_budding.txt", "flowering.txt", "unripe_fruits.txt" and "ripe_fruits.txt" show the frequencies of the intensity of each phenophase thought the year. These tables are meant to create the graphs.
* The table "data_log.txt" has the environmental variables log transformed in order to reduce the impact of different scales in the analyses.
* The tables "pheno_comp1" and "phenocomp2" represent the same data, however the second is represented by percentage to create the graphs.

Summary
  - tables to be used in the Circular analyses of seasonality (.txt extension with the names of the phenophases)
  - script with Circular analyses and graphs concerning L. lucidum phenology (phenology_Exp.R)
  - graphs of each phenophase (.pdf extension with the names of the phenophases)
  - table with the Fournier Index of each phenophase and the environmental variables from September 2015 to August 2017 (data_log.txt)
  - "data_log.txt" guide (data_log_guide.txt)
  - script with the model analyses for each phenophase (pheno_model.R)
  - tables with the occurence of reproductive phenophases of L. luciudum, NAF and UAF (pheno_comp1 and pheno_comp2.txt)
  - script with Circular analyses, tests and graphs concerning the reproductive phenology of L. lucidum and Araucaria Forest native areas (phenology_test.R)
  - graphs with the flowering period comparison between L. lucidum (purple) and Araucaria forest native areas (urban - orange (UxL_fl.pdf)/ countryside - green (NxL_fl.pdf)
  - graphs with the fruiting period comparison between L. lucidum (purple) and Araucaria forest native areas (urban - orange (UxL_fr.pdf)/ countryside - green (NxL_fr.pdf)  
  - tables with the analyses of the results (analyses_result_table.pdf)

### Main conclusions:
  * All analyzed phenophases exhibit a seasonal pattern.
  * Day length from the current, and previous months are the main environmental variables affecting Ligustrum's phenology.
  * Ligustrum lucidum flowering and fruting periods differ from the ones found for Araucaria Forest native areas
