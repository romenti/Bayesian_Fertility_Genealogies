The folder contains the following data files

- **gen_population_pyramids.RData** contains age- and sex-specific population counts from FamiLinx and (upon availability) from more reliable data sources
- **historical_TFR.RData** contains historical TFR values for all countries under analysis derived from various sources (Human Fertility Collection, studies in historical demography etc.)
- **motivation_datasets.RData** contains the data sets reporting the number of children in FamiLinx with missing maternal ages
- **indirect_estimation_data_input.RData** contains data needed to implement the indirect estimation methods
  - Annual Child-Woman ratios from FamiLinx for all countries over the period 1751-1910
  - 'True' Child-Woman ratios from more reliable sources for all countries upon data availability
  - Annual probability of death for children under 5 for all countries over the period 1751-1910
- **model_data_input.RData** contains data needed to implement the Bayesian model
  - input data to run the model
  - initial values for all the model parameters
  - list of constants to define the range of the indices within the Nimble model




