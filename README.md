

<div align='center'>
    <a href='https://tula.org'><img height='75px' src=docs/logos/tula-logo.png /></a>
    &nbsp;&nbsp;&nbsp;&nbsp;
    <a href='https://hakai.org'><img height='75px' src=docs/logos/hakai-logo.png /></a>
</div>

# Hakai Institute Nearshore Program - Macrocystis kelp canopy productivity data from BC Central Coast, v1.4.0

The macrocystis canopy productivity dataset is a component of the Hakai Institute Nearshore research and monitoring program. This dataset documents seasonal and annual changes in Macrocystis pyrifera biomass, density, productivity and reproduction at multiple sites on the Central Coast of British Columbia during the primary growing season (April to October). From 2014-2018, density and morphometric measurements of kelps in situ were quantified within three to five permanent monitoring plots to determine kelp size structure, growth rates and loss rates throughout the growing season . Additional sites are visited annually (ongoing since 2014) to estimate annual change in biomass at these locations. Outcomes of this research show that biomass and productivity of M. pyrifera varies spatially and temporally. Based on these data we can determine which field parameters best correlate with overall M. pyrifera productivity and biomass, and ultimately, refine metrics for long-term assessments of this canopy kelp status on BC’s coast and find what environment drivers are most important to its density, spread and health. 

 This data package is freely available to everyone, following the principles of equitable access and benefit sharing. However, we expect all data users to give attribution to the data providers (read our data license) and the use of these data should happen in the light of fair use, i.e.: 1) respect the data providers, and provide helpful feedback on data quality, and 2) communicate and/or collaborate with the providers if you are considering using this dataset for manuscripts or other forms of reporting.

This data package contains the following: 

- Survey data documents:
  1. Underwater frond density per Macrocystis pyrifera plant along transects within each plot (at a site )inter- and intra-annually (macro_density.csv)
  2.Underwater size measurements of M. pyrifera plants along transects within each plot (at a site) inter- and intra-annually (macro_metrics.csv)
  3. Above water measurements taken on harvested M. pyrifera plants, at sites outside of plots inter-annually (macro_harvest.csv)
Dried M. pyrifera tissue (macro_wetdry.csv)
  4. Biomass estimates by plot/site based on calculations found in the frond_count_biomass_estimates.R script (macro_biomass_plot.csv)
  5. Regression coefficients for important morphometric relationships by site or region (plant_weight_site_coeff)

- Scripts: 
  1. Calculations for estimating overall biomass per m2 at each site (within a plot) based on frond count per plant  (frond_count_biomass_estimates.R)
  2. Calculations for estimating overall biomass per m2 at each site (within a plot) based on cumulative frond length per plant and method comparison (plant_length_biomass_estimates.R)

- Protocols: Description of field survey methods and site information. (Protocols.pdf)
- Variables: Description of all variables contained in this package. (Data_dictionary.csv)
- Package Changes: Changelog for additions and changes to this data package. (Changelog.txt)

- Licensing: This work is licensed under the Creative Commons Attribution 4.0 International License (CC-BY), for a copy of this license, see LICENSE.txt. Please attribute material in this data package as :


```
 Pontier, O., Krumhansl, K., and Hessing-Lewis, M.. Macrocystis kelp canopy productivity data from BC Central Coast, v1.4.0 [Data accesed]. Hakai Institute dataset. https://doi.org/10.21966/k88j-1k50
```


Link to any associated resources:

- Previous versions are archived [here](https://drive.google.com/drive/folders/1_w-ruJOlgZy3nC4GeozNiPs6rM5vBCnb)
- Data Management Plan
- CIOOS CKAN record
- ERDDAP Dataset
- External Data Repositories
- Publications


## How to contribute

To contribute to the code or contents, please fork this repository, make your suggested modifications and generate a pull request (PR). For any issues related to the code or contents, please create an issue in this repository. A good issue sufficiently outlines the problem, and where possible suggests a solution. Please give the owners of this repository ample time to review issues or suggestions. Finally, for general inquires related to the data, please contact the data provider as listed in the referenced metadata record in the Hakai Catalogue. 

*This repository is generated via the [Hakai dataset repository template](https://github.com/HakaiInstitute/hakai-dataset-repository-template)*

