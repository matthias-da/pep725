---
title: 'pep725: An R package for Pan-European Phenological Data Analysis'
tags:
  - R
  - phenology
  - climate change
  - ecology
  - spatial analysis
  - environmental data
authors:
  - name: Barbara Templ
    orcid: 0000-0002-9391-5888
    affiliation: "1"
    equal-contrib: true
  - name: Matthias Templ
    orcid: 0000-0002-8638-5276
    affiliation: "2"
    equal-contrib: true
    corresponding: true
affiliations:
 - name: Swiss Federal Institute for Forest, Snow and Landscape Research (WSL), Switzerland
   index: 1
   ror: 04d81q302
 - name: University of Applied Sciences Northwestern Switzerland (FHNW), Switzerland
   index: 2
   ror: 02gz82p86
date: 14 February 2026

bibliography: paper.bib
---

# Summary

Phenological observations are a key source of evidence for biological responses to climate variability and change [@parmesan2006ecological; @thackeray2016phenological; @piao2019phenology]. The Pan European Phenology database [PEP725; @templ2018pep725; @templ2026pep725] is an open-access infrastructure for plant phenology data, unifying more than 13 million observations from over 30 countries, spanning the period from 1868 to the present and covering approximately 265 plant species and 46 phenophases. While these datasets offer exceptional scientific value, their analysis is challenged by uneven spatial coverage, heterogeneous data quality and temporal gaps.

The **pep725** R package provides a coherent framework for the spatio-temporal analysis of phenological data from PEP725 and similar ground-based datasets. It supports systematic data quality diagnostics, the computation of phenological normals and anomalies, the estimation of spatial gradients along environmental axes, the assessment of phenological synchrony across space and time, and both interactive and publication-ready spatial visualization. Additional capabilities include partial least squares (PLS) regression for identifying temperature-sensitive periods affecting phenological timing, sequential Mann-Kendall analysis for detecting trend turning points, and detection of second flowering or repeated phenological events linked to climate anomalies. The package emphasizes reproducibility and transparency and is designed to complement phenological modeling and remote-sensing tools by enabling a phenology-first exploration of spatial structure and variability in large, heterogeneous observation networks.

# Statement of need

Ground-based phenological datasets pose distinct analytical challenges: observation density varies across regions, long-term records are often incomplete, and data quality differs among contributors and time periods. Without explicit diagnostics and spatial characterization, such heterogeneity can bias spatial comparisons, trend analyses, and downstream model calibration. The **pep725** package addresses these challenges by providing a dedicated framework for characterizing station-based phenological datasets prior to modeling, separating data diagnostics from downstream inference to enable more robust and reproducible analyses.


# State of the field

Phenological analysis in R is supported by packages addressing process-based modeling [e.g. **chillR**, @luedeling2012chillr; **DyMEP**, @tschurr2025dymep; **phenolocrop**, @taniguchi2025phenolocrop], statistical estimation of phenological timing [**phenesse**, @belitz2025phenesse; **phenology**, @girondot2010phenology; **pheno**, @schaber2026pheno; **spphpr**, @shi2017spphpr; **sephora**, @gomez2024sephora], and remote-sensing-based phenology [**npphen**, @chavez2023npphen; **phenex**, @lange2017phenex; **phenomap**, @zhang2020phenomap; **phenor**, @hufkens2018phenor]. These tools are essential for prediction and process-based understanding but typically emphasize modeling outcomes rather than the spatial structure and reliability of observation networks.

Despite this rich ecosystem, few tools focus on quality-aware spatial analysis of ground-based phenological observation networks. Large databases such as PEP725 combine millions of records collected across heterogeneous environments and national protocols [@templ2018pep725; @templ2026pep725], requiring explicit handling of data quality issues before spatial patterns can be interpreted reliably. Building on the PEP725 infrastructure and complementary to modeling-oriented tools, **pep725** enables systematic diagnostics, estimation of spatial gradients, and quantification of phenological synchrony—key derived quantities in contemporary phenological research [e.g. @liu2016temperature].


# Software design

The design of **pep725** is guided by three principles: (i) explicit treatment of data quality and uncertainty, (ii) transparent and reproducible spatial analysis workflows, and (iii) modular integration with the broader R ecosystem.

The package implements an object-oriented design using S3 classes (\autoref{fig:workflow}). Data enter the pipeline through `pep_import()`, `pep_download()`, or `as.pep()` and are stored as a `pep` object—an extended `data.table` with validated spatial and temporal attributes. Class-preserving subsetting (`pep[...]`) allows filtering by species, region, phase, or time period before analysis. From there, functions branch into quality diagnostics (e.g. `pep_quality()`, `flag_outliers()`) and analytical methods (e.g. `pheno_gradient()`, `pheno_synchrony()`), all returning structured S3 objects with dedicated `print`, `summary`, and `plot` methods. Gradient and synchrony analyses support robust estimation (including `lmrob` and quantile regression) to reduce sensitivity to outliers and heterogeneous sampling. \autoref{fig:quality} shows an example of the `pep_quality()` output for apple flowering stations in Austria and Switzerland. Four vignettes provide reproducible workflows covering data access, phenological analysis, spatial patterns, and data quality assessment.

![Package workflow. Data access functions produce a `pep` object that supports class-preserving subsetting. Quality diagnostics and analysis functions accept `pep` input and return S3 objects with `print()`, `summary()`, and `plot()` methods. Interactive mapping is provided by `map_pep()` and `leaflet_pep()`.\label{fig:workflow}](figures/workflow_diagram.png){ width=95% }



# Example usage

The following example illustrates the quality-first workflow that **pep725** is designed to support: assess station-level data quality before further analysis.

```r
library(pep725)
pep <- pep_download()

# Alpine apple flowering
apple <- pep[species == "Malus domestica" &
             country %in% c("Switzerland", "Austria")]

# Grade each station's record quality (A-D)
quality <- pep_quality(apple, by = c("s_id", "phase_id"))
summary(quality)
plot(quality, which = "overview", pep = apple)  # cf. Fig. 1
```

![Data quality overview for apple flowering stations in Austria and Switzerland. Left: distribution of quality grades (A = best, D = poorest) across 6,394 station-phase combinations. Right: geographic distribution of 1,402 stations colored by quality grade.\label{fig:quality}](figures/quality_overview.png){ width=95% }


# Research impact statement

The **pep725** package has been developed in close connection with the PEP725 database, which has contributed to more than 115 peer-reviewed publications, including 17 in *Nature* and 2 in *Science* [@templ2026pep725]. These studies address phenological questions that the package's analytical functions directly support: climate sensitivity analyses such as @fu2015declining, who demonstrated declining warming effects on spring leaf unfolding at 1,245 PEP725 sites across Europe; and pan-European trend detection as in @menzel2020climate, who identified attributable climate change fingerprints in 97,000 PEP725 time series spanning 1951--2018.

By implementing the methods described in @templ2026pep725 as reproducible R workflows with explicit quality diagnostics, **pep725** lowers the barrier to conducting such analyses on large phenological observation networks. Four vignettes support both research applications and teaching in phenology and environmental data analysis.


# AI usage disclosure

AI-based tools were used as an editorial aid for language polishing and structural improvements in parts of the documentation. All scientific concepts, methodological design, software architecture, implementation, and validation were conceived, developed, and verified by the authors.


# Acknowledgements

We acknowledge the national phenological services and numerous observers contributing to the PEP725 database. We also thank members of the phenology and climate-impact research community for feedback during the development of this package. Dr. Barbara Templ was supported by the Berner Nachwuchsförderung (BNF) program of the University of Bern for developing the **pep725** R package. We thank Dr. Yann Vitasse for hosting and supporting this work at the Swiss Federal Institute for Forest, Snow and Landscape Research (WSL), which provided an excellent research environment for conducting parts of the development and analysis.

# References
