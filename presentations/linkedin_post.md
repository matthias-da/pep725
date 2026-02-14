# LinkedIn Post for pep725 R Package

---

## Post (ready to copy)

---

🌸 **Excited to announce the pep725 R package for phenological data analysis!**

After years of working with phenological data, we've developed a comprehensive toolkit that makes analyzing Europe's largest phenology database accessible to everyone.

**What is phenology?** The study of seasonal biological events – when plants flower, when leaves fall, when crops are ready for harvest. This data is crucial for understanding climate change impacts on nature.

**What does pep725 offer?**

📊 **Data Access** – Easy import of PEP725 data (12M+ observations, 19,000+ stations across Europe)

📈 **Climate Normals & Anomalies** – WMO-standard 30-year baselines with a single function call

🔍 **Trend Analysis** – Robust Kendall's tau statistics for detecting phenological shifts

🗺️ **Spatial Analysis** – Elevation/latitude gradients and spatial synchrony

✅ **Quality Control** – Outlier detection, completeness assessment, second flowering detection

🎨 **Visualization** – Interactive leaflet maps and publication-ready static maps (no API key needed!)

**Who is this for?**
- Climate scientists studying biological impacts
- Agricultural researchers tracking crop timing
- Ecologists investigating ecosystem changes
- Students learning phenological analysis

**Getting started is simple:**
```r
library(pep725)
pep <- pep_download()
normals <- pheno_normals(pep, period = 1991:2020)
pheno_map(pep, background = "none", color_by = "mean_doy")
```

The package includes four comprehensive vignettes guiding you from basics to advanced spatial analysis.

🔗 GitHub: https://github.com/matthias-da/pep725

Would love to hear from the phenology and climate science community – what features would be most useful for your research?

#RStats #Phenology #ClimateChange #DataScience #OpenScience #Ecology #Agriculture #RPackage #ClimateScience #OpenSource

---

## Shorter Version (for character limits)

---

🌸 **New R package: pep725 – Complete toolkit for phenological data analysis**

Analyze Europe's largest phenology database (12M+ observations) with ease:

✅ Climate normals & anomalies
✅ Robust trend detection
✅ Spatial gradients & synchrony
✅ Quality control & outlier detection
✅ Interactive & static mapping

Perfect for climate scientists, ecologists, and agricultural researchers studying how seasonal timing is shifting.

```r
library(pep725)
pep <- pep_download()
normals <- pheno_normals(pep, period = 1991:2020)
```

🔗 https://github.com/matthias-da/pep725

#RStats #Phenology #ClimateChange #DataScience #OpenScience

---

## Image Suggestions

For the post, consider attaching:
1. `mindpheno_map725.png` – Shows package structure at a glance
2. A map output from `pheno_map()` showing European stations
3. A trend plot showing phenological shifts over time

---

## Posting Tips

1. **Best times to post:** Tuesday-Thursday, 8-10 AM or 5-6 PM (your timezone)
2. **Engage early:** Reply to comments within the first hour
3. **Tag relevant accounts:** Climate research institutes, R community pages
4. **Cross-post:** Consider posting to Twitter/X with the shorter version
