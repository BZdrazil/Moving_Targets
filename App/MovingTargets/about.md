   
   
This application accompanies [Moving Targets: Monitoring Target Trends in Drug Discovery by Mapping Targets, GO Terms, and Diseases](https://www.biorxiv.org/content/10.1101/691550v1) by Zdrazil et al. The work attempts to characterize trends in attention for targets and diseases based on publication of bioactivity data in ChEMBL. The disease trends are imputed from the targets associated with the disease.

### Availability

The application is based on ChEMBL 25 and the source code and datasets for the application is available at [https://github.com/BZdrazil/Moving_Targets/App](https://github.com/BZdrazil/Moving_Targets/App)

### Frequently Asked Questions

* _How is the `Other` group defined in the barcharts?_ Within a given year, if a category (target family, target class or GO BP term) occurs less than 20% of the category with the maximum percentage, it is reassigned to te `Other` class.

* _What do `% Disease Bioactivities` or `% GO BP Bioactivies` mean?_ In the stacked barcharts, the percentage is computed within a year and the selected term (disease of GO BP). This is in contrast to the line charts, where the percentage is computed with respect to all entities (target, disease or GO BP terms) associated with that year

### Contact

For questions about the Shiny application contact [Rajarshi Guha](mailto:rajarshi.guha@gmail.com). For questions about the methodology contact [Barbara Zdrazil](mailto:barbara.zdrazil@univie.ac.at)