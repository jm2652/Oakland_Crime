This project seeks to discover and understand crime trends in Oakland, CA using publicly available crime data provided by the Oakland Police Department (OPD).

The project consists of 7 components:

• Data pipeline to download and format the most up-to-date version of OPD's dataset.

• Tools for cleaning and organizing the crime data, which is at times messy and inconsistent.

• Tools for correcting incorrect police beat locations in OPD dataset, and adding additional neighborhood location variable.

• Tools for appending Census demographic information (using Kyle Walker's tidycensus package) to geographic boundary files, to create custom demographic shapefiles.

• Tools for aggregating and standardizing crime dataset to enable analysis.

• Data analysis and visualizations of crime trends, using regression and time series analysis to analyze trends across police beats, neighborhoods, and citywide.

• Spatial analysis tools for visualizing and analyzing changes over time, including clustering (Moran's I statistic), hot spot (Getis-Ord GI statistic) analysis, and spatial regression.
