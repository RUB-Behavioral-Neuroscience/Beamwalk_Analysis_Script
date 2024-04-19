# Beamwalk Analysis Script (BAS)

These scripts are part of the following publication: <https://www.nature.com/articles/s41598-024-59187-0>

![Main Figure][def2]

The beamwalk analysis script (BAS) counts and classifies minor and major hindpaw slips, as well as a more in depth analysis by visualizing the ratio relative to the top of the beam for each tracked body part by DeepLabCut (DLC). The main script automatically detects the correct .csv files for the analysis and generates a main_table containing all trials. The gg_box_plots_after_slip_detection and gg_reverse_ratio_density functions are highly customizable and automatically plot each group and subgroup against each other while also providing p-values that are saved inside an additional data frame. Further useful functions are provided, such as replacing all common special characters with underscores. So these files are worth a look even if you don't want to analyze data with them. So take as much inspiration from it as you want. But be aware, the code most likely does not follow conventional code formatting/linting, as I did not know that was a thing while I was working on it. But I tried to make it as humanly readable as possible. For me, this code is the most readable but I understand that others will have trouble understanding it. So do not hesitate to ask questions on how something works!

![Workflow Diagram][def]

For the best results, download all scripts and then use the according R notebooks (the .Rmd files) to run the matching functions (inside the .r files).

Explanations on how to use the provided functions are found inside the R notebooks.

Proper documentation, some more features (including requested ones), a step-by-step tutorial, bug fixes, as well as some diagrams are also planned.

R (version 4.2.2) in combination with RStudio (version 2023.03.0+386) were used, as well as the following packages: raster (version 3.6-20), photobiology (version 0.10.17), ggplot2 (version 3.4.2), ggpubr (version 0.6.0) and svglite (version 2.1.1)

Feel free to approach me for any kind of feedback!

[def]: https://github.com/RUB-Behavioral-Neuroscience/Beamwalk_Analysis_Script/blob/main/workflow.svg

[def2]: https://media.springernature.com/full/springer-static/image/art%3A10.1038%2Fs41598-024-59187-0/MediaObjects/41598_2024_59187_Fig1_HTML.png
