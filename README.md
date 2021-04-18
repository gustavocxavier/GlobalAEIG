

Overview
--------

This repository contains code to create the dataset for my work in progress called `Global Aggregate Expected Investment Growth`, which intend to analyze, across several countries, how the aggregate investment plans, conditioned to different life-cycle stages, relates to future wide market returns of each country. To reproduce, you can either clone this repository with git, or download the entire content as a zip file by clicking on the `Download ZIP` button on `Code` menu above. 

How to Run the Code
-------------------
Open `start.R` in your R studio. The code creates some subfolders where it will store temporary data for each step, so that you don't have to run the entire
code more than once. 

CORE PRINCIPLE:
--------------
1. Never modify 0_data
2. Only save/load to "pipeline" folder

Additional notes
----------------
- I structured this code based on [Ties de Tok tips](https://arc.eaa-online.org/blog/how-keep-your-projects-organized-part-1-folder-structure)
