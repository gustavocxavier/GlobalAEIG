Global Aggregate Expected Investment Growth


Open start.R in your R studio

CORE PRINCIPLE:
--------------
1. Never modify 0_data
2. Only save to "pipeline" folder
3. Only load from "2_pipeline/2_out" folder or "0_data"
4. Anything in "2_pipeline/0_tmp" can be deleted

Anout 2_pipeline:
----------------
-> 0_tmp contains files that I save for inspection purposes or some other temporary reason. The basic principle is that we should not have to worry about anything in the tmp folder being deleted as it only serves a temporary purpose.

-> 1_store contains files that I save with the intention of loading them in the current code file. This is, for example, used in scenarios where it takes a while to run parts of your code and to avoid having to re-run these parts every time I might want to intermittently save the progress of your generated data to the store folder.

-> 2_out contains files that I save with the intention of loading them in a future code file. These are usually the “end-products” of the current code file.

--- /// ---

Based on Ties de Tok tips: https://arc.eaa-online.org/blog/how-keep-your-projects-organized-part-1-folder-structure
