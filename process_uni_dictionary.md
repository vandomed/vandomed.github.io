## Data dictionary for variables produced by **accelerometry** and **nhanesaccel** packages.

Table 1. Data dictionary for variables produced by **process_uni** function in *accelerometry* package.

Variable name      | brevity | Description                                                
-------------------|---------|------------------------------------------------------------
id                 | 1+	     | Participant ID number.                                     
day                | 1+	     | Day of week (1 = Sunday, …, 7 = Saturday).                 
valid_day	         | 1+	     | 1 if day is considered valid for analysis, otherwise 0.    
valid_days	       | 1+	     | Number of valid days. Only included in per-person summary. 
valid_week_days	   | 1+	     | Number of valid weekdays. Only included in per-person summary.
valid_weekend_days | 1+	     | Number of valid weekend days. Only included in per-person summary.
include            | 1+	     | 1 if participant is deemed to have valid data for analysis, otherwise 0.
valid_min          | 1+	     | Number of minutes classified as valid wear time.
counts	           | 1+	     | Total counts accumulated during wear time minutes.
cpm	               | 1+	     | counts/valid_min.
steps	             | 2+	     | Total steps accumulated during weartime minutes.
sed_min	           | 2+	     | Sedentary minutes, or minutes with counts < int_cuts[1]. 
light_min          | 2+	     | Light intensity minutes, or minutes with int.cuts[1] <= counts < int.cuts[2].
life_min	         | 2+      | Lifestyle intensity minutes, or minutse with int.cuts[2] <= counts < int.cuts[3].
mod_min	           | 2+      | Moderate intensity minutes, or minutes with int.cuts[3] <= counts < int.cuts[4].
vig_min	           | 2+	     | Vigorous intensity minutes, or minutes with counts >= int.cuts[4].
lightlife_min	     | 2+	     | Light-to-lifestyle intensity minutes.
mvpa_min	         | 2+	     | Moderate-to-vigorous intensity minutes.
active_min	       | 2+	     | Active (i.e. non-sedentary) minutes.
sed_percent	       | 2+	     | sed_min/valid_min.
light_percent	     | 2+	     | light_min/valid_min.
life_percent	     | 2+	     | life_min/valid_min.
