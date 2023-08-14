library(tidyverse)
library(gtsummary)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Customization of `tbl_summary()`

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir))


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing")


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		sleep_wkdy ~ "Sleep Schedule Weekdays",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")

tbl_summary(
	nlsy,
	by = sex_cat,
	include=c(starts_with("region","race","income","sleep")),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		income_cat ~ "Income",
		sleep_wkdy_cat ~ "Sleep during Weekdays",
		sleep_wknd_cat ~ "Sleep during Weekends",
		region_cat~"Region"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	statistic=list(income_cat()~"{median}({p10},{p90})"
	modify_footnote(update = starts_with("race") ~ https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data) |>

	#answer
		tbl_summary(
			nlsy,
			by = sex_cat,
			include=c(region_cat, race_eth_cat, income, starts_with("sleep")),
			label = list(
				race_eth_cat ~ "Race/ethnicity",
				income ~ "Income",
				sleep_wkdy ~ "Sleep during Weekdays",
				sleep_wknd~ "Sleep during Weekends",
				region_cat~"Region"
			),
			statistic=list(
				income~"10th {p10}, 90th {p90}",
				starts_with("sleep")~"min={min};max={max}"
			),
			digits=list(
				income~c(3, 3),
				starts_with("sleep")~c(1,1)
			)) %>%

	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
		add_overall(col_label = "**Total**") |>
		modify_table_styling(
			columns=label,
			rows=label=="Race/ethnicity",
			footnote="seehttps://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data"
		)
