#!/usr/bin/env Rscript

# setup ---------------------------------------------------------------------------------------
suppressPackageStartupMessages({
	library(tidyverse)
	library(ggpubr)
})

source("./src/init.R", echo = FALSE, local = TRUE)


# stats ---------------------------------------------------------------------------------------
# ANOVA
aov(percent_germinated ~ treatments, data = spore_germination_df) |>
	summary()

# Dunnett's letter test
dunnett <- DescTools::DunnettTest(
	formula = percent_germinated ~ name_format,
	data = spore_germination_df,
	control = "control",
	conf.level = 0.95
)

dunnett_df <- as.data.frame(dunnett$control) |>
	rownames_to_column("name") |>
	mutate(
		name = str_split_i(name, "-", 1),
		stars = gtools::stars.pval(pval),
		pval.format = if_else(
			pval < 0.001,
			"italic(P) < 0.001",
			sprintf("italic(P) == \"%.3f\"", pval)
		)
	 )


# plotting ------------------------------------------------------------------------------------
p_germ_rate <- spore_germination_df |>
	ggbarplot(
		x = "name_format",
		y = "percent_germinated",
		fill = "technology",
		add = "mean_se",
		error.plot = "linerange"
	) +
	geom_text(
		data = dunnett_df,
		inherit.aes = FALSE,
		aes(x = name, y = 0.95, label = pval.format),
		parse = TRUE,
		size = 5,
		size.unit = "pt"
	) +
	scale_x_discrete(guide = guide_axis(angle = 20), labels = scales::label_math(.x ~ "mg kg"^-1)) +
	scale_y_continuous(limits = c(0,1), labels = scales::label_percent(scale = 100), expand = c(0,0,0.05,0)) +
	mytheme(brewer_pal = NULL) +
	theme(
		plot.margin = unit(c(0, 0, 0, 5), "mm")
	) +
	labs(
		title = "Spore germination rate",
		subtitle = "Mean germination rate per treatment + SE + Dunnett's test",
		x = NULL,
		y = "Spore germination rate"
	); p_germ_rate


ggsave_springer(
	plot = p_germ_rate,
	filename = "Fig1D.eps",
	height = 70
)
