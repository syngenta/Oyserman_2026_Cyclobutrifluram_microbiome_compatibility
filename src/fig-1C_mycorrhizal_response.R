#!/usr/bin/env Rscript

# setup ---------------------------------------------------------------------------------------
suppressPackageStartupMessages({
	library(tidyverse)
	library(ggpubr)
	library(foreach)
})

source("./src/init.R", echo = FALSE, local = TRUE)


# ANOVA stats ---------------------------------------------------------------------------------
summary(aov(
	DSW ~ amf_pretty * cyc_pretty,
	data = filter(mycorrhizal_response_df, experiment == "A")
))

summary(aov(
	DSW ~ amf_pretty * cyc_pretty,
	data = filter(mycorrhizal_response_df, experiment == "B")
))


# plot DSW vs treatment -----------------------------------------------------------------------

print(shapiro.test(mycorrhizal_response_df$DSW))

treat_aov <- aov(DSW ~ experiment : amf_pretty : cyc_pretty, data = mycorrhizal_response_df)
print(summary(treat_aov))

thsd <- TukeyHSD(treat_aov)
multcomp_groups <- multcompView::multcompLetters4(treat_aov, thsd, Letters = LETTERS)

signif_groups <- data.frame(
	signif_group = multcomp_groups[[length(multcomp_groups)]]$Letters
) |>
	rownames_to_column("fit_interaction")


mycorrhizal_response_df |>
	mutate(fit_interaction = paste(experiment, amf_pretty, cyc_pretty, sep = ":")) |>
	left_join(signif_groups, by = "fit_interaction") |>
	ggplot(aes(x = treatment, y = DSW, fill = signif_group)) +
	geom_boxplot(color = "black", width = 0.8, position = "dodge") +
	stat_mean(  # bit of a hack; draws only once per group instead of per record
		geom = "label",
		aes(y = max(DSW, na.rm = TRUE)*1.1, label = signif_group),
		show.legend = FALSE,
		vjust = 1,
		hjust = 0.5,
		fontface = "bold",
		size = 6,
		size.unit = "pt",
		color = "black",
		fill = "#FFFFFFC0"
	) +
	facet_grid(cols = vars(experiment), labeller = "label_both") +
	mytheme(brewer_pal = NULL) +
	theme(panel.spacing = unit(4, "mm")) +
	scale_y_continuous(
		limits = c(0, NA),
		labels = scales::label_number(accuracy = 1, scale = 1, scale_cut = scales::cut_si("g"))
	) +
	labs(
		x = NULL,
		y = "Shoot dry weight (SDW)",
		fill = "cyclobutrifluram"
	)


ggsave_springer(
	filename = "Fig1C.eps",
	height = 60,
)
