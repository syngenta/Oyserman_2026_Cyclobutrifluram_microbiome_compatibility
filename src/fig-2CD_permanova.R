#!/usr/bin/env Rscript

# setup ---------------------------------------------------------------------------------------
suppressPackageStartupMessages({
	library(tidyverse)
	library(ggpubr)
	library(phyloseq)
})

source("./src/init.R", echo = FALSE, local = TRUE)


# data loading --------------------------------------------------------------------------------
ps <- load_phylo("16S")


# PERMANOVA -----------------------------------------------------------------------------------
permanova_df <- vegan::adonis2(
	formula = distance(ps$phyloseq, method = "bray") ~ Soil * Crop * Treatment,
	data = ps$metadata,
	by = "terms"
) |>
	rownames_to_column("Interaction") |>
	mutate(
		SumOfSqs = round(SumOfSqs, 2),
		`R2` = round(`R2`, 2),
		`F` = round(`F`, 1),
		Sig = gtools::stars.pval(`Pr(>F)`)
	)

permanova_df |>
	writexl::write_xlsx(path = file.path(RESULTS_DIR, sprintf("permanova_%s.xlsx", ps$target)))


cairo_ps(
	filename = file.path(RESULTS_DIR, "eps", sprintf("Fig2CD_%s.eps", ps$target)),
	width = 75 /25.6,
	height = 50 /25.6,
	fallback_resolution = 1200
)

permanova_gg <- permanova_df %>%
	replace(., is.na(.), "---") |>
	select(-Interaction) |>
	gridExtra::tableGrob(
		rows = permanova_df$Interaction,
		cols = c("D.f.", "SSQ", "R^2", "F", "P(>F)", ""),
		theme = gridExtra::ttheme_minimal(
			base_size = 5,
			base_family = "Arial",
			padding = unit(c(2, 3), "pt"),
			colhead = list(
				fg_params = list(
					hjust = 0,
					x = 0
				)
			),
			core = list(
				fg_params = list(
					hjust = 0,
					x = 0.1
				)
			)
		)
	) |>
	gtable::gtable_add_grob(  # add header line
		grobs = grid::segmentsGrob(gp = grid::gpar(lwd=2), x0=0, y0=0, x1=1, y1=0),
		t = 1, b = 1, l = 1, r = ncol(permanova_df)
	) |>
	gtable::gtable_add_grob(  # add row names line
		grobs = grid::segmentsGrob(gp = grid::gpar(lwd=1), x0=1, y0=0, x1=1, y1=1),
		t = 1, b = nrow(permanova_df)+1, l = 1, r = 1
	); grid::grid.newpage(); grid::grid.draw(permanova_gg)

dev.off()
