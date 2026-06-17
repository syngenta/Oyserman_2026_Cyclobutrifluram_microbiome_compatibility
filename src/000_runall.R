#!/usr/bin/env Rscript

SOURCE_FILES_PATH <- "./src"

argv <- commandArgs(trailingOnly = TRUE)
if(length(argv) >= 1) {
	SOURCE_FILES_PATH <- argv[1]
}

files_to_run <- list.files(
	path = SOURCE_FILES_PATH,
	pattern = r"(^fig\-.*\.R$)",
	full.names = TRUE,
	ignore.case = TRUE
)


for(i in seq_along(files_to_run)) {
	write(sprintf("[%d/%d] Running file '%s' ...", i, length(files_to_run), files_to_run[i]), file = stderr())
	src_env <- new.env(parent = .GlobalEnv)
	source(
		file = files_to_run[i],
		local = src_env,
		echo = FALSE
	)
}
