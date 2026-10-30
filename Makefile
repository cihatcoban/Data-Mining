.PHONY: setup lint test run render clean

R ?= Rscript

setup:
	$(R) scripts/install_deps.R

lint:
	$(R) -e "if (!requireNamespace('lintr', quietly = TRUE)) stop('Install lintr first'); lintr::lint_dir('R'); lintr::lint_dir('scripts'); lintr::lint_dir('tests')"

test:
	$(R) -e "if (!requireNamespace('testthat', quietly = TRUE)) stop('Install testthat first'); for (f in list.files('R', full.names = TRUE)) source(f); testthat::test_dir('tests/testthat')"

run:
	$(R) scripts/run_carseats_pipeline.R

render:
	$(R) -e "for (nb in list.files('notebooks', pattern = '[.]Rmd$$', full.names = TRUE)) rmarkdown::render(nb)"

clean:
	rm -f notebooks/*.html notebooks/*.nb.html .Rhistory .RData
	rm -rf notebooks/*_files notebooks/*_cache outputs/plots/*.png
