testall:
	Rscript -e "tinytest::build_install_test(ncpu = 8)"

testone:
	Rscript -e "pkgload::load_all();tinytest::run_test_file('$(testfile)')"

check:
	Rscript -e "devtools::check()"

deploy:
	Rscript -e "pkgdown::deploy_to_branch()"