default: install

check:
    #!/usr/bin/env -S Rscript --vanilla
    devtools::check(error_on = "error")

doc:
    #!/usr/bin/env -S Rscript --vanilla
    devtools::document(roclets = c('rd', 'collate', 'namespace'))

install: doc
    #!/usr/bin/env -S Rscript --vanilla
    devtools::install(
      build_vignettes = TRUE, build = TRUE, force = TRUE, upgrade = "never"
    )

test:
    #!/usr/bin/env -S Rscript --vanilla
    devtools::test()

update_data:
    for f in data-raw/*; do echo $f; Rscript --vanilla $f; done

update_data_and_check: update_data check
