check: update_data
    Rscript --vanilla -e 'devtools::check()'

update_data:
    for f in data-raw/*; do echo $f; Rscript --vanilla $f; done
