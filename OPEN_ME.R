

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              Run the Dashboard                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xfun::Rscript_call(
  rmarkdown::render,
  list(
    input = 'kpi_dashboard.Rmd', 
       #output_format = 'flex_dashboard',
       output_file = glue::glue('Kpi Dashboard - {lubridate::today()}'),
       output_dir = 'output_dashboard'
    )
)
