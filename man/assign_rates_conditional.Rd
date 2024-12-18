% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign_rates.R
\name{assign_rates_conditional}
\alias{assign_rates_conditional}
\title{Assign rates to the plots of experimental plots for a single input based on existing trial designs created by assign_rates()}
\usage{
assign_rates_conditional(exp_data, rate_info, existing_design)
}
\arguments{
\item{exp_data}{experiment plots created by make_exp_plots()}

\item{rate_info}{rate information created by prep_rate()}

\item{existing_design}{trial design of another input created with assign_rates()}
}
\value{
trial design as sf (experiment plots with rates assigned)
}
\description{
This functions assign input rates for the plots created by make_exp_plots() for a single input according to the rate design specified by the user in rate_info. It assigns rates to the input so that the resulting design avoids significant correlation with the rate of another input specified as existing_design.
}
\examples{
#--- load experiment plots made by make_exp_plots() ---#
data(td_single_input)
exp_data

seed_plot_info <-
 prep_plot(
   input_name = "seed",
   unit_system = "imperial",
   machine_width = 60,
   section_num = 24,
   harvester_width = 30,
   plot_width = 30
 )

exp_data <-
 make_exp_plots(
   input_plot_info = seed_plot_info,
   boundary_data = system.file("extdata", "boundary-simple1.shp", package = "ofpetrial"),
   abline_data = system.file("extdata", "ab-line-simple1.shp", package = "ofpetrial"),
   abline_type = "free"
 )

seed_rate_info <-
 prep_rate(
   plot_info = seed_plot_info,
   gc_rate = 32000,
   unit = "seed",
   min_rate = 16000,
   max_rate = 40000,
   num_rates = 5,
   design_type = "ls"
 )

assign_rates_conditional(
 exp_data = exp_data, 
 rate_info = seed_rate_info, 
 existing_design = td_single_input
)
}
