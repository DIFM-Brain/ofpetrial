# ofpetrial 0.1.1

+ option of using leaflet to see experimental plots on a satellite image of the field was added to `viz()` 
+ `create_strips()` (an internal function) was overhauled to pass R CMD check on Fedora with ATLAS. Specifically, `st_tilt()`, which used to be used in the function, has been abandoned.
+ fixed the bug in `viz()` which presented `gc_rate`

# ofpetrial 0.1

The first complete set of functions that make the package functional and useful for the users to design and implement on-farm precision experiments.

## Prepare plot and rate information 

- prep_plot()
- prep_rate()

## Generate trial designs

- make_exp_plots()
- assign_rates()

## Modify input rates

- add_blocks()
- change_rates()

## Diagnose trial design

- check_ortho_inputs()
- check_ortho_with_chars()
- check_alignment()

## Visualization
- viz()

## Make trial design reports
- make_trial_report()

## Write out files
- write_trial_files()

## Data
- plot_info
- exp_data
- rate_info
- td_single_input
- td_two_input
- td_curved
