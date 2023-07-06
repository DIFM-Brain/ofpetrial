
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ofpetrial: Design On-farm Precision Experiments

<!-- badges: start -->
<!-- badges: end -->

The `ofpetrial` package allows the user to design agronomic input
experiments in a reproducible manner without using ArcGIS or QGIS.

## Installation

You can install the development version of ofpetrial from
[Github](https://github.com/DIFM-Brain/TrialDesign):

``` r
devtools::install_github("DIFM-Brain/ofpetrial")
```

## Example

Here, we demonstrate how to use the `ofpetrial` package to create
two-input on-farm experiment trial designs.

### Create experimental plots

We start with specifying plot and machine information for inputs using
`prep_plot_fs()`, which simply creates a data.frame of the specified
information with some internal unit conversion of length (feet to
meter).

``` r
seed_plot_info <-
  prep_plot_fs(
    form = "seed",
    machine_width = 60,
    section_num = 24,
    harvester_width = 30,
    plot_width = 30
  )
#> 

seed_plot_info
#>   form machine_width section_num section_width
#> 1 seed        18.288          24         0.762
#>   harvester_width plot_width headland_length side_length
#> 1           9.144      9.144          36.576       9.144
#>   min_plot_length max_plot_length
#> 1          73.152          79.248

n_plot_info <-
  prep_plot_ms(
    form = "NH3",
    machine_width = measurements::conv_unit(60, "ft", "m"),
    section_num = 1,
    harvester_width = measurements::conv_unit(30, "ft", "m"),
    plot_width = measurements::conv_unit(60, "ft", "m")
  )
#> 

n_plot_info
#>   form machine_width section_num section_width
#> 1  NH3        18.288           1        18.288
#>   harvester_width plot_width headland_length side_length
#> 1           9.144     18.288          36.576      18.288
#>   min_plot_length max_plot_length
#> 1          73.152          79.248
```

Now that plot and machine specifications for the inputs are ready, we
can create experiment plots based on them using `make_exp_plots()`.

``` r
input_plot_info <- list(seed_plot_info, n_plot_info)

exp_data <-
  make_exp_plots(
    input_plot_info = input_plot_info,
    boundary_file = system.file("extdata", "boundary-simple1.shp", package = "ofpetrial"),
    abline_file = system.file("extdata", "ab-line-simple1.shp", package = "ofpetrial"),
    abline_type = "free"
  )
```

The experiment plots created by `make_exp_plots()` is stored in
`exp_plots`.

``` r
exp_data$exp_plots
#> [[1]]
#> Simple feature collection with 1209 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 352996.6 ymin: 4331460 xmax: 353368.4 ymax: 4332198
#> Projected CRS: WGS 84 / UTM zone 16N
#> First 10 features:
#>    plot_id strip_id poly_line                       geometry
#> 1        1        1       1_1 POLYGON ((353005.7 4331460,...
#> 2        2        1       1_1 POLYGON ((353006.2 4331483,...
#> 3        3        1       1_1 POLYGON ((353006.7 4331507,...
#> 4        4        1       1_1 POLYGON ((353007.1 4331530,...
#> 5        5        1       1_1 POLYGON ((353007.6 4331553,...
#> 6        6        1       1_1 POLYGON ((353008.1 4331576,...
#> 7        7        1       1_1 POLYGON ((353008.6 4331599,...
#> 8        8        1       1_1 POLYGON ((353009 4331623, 3...
#> 9        9        1       1_1 POLYGON ((353009.5 4331646,...
#> 10      10        1       1_1 POLYGON ((353010 4331669, 3...
#> 
#> [[2]]
#> Simple feature collection with 589 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 352996.6 ymin: 4331460 xmax: 353359.3 ymax: 4332197
#> Projected CRS: WGS 84 / UTM zone 16N
#> First 10 features:
#>    plot_id strip_id poly_line                       geometry
#> 1        1        1       1_1 POLYGON ((353014.8 4331460,...
#> 2        2        1       1_1 POLYGON ((353015.3 4331483,...
#> 3        3        1       1_1 POLYGON ((353015.8 4331507,...
#> 4        4        1       1_1 POLYGON ((353016.3 4331530,...
#> 5        5        1       1_1 POLYGON ((353016.8 4331553,...
#> 6        6        1       1_1 POLYGON ((353017.2 4331576,...
#> 7        7        1       1_1 POLYGON ((353017.7 4331599,...
#> 8        8        1       1_1 POLYGON ((353018.2 4331623,...
#> 9        9        1       1_1 POLYGON ((353018.7 4331646,...
#> 10      10        1       1_1 POLYGON ((353019.2 4331669,...
```

`exp_data$exp_plots` is a list and you can access the individual
experiment plots (an `sf` object) like this.

``` r
exp_data$exp_plots[[1]]
#> Simple feature collection with 1209 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 352996.6 ymin: 4331460 xmax: 353368.4 ymax: 4332198
#> Projected CRS: WGS 84 / UTM zone 16N
#> First 10 features:
#>    plot_id strip_id poly_line                       geometry
#> 1        1        1       1_1 POLYGON ((353005.7 4331460,...
#> 2        2        1       1_1 POLYGON ((353006.2 4331483,...
#> 3        3        1       1_1 POLYGON ((353006.7 4331507,...
#> 4        4        1       1_1 POLYGON ((353007.1 4331530,...
#> 5        5        1       1_1 POLYGON ((353007.6 4331553,...
#> 6        6        1       1_1 POLYGON ((353008.1 4331576,...
#> 7        7        1       1_1 POLYGON ((353008.6 4331599,...
#> 8        8        1       1_1 POLYGON ((353009 4331623, 3...
#> 9        9        1       1_1 POLYGON ((353009.5 4331646,...
#> 10      10        1       1_1 POLYGON ((353010 4331669, 3...
```

We can visualize the layout of the experiment plots using `viz_ep()`.

``` r
viz_ep(exp_data)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

### Assign rates

Let’s now assign input rates to the experimental plots we just created.
Before doing so, we need to prepare rate information for both inputs
using `prep_rates_s()`.

``` r
#!===========================================================
#! Assign rates
#!===========================================================
seed_rate_info <-
  prep_rates_s(
    plot_info = seed_plot_info,
    gc_rate = 32000,
    unit = "seed",
    min_rate = 16000,
    max_rate = 40000,
    num_rates = 5,
    design_type = "jcls"
  )
#> Trial rates were not directly specified, so the trial rates were calculated using min_rate, max_rate, gc_rate, and num_rates

seed_rate_info
#>   form design_type gc_rate unit
#> 1 seed        jcls   32000 seed
#>                                         rates_data
#> 1 16000, 21333, 26667, 32000, 40000, 1, 2, 3, 4, 5
#>   rank_seq_ws rank_seq_as
#> 1        NULL        NULL

n_rate_info <-
  prep_rates_s(
    plot_info = n_plot_info,
    gc_rate = 180,
    unit = "lb",
    rates = c(100, 140, 180, 220, 260),
    design_type = "ls",
    rank_seq_ws = c(5, 4, 3, 2, 1)
  )

n_rate_info
#>   form design_type gc_rate unit
#> 1  NH3          ls     180   lb
#>                               rates_data   rank_seq_ws
#> 1 100, 140, 180, 220, 260, 1, 2, 3, 4, 5 5, 4, 3, 2, 1
#>   rank_seq_as
#> 1        NULL
```

We can now use `assign_rates()` to assign rates to experiment plots.

``` r
trial_design <- assign_rates(exp_data, rate_info = list(seed_rate_info, n_rate_info))
```

Here is the visualization of the trial design done by `viz_td()`.

``` r
viz_td(trial_design)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />
