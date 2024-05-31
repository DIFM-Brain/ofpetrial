get_trial_plot_data <- function(tot_plot_length, min_plot_length, max_plot_length) {
  #* +++++++++++++++++++++++++++++++++++
  #* For debugging
  #* +++++++++++++++++++++++++++++++++++

  # tot_plot_length <- final_exp_plots$tot_plot_length[[5]]
  # min_plot_length <- 73.152
  # max_plot_length <- 91.44

  #* +++++++++++++++++++++++++++++++++++
  #* Main
  #* +++++++++++++++++++++++++++++++++++
  #--- use minimum length to start ---#
  num_comp_plots <- tot_plot_length %/% min_plot_length
  #--- find remainder of available plot length ---#
  remainder <- tot_plot_length %% min_plot_length

  #--- how much additional length can be added to each plot ---#
  additional_length = remainder/num_comp_plots
  if (length(num_comp_plots) == 0 | num_comp_plots == 0) {
    return_data <- NULL
  } else if(additional_length + min_plot_length > max_plot_length){
    #--- in this case there is more length than can be used with max plot length ---#
    #--- thus, we will use the max ---#
    return_data <-
      data.table::data.table(
        plot_id = seq_len(num_comp_plots),
        plot_length = max_plot_length
      )
  } else {
    #--- here the additional length plus the minimum plot length do not exceed the max length ---#
    #--- can divide up the remainder among the plots ---#
    return_data <-
      data.table::data.table(
        plot_id = seq_len(num_comp_plots),
        plot_length = (min_plot_length + additional_length)
      )
  }

  return(return_data)
}
