library(sf)
library(tidyverse)
data(td_single_input)

## Set up
exp_plots <- td_single_input$exp_plots[[1]]
field_sf <- td_single_input$field_sf

## Union approach
uni_plots <- st_union(exp_plots$geometry)

ggplot() +
  geom_sf(data = field_sf) +
  geom_sf(data = uni_plots, fill = "blue")

temp <- 
  st_difference(field_sf, uni_plots) %>%
  st_cast("POLYGON") %>%
  st_as_sf() %>%
  mutate(area = st_area(.)) %>%
  arrange(desc(area)) %>%
  slice(1)
  
ggplot(temp) +
  geom_sf(fill = NA)

## One by one

i = 1
headland <- field_sf

for (i in 1:nrow(exp_plots)) {
for (i in 1:10) {
  headland <- 
    st_difference(headland, exp_plots[i, ])
}


ggplot(list(headland[[1]][[3]]) %>% 
  st_polygon()) +
  geom_sf(fill = "blue")

headland[[1]][[1]]
headland[[1]][[2]]
headland[[1]][[3]]
headland[[1]][[4]]
headland[[1]][[5]]
