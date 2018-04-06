# Adapted from: The Travelling Salesman Portrait by Antonio Sánchez Chinchón
# proj: https://github.com/aschinchon/travelling-salesman-portrait
# src: https://github.com/aschinchon/travelling-salesman-portrait/blob/master/frankenstein_TSP.R

# load libs
library(imager)
suppressPackageStartupMessages(library(tidyverse))
library(scales)
library(TSP)

# load local image
file = here::here("archness", "lana", "lanakane.jpg")

# Load, convert to grayscale, filter image (to convert it to bw) and sample
load.image(file) %>%
  grayscale() %>%
  threshold("45%") %>%
  as.cimg() %>%
  as.data.frame()  %>%
  sample_n(8000, weight=(1-value)) %>%
  select(x,y) -> data

# Compute distances and solve TSP (it may take a minute)
as.TSP(dist(data)) %>%
  solve_TSP(method = "arbitrary_insertion") %>%
  as.integer() -> solution

# Create a dataframe with the output of TSP
data.frame(id=solution) %>%
  mutate(order=row_number()) -> order

# Rearrange the original points according the TSP output
data %>%
  mutate(id=row_number()) %>%
  inner_join(order, by="id") %>% arrange(order) %>%
  select(x,y) -> data_to_plot

# A little bit of ggplot to plot results
ggplot(data_to_plot, aes(x,y)) +
  geom_path() +
  scale_y_continuous(trans=reverse_trans())+
  coord_fixed()+
  theme_void()

# Do you like the result? Save it! (Change the filename if you want)
ggsave(here::here("archness", "lana", "lanaTSP.png"), dpi=600, width = 4, height = 5)

# save data for transformer
write_csv(data_to_plot, here::here("archness", "lana", "data_to_plot.csv"))
write_csv(order, here::here("archness", "lana", "order.csv"))
