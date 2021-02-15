
library(blogdown)
library(tidyverse)



blogdown::install_hugo()

blogdown::new_site(theme = "bjacquemet/personal-web", theme_example = TRUE)# en theme se debe colocar el nombre de la plantilla de hugo elegida 


setwd("C:\\Users\\pc\\Desktop\\page")

blogdown::serve_site()

blogdown::stop_server()

