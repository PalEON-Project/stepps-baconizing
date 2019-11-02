## ----load_libraries, echo=FALSE, message=FALSE, warnings = FALSE, results='hide'----
knitr::opts_chunk$set(cache=TRUE)

# Version numbering and other important parameters are stored in a file `settings.yaml`


source('R/load_packages.R')



## ----get_cores, echo=FALSE, message=FALSE, results='hide', warning=FALSE----

source("R/getNAcores.R")
source("R/get_allpublications.R")
source("R/get_allchronologies.R")
source("R/build_newchrons.R", echo = FALSE)
source("R/figures/plot_recal_diffs.R")

all_downloads <- north_american_cores(settings$version)
pubs <- get_allpublications(all_downloads, settings)
chronologies <- get_allchronologies(all_downloads, settings)

# Generate age models using BChron:
# Get chronologies and rebuild, but drop som specified sample sites
# because of strange behavior:

age_seq <- seq(1000, 21000, by = 1000)

new_chrons <- build_newchrons(all_downloads, chronologies, settings) %>%
  filter(!core %in% c("Chalko Lake", "Aghnaghak", "Pittsburg Basin",
                      "Lac à l'Ange", "Marl Pond", "Cheyenne Bottoms",
                      "San Agustin Plains")) %>%
  group_by(handle) %>%
  mutate(max = findInterval(max(age.lin, na.rm = TRUE), age_seq)) %>%
  na.omit

plot_recal_diffs(new_chrons)



## ---- plot.chron.change, fig.width=6, echo=FALSE, message=FALSE, results='hide', warning=FALSE, dev='svg'----

source('R/figures/ageplot_histogram.R')

hists <- chronology_type_histogram(all_downloads, pubs, settings)



## ---- results = 'hide', echo = FALSE, warning=FALSE----------------------

source("R/get_allcontrols.R")

controls <- get_allcontrols(all_downloads, settings)

control_type <- controls %>%
  map(function(x)data.frame(control = as.character(x$chron.control$control.type))) %>%
  bind_rows() %>%
  group_by(control) %>%
  summarise(count = n())

knitr::kable(control_type[order(control_type$count, decreasing = TRUE), ])



## ---- echo=FALSE, message=FALSE, results='hide', warning=FALSE, dev='svg'----
source("R/get_allgeochron.R")

all_geochron <- get_allgeochron(settings)

source("R/lead_plotting.R")

lead_plots(all_geochron, all_downloads)



## ---- lead_comparison, results = 'hide', echo = FALSE, warning=FALSE, dev='svg', eval=FALSE----
## 
## ### NOTE:  THE ADDITION OF HUSTLER LAKE RESULTS IN A WEIRD SET OF VALUES
## 
## source("R/lead_ages.R")
## comp_doc <- compare_lead(coredir = "../bacon-agegen",
##                          all_geochron,
##                          settings = settings,
##                          param_file = "data/input/bacon_params_v1.csv")
## 
## model  <- lm(I(1950 - bacon_age1) ~ age, data = comp_doc[[2]])
## uncert <- lm(bacon_error1 ~ e.older, data = comp_doc[[2]])
## 
## comp_doc[[1]]
## 


## ----eval=FALSE----------------------------------------------------------
## Bacon-modeled ages are linearly relatedto ^210^Pb ages, but with the Bacon ages older than the ^210^Pb ages (slope = `r round(model$coefficients[2], 2)`, p < 0.01; Figure 4).  Uncertainty estimates for the Bacon chronologies are also consistently larger than the ^210^Pb error estimates for the point samples and the difference increases with depth (slope = `r round(uncert$coefficients[2],2)`, p < 0.01).


## ---- error_by_age, results = 'hide', echo = FALSE, warning=FALSE, eval=FALSE----
## 
## source("R/figures/comp_lead_plot.R")
## 
## comp_lead_plot(comp_doc)
## 


## ---- elicitation_plot, echo = FALSE, message = FALSE, warning = FALSE, error = FALSE----

source("R/elicitation_plot.R")
# Double check that this is the right depth. . .
elicitation_plots <- expert_elicitation()



## ---- echo = FALSE, message = FALSE, warning = FALSE, error = FALSE, dev = 'svg'----
elicitation_plots[[1]]


## ---- get_acc_rates, echo = FALSE, message = FALSE, warning = FALSE, error = FALSE, dev = 'svg'----

source("R/acc_rate_priors.r")

plot_acc_rates(accs)



## ---- get_thickness, echo = FALSE, message = FALSE, warning = FALSE, error = FALSE----

source("R/thick_model.R")
model_plot <- allan_thick('../bacon-agegen')



## ---- plot_thicknessmodel, echo = FALSE, message = FALSE, warning = FALSE, error = FALSE, dev = 'svg'----

model_plot[[1]]


