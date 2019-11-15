## ----load_libraries, echo=FALSE, message=FALSE, warnings = FALSE, results='hide'----
knitr::opts_chunk$set(cache=TRUE)

# Version numbering and other important parameters are stored in a file `settings.yaml`

source('R/load_packages.R')


## ----get_cores, echo=FALSE, message=FALSE, results='hide', warning=FALSE------

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


## ---- plot_chron_change, fig.width=6, echo=FALSE, message=FALSE, results='hide', warning=FALSE, dev='svg'----

source('R/figures/ageplot_histogram.R')

hists <- chronology_type_histogram(all_downloads, pubs, settings)


## ----get_chroncontroltypes, results = 'hide', echo = FALSE, warning=FALSE-----

source("R/get_allcontrols.R")

controls <- get_allcontrols(all_downloads, settings)

control_type <- controls %>%
  map(function(x) {
    cty <- as.character(x$chron.control$control.type)
    return(
      data.frame(controlid = x$parent$dataset.id,
                 control = cty,
                 stringsAsFactors = FALSE))
  }) %>%
  bind_rows() %>%
  group_by(control) %>%
  summarise(count = n())

knitr::kable(control_type[order(control_type$count, decreasing = TRUE), ])


## ----plotlead210, echo=FALSE, message=FALSE, results='hide', warning=FALSE, dev='svg'----
source("R/get_allgeochron.R")

all_geochron <- get_allgeochron(settings)

source("R/lead_plotting.R")

lead_plots(all_geochron, all_downloads)


## ---- lead_comparison, results = 'hide', echo = FALSE, warning=FALSE, dev='svg', eval=TRUE, message=FALSE----

source("R/lead_ages.R")
comp_doc <- compare_lead(coredir = "../bacon-agegen",
                         all_geochron,
                         settings = settings,
                         param_file = "../bacon-agegen/data/params/bacon_params_v1.csv")

model  <- lm(I(1950 - bacon_age1) ~ age, data = comp_doc[[2]])
uncert <- lm(bacon_error1 ~ e.older, data = comp_doc[[2]])

comp_doc[[1]]


## ---- error_by_age, results = 'hide', echo = FALSE, warning=FALSE-------------

source("R/figures/comp_lead_plot.R")

comp_lead_plot(comp_doc)


## ---- elicitation_plot, echo = FALSE, message = FALSE, warning = FALSE, error = FALSE----
source("R/elicitation_plot.R")
# Double check that this is the right depth. . .
elicitation_plots <- expert_elicitation()


## ---- echo = FALSE, message = FALSE, warning = FALSE, error = FALSE, dev = 'svg'----
elicitation_plots[[1]]


## ---- get_acc_rates, echo = FALSE, message = FALSE, warning = FALSE, error = FALSE, results='as-is'----

source("R/acc_rate_priors.r")
accs <- get_accs(bacon_params, path = "../bacon-agegen/Cores/")

means <- accs %>%
  group_by(era) %>%
  summarise(mean  = mean(rate_sqrt),
            var   = var(rate_sqrt),
            rate  = fitdistr(rate_sqrt, 'gamma')$estimate['rate'],
            shape = fitdistr(rate_sqrt, 'gamma')$estimate['shape']) %>%
            mutate(era = ifelse(era == 'modern',
                                   'Post-Settlement',
                                   'Pre-Settlement'))

knitr::kable(means, digits = 2,
             col.names = c("Period", "Average (yr/cm)",
                           "Variance", "Gamma Rate", "Gamma Shape"))


## ---- get_acc_plot, echo = FALSE, message = FALSE, warning = FALSE, error = FALSE, dev = 'svg'----

plot_acc_rates(accs)



## ---- get_thickness, echo = FALSE, message = FALSE, warning = FALSE, error = FALSE----

source("R/thick_model.R")
model_plot <- allan_thick(coredir = '../bacon-agegen',
                          paramfile = '../bacon-agegen/data/params/bacon_params_v1.csv')



## ---- plot_thicknessmodel, echo = FALSE, message = FALSE, warning = FALSE, error = FALSE, dev = 'svg'----

model_plot[[1]]


