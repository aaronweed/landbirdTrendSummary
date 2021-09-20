library(devtools)
devtools::install_github("BrandonEdwards/bbsBayes")

library(bbsBayes)
# https://bookdown.org/adam_smith2/bbsbayes_intro_workshop/

# access all BBS data
bbs<-fetch_bbs_data()

# All of the models supported by bbsBayes  expect the BBS route-level data to be
#grouped into geographic strata.These strata are subregions of the survey area, and allow the 
#parameters that model abundance and trend to vary across a species’ range.

bbs.bcr<-stratify(by = "bbs_usgs") # allows the user to select summaries for composite regions (regions made up of collections of strata), such as countries, provinces/states, Bird Conservation Regions, etc.

# Export list of species in DB
View(bbs.bcr$species_strat)

# Prepare the stratified data for use in a JAGS model

jags_data <- prepare_data(strat_data = bbs.bcr,
                               species_to_run = "Ovenbird",
                               model = "slope",
                               min_year = 2006, max_year = 2019,
                               min_max_route_years = 2,
                               strata_rem=  NULL,
                               heavy_tailed = TRUE)

# Now run the model
jags_mod <- run_model(jags_data = jags_data,
                      n_iter = 24000,
                      n_adapt = 1000,
                      n_burnin = 20000,
                      n_iter = 10000,
                      n_thin = 20,
                      parallel= TRUE,
                      parameters_to_save = c("n","beta","strata","n3"))# “n3”  tracks the population trajectory after removing the year-effects. This “n3” parameter represents the smooth-only population trajectory from the gamye and the linear-slope component of the trajectory from the slope model. In the CWS annual analyses, the “n3” parameter is used to estimate trends and the “n” parameter is used to track the full population trajectory

# Convergence of the MCMC
jags_mod$n.eff #shows the effective sample size for each monitored parameter
jags_mod$Rhat # shows the Rhat values for each monitored parameter

library(shinystan)

my_sso <- shinystan::launch_shinystan(shinystan::as.shinystan(jags_mod$samples, model_name = "Ovenbird BBS"))




# Estimating Annual Indices and Trends

indices <- generate_indices(jags_mod = jags_mod,
                            jags_data = jags_data, regions =c("bcr", "prov_state","stratum", "national"))

head(indices$data_summary[,c(1,2,4,7,8,13,14,15,16)])

write.csv(indices$data_summary, "OVEN_trajectories.csv")

View(indices$data_summary)


# Population Trends

trends <- generate_trends(indices = indices,
                          Min_year = 2006,
                          Max_year = 2019,
                          slope= TRUE, prob_increase = c(50), prob_decrease = c(100)) # (e.g., prob_increase = c(100) would result in a calculation of the probability that the population has increased by more than 100 percent, i.e., doubled, over the period of the trend. e.g., prob_decrease = c(50) would result in a calculation of the probability that the population has decreased by more than 50 percent over the period of the trend, i.e., less than half the population remains.

head(trends[,c(1,3,8,9,14)])

## Plotting the trends and abundance index

# create the list of plots
tp = plot_indices(indices = indices,
                  species = "Ovenbird",
                  add_observed_means = TRUE,
                  add_number_routes = FALSE,
                  title_size = 20,
                  axis_title_size = 12,
                  axis_text_size = 12)

# extract region specific plots
BCR.plot<-tp$BCR_28

state.plot<-tp$NEW_JERSEY

#ggarrange(plotlist=tp)


NETN.plot<-ggplot(data= dplyr::filter(plotdata, common == "Ovenbird (3)"), aes(x = Year, y = value, color = variable))+expand_limits(y=0)+
  geom_point(data= dplyr::filter(plotdata, variable == "Field Observation" & common == "Ovenbird (3)"),size=2) + 
  geom_line(data= dplyr::filter(plotdata, variable == "Field Observation" & common == "Ovenbird (3)"))+
  #geom_line(data = filter(plotdata, Nonsig == FALSE & variable == "Trend Estimate")) +
  #geom_errorbar(aes(ymin= mean-se, ymax= mean +se), width = .2, color = "#2171b5")+
  theme_classic()+
  theme(axis.title.y =element_text(size = 14, face ="bold", vjust= 1))+
  theme(axis.title.x =element_text(size = 14, face ="bold", vjust= 1))+
  theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 12))+
  theme(axis.text.x = element_text(color="black", size = 10))+
  scale_x_continuous(breaks = seq(params$minYear, params$maxYear,2))+
  scale_color_manual(values = c("#2171b5", "black"))+
  labs(y="Number detected per point" , color = "", title = paste0( plotdata$park[1]))+
  theme(legend.position = "top")+ 
  theme(legend.text = element_text(size = 12))+
  theme(panel.background =  element_rect(fill="white", colour="black")) +
  theme(plot.title=element_text(size=12, vjust=2, face= "bold")) +
  theme(strip.background= element_rect(size=10, color="gray", linetype ="solid" ))+
  theme(strip.text=element_text(size=12, vjust=0, face= "bold"))+
 #facet_wrap(~common, ncol = 2, shrink= FALSE)+
  geom_smooth(data= filter(plotdata, Nonsig == TRUE & variable == "Trend Estimate" & common == "Ovenbird (3)"), method = "lm", se= F)



NETN.plot<-NETN.plot+
  geom_text(data= dplyr::filter(plotdata, variable == "Trend Estimate" & common == "Ovenbird (3)")  %>% distinct(common, park, variable, mean, lowCI, highCI, per_change), show.legend = FALSE, aes(x = Inf, y = Inf, label=  paste(round(mean,3)," (",round(lowCI,2),",",round(highCI,2),")") , hjust = 1.05,vjust   = 1))
  

print(BCR.plot)
print(state.plot)

library(ggpubr)

ggarrange(plotlist = list(BCR.plot,state.plot, NETN.plot), nrow =1)

# Mapping the trends

mp = generate_map(trend = trends,
                  select = FALSE,
                  stratify_by = "bbs_usgs",
                  species = "Ovenbird",
                  slope= FALSE, col_viridis = TRUE
                  )
print(mp)



# geofacets of abundance index

gf <- geofacet_plot(indices_list = indices,
                    select = TRUE,
                    stratify_by = "bbs_usgs",
                    multiple = FALSE,
                    trends = trends,
                    slope = FALSE,
                    species = "Ovenbird") 

print(gf)






