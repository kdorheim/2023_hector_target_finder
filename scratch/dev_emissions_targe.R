# Objective: read in some target and get  possible emissions pathway, this is under devlopment.
remotes::install_github("jgcri/hector")
library(dplyr) # i know that i should not use this but during devlopment it is so nice!
library(hector)
library(magrittr)
library(ggplot2)
library(nleqslv)


theme_set(theme_bw())

# First we will want to do a Hector run and figure out what a possible temperature target could be.
inifile <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- newcore(inifile)

run(hc)
original_outputs <- fetchvars(hc, dates = 1750:2100, vars = c(GLOBAL_TAS(), RF_TOTAL(), FFI_EMISSIONS()))

# plot the original results
original_outputs %>%
  ggplot(aes(year, value)) +
  geom_line() +
  facet_wrap("variable", scales = "free")


# set up the target temperature ts
original_outputs %>%
  filter(variable == GLOBAL_TAS() & year %in% c(2090:2100)) %>%
  mutate(value = value * .75) ->
  temperature_goal

target_df <- temperature_goal


# now let's take a look at the difference between the default and the target output
ggplot() +
  geom_line(data = original_outputs %>% filter(variable == GLOBAL_TAS()), aes(year, value, color = "original Hector results")) +
  geom_line(data = temperature_goal, aes(year, value, color = "target")) +
  facet_wrap("variable", scales = "free")

# # 1. Attempt 1 ------------------------------------------------------------------------------
# # note that there is probably a better way to do this... emiss
# # okay trying to follow the example from Robert's exmpale but I dont think g
# f <- function(emiss_vals){
#
#   input_df <- data.frame(year = target_df$year, values = emiss_vals, unit = getunits(FFI_EMISSIONS()), variable = FFI_EMISSIONS())
#   setvar(core = hc, dates = input_df$year, var = input_df$variable, unit = input_df$unit, values = input_df$values)
#
#   reset(hc)
#   run(hc)
#   run_output <- fetchvars(core = hc, dates = target_df$year, vars = unique(target_df$variable))
#
#   out <- run_output$value - target_df$value
#
#   return(out)
# }
#
#
# f(emiss_vals = rep(10, 11))
#
# x <- rep(10, times = 11)  # 2000:2001 includes both endpoints
# f(x)
# slv <- nleqslv::nleqslv(x, f, method = "Broyden")
# max(abs(slv$fvec))
#
# x2 <- rep(40, times = 11)  # 2000:2001 includes both endpoints
# f(x2)
# f(x)
# slv2 <- nleqslv::nleqslv(x, f, method = "Broyden")
# max(abs(slv2$fvec))


# # Attempt 2 ---------------------------------------------------------------------------------
# # well this is not going as expected!
# # attempting with the library(FME) okay so I think... that this might be the way to go or might want to do optim...
# # the challange is going to be including if else statements to control if there is something going on with the
# # negative emissions... ugh
# library(FME)
#
#
#
# objective <- function(p){
#
#   print(p)
#
#   data.frame(time = target_df$time,
#              values = p,
#              unit = getunits(FFI_EMISSIONS()),
#              variable = FFI_EMISSIONS()) %>%
#     mutate(variable = if_else(values < 0, DACCS_UPTAKE(), variable)) ->
#     input_df
#
#   run_output <- tryCatch({
#
#     #print(input_df)
#     setvar(core = hc, dates = input_df$time, var = input_df$variable, unit = input_df$unit, values = input_df$values)
#     reset(hc)
#     run(hc)
#     out <- fetchvars(core = hc, dates = target_df$time, vars = unique(target_df$variable))
#     names(run_output) <- c("scenario", "time", "variable", "value",  "units")
#
#     return(out)
#   }, error = function(e){
#     #print("some error")
#     n <- length(input_df$time)
#     out <- data.frame(time = input_df$time, value = sample(300:999, size = n))
#     return(out)
#   })
#
#   names(target_df) <- c("scenario", "time", "variable", "value", "units")
#   out <- modCost(model = run_output %>%  select(time, value), obs = target_df %>% select(time, value))
#
#   return(out)
#
# }
#
# Fit <- modFit(p = rep(10, 11) , f = objective)
#



# Attempt 3 ---------------------------------------------------------------------------------------------
# okay let's try for good old optim although I am worried I will run into the same issues here! fml


target_df


par <- 10
# we will need the comparison data aka the target
# the the inputs that would be set up from randomly generated
# then hector run resultsss

objective_fxn <- function(par){
  print(par)
  data.frame(year = target_df$year,
             variable = FFI_EMISSIONS(),
             value = par,
             units = getunits(FFI_EMISSIONS())) %>%
    mutate(variable = if_else(value < 0, DACCS_UPTAKE(), variable)) ->
    hector_input_df

  MSE <- tryCatch({
    setvar(core = hc, dates = hector_input_df$year, var = hector_input_df$variable, values = hector_input_df$value, unit = hector_input_df$units)
    reset(hc)
    run(hc)
    hector_results <- fetchvars(core = hc, dates = hector_input_df$year, vars = GLOBAL_TAS())
    mean((target_df$value - hector_results$value)^2)
  }, error = function(e){
    sample(1e3:1e4, size = 1)}
  )

  return(MSE)
}


Fit <- optim(par = rep(10, 11), f = objective_fxn)

inifile <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- newcore(inifile)

setvar(hc, dates = target_df$year, var = FFI_EMISSIONS(), unit = getunits(FFI_EMISSIONS()), values = Fit$par)
reset(hc)
run(hc)
new_output <- fetchvars(hc, dates = 1750:2100, vars = c(GLOBAL_TAS(), RF_TOTAL(), FFI_EMISSIONS()))


yrs <- 2050:2100
ggplot() +
  geom_line(data = original_outputs %>% filter(year %in% yrs), aes(year, value, color = "og")) +
  geom_line(data = new_output  %>% filter(year %in% yrs), aes(year, value, color = "new")) +
  geom_line(data = target_df, aes(year, value, color = "target")) +
  facet_wrap("variable", scales = "free")



# okay so the convergence did not happen... I suspect that is beccause even with a temperautre target there is not enough time with the
# emissions and committed warming





