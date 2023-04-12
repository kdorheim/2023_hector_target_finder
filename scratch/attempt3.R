# okay so techniclly this works! however I am not supper thrilled with it
# things that I learned... well it is really senstive to the pathways a
library(assertthat)
library(FME)
library(hector)
library(dplyr)
library(ggplot2)

inifile <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- newcore(inifile)
run(hc)

fetchvars(hc, 1950:2100, vars = GLOBAL_TAS()) %>%
  select(time = year, value, variable) %>%
  tidyr::spread(variable, value) ->
  original_temp

# The original target okay I think it was actually too big of a jump! and also
# not a smooth transition! from the historical to the future period so let's do
fetchvars(hc, 2020:2100, vars = GLOBAL_TAS()) %>%
  select(time = year, value, variable) %>%
  mutate(value = value *.75) %>%
  tidyr::spread(variable, value) ->
  old_target


# going to make the residuals small and lo

2020:2100

resids <- 1e-5*(1 + 1e-3)^1:81

fetchvars(hc, 2020:2100, vars = GLOBAL_TAS()) %>%
  mutate(resids =  1e-3*(1 + 1e-2)^0:dim(.)[1]) %>%
  mutate(value = value + resids) %>%
  select(time = year, value, variable) %>%
  tidyr::spread(variable, value) ->
  new_target

ggplot() +
  geom_line(data = original_temp, aes(time, global_tas)) +
  geom_line(data = old_target, aes(time, global_tas), alpha = 0.5) +
  geom_line(data = new_target, aes(time, global_tas, color = "new target"))


fetchvars(hc, 2020:2100, vars = FFI_EMISSIONS()) %>%
  mutate(value = value * 0.75) %>%
  mutate(variable = if_else(value < 0, DACCS_UPTAKE(), variable)) %>%
  mutate(value = if_else(variable == DACCS_UPTAKE(), value * -1, value)) ->
  inputs

setvar(core = hc, dates = inputs$year, var = inputs$variable,
       values = inputs$value, unit = inputs$units)
reset(core = hc)
run(core = hc)

fetchvars(core = hc, dates = inputs$year, vars = GLOBAL_TAS()) %>%
  select(time = year, value, variable) %>%
  tidyr::spread(variable, value) ->
  rslts

make_objective_fxn <- function(input_df, comp_df, hc){

  assert_that("time" %in% names(comp_df), msg = "comp_data must contain time column")

  # The required columns for the comparison data frame
  req_cols <- c("time", "global_tas")
  assert_that(all(req_cols %in% names(comp_df)))

  # The required columns for the input data frame
  req_cols <- c("year", "variable", "value", "units")
  assert_that(all(req_cols %in% names(input_df)))

  assert_that(isactive(hc))

  fxn <- function(x){

    print(x)

    rslts <- tryCatch({

      emiss <- input_df
      emiss$value <- x
      emiss %>%
        mutate(variable = if_else(value < 0, DACCS_UPTAKE(), variable)) %>%
        mutate(value = if_else(value < 0, value * -1, value)) ->
        emiss

      setvar(core = hc,
             dates = emiss$year,
             var = emiss$variable,
             values = x,
             unit = emiss$units)
      reset(hc)
      run(hc)

      fetchvars(core = hc,
                dates = input_df$year,
                vars = GLOBAL_TAS()) %>%
        select(time = year, value, variable) %>%
        tidyr::spread(variable, value)

    },
    error = function(e){
      data.frame(time = emiss$year,
                 global_tas = 90)
    })


    out <-  modCost(model = rslts, obs = comp_df)
    return(out)

  }
  return(fxn)


}


obj <- make_objective_fxn(input_df = inputs, comp_df = new_target, hc)

x <- inputs$value
obj(x)

# tried with limits of -20 to 20 failed after maxiter == 100, okay it failed mulitple times but let's see if there is a smooth transition
# between the historical and future then let's see how it goes hopefully it will converge!
Fit1 <- modFit(p = inputs$value, f = obj, lower = -20, upper = 20, control = list(maxiter = 500))

# Comparison!

inifile <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- newcore(inifile)
run(hc)

fetchvars(hc, 1950:2100, vars = c(GLOBAL_TAS(), FFI_EMISSIONS())) %>%
  mutate(scn = "original pathway") ->
  og_data

setvar(core = hc, dates = 2020:2100, var  = FFI_EMISSIONS(), unit = getunits(FFI_EMISSIONS()), values = Fit1$par)
reset(hc)
run(hc)

fetchvars(hc, 1950:2100, vars = c(GLOBAL_TAS(), FFI_EMISSIONS())) %>%
  mutate(scn = "optimized pathway") ->
  out_data

new_target$variable <- "global_tas"


ggplot() +
  geom_line(data = og_data, aes(year, value, color = "ssp245")) +
  geom_line(data = out_data, aes(year, value, color = "new pathway")) +
  geom_line(data = new_target, aes(time, global_tas, color = "target"), linetype = 2) +
  facet_wrap("variable", scales = "free") +
  theme_bw()



# okay now let's see what happens when we start with a different inital guess
x <- rep(1, 81)
Fit2 <- modFit(p = x, f = obj, lower = -50, upper = 50, control = list(maxiter = 500))


# okay so they came to the pretty clsoe same answer minor differenes though in the last year of inputs...
ggplot() +
  geom_line(aes(x = 2020:2100, y = Fit1$par)) +
  geom_line(aes(x = 2020:2100, y = Fit2$par, color = "attempt2 "))



