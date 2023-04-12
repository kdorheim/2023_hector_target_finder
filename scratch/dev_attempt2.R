
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

# Let's define our target, we will have to say that year 2100 temperature is 3/4 of wwhat it was....
original_outputs %>%
  filter(variable == GLOBAL_TAS() & year %in%  2020:2100) %>%
  mutate(value = value * .75) ->
  target_df






# need to set up the information about what variables can be varied
# data.frame(year = 2025:2100, variable = FFI_EMISSIONS(), units = getunits(FFI_EMISSIONS())) %>%
#   mutate(variable = if_else(value < 0, DACCS_UPTAKE(), variable)) ->
#   input_vars
#
# MSE <- tryCatch({
#   setvar(core = hc, dates = input_vars$year, values = input_vars$value, var = input_vars$variable, unit = input_vars$units)
#   reset(hc)
#   run(hc)
#   output_4_comparison <- fetchvars(core = hc, dates = target_df$year, vars = target_df$variable) %>%
#     rename(out_value = value)
#
#   # MSE between target and output data
#   target_df %>%
#     inner_join(output_4_comparison, by = c("year", "variable")) %>%
#     mutate(SE = (value - out_value)^2) %>%
#     pull(SE) %>%
#     mean
#
# }, error = function(e){
#   print("using error MSE")
#   sample(1e3:1e4, 1)
# })



df <- data.frame(year = 2020:2100, variable = FFI_EMISSIONS(), units = getunits(FFI_EMISSIONS()))
target_df


make_objective_fxn <- function(input_df, target_df){

  # Okay so I think that this would have to be some sort of set up for input_df and p....
  # could potentially set up a make sure that p is the correct length here...


  function(p){

    print(p)
    input_df %>%
      mutate(value = p) %>%
      mutate(variable = if_else(value < 0, DACCS_UPTAKE(), variable)) ->
      input_vars

    MSE <- tryCatch({
      setvar(core = hc, dates = input_vars$year, values = input_vars$value, var = input_vars$variable, unit = input_vars$units)
      reset(hc)
      run(hc)
      output_4_comparison <- fetchvars(core = hc, dates = unique(target_df$year), vars = unique(target_df$variable)) %>%
        rename(out_value = value)

      # MSE between target and output data
      target_df %>%
        inner_join(output_4_comparison, by = c("year", "variable")) %>%
        mutate(SE = (value - out_value)^2) %>%
        pull(SE) %>%
        mean

    }, error = function(e){
      print("using error MSE")
      sample(1e3:1e4, 1)
    })

    return(MSE)
  }




}


obj_fn <- make_objective_fxn(input_df = df, target_df = target_df)

p <- rep(1, 81)


obj_fn(p = p)

Fit1 <- optim(par  = p, fn = obj_fn, control= list(maxit = 1e3))
# okay well if we are doing long term cliamte target and stuff I think that we could do the other algothem hight might be easier....




