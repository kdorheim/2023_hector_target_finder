# Use an optimization routine to solve for a FFI emissions pathway that matches some sort of novel scenario.


# 0. Set up --------------------------------------------------------------------------------------------------
# load the required R packages
library(assertthat)
library(dplyr)
library(ggplot2)
library(FME)
library(hector)
library(tidyr)

theme_set(theme_bw())


# Function used within by solve_ffipathway to find the emissions pathways for a particular temperature target
# This is a helper function to return the function that will be used by modFit
# Args
#   input_df: data frame of ffi_emissions that are to vary
#   comp_df: data frame of the comparison data aka the temperature target data
#   hc: an active Hector core
# Return: the function that will be used to calcualte the modCost between a Hector run and the target temperature pathway
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

    #print(x)

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

# Function to use to solve for a ffi emissions pathway!
# Args
#   inputs_df: dataframe of Hector ffi emission to vary should be the same dimensions at the
#   target_df: datafrmae of the target data!
#   hc: an active Hector core
# Returns: an object created by FME::modFit (this is )
solve_ffipathway <- function(input_df, target_df, hc){

  # Check the inputs
  req_names <- c("year", "variable", "value", "units")
  assert_that(all(req_names %in% names(input_df)))
  req_names <- c("year", "value", "variable")
  assert_that(all(req_names %in% names(target_df)))

  # For now let's limit to FFI and GLOBAL TAS but in the future this might be something else to do
  assert_that(unique(input_df$variable) == FFI_EMISSIONS())
  assert_that(unique(target_df$variable) == GLOBAL_TAS())
  assert_that(all(input_df$year == target_df$year))

  # Format the comparison data
  target_df %>%
    select(time=year, value, variable) %>%
    spread(variable, value) ->
    comp_df


  fxn <- make_objective_fxn(input_df = input_df, comp_df = comp_df, hc = core)
  Fit <- modFit(p = input_df$value, f = fxn, lower = -100, upper = 100)

  input_df$value <- Fit$par

  # This would need to be changed so that it is considering more than FFI emissions
  input_df %>%
    mutate(variable = if_else(value < 0, DACCS_UPTAKE(), variable)) %>%
    mutate(value = if_else(value < 0, value * -1, value)) ->
    pathway

  Fit[["pathway"]] <- pathway

  return(Fit)

}




# 1. Example 1 --------------------------------------------------------------------------------------------------
# Set up the Hector core to use the ssp245 scenario
inifile <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- newcore(inifile)


# Run the ssp245 scenario and save the results to be used to make our target and in comparisons
run(hc)
fetchvars(hc, 1900:2100, vars = c(GLOBAL_TAS(), FFI_EMISSIONS(),
                                  CONCENTRATIONS_CO2(), RF_TOTAL())) %>%
  mutate(scenario = "original ssp245") ->
  original_hector_output


# From earlier attempts we know that the target pathway has to have a smooth transition from the historical
# period the easiest way I have found to do that is by adding a time series of increasing residuals.
original_hector_output %>%
  filter(year %in% 2020:2100) %>%
  filter(variable == GLOBAL_TAS()) %>%
  mutate(resids = seq(from = 0.001, by = 0.0015, length.out = nrow(.))) %>%
  mutate(value = value + resids) %>%
  mutate(scenario = "target data") ->
  temp_target_data

# Take a look at the original and temperature target data
ggplot() +
  geom_line(data = original_hector_output %>% filter(variable == GLOBAL_TAS()), aes(year, value, color = scenario)) +
  geom_line(data = temp_target_data , aes(year, value, color = scenario))


# Extract the FFI emissions used in the original run as a starting point
original_hector_output %>%
  filter(variable == FFI_EMISSIONS()) %>%
  filter(year %in% 2020:2100) ->
  input_df


# Set up a fresh core
inifile <- system.file(package = "hector", "input/hector_ssp245.ini")
core <- newcore(inifile)

out1 <- solve_ffipathway(input_df = input_df, target_df = temp_target_data, hc = core)


# Now run Hector with the new inputs!
use_this_pathways <- out1$pathway
setvar(core = core, dates = use_this_pathways$year, values = use_this_pathways$value,
       var = use_this_pathways$variable, unit = use_this_pathways$units)
reset(core)
run(core)

fetchvars(core, 1900:2100, vars = c(GLOBAL_TAS(), FFI_EMISSIONS(),
                                    CONCENTRATIONS_CO2(), RF_TOTAL())) %>%
  mutate(scenario = "new scenario") ->
  new_hector_output

# Compare the plots
rbind(original_hector_output, new_hector_output) %>%
  filter(year > 2015) %>%
  ggplot() +
  geom_line(aes(year, value, color = scenario)) +
  facet_wrap("variable", scales = "free")
