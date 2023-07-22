# Use an optimization routine to solve for a FFI emissions pathway that matches some sort of novel scenario.

# to consdier doing - can we run with a negative emissions? Try a reduction run, also what happens if the data is
# read in at like 5 year periods? would that decrease the solve time?
# 0. Set up --------------------------------------------------------------------------------------------------
# load the required R packages
library(assertthat)
library(dplyr)
library(ggplot2)
library(FME)
#remotes::install_github("jgcri/hector@v3.1.0")
library(hector)
library(tidyr)

theme_set(theme_bw())


# Function used within by solve_ffipathway to find the emissions pathways for a particular temperature target
# This is a helper function to return the function that will be used by modFit
# Args
#   input_df: data frame of ffi_emissions that are to vary
#   comp_df: data frame of the comparison data aka the temperature target data
#   hc: an active Hector core
# Return: the function that will be used to calculate the modCost between a Hector run and the target temperature pathway
make_objective_fxn <- function(input_df, comp_df, hc){

  assert_that("time" %in% names(comp_df), msg = "comp_data must contain time column")

  # The required columns for the comparison data frame
  req_cols <- c("time", "global_tas")
  assert_that(all(req_cols %in% names(comp_df)))

  # The required columns for the input data frame
  req_cols <- c("year", "variable", "value", "units")
  assert_that(all(req_cols %in% names(input_df)))
  assert_that(unique(input_df$variable) == FFI_EMISSIONS()) # TODO this might be something to change

  assert_that(isactive(hc))

  fxn <- function(x){

    # will print out the emission values being used this can be help give an idea of how the
    # optimization reoutine it doing
    #print(x

    rslts <- tryCatch({

      # Set up ffi and daccs input tables set the value equal to 0, then populate the data frames
      # with the values being proposed by the optimization routine.
      ffi_input <- data.frame(year = input_df$year,
                              value = 0,
                              variable = FFI_EMISSIONS(),
                              units = getunits(FFI_EMISSIONS()))
      daccs_input <- data.frame(year = input_df$year,
                                value = 0,
                                variable = DACCS_UPTAKE(),
                                units = getunits(DACCS_UPTAKE()))

      # Update the values, note that the ffi emission and daccs update must be strictly positive.
      which_ffi <- which(x > 0)
      ffi_input$value[which_ffi] <- x[which_ffi]
      which_daccs <- setdiff(1:length(x), which_ffi)
      daccs_input$value[which_daccs] <- -1 * x[which_daccs]

      # Pass the ffi emissions and daccs uptake to the Hector core.
      setvar(core = hc,
             dates = ffi_input$year,
             var = ffi_input$variable,
             values = ffi_input$value,
             unit = ffi_input$units)
      setvar(core = hc,
             dates = daccs_input$year,
             var = daccs_input$variable,
             values = daccs_input$value,
             unit = daccs_input$units)

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
# Returns: an object created by FME::modFit ths solver
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


  # This would need to be changed so that it is considering more than FFI emissions
  ffi_input <- data.frame(year = input_df$year,
                          value = 0,
                          variable = FFI_EMISSIONS(),
                          units = getunits(FFI_EMISSIONS()))
  daccs_input <- data.frame(year = input_df$year,
                            value = 0,
                            variable = DACCS_UPTAKE(),
                            units = getunits(DACCS_UPTAKE()))

  # Update the values, note that the ffi emission and daccs update must be strictly positive.
  x <-  Fit$par
  which_ffi <- which(x > 0)
  ffi_input$value[which_ffi] <- x[which_ffi]
  which_daccs <- setdiff(1:length(x), which_ffi)
  daccs_input$value[which_daccs] <- -1 * x[which_daccs]

  pathway <- rbind(ffi_input, daccs_input)


  Fit[["pathway"]] <- pathway

  return(Fit)

}


# Function used to run Hector with the solved pathway
# Args
#   obj: object returned by solve_ffipathway
#   hc: hector core to use
#   yrs: vector of hector output to save
#   vars: vector of hector variables to return
# Returns: data frame of hector results
use_ffipathway <- function(obj, hc, yrs = 1900:2100, vars = c(GLOBAL_TAS(), FFI_EMISSIONS(),
                                                              CONCENTRATIONS_CO2(), RF_TOTAL(), DACCS_UPTAKE())){
  req_names <- c("par", "hessian", "residuals", "info", "message", "iterations", "rsstrace",
                 "ssr", "diag", "ms", "var_ms", "var_ms_unscaled", "var_ms_unweighted", "rank",
                 "df.residual", "pathway")
  assertthat::assert_that(has_name(x = obj, which = req_names), msg = "df should be object returned by solve_ffipathway")

  reset(hc)
  x <- split(obj[["pathway"]], obj[["pathway"]][["variable"]]) %>%
    lapply(function(dat){
      setvar(core = hc, dates = dat$year, values = dat$value,
             var = dat$variable, unit = dat$units)
      reset(hc)
    })
  run(hc)
  fetchvars(core = hc, 1900:2100, vars) %>%
    mutate(scenario = "new scenario") ->
    out
  return(out)
}


# 1. Example 1 (Warmer than SSP245)--------------------------------------------------------------------------------------------------
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

# Solve for the pathway and use it to run hector!
fit1 <- solve_ffipathway(input_df = input_df, target_df = temp_target_data, hc = core)
out1 <- use_ffipathway(obj = fit1, hc = core)

# Compare the plots
rbind(original_hector_output, out1) %>%
  filter(year > 2015) %>%
  ggplot() +
  geom_line(aes(year, value, color = scenario)) +
  geom_line(data = temp_target_data, aes(year, value, color = "target"), linetype = 2) +
  facet_wrap("variable", scales = "free")

# 2. Example 2 (Cooler than SSP245)--------------------------------------------------------------------------------------------------
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
  mutate(resids =-1*seq(from = 0.001, by = 0.0015, length.out = nrow(.))) %>%
  mutate(value = value + resids) %>%
  mutate(scenario = "target data") ->
  temp_target_data2

# Take a look at the original and temperature target data
ggplot() +
  geom_line(data = original_hector_output %>% filter(variable == GLOBAL_TAS()), aes(year, value, color = scenario)) +
  geom_line(data = temp_target_data2 , aes(year, value, color = scenario))


# Extract the FFI emissions used in the original run as a starting point
original_hector_output %>%
  filter(variable == FFI_EMISSIONS()) %>%
  filter(year %in% 2020:2100) ->
  input_df


# Set up a fresh core
inifile <- system.file(package = "hector", "input/hector_ssp245.ini")
core <- newcore(inifile)

fit2 <- solve_ffipathway(input_df = input_df, target_df = temp_target_data2, hc = core)
out2 <- use_ffipathway(obj = fit2, hc = core)

# Compare the plots
rbind(original_hector_output, out2) %>%
  filter(year > 2015) %>%
  ggplot() +
  geom_line(aes(year, value, color = scenario)) +
  geom_line(data = temp_target_data2, aes(year, value, color = "target"), linetype = 2) +
  facet_wrap("variable", scales = "free")


# 3. Example 3 (Cooler than SSP119)--------------------------------------------------------------------------------------------------
# Set up the Hector core to use the ssp245 scenario
inifile <- system.file(package = "hector", "input/hector_ssp119.ini")
hc <- newcore(inifile)


# Run the ssp245 scenario and save the results to be used to make our target and in comparisons
run(hc)
fetchvars(hc, 1900:2100, vars = c(GLOBAL_TAS(), FFI_EMISSIONS(),
                                  CONCENTRATIONS_CO2(), DACCS_UPTAKE(), RF_TOTAL())) %>%
  mutate(scenario = "original ssp245") ->
  original_hector_output


# From earlier attempts we know that the target pathway has to have a smooth transition from the historical
# period the easiest way I have found to do that is by adding a time series of increasing residuals.
original_hector_output %>%
  filter(year %in% 2020:2100) %>%
  filter(variable == GLOBAL_TAS()) %>%
  mutate(resids =-1*seq(from = 0.001, by = 0.0015, length.out = nrow(.))) %>%
  mutate(value = value + resids) %>%
  mutate(scenario = "target data") ->
  temp_target_data3

# Take a look at the original and temperature target data
ggplot() +
  geom_line(data = original_hector_output %>% filter(variable == GLOBAL_TAS()), aes(year, value, color = scenario)) +
  geom_line(data = temp_target_data3 , aes(year, value, color = scenario))


# Extract the FFI emissions used in the original run as a starting point
original_hector_output %>%
  filter(variable == FFI_EMISSIONS()) %>%
  filter(year %in% 2020:2100) ->
  input_df

# Set up a fresh core
inifile <- system.file(package = "hector", "input/hector_ssp119.ini")
core <- newcore(inifile)

fit3 <- solve_ffipathway(input_df = input_df, target_df = temp_target_data3, hc = core)
out3 <- use_ffipathway(obj = fit3, hc = core)

# Compare the plots
rbind(original_hector_output, out3) %>%
  filter(year > 2015) %>%
  ggplot() +
  geom_line(aes(year, value, color = scenario)) +
  geom_line(data = temp_target_data3, aes(year, value, color = "target"), linetype = 2) +
  facet_wrap("variable", scales = "free")
