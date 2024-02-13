
# Install & Load ----------------------------------------------------------

install.packages("sf")
install.packages("geoR")
install.packages("purrr")
install.packages("dplyr")
install.packages("insight")

library(sf)
library(geoR)
library(purrr)
library(dplyr)
library(insight)



# Functions ---------------------------------------------------------------

## function to simulate a Contaminated Gaussian Random Field
simulate_grfs <- function(
		n = 10, grid = "reg", 
		mean = 0, cov.pars = stop("missing covariance parameters sigmasq and phi"),
		nugget = 0,	kappa = 1, seed = 1992
) {
	
	# Gaussian Random Field
	set.seed(seed)
	res_grf <- geoR::grf(
		n = n, grid = grid, 
		mean = mean, cov.pars = cov.pars, 
		nugget = nugget, messages = FALSE
	)
	coords <- res_grf$coords
	grf_data <- res_grf$data

	# Contaminated Gaussian Random Field
	set.seed(seed)
	kappa_cov.pars <- (kappa ^ 2) * cov.pars
	cgrf_data <- geoR::grf(
		n = n, grid = grid, 
		mean = mean, cov.pars = kappa_cov.pars, 
		nugget = nugget, messages = FALSE
	)$data
	
	res <- list("geoR" = res_grf, "coords" = coords, "grf" = grf_data, "cgrf" = cgrf_data)
	return(res)
	
}

## function to sample from the two Gaussian Random Fields
sample_grfs <- function(simulated_grfs, epsilon) {
	
	# initialize the results
	n <- nrow(simulated_grfs$coords)
	res_data <- vector("numeric", n)
	
	# sampling from the two Gaussian Random Fields with probability epsilon
	if (epsilon == 0) {
		res_data <- simulated_grfs$grf
	} else if (epsilon == 1) {
		res_data <- simulated_grfs$cgrf
	} else {
		grf_id <- sample(1:2, n, replace = TRUE, prob = c(1 - epsilon, epsilon))
		res_data[grf_id == 1] <- simulated_grfs$grf[grf_id == 1]
		res_data[grf_id == 2] <- simulated_grfs$cgrf[grf_id == 2]
	}
	
	res <- list("geoR" = simulated_grfs$geoR, "coords" = simulated_grfs$coords, "data" = res_data)
	return(res)
	
}

## function to create the x, y, z matrix
combine_results <- function(sampled_grfs) {
	res_mat <- cbind(
		sampled_grfs$coords,
		matrix(sampled_grfs$data, ncol = 1, dimnames = list(NULL, "z"))
	)
	res_geoR <- sampled_grfs$geoR
	res_geoR$data <- sampled_grfs$data
	res <- list("mat" = res_mat, "geoR" = res_geoR)
	return(res)
}

# function to transform data into geodata format
geodata_transform <- function(data, coords.col = 1:2, data.col = 3) {
	geo_data <- geoR::as.geodata(data, coords.col = coords.col, data.col = data.col)
	return(geo_data)
}

# function to estimate variogram model
variog_fit <- function(geo_data, cov.model = "exp", weights = "equal") {
	
	br <- seq(0, 1, 0.05)
	## Classical Variogram (Matheron)
	sample_variog <- geoR::variog(
		geo_data, uvec = br, estimator.type = "classical", messages = FALSE
	)
	variog_fit <- geoR::variofit(
		sample_variog, cov.model = cov.model, weights = weights, messages = FALSE
	)
	## Robust Variogram (Cressie)
	robust_variog <- geoR::variog(
		geo_data, uvec = br, estimator.type = "modulus", messages = FALSE
	)
	robust_variog_fit <- geoR::variofit(
		robust_variog, cov.model = cov.model, weights = weights, messages = FALSE
	)
	
	res_variog <- list(
		"sample_variog" = sample_variog,
		"variog_fit" = variog_fit,
		"robust_variog" = robust_variog,
		"robust_variog_fit" = robust_variog_fit
	)
	return(res_variog)
	
}

# function to plot GRF simulation
plot_grf <- function(geoR_simulation, kappa, epsilon, title = TRUE) {
	image(geoR_simulation)
	if (title) { title(main = paste0("kappa = ", kappa, "; epsilon = ", epsilon)) }
}

# function to plot variogram model
plot_variogram <- function(variog_fitted, kappa, epsilon, geoR_simulation, plot_type = 1) {
	
	sample_variog <- variog_fitted$sample_variog
	variog_fit <- variog_fitted$variog_fit
	robust_variog <- variog_fitted$robust_variog
	robust_variog_fit <- variog_fitted$robust_variog_fit
	
	if (plot_type == 1) {
		
		par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
		plot_grf(geoR_simulation, kappa, epsilon, title = FALSE)
		plot(sample_variog)
		lines(variog_fit, col = 1)
		lines(robust_variog_fit, col = 2)
		title(main = paste0("Variogram Estimation: kappa = ", kappa, "; epsilon = ", epsilon), outer = TRUE)
		
	} else if (plot_type == 2) {
		
		plot(sample_variog)
		points(x = robust_variog$u, y = robust_variog$v, col = 2)
		lines(variog_fit, col = 1)
		lines(robust_variog_fit, col = 2)
		title(main = paste0("Variogram Estimation: kappa = ", kappa, "; epsilon = ", epsilon))
		
	} else if (plot_type == 3) {
		
		par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
		plot(sample_variog)
		lines(variog_fit, col = 1)
		plot(robust_variog, col = 2)
		lines(robust_variog_fit, col = 2)
		title(main = paste0("Variogram Estimation: kappa = ", kappa, "; epsilon = ", epsilon), outer = TRUE)
		
	} else if (plot_type == 4) {
		
		variog_table <- data.frame(
			"h" = sample_variog$u,
			"Classic" = round(sample_variog$v, 2),
			"Robust" = round(robust_variog$v, 2)
		)
		print(variog_table)
		return(variog_table)
		
	} else {
		stop("Invalid plot_type")
	}

}

# function to generate a table of varigram model estimates
extract_variogram_estimates <- function(variog_fitted, kappa, epsilon) {
	
	variog_fit <- variog_fitted$variog_fit
	robust_variog_fit <- variog_fitted$robust_variog_fit
	res_table <- data.frame(
		"kappa" = kappa,
		"epsilon" = epsilon,
		"nugget" = variog_fit$nugget, 
		"sigmasq" = variog_fit$cov.pars[1],
		"phi" = variog_fit$cov.pars[2],
		"robust_nugget" = robust_variog_fit$nugget,
		"robust_sigmasq" = robust_variog_fit$cov.pars[1],
		"robust_phi" = robust_variog_fit$cov.pars[2]
	)
	return(res_table)
	
}


# Simulation --------------------------------------------------------------

## parameters
n_sim = 1000
grid_sim = "reg"
mean_sim = 0
cov_sim = c(1, 0.25) # sigma^2, phi
kappa <- c(5, 10)
epsilon <- c(0.05, 0.1, 0.2)
params_grid <- rbind(c(0, 1), expand.grid(epsilon, kappa)) |> 
	as.data.frame() |> 
	purrr::set_names(c("epsilon", "kappa"))

## step-by-step simulation
# sim_res <- simulate_grfs(
# 	n = n_sim, grid = grid_sim, 
# 	mean = mean_sim, cov.pars = cov_sim, 
# 	nugget = 0, kappa = 1, seed = 1992
# )
# sim_sampled <- sample_grfs(sim_res, epsilon = 0) 
# sim_combined <- combine_results(sim_sampled)
# sim_combined

# multiple simulations
multi_sim_res <- purrr::map2(
	params_grid$kappa, params_grid$epsilon,
	~ simulate_grfs(
		n = n_sim, grid = grid_sim, 
		mean = mean_sim, cov.pars = cov_sim, 
		nugget = 0, kappa = .x, seed = 1992
	) |> 
		sample_grfs(epsilon = .y) |> 
		combine_results()
)



# Exploration -------------------------------------------------------------

geoR_sim <- multi_sim_res |> purrr::map("geoR") 
purrr::map(
	seq_along(geoR_sim), 
	~ plot_grf(
		geoR_sim[[.x]], 
		kappa = params_grid$kappa[.x], epsilon = params_grid$epsilon[.x]
	)
)

image(multi_sim_res[[1]]$geoR, main = "kappa = 0; epsilon = 0")
par(mfrow = c(2, 3))
for (i in 2:length(multi_sim_res)) {
	tmp_title <- paste0("kappa = ", params_grid$kappa[i], "; epsilon = ", params_grid$epsilon[i])
	tmp_res <- multi_sim_res[[i]]$geoR
	image(tmp_res, main = tmp_title)
}



# Estimation --------------------------------------------------------------

# geodata conversion
multi_geo_data <- multi_sim_res |> 
	purrr::map("mat") |> 
	purrr::map(geodata_transform)
multi_geo_data |> purrr::map(plot)

# step-by-step estimation
# br <- seq(0, 1, 0.05)
## Classical Variogram (Matheron)
# sample_variog <- variog(multi_geo_data[[1]], uvec = br, estimator.type = "classical")
# variog_fit <- variofit(sample_variog, cov.model = "exp")
## Robust Variogram (Cressie)
# robust_variog <- variog(multi_geo_data[[1]], uvec = br, estimator.type = "modulus")
# robust_variog_fit <- variofit(robust_variog, cov.model = "exp")
# par(mfrow = c(1, 2))
# plot(sample_variog)
# lines(variog_fit, col = 2)
# plot(robust_variog)
# lines(robust_variog_fit, col = 2)
# title(main = "Variogram Estimation", outer = TRUE)

# multiple estimations
cov_model <- "exp" # "linear", "sph", "exp"
weights_model <- "equal" # "npairs", "cressie", "equal"
multi_variog_fitted <- multi_geo_data |> 
	purrr::map(~ variog_fit(.x, cov.model = cov_model, weights = weights_model))

purrr::map(
	seq_along(multi_variog_fitted), 
	~ extract_variogram_estimates(
		multi_variog_fitted[[.x]], 
		kappa = params_grid$kappa[.x], epsilon = params_grid$epsilon[.x]
	)
) |> 
	dplyr::bind_rows() |> 
	dplyr::mutate("process" = ifelse(kappa == 1 & epsilon == 0, "GRF", "CGRF"), .before = 1) |> 
	dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., digits = 2))) |> 
	insight::export_table(df, format = "html")

purrr::map(
	seq_along(multi_variog_fitted), 
	~ plot_variogram(
		multi_variog_fitted[[.x]], 
		kappa = params_grid$kappa[.x], epsilon = params_grid$epsilon[.x], 
		geoR_simulation = geoR_sim[[.x]],
		plot_type = 1
	)
)

purrr::map(
	seq_along(multi_variog_fitted), 
	~ plot_variogram(
		multi_variog_fitted[[.x]], 
		kappa = params_grid$kappa[.x], epsilon = params_grid$epsilon[.x], 
		plot_type = 2
	)
)

plot_variogram(
	multi_variog_fitted[[1]], 
	kappa = params_grid$kappa[1], epsilon = params_grid$epsilon[1], 
	plot_type = 2
)
par(mfrow = c(2, 3))
for (i in 2:length(multi_variog_fitted)) {
	plot_variogram(
		multi_variog_fitted[[i]], 
		kappa = params_grid$kappa[i], epsilon = params_grid$epsilon[i], 
		plot_type = 2
	)
}

purrr::map(
	seq_along(multi_variog_fitted), 
	~ plot_variogram(
		multi_variog_fitted[[.x]], 
		kappa = params_grid$kappa[.x], epsilon = params_grid$epsilon[.x], 
		plot_type = 3
	)
)

purrr::map(
	seq_along(multi_variog_fitted), 
	~ plot_variogram(
		multi_variog_fitted[[.x]], 
		kappa = params_grid$kappa[.x], epsilon = params_grid$epsilon[.x], 
		plot_type = 4
	)
)
