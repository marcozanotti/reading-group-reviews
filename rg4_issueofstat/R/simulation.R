
# Install & Load ----------------------------------------------------------

install.packages("tidyverse")
install.packages("broom")
install.packages("mice")
install.packages("quantreg")
install.packages("DT")
install.packages("naniar")
install.packages("patchwork")

library(tidyverse)
library(broom)
library(mice)
library(quantreg)
library(DT)
library(naniar)
library(patchwork)



# Functions ---------------------------------------------------------------

# function to simulate data
simulate_data <- function(n_sim, min_vector, max_vector, intercetpt, beta_vector, seed = 1992) {
	set.seed(seed)
	x <- runif(n_sim, min_vector[1], max_vector[1])
	z <- runif(n_sim, min_vector[2], max_vector[2])
	e <- rnorm(n_sim, 0, 1)
	y <- intercetpt + beta_vector[1] * x + beta_vector[2] * z + e
	data <- tibble::tibble("y" = y, "x" = x, "z" = z)
	return(data)
}
# simulate_data <- function(n_sim, mean_vector, sigma_vector, intercetpt, beta_vector, seed = 1992) {
# 	set.seed(seed)
# 	x <- rnorm(n_sim, mean_vector[1], sigma_vector[1])
# 	z <- rnorm(n_sim, mean_vector[2], sigma_vector[2])
# 	e <- rnorm(n_sim, 0, 1)
# 	y <- intercetpt + beta_vector[1] * x + beta_vector[2] * z + e
# 	data <- tibble::tibble("y" = y, "x" = x, "z" = z)
# 	return(data)
# }

# function to simulate missing values
simulate_missing <- function(data, p_miss, seed = 1992) {
	data_sim <- data
	set.seed(seed)
	z_missing <- sample(c(TRUE, FALSE), nrow(data_sim), replace = TRUE, prob = c(p_miss, 1 - p_miss))
	data_sim$z[z_missing] <- NA_real_
	return(data_sim)
}

# function to impute missing values
impute_data <- function(data, method) {
	if (method == "remove") {
		data_complete <- data |> tidyr::drop_na()
	} else if (method == "median") {
		data_complete <- data |> 
			dplyr::mutate(z = ifelse(is.na(z), median(z, na.rm = TRUE), z))
	} else {
		data_complete <- mice(data, m = 1, method = method, printFlag = FALSE) |> 
			complete() |> 
			tibble::as_tibble()
	}
	return(data_complete)
}

# function to impute multiple datasets
impute_multiple_data <- function(data_list, method) {
	data_list_imputed <- vector("list", length(data_list) * length(method))
	for (i in seq_along(data_list)) {
		data_tmp <- purrr::map(method, ~ impute_data(data_list[[i]], .x))
		idx <- 1:4 + (4 * (i - 1))
		data_list_imputed[idx] <- data_tmp
	}
	return(data_list_imputed)
}

# function to fit rq model
fit_rq <- function(data, tau) {
	mod_fit <- rq(y ~ x + z, tau = tau, data = data)
	return(mod_fit)
}

# function to get the summary of rq model
summary_rq <- function(rq_fit, se = "ker", R = 200) {
	if (se == "ker") {
		summ <- summary(rq_fit, se = "ker")
	} else if (se == "boot") {
		summ <- summary(rq_fit, se = "boot", R = R)
	} else {
		stop("se must be 'ker' or 'boot'")
	}
	return(summ)
}

# function to extract coefficients from summary of rq model in a tidy format
extract_rq_coef <- function(summary_rq) {
	coefficients <- summary_rq$coefficients |> 
		as.data.frame() |> 
		tibble::rownames_to_column("term") |> 
		tibble::as_tibble() |> 
		dplyr::select(1:3) |> 
		purrr::set_names(c("term", "estimate", "std.error"))
	return(coefficients)
}

# function to convert summary of rq model to tidy format
tidy_rq <- function(summary_rq) {
	n_taus <- length(summary_rq[["tau"]])
	if (n_taus == 1) {
		summ_taus <- summary_rq[["tau"]]
		summ_tidy <- summary_rq |> 
			extract_rq_coef() |> 
			dplyr::mutate("tau" = summ_taus)
	} else {
		summ_taus <- summary_rq |> purrr::map("tau") |> unlist()
		summ_tidy <- purrr::map(summary_rq, extract_rq_coef) |> 
			purrr::map2(summ_taus, ~ mutate(.x, "tau" = .y)) |>
			purrr::set_names(paste0("tau_", summ_taus))
	}
	return(summ_tidy)
}

# function to extract results
extract_model_table <- function(data, .parameter, .digits = 4) {
	res_tbl <- data |> 
		dplyr::select(dplyr::all_of(c(.parameter, "method", "p_miss"))) |>
		dplyr::mutate(p_miss = paste0("p_miss = ", p_miss)) |> 
		tidyr::pivot_wider(names_from = "p_miss", values_from = dplyr::all_of(.parameter)) |> 
		dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., digits = .digits)))
	return(res_tbl)
}

extract_model_table2 <- function(data, .digits = 4) {
	res_tbl <- data |> 
		dplyr::select(dplyr::all_of(c("estimate", "std.error", "method", "p_miss", "coverage"))) |>
		dplyr::mutate(
			value = paste0(
				round(estimate, digits = .digits), " (", 
				round(std.error, digits = .digits), " - ", 
				round(coverage, digits = .digits), ")"),
			p_miss = paste0("p_miss = ", p_miss)
		) |> 
		dplyr::select(-estimate, -std.error, -coverage) |> 
		tidyr::pivot_wider(names_from = "p_miss", values_from = "value")
	return(res_tbl)
}

# function to draw html table
dt_table <- function(data, title = "", caption = "", rownames = FALSE) {
	
	p_len <- nrow(data)
	res <- data |>
		DT::datatable(
			extensions = "Buttons",
			options = list(
				pageLength = p_len,
				paging = FALSE,
				searching = FALSE,
				ordering = FALSE,
				lenghtChange = FALSE,
				autoWidth = FALSE,
				dom = "Bfrtip",
				buttons = c("copy", "print", "csv", "excel", "pdf"),
				drawCallback = DT::JS(
					c(
						"function(settings){",
						"  var datatable = settings.oInstance.api();",
						"  var table = datatable.table().node();",
						paste0("  var caption = '", caption, "'"),
						"  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
						"}"
					)
				)
			),
			rownames = rownames,
			caption = title
		)
	return(res)
	
}

# function to compute coverage
compute_coverage <- function(y, lower, upper) {
	coverage <- sum(y >= lower & y <= upper) / length(y)
	return(coverage)
}

# function to perform bootstrap imputing procedure
boostrap_missing_rq <- function(
		data, .p_miss, .impute_method, .taus, .n_boot, .ci_quantile = 0.975, verbose = TRUE
) {
	
	if (verbose) 
		cat(paste0(
			"Estimating Quantile Regression for: p_miss = ", .p_miss, 
			", impute_method = ", .impute_method, "\n"
		))
	data_miss <- simulate_missing(data, .p_miss, seed = 7)
	n <- nrow(data_miss)
	boot_coverage <- vector("numeric", .n_boot)
	boot_estimates <- vector("list", .n_boot)
	
	for (b in seq_len(.n_boot)) {
		
		if (verbose) cat(paste0("Bottstrap iteration: ", b, "\n"))
		idx_tmp <- sample(1:n, size = n, replace = TRUE) # boot id
		boot_data <- data_miss[idx_tmp, ] # boot sample
		boot_imputed <- impute_data(boot_data, .impute_method) # imputation
		boot_fit <- fit_rq(boot_imputed, .taus) # quantile regression fitting
		boot_pred <- predict(boot_fit) # compute predictions for each quantile
		boot_coverage[b] <- compute_coverage(
			y = boot_data$y, 
			lower = boot_pred[, 1], # lower quantile
			upper = boot_pred[, ncol(boot_pred)] # upper quantile
		)
		boot_estimates[[b]] <- boot_fit |> # extract summary of estimates
			summary_rq(se = "ker") |> 
			tidy_rq() |> 
			dplyr::bind_rows() |> 
			dplyr::mutate("boot" = b, .before = 1)
		
	}
	
	ci_q <- qnorm(.ci_quantile)
	boot_estimates <- boot_estimates |> dplyr::bind_rows()
	boot_estimates_aggr <- boot_estimates |> 
		dplyr::group_by(term, tau) |>
		dplyr::summarise("mean" = mean(estimate), "std.error" = sd(estimate), .groups = "drop") |> 
		dplyr::rename("estimate" = "mean") |> 
		dplyr::mutate("lower_95" =  estimate - ci_q * std.error, "upper_95" = estimate + ci_q * std.error) |>
		dplyr::mutate("method" = .impute_method, "p_miss" = .p_miss, .before = 1)
	
	boot_ci <- boot_estimates_aggr |> dplyr::select(term, tau, lower_95, upper_95)
	boot_beta_coverage <- vector("numeric", nrow(boot_ci))
	for (i in 1:nrow(boot_ci)) {
		term_tmp <- boot_ci$term[i]
		tau_tmp <- boot_ci$tau[i]
		lower_tmp <- boot_ci$lower_95[i]
		upper_tmp <- boot_ci$upper_95[i]
		betas_tmp <- boot_estimates |> 
			dplyr::filter(term == term_tmp, tau == tau_tmp) |> 
			dplyr::pull("estimate")
		boot_beta_coverage[i] <- compute_coverage(y = betas_tmp,	lower = lower_tmp, upper = upper_tmp)
	}
	boot_estimates_aggr <- boot_estimates_aggr |> 
		dplyr::mutate("coverage" = boot_beta_coverage)
	boot_coverage <- tibble::tibble(
		"method" = .impute_method, "p_miss" = .p_miss, "coverage" = mean(boot_coverage)
	)
	
	res <- list("estimates" = boot_estimates_aggr, "coverage" = boot_coverage)
	return(res)
	
}



# Simulation --------------------------------------------------------------

# * Simulate Data ---------------------------------------------------------
# simulation parameters
n_sim <- 500
min_vector <- c(3, -1)
max_vector <- c(8, 5)
intercept <- 0
beta_vector <- c(3, -0.5)

data_sim <- simulate_data(n_sim, min_vector, max_vector, intercept, beta_vector)

# * Simulate Missing Data ------------------------------------------------
# missing values parameters
p_miss <- c(0.1, 0.4)

data_miss <- purrr::map(p_miss, ~ simulate_missing(data_sim, .x, seed = 7))

purrr::walk(data_miss, ~ print(vis_miss(.x)))	
vis_miss(data_miss[[1]]) + vis_miss(data_miss[[2]]) 
purrr::walk(
	data_miss, 
	~ print(ggplot(data = .x, aes(x = z,	y = y)) + geom_miss_point())
)	


# Imputation ---------------------------------------------------------------

# imputation parameters
imp_method <- c("remove", "sample", "mean", "median")

data_imputed <- impute_multiple_data(data_miss, imp_method)
# l'ordine è 4 metodi per p 0.1 e 4 metodi per p 0.2



# Modelling ---------------------------------------------------------------

# modelling parameters
taus <- c(0.25, 0.5, 0.75)
param_grid <- rbind(expand.grid("complete", 0),	expand.grid(imp_method, p_miss)) |> 
	set_names(c("method", "p_miss"))
param_grid |> dt_table()

model_grid <- rbind(expand.grid("complete", 0),	expand.grid(imp_method, p_miss)) |> 
	set_names(c("method", "p_miss"))
model_grid
data_model <- c(list(data_sim), data_imputed)

model_fit <- purrr::map(data_model, ~ fit_rq(.x, taus))

res <- model_fit |> 
	purrr::map(summary_rq, se = "ker") |> 
	purrr::map(tidy_rq) |> 
	purrr::map(bind_rows) |> 
	purrr::map2(model_grid$p_miss, ~ mutate(.x, "p_miss" = .y)) |>
	purrr::map2(model_grid$method, ~ mutate(.x, "method" = .y)) |>
	bind_rows()

# create table of results
quant <- taus
terms <- c("x", "z")
params <- c("estimate", "std.error")

for (q in quant) {
	for (t in terms) {
		for (p in params) {
			tit <- paste0(
				"Variable: ", toupper(t), ", Parameter: ", 
				ifelse(p == "estimate", "Beta", "Std.Error"), 
				", Quantile = ", q
			)
			res |> 
				dplyr::filter(tau == q) |> 
				dplyr::filter(term == t) |> 
				extract_model_table(.parameter = p, .digits = 3) |> 
				set_names(c("Method", "0%", "10%", "40%")) |> 
				dt_table(title = tit) |> 
				print()
		}
	}
}

quant <- taus
terms <- c("x", "z")

for (q in quant) {
	for (t in terms) {
		tit <- paste0("Variable: ", toupper(t), ", Quantile = ", q)
		res |> 
			dplyr::filter(tau == q) |> 
			dplyr::filter(term == t) |> 
			extract_model_table2(.digits = 3) |> 
			set_names(c("Method", "0%", "10%", "40%")) |> 
			dt_table(title = tit) |> 
			print()
	}
}



# Bootstrapping -----------------------------------------------------------

# parameters
data_sim
p_miss <- c(0.1, 0.4)
imp_method <- c("sample", "mean", "median")
param_grid <- expand.grid(imp_method, p_miss) |> 
	set_names(c("method", "p_miss")) |> 
	mutate(method = as.character(method))
param_grid |> dt_table()
taus <- c(0.25, 0.5, 0.75)
n_boot <- 200

# testing
boostrap_missing_rq(
	data = data_sim, 
	.p_miss = 0.1, 
	.impute_method = "sample", 
	.taus = c(0.25, 0.5, 0.75), 
	.n_boot = 10,
	.ci_quantile = 0.975
)

# estimation
res <- purrr::map2(
	param_grid$method, param_grid$p_miss,
	~ boostrap_missing_rq(
		data = data_sim, 
		.p_miss = .y, 
		.impute_method = .x, 
		.taus = taus, 
		.n_boot = n_boot,
		.ci_quantile = 0.975,
		verbose = TRUE
	)
)
boot_est <- purrr::map(res, "estimates") |> dplyr::bind_rows()
boot_cov <- purrr::map(res, "coverage") |> dplyr::bind_rows()

quant <- taus
terms <- c("x", "z")
for (q in quant) {
	for (t in terms) {
		tit <- paste0("Variable: ", toupper(t), ", Quantile = ", q)
		boot_est |> 
			dplyr::filter(tau == q) |> 
			dplyr::filter(term == t) |> 
			extract_model_table2(.digits = 3) |> 
			set_names(c("Method", "10%", "40%")) |> 
			dt_table(title = tit) |> 
			print()
	}
}

boot_cov |> 
	pivot_wider(names_from = "p_miss", values_from = "coverage") |> 
	mutate(across(where(is.numeric), ~ round(.x, digits = 3))) |> 
	set_names(c("Method", "10%", "40%")) |> 
	dt_table(title = "Coverage")

