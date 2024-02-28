
# Install & Load ----------------------------------------------------------

install.packages("tidyverse")
install.packages("broom")
install.packages("mice")
install.packages("quantreg")
install.packages("DT")
install.packages("naniar")

library(tidyverse)
library(broom)
library(mice)
library(quantreg)
library(DT)
library(naniar)



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
extract_model_table <- function(data, .term, .parameter, .digits = 4) {
	res_tbl <- data |> 
		dplyr::filter(term == .term) |> 
		dplyr::select(dplyr::all_of(c(.parameter, "method", "p_miss"))) |>
		dplyr::mutate(p_miss = paste0("p_miss = ", p_miss)) |> 
		pivot_wider(names_from = "p_miss", values_from = all_of(.parameter)) |> 
		dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., digits = .digits)))
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



# Simulation --------------------------------------------------------------

# * Simulate Data ---------------------------------------------------------
# simulation parameters
n_sim <- 300
min_vector <- c(0, 0)
max_vector <- c(1, 1)
intercept <- 0
beta_vector <- c(2, 2)

data_sim <- simulate_data(n_sim, min_vector, max_vector, intercept, beta_vector)

# * Simulate Missing Data ------------------------------------------------
# missing values parameters
p_miss <- c(0.1, 0.2)

data_miss <- purrr::map(p_miss, ~ simulate_missing(data_sim, .x, seed = 7))

purrr::walk(data_miss, ~ print(vis_miss(.x)))	
purrr::walk(
	data_miss, 
	~ print(ggplot(data = .x, aes(x = z,	y = y)) + geom_miss_point())
)	


# Imputation ---------------------------------------------------------------

# imputation parameters
imp_method <- c("remove", "sample", "mean", "median")

data_imputed <- impute_multiple_data(data_miss, imp_method)



# Modelling ---------------------------------------------------------------

# modelling parameters
taus <- c(0.5) # seq(0.1, 0.9, by = 0.1)
model_grid <- rbind(expand.grid("complete", 0),	expand.grid(imp_method, p_miss)) |> 
	set_names(c("method", "p_miss"))
model_grid
data_model <- c(list(data_sim), data_imputed)

res_ker <- purrr::map(data_model, ~ fit_rq(.x, taus)) |> 
	purrr::map(summary_rq, se = "ker") |> 
	purrr::map(tidy_rq) |> 
	purrr::map(bind_rows) |> 
	purrr::map2(model_grid$p_miss, ~ mutate(.x, "p_miss" = .y)) |>
	purrr::map2(model_grid$method, ~ mutate(.x, "method" = .y)) |>
	bind_rows()

res_boot <- purrr::map(data_model, ~ fit_rq(.x, taus)) |> 
	purrr::map(summary_rq, se = "boot", R = 200) |> 
	purrr::map(tidy_rq) |> 
	purrr::map(bind_rows) |> 
	purrr::map2(model_grid$p_miss, ~ mutate(.x, "p_miss" = .y)) |>
	purrr::map2(model_grid$method, ~ mutate(.x, "method" = .y)) |> 
	bind_rows()


# create table of results
quant <- taus
terms <- c("x", "z")
params <- c("estimate", "std.error")
results <- res_ker # cambiare con res_boot

for (q in quant) {
	for (t in terms) {
		for (p in params) {
			tit <- paste0(
				"Variable: ", toupper(t), ", Parameter: ", 
				ifelse(p == "estimate", "Beta", "Std.Error"), 
				", Quantile = ", q
			)
			results |> 
				dplyr::filter(tau == q) |> 
				extract_model_table(.term = t, .parameter = p, .digits = 3) |> 
				set_names(c("Method", "0%", "10%", "20%")) |> 
				dt_table(title = tit) |> 
				print()
		}
	}
}

