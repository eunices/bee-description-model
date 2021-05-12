get_loo <- function(fit) {

    # manual/ alternative code to `extract_log_lik`
    m_log_lik <- as.array(fit, pars = "log_lik")
    m_log_lik <- m_log_lik[,,apply(m_log_lik, 3, sd) != 0]

    m_r_eff <- relative_eff(exp(m_log_lik), cores = 2)

    m_loo <- loo(m_log_lik, r_eff = m_r_eff, cores = 2)
    m_loo

}

list_to_df <- function(x, i) {
    x <- setDT(tstrsplit(as.character(x), ", ", fixed=TRUE))[]
    names(x) <- paste0("year_", 1:dim(x)[2])
    
    x[] <- lapply(x, function(x) {
        x <- gsub("c\\(|\\)", "", x)
        as.numeric(x)
    })

    x$group <- 1:dim(x)[1]
    x$sim <- i
    x
}


# All code below obtained from  
# https://github.com/betanalpha/knitr_case_studies/blob/master/qr_regression/stan_utility.R
# based on https://betanalpha.github.io/assets/case_studies/rstan_workflow.html

# Check transitions that ended with a divergence
check_div <- function(fit) {

	print(paste0(Sys.time(),  " ---- CHECKING DIVERGENCES: "))

	sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
	divergent <- do.call(rbind, sampler_params)[,'divergent__']
	n <- sum(divergent)
	N <- length(divergent)

	print(sprintf(
		'%s of %s iterations ended with a divergence (%s%%)',
		n, N, round(100 * n / N, 2)
	))

	if (n > 0)
		print('Try running with larger adapt_delta to remove the divergences')
}


# Check transitions that ended prematurely due to maximum tree depth limit
check_treedepth <- function(fit, max_depth = 12) {

	print(paste0(Sys.time(),  " ---- CHECKING TREE DEPTH: "))

	sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
	treedepths <- do.call(rbind, sampler_params)[,'treedepth__']
	n <- length(treedepths[sapply(treedepths, function(x) x == max_depth)])
	N <- length(treedepths)

	print(sprintf(
		'%s of %s iterations saturated the maximum tree depth of %s (%s%%)',
		n, N, max_depth, round(100 * n / N, 2)
	))
				
	if (n > 0)
		print('Run again with max_depth set to a larger value to avoid saturation')
}


# Checks the energy Bayesian fraction of missing information (E-BFMI)
check_energy <- function(fit) {

	print(paste0(Sys.time(),  " ---- CHECKING E-BFMI: "))

	sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
	no_warning <- TRUE
	for (n in 1:length(sampler_params)) {

		energies <- sampler_params[n][[1]][,'energy__']
		numer <- sum(diff(energies)**2) / length(energies)
		denom <- var(energies)

		if (numer / denom < 0.2) {

			print(sprintf('Chain %s: E-BFMI = %s', n, 
						  round(numer / denom * 100, 2)))
			no_warning <- FALSE

		}
  }

  if (no_warning)
    print('E-BFMI indicated no pathological behavior')
  else
    print('E-BFMI below 0.2 indicates you may need to reparameterize your model')

}


# Checks the effective sample size per iteration
check_n_eff <- function(fit) {

	print(paste0(Sys.time(),  " ---- CHECKING N EFFECTIVE SAMPLE SIZE: "))

	fit_summary <- summary(fit, probs = c(0.5))$summary
	N <- dim(fit_summary)[[1]]

	iter <- dim(rstan::extract(fit)[[1]])[[1]]

	no_warning <- TRUE
	for (n in 1:N) { # each param

		ratio <- fit_summary[, 5][n] / iter # param n_eff / iterations

		if (ratio < 0.001 & !is.na(ratio)) {
			
			print(sprintf(
				'n_eff / iter for parameter %s is %s!',
				rownames(fit_summary)[n], ratio
			))

			no_warning <- FALSE

		}
		
		# if (is.na(ratio)) {
		# 	print(sprintf('Note: Parameter %s is NA', rownames(fit_summary)[n]))
		# }

	}

	if (no_warning)
		print('n_eff / iter looks reasonable for all parameters')
	else
		print('  n_eff / iter below 0.001 indicates that the effective sample size has likely been overestimated')

}


# Checks the potential scale reduction factors
check_rhat <- function(fit) {

	print(paste0(Sys.time(),  " ---- CHECKING RHAT: "))

	fit_summary <- summary(fit, probs = c(0.5))$summary
	N <- dim(fit_summary)[[1]]

	no_warning <- TRUE
	for (n in 1:N) {

		rhat <- fit_summary[, 6][n]

		if ((rhat > 1.05 || is.infinite(rhat)) & !is.na(rhat)) {

			print(sprintf('Rhat for parameter %s is %s!',
						rownames(fit_summary)[n], rhat))

			no_warning <- FALSE

		}

		# if (is.nan(rhat)) {
		# 	print(sprintf(
		# 		'Note: Parameter %s is NA', rownames(fit_summary)[n]
		# 	))
		# }

	}

	if (no_warning)
		print('Rhat looks reasonable for all parameters')
	else
		print('Rhat above 1.1 indicates that the chains very likely have not mixed')
}


check_all_evaluate <- function(fit, tree_depth = 12) {
	check_n_eff(fit)
	check_rhat(fit)
	check_div(fit)
	check_treedepth(fit, tree_depth)
	check_energy(fit)
}

# Returns parameter arrays separated into divergent and non-divergent transitions
partition_div <- function(fit) {
	nom_params <- rstan::extract(fit, permuted=FALSE)
	n_chains <- dim(nom_params)[2]
	params <- as.data.frame(do.call(rbind, 
	lapply(1:n_chains, function(n) nom_params[,n,])))

	sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
	divergent <- do.call(rbind, sampler_params)[,'divergent__']
	params$divergent <- divergent

	div_params <- params[params$divergent == 1,]
	nondiv_params <- params[params$divergent == 0,]

	return(list(div_params, nondiv_params))
}

