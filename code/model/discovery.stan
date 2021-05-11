functions { 

	vector count_series_lp(
		int[] y,   // data for spp. 
		int[] off, // data for publications as offset

		// Poisson temporal correlation
		real phi, vector delta, real alpha, real beta, 

		// Zero-inflation
		real gamma, real eta
	) {   

		int Y;
		vector[size(y)] loglik;

		vector[size(y)] lambda;
		vector[size(y)] omega;

		vector[size(y)] theta;

		Y = size(y);

		// temporal correlation
		omega[1] = phi;
		lambda[1] = omega[1];
		
		for(i in 2:Y) {
			omega[i] = exp(delta[1] + delta[2] * i);
			lambda[i] = omega[i] + 
						alpha * y[i - 1] +     // short term lag
						beta * lambda[i - 1];  // long term trend
		}

		// zero inflation as two state markov process
		theta[1] = 0;

		for(i in 2:Y) {
			if(y[i-1] == 0) { // dependent on previous time step
				theta[i] = gamma; 
			} else {
				theta[i] = eta;
			}
		}

		// log prob
		for(i in 1:Y) {

			if(y[i] == 0) {
				loglik[i] = log_sum_exp(
					bernoulli_lpmf(1 | theta[i]),
					
					bernoulli_lpmf(0 | theta[i]) + 
					poisson_lpmf(y[i] | (off[i] + 1) * lambda[i])
				);
			} else {
				loglik[i] = bernoulli_lpmf(0 | theta[i]) +
					poisson_lpmf(y[i] | (off[i] + 1) * lambda[i]);
			}

		}
		
		return loglik;
	}

}


data {
	int<lower=0> N; // N years
	int<lower=0> P; // P groups
	
	int<lower=0> str[P]; // index of 1st value that is != 0 for each group
	int<lower=0> end[P]; // length of each group

	int<lower=0> counts[P, N]; // N species data [group x years]
	int<lower=0> off[P, N];    // N offset data [group x years]
}


parameters {
	// Poisson model at t=1
	real<lower=0> phi[P];   
	real mu_phi;            
	real<lower=0> sigma_phi;

	// Poisson model for t>1
	vector[2] delta[P];
	vector[2] mu;
	corr_matrix[2] Omega;
	vector<lower=0>[2] tau;

	// Temporal autocorrelation for t>1
	real<lower=0,upper=1> alpha[P];
	real<lower=0,upper=1> beta_unc[P];

	// Zero-inflation
	real<lower=0,upper=1> gamma[P];
	real<lower=0,upper=1> eta[P];
}


transformed parameters {
	real<lower=0,upper=1> beta[P];
	cov_matrix[2] Sigma;

	matrix[P, N] log_lik;

	for(p in 1:P) {
		beta[p] = (1 - alpha[p]) * beta_unc[p];
	}

	Sigma = quad_form_diag(Omega, tau);

	// Update log prob
	for(p in 1:P) {

		log_lik[p] =  to_row_vector(
			append_row(

				// pad log_lik_row with 0
				rep_vector(0, (N - size(counts[p][str[p]:end[p]]))),
				count_series_lp(
					counts[p][str[p]:end[p]], off[p][str[p]:end[p]], 
					phi[p], delta[p], alpha[p], beta[p], 
					gamma[p], eta[p]
				)
			)
		);

	}
}


model {

	// At t=1
	mu_phi ~ normal(0, 1);
	sigma_phi ~ cauchy(0, 1);
	phi ~ lognormal(mu_phi, sigma_phi);

	# For t>1
	// Coefficient delta
	mu[1] ~ normal(0, 5);
	mu[2] ~ normal(0, 1); 

	tau ~ cauchy(0, 1);   // for Sigma
	Omega ~ lkj_corr(2);  // for Sigma

	for(p in 1:P) {
		delta[p] ~ multi_normal(mu, Sigma);
	}

	// Temporal autocorrelation for alpha and beta lags
	for(p in 1:P) {
		alpha[p] ~ beta(1, 3);
		beta_unc[p] ~ beta(1, 3);
	}

	// Update log prob
	for(p in 1:P) {
		target += sum(log_lik[p]);
	}

}

generated quantities {

}
