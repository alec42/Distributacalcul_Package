




E_weibull <- function(tau, beta, k = 1)
{
    1/(beta^k) * gamma(1 + k/tau)
}

V_weibull <- function(tau, beta)
{
    E_weibull(tau = tau, beta = beta, k = 2) - (E_weibull(tau = tau, beta = beta, k = 1))^2
}

#### Lognormale ####


#### Beta ####



#### Erlang ####

E_erlang <- function(n, b)
{
    n/b
}

V_erlang <- function(n, b)
{
    n/b^2
}

perlang <- function(x, n, b, lower.tail = T)
{
    if(lower.tail == T)
        (1 - exp(-b * x) * sum(sapply(0:(n - 1), function(j) ((b * x)^j)/factorial(j))))
    else
        exp(-b * x) * sum(sapply(0:(n - 1), function(j) ((b * x)^j)/factorial(j)))
}

derlang <- function(x, n, b)
{
    ((b ^ n) / gamma(n)) * (x^(n - 1)) * exp(-b * x)
}

#### Log-logistique ####


V_llogis <- function(lam, tau)
{
    kthmoment_llogis(k = 2,
                     lam = lam,
                     tau = tau) -
        kthmoment_llogis(k = 1,
                         lam = lam,
                         tau = tau)^2
}



#### Hypergéometrique ####

E_hyper <- function(N, m, n)
{
    n * (m / N)
}

V_hyper <- function(N, m, n)
{
    (n * (m / N)) * ((((n - 1) * (m - 1)) / (N - 1)) + 1 - (n * (m / N)))
}

#### Pareto ####


#### Logarithmique ####

E_logarithmique <- function(gam)
{
    (-gam) / (log(1 - gam) * (1 - gam))
}

V_logarithmique <- function(gam)
{
    (gam + log(1 - gam)) / ((1 - gam)^2 * (log(1 - gam))^2)
}


E_IG <- function(mu)
{
    mu
}

V_IG <- function(mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    mu * beta
}

d_IG <- function(x, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    dinvgauss(x = x, mean = mu, dispersion = dispersion)
}

p_IG <- function(q, mu, beta = dispersion * mu^2, dispersion = beta / mu^2, lower.tail = T)
{
    pinvgauss(q = q, mean = mu, dispersion = dispersion, lower.tail = lower.tail)
}


#### Poisson Composée ####

E_PCOMP <- function(rate, shape, lambda, distr_severity = "Gamma"){
    if(distr_severity == "Gamma"){
        shape / rate * lambda
    }
    else if (distr_severity == "Lognormale"){
        exp(shape + rate / 2) * lambda
    }
}

V_PCOMP <- function(rate, shape, lambda, distr_severity = "Gamma"){
    if(distr_severity == "Gamma"){
        lambda * shape/(rate) * (shape + 1)
    }
    else if (distr_severity == "Lognormale"){
        lambda * kthmoment_lnorm(k = 2, mu = shape, sig = sqrt(rate))
    }
}

p_Pcomp <- function(x, lambda, shape, rate, ko = 300, distr_severity = "Gamma"){
    if(distr_severity == "Gamma"){
        (dpois(x = 0, lambda = lambda) + sum(sapply(1:ko, function(k) dpois(x = k, lambda = lambda) * pgamma(q = x, shape = shape * k, rate = rate))))
    }
    else if (distr_severity == "Lognormale"){
        dpois(x = 0, lambda = lambda) + sum(sapply(1:ko, function(k) dpois(x = k, lambda = lambda) * plnorm(q = x, meanlog = shape * k, sdlog = sqrt(rate))))
    }
}

VaR_PComp <- function(k, ko = 300, lambda, shape, rate, distr_severity = "Gamma")
{
    if(k <= p_Pcomp(x = 0, lambda = lambda, shape = shape, rate = rate, ko = ko))
        0
    else
        optimize(function(i) abs(p_Pcomp(x = i, lambda = lambda, shape = shape, rate = rate, ko = ko) - k), c(0, ko))$minimum
}




#### BN Composée ####

E_BNComp <- function(r, q, shape, rate, distr_severity = "Gamma")
{
    if(distr_severity == "Gamma")
    {
        shape / rate * (r * (1 - q)/q)
    }
    else if (distr_severity == "Lognormale")
        (r * (1 - q)/q) * E_lnorm(mu = shape, sig = sqrt(rate))
}

V_BNComp <- function(r, q, shape, rate, distr_severity = "Gamma")
{
    if(distr_severity == "Gamma")
    {
        (shape / rate)^2 * (r * (1 - q)/(q^2)) + (shape / rate^2) * (r * (1 - q)/q)
    }
    else if (distr_severity == "Lognormale")
        (r * (1 - q)/q)*(E_lnorm(mu = shape, sig = sqrt(rate))/q + V_lnorm(mu = shape, sig = sqrt(rate)))
}

# pas certain si la LNORM est bien faite
p_BNComp <- function(x, r, q, ko, shape, rate, distr_severity = "Gamma")
{
    if(distr_severity == "Gamma")
    {
        dnbinom(x = 0, size = r, prob = q) + sum(sapply(1:ko, function(i) dnbinom(x = i, size = r, prob = q) * pgamma(q = x, shape = shape * i, rate = rate)))
    }
    else if (distr_severity == "Lognormale")
    {
        dnbinom(x = 0, size = r, prob = q) + sum(sapply(1:ko, function(i) dnbinom(x = i, size = r, prob = q) * plnorm(q = x, meanlog = shape * i, sdlog = sqrt(rate))))
    }
}

VaR_BNComp <- function(k, ko = 300, r, q, shape, rate, distr_severity = "Gamma")
{
    if(k <= p_BNComp(0, r = r, q = q, ko = ko, shape = shape, rate = rate))
        0
    else
        optimize(function(i) abs(p_BNComp(i, r = r, q = q, ko = ko, shape = shape, rate = rate) - k), c(0, ko))$minimum
}

#### Bin Composée ####


E_BINComp <- function(n, q, shape, rate, distr_severity = "Gamma")
{
    if(distr_severity == "Gamma")
    {
        shape / rate * n * q
    }
    else if (distr_severity == "Lognormale")
        n * q * E_lnorm(mu = shape, sig = sqrt(rate))
}

V_BINComp <- function(n, q, shape, rate, distr_severity = "Gamma")
{
    if(distr_severity == "Gamma")
    {
        (shape / rate)^2 * n * q * (1 - q) + n * q * V_gamma(a = shape, b = rate)
    }
    else if (distr_severity == "Lognormale")
        E_lnorm(mu = shape, sig = sqrt(rate))^2 * n * q * (1 - q) + n * q * V_lnorm(mu = shape, sig = rate)
}

p_BINComp <- function(x, n, q, ko, shape, rate, distr_severity = "Gamma")
{
    if(distr_severity == "Gamma")
    {
        dbinom(x = 0, size = n, prob = q) + sum(sapply(1:ko, function(i) dbinom(x = i, size = n, prob = q) * pgamma(q = x, shape = shape * i, rate = rate)))
    }
    else if (distr_severity == "Lognormale")
    {
        dbinom(x = 0, size = n, prob = q) + sum(sapply(1:ko, function(i) dbinom(x = i, size = n, prob = q) * plnorm(q = x, meanlog = shape * i, sdlog = sqrt(rate))))
    }
}


VaR_BINComp <- function(k, ko = 300, n, q, shape, rate, distr_severity = "Gamma")
{
    if(k <= p_BINComp(0, n = n, q = q, ko = ko, shape = shape, rate = rate))
        0
    else
        optimize(function(i) abs(p_BINComp(i, n = n, q = q, ko = ko, shape = shape, rate = rate) - k), c(0, ko))$minimum
}




#### Uniforme Discrète ####
E_unifD <- function(a, b){
    (a + b)/2
}

V_unifD <- function(a, b){
    ((b - a + 1)^2 - 1)/12
}

d_unifD <- function(x, a, b){
    1 / (b - a + 1)
}

p_unifD <- function(q, a, b){
    if(q < a){
        return(0)
    }
    else if(a <= q & q < b){
        (round(q) - a + 1)/(b - a + 1)
    }
    else{
        return(1)
    }
}
