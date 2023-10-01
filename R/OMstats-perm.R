################################################################################
#### OM descriptive statistics & SPM priors ####################################
################################################################################
if(FALSE){
require(FLCore)
require(FLBRP)
require(mpb)
require(ggplotFL)

pars=lhPar(FLPar(linf=100))

### Entropy ##################################################
require(statcomp)

data(ple4)
data(ple4brp)

permutation_entropy(ordinal_pattern_distribution(x=ssb(ple4), ndemb=5))
permutation_entropy(ordinal_pattern_distribution(x=stock(ple4), ndemb=5))
permutation_entropy(ordinal_pattern_distribution(x=FLCore:::vb(ple4), ndemb=5))
permutation_entropy(ordinal_pattern_distribution(x=rec(ple4), ndemb=5))

permutation_entropy(ordinal_pattern_distribution(x=seq(0,10,0.1), ndemb=5))
permutation_entropy(ordinal_pattern_distribution(x=rlnorm(100), ndemb=5))
}