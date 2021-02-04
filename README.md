
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LRE <img src='man/figures/LRE_logo.jpg' align="right" height="138.5" />

## The Loads Regression Estimator

[![R-CMD-check](https://github.com/pkuhnert/LRE/workflows/R-CMD-check/badge.svg)](https://github.com/pkuhnert/LRE/actions)

`LRE` pkgdown site with vignette: <https://pkuhnert.github.io/LRE/>.

## About the LRE Package

This tool estimates loads using a generalised rating curve approach with
uncertainties.

## Installation

You can install a development version of the LRE package from
[GitHub](https://github.com/pkuhnert/LRE)

    # install.packages("devtools")
    remotes::install_github(repo = "pkuhnert/LRE", build_vignettes = TRUE, force = TRUE)

## Authors

Dr Petra Kuhnert, CSIRO Data61, Canberra, (Author and Maintainer)

Email: <Petra.Kuhnert@data61.csiro.au>

Dr Dan Pagendam, CSIRO Data61, Brisbane (Contributor)

Dr Brent Henderson, CSIRO Data61, Canberra (Contributor)

## Acknowledgements

Additional contributions have been made by:

Dr Stephen Lewis (JCU, Townsville)

Dr Zoe Bainbridge (JCU, Townsville)

Dr Ryan Turner (DSITI, Brisbane)

## LRE Methodology

We have developed a statistical methodology for estimating pollutant
loads with uncertainties. The approach is regression based and
incorporates a four step process:

1.  Methods for flow regularisation to correct for sampling bias,

2.  Statistical model for concentration

3.  The load calculated at regular time intervals, and

4.  An estimate of the uncertainty in the loads estimate.

The statistical model incorporates terms for flow, and other
characteristics of flow (e.g. rising or falling limb or flow history),
in an attempt to mimic some of the hydrological phenomena observed in
these complex systems, while two different sources of uncertainties are
captured by the model to address error in the concentration samples and
error in the flow measurements. The regression approach is flexible and
can be shown to encompass other existing load estimation methods such as
the average estimators.

## Examples

A vignette for the LRE package is available and contains examples
relating to loads estimation for the Burdekin and Tully end of catchment
sites.

    vignette("LRE")
    browseVignettes(package = "LRE")

## License

LRE is free under the GNU General Public License (GPL &gt;= 3.0)

The Burdekin and Tully flow and TSS datasets have been provided by DSTIA
and are provided under the Creative Commons Attribution 4.0.

Disclaimer: While the LRE package has attempted to foresee different
data characteristics and applications of the underlying methodology, the
use of the LRE package, or any part of it, is the sole risk of the User.

## References

Kuhnert, P.M., Henderson, B.L., Lewis, S.E., Bainbridge, Z.T.,
Wilkinson, S.N. and Brodie, J.E. (2012) [Quantifying total suspended
sediment export from the Burdekin River catchment using the loads
regression estimator
tool](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2011WR011080),
Water Resources Research, 48, W04533,<doi:10.1029/2011WR011080>.

Kroon, F.J., Kuhnert, P.M., Henderson, B.L., Wilkinson, S.N.,
Kinsey-Henderson, A., Abbott, B., Brodie, J.E. and Turner, R.D.
(2012)[River loads of suspended solids, nitrogen, phosphorus and
herbicides delivered to the Great Barrier Reef
Lagoon](https://www.sciencedirect.com/science/article/pii/S0025326X11005583),
Marine Pollution Bulletin, 65, 4-9, 167-181.
