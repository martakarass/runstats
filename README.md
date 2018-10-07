
[![Travis build status](https://travis-ci.com/martakarass/runstats.svg?branch=master)](https://travis-ci.com/martakarass/runstats)

<!-- README.md is generated from README.Rmd. Please edit that file -->
runstats
========

Provides methods for fast computation of running sample statistics for time series. These include: (1) mean, (2) standard deviation, and (3) variance over a fixed-length window of time-series, (4) correlation, (5) covariance, and (6) Euclidean distance (L2 norm) between short-time pattern and time-series. Implemented methods utilize Convolution Theorem to compute convolutions via Fast Fourier Transform (FFT).
