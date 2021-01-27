#!/bin/sh

rm -f contRV_0.1.0.tar.gz

mv R/demo.R demo.R

R CMD build .

R CMD INSTALL contRV_0.1.0.tar.gz

mv demo.R R/demo.R
