#!/bin/sh

rm -f contRV_0.1.0.tar.gz

mv ContRV/demo.R demo.R

R CMD build ContRV

R CMD INSTALL contRV_0.1.0.tar.gz

mv demo.R ContRV/demo.R