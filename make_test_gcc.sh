#!/bin/sh
g++ cgal_polyhedra.cc -std=c++11 -o cc_cgal_polyhedra.o -c -frounding-math

gfortran cc_cgal_polyhedra.o cgal_polyhedra.f90 test.f90 -lstdc++ -lCGAL -o test

