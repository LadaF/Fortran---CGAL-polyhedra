program test
  use iso_c_binding
  use cgal_polyhedra

  type(c_ptr) :: ptr
  
  !outside reference point
  real :: refx = 1000., refy = 1000., refz = 1000.
  !bounding box
  real :: xmin,ymin,zmin, xmax,ymax,zmax
  !closest point
  real :: xclose, yclose, zclose

  call cgal_polyhedron_read(ptr, "X.off")
  
  call cgal_polyhedron_bbox(ptr, xmin,ymin,zmin, xmax,ymax,zmax)
  
  print *, "Bounding box:", xmin,ymin,zmin, xmax,ymax,zmax

  print *, "Is point (30., 10., 30.) inside?:", cgal_polyhedron_inside(ptr, 30., 10., 30., refx, refy, refz)
  
  print *, "Is point (0., 0., 0.) inside?:", cgal_polyhedron_inside(ptr, 0., 0., 0., refx, refy, refz)
  
  call cgal_polyhedron_closest(ptr, 0., 0., 0., xclose, yclose, zclose)
  
  print *, "Closest point to (0., 0., 0.):", xclose, yclose, zclose
  
  call cgal_polyhedron_finalize(ptr)
  
end program
  
  
  
  