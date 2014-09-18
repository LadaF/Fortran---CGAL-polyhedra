!(C) Vladimir Fuka 2014
!GNU GPL license v3
module CGAL_Polyhedra
  use iso_c_binding
  
  implicit none
  
  private
  
  public cgal_polyhedron_read, &
         cgal_polyhedron_closest, &
         cgal_polyhedron_inside, &
         cgal_polyhedron_intersects_ray, &
         cgal_polyhedron_bbox, &
         cgal_polyhedron_finalize
  
  type, bind(C) :: d3
    real(c_double) :: x, y, z
  end type
  
  interface
  
    subroutine polyhedron_from_file(ptree, fname, verbose, ierr) bind(C,name="polyhedron_from_file")
      import
      type(c_ptr),intent(out) :: ptree
      character(kind=c_char),intent(in) :: fname(*)
      integer(c_int),value :: verbose !avoid bool in C++, not equal to c_bool
      integer(c_int),intent(out) :: ierr
    end subroutine
    
    subroutine polyhedron_closest(ptree, query, near) bind(C,name="polyhedron_closest")
      import
      type(c_ptr),value :: ptree
      type(d3),intent(in) :: query
      type(d3),intent(out) :: near
    end subroutine
    
    function polyhedron_inside(ptree, query, outside_ref) result(res) bind(C,name="polyhedron_inside")
      import
      logical(c_bool) :: res
      type(c_ptr),value :: ptree
      type(d3),intent(in) :: query, outside_ref
    end function
    
    function polyhedron_intersects_ray(ptree, origin, vec) result(res) bind(C,name="polyhedron_intersects_ray")
      import
      logical(c_bool) :: res
      type(c_ptr),value :: ptree
      type(d3),intent(in) :: origin, vec
    end function
    
    subroutine polyhedron_bbox(ptree, min, max) bind(C,name="polyhedron_bbox")
      import
      type(c_ptr),value :: ptree
      type(d3),intent(out) :: min, max
    end subroutine
    
    subroutine polyhedron_finalize(ptree) bind(C,name="polyhedron_finalize")
      import
      type(c_ptr),intent(inout) :: ptree
    end subroutine
    
  end interface
  
  interface cgal_polyhedron_closest
    module procedure cgal_polyhedron_closest_s
    module procedure cgal_polyhedron_closest_d
  end interface
  
  interface cgal_polyhedron_inside
    module procedure cgal_polyhedron_inside_s
    module procedure cgal_polyhedron_inside_d
  end interface
  
  interface cgal_polyhedron_intersects_ray
    module procedure cgal_polyhedron_intersects_ray_s
    module procedure cgal_polyhedron_intersects_ray_d
  end interface
  
  interface cgal_polyhedron_bbox
    module procedure cgal_polyhedron_bbox_s
    module procedure cgal_polyhedron_bbox_d
  end interface
  
contains


  subroutine cgal_polyhedron_read(ptree, fname)
    type(c_ptr),intent(out) :: ptree
    character(*),intent(in) :: fname
    integer(c_int) :: ierr
    integer :: imaster
    
    call polyhedron_from_file(ptree, fname//c_null_char, 1, ierr)
    
    if (ierr==1) then
      write (*,*) "Error reading file "//fname//", it appears to be empty."
      stop
    else if (ierr==2) then
      write (*,*) "Error reading file "//fname//"."
      stop
    end if
  end subroutine

  subroutine cgal_polyhedron_closest_s(ptree, xq,yq,zq, xn,yn,zn)
    type(c_ptr),intent(in) :: ptree
    real(c_float),intent(in)  :: xq,yq,zq
    real(c_float),intent(out) :: xn,yn,zn
    type(d3) :: query, near
     
    query = d3(real(xq,c_double),real(yq,c_double),real(zq,c_double))
    call polyhedron_closest(ptree, query, near) 
    xn = real(near%x,c_float)
    yn = real(near%y,c_float)
    zn = real(near%z,c_float)
  end subroutine
  
  subroutine cgal_polyhedron_closest_d(ptree, xq,yq,zq, xn,yn,zn)
    type(c_ptr),intent(in) :: ptree
    real(c_double),intent(in)  :: xq,yq,zq
    real(c_double),intent(out) :: xn,yn,zn
    type(d3) :: query, near

    query = d3(xq,yq,zq)
    call polyhedron_closest(ptree, query, near) 
    xn = near%x
    yn = near%y
    zn = near%z
  end subroutine
  
  subroutine cgal_polyhedron_bbox_s(ptree, xmin,ymin,zmin, xmax,ymax,zmax)
    type(c_ptr),intent(in) :: ptree
    real(c_float),intent(out)  :: xmin,ymin,zmin
    real(c_float),intent(out) :: xmax,ymax,zmax
    type(d3) :: min, max

    call polyhedron_bbox(ptree, min, max) 
    xmin = real(min%x,c_float)
    ymin = real(min%y,c_float)
    zmin = real(min%z,c_float)
    xmax = real(max%x,c_float)
    ymax = real(max%y,c_float)
    zmax = real(max%z,c_float)
  end subroutine
  
  subroutine cgal_polyhedron_bbox_d(ptree, xmin,ymin,zmin, xmax,ymax,zmax)
    type(c_ptr),intent(in) :: ptree
    real(c_double),intent(out)  :: xmin,ymin,zmin
    real(c_double),intent(out) :: xmax,ymax,zmax
    type(d3) :: min, max

    call polyhedron_bbox(ptree, min, max) 
    xmin = min%x
    ymin = min%y
    zmin = min%z
    xmax = max%x
    ymax = max%y
    zmax = max%z
  end subroutine
  
  function cgal_polyhedron_inside_s(ptree, xq,yq,zq, xr,yr,zr) result(res)
    logical :: res
    type(c_ptr),intent(in) :: ptree
    real(c_float),intent(in) :: xq,yq,zq, xr,yr,zr
    type(d3) :: query, ref
    
    query = d3(real(xq,c_double),real(yq,c_double),real(zq,c_double))
    ref = d3(real(xr,c_double),real(yr,c_double),real(zr,c_double))
    res = polyhedron_inside(ptree, query, ref)
  end function

  function cgal_polyhedron_inside_d(ptree, xq,yq,zq, xr,yr,zr) result(res)
    logical :: res
    type(c_ptr),intent(in) :: ptree
    real(c_double),intent(in) :: xq,yq,zq, xr,yr,zr
    type(d3) :: query, ref
 
    query = d3(xq,yq,zq)
    ref = d3(xr,yr,zr)
    res = polyhedron_inside(ptree, query, ref)

  end function
  
  function cgal_polyhedron_intersects_ray_s(ptree, xc,yc,zc, a,b,c) result(res)
    logical :: res
    type(c_ptr),intent(in) :: ptree
    real(c_float),intent(in) :: xc,yc,zc, a,b,c
    type(d3) :: origin, vec
    
    origin = d3(real(xc,c_double),real(yc,c_double),real(zc,c_double))
    vec = d3(real(a,c_double),real(b,c_double),real(c,c_double))
    res = polyhedron_intersects_ray(ptree, origin, vec)
  end function

  function cgal_polyhedron_intersects_ray_d(ptree, xc,yc,zc, a,b,c) result(res)
    logical :: res
    type(c_ptr),intent(in) :: ptree
    real(c_double),intent(in) :: xc,yc,zc, a,b,c
    type(d3) :: origin, vec
    
    origin = d3(xc,yc,zc)
    vec = d3(a,b,c)
    res = polyhedron_intersects_ray(ptree, origin, vec)
  end function

  subroutine cgal_polyhedron_finalize(ptree)
    type(c_ptr),intent(inout) :: ptree
    
    call polyhedron_finalize(ptree)
  end subroutine

  
end module CGAL_Polyhedra
