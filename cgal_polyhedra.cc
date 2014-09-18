
// Adapted from CGAL example (Author: Pierre Alliez) by Vladimir Fuka.

#include <iostream>
#include <fstream>
#include <list>

#include <CGAL/IO/Polyhedron_iostream.h>
#include <CGAL/Simple_cartesian.h>
#include <CGAL/AABB_tree.h>
#include <CGAL/AABB_traits.h>
#include <CGAL/Polyhedron_3.h>
#include <CGAL/AABB_polyhedron_triangle_primitive.h>


typedef CGAL::Simple_cartesian<double> K;
typedef K::FT FT;
typedef K::Vector_3 Vector;
typedef K::Point_3 Point;
typedef K::Segment_3 Segment;
typedef K::Ray_3 Ray;
typedef CGAL::Polyhedron_3<K> Polyhedron;
typedef CGAL::AABB_polyhedron_triangle_primitive<K,Polyhedron> Primitive;
typedef CGAL::AABB_traits<K, Primitive> Traits;
typedef CGAL::AABB_tree<Traits> Tree;
typedef Tree::Object_and_primitive_id Object_and_primitive_id;
typedef Tree::Point_and_primitive_id Point_and_primitive_id;
typedef CGAL::Bbox_3 Bbox_3;

typedef struct {double x,y,z;} d3;

typedef struct {Polyhedron *poly; Tree *tree;} Polytree;

using std::cout;
using std::endl;

extern "C" int debuglevel;

extern "C" {
 
  void polyhedron_from_file (Polytree  **pptree, const char *fname, int verbose, int * const ierr){
    Polyhedron *polyhedron = new Polyhedron;
    
    std::ifstream in(fname);

    if (verbose) {cout << " Reading file " << fname << " " << endl;}

    try {
      in >> *polyhedron;
    }
    catch(...) {
      *ierr = 2;
      return;
    }
    
    Tree *tree = new Tree(polyhedron->facets_begin(),polyhedron->facets_end());
    
    if (verbose) {
      cout << " facets: " << polyhedron->size_of_facets() << endl;
      cout << " halfedges: " << polyhedron->size_of_halfedges() << endl;
      cout << " vertices: " << polyhedron->size_of_vertices() << endl;
    }
    
    if (polyhedron->size_of_facets()==0 ||
        polyhedron->size_of_halfedges()==0 ||
        polyhedron->size_of_vertices()==0){
          *ierr = 1;
          return;
        };
    
    tree->accelerate_distance_queries();
    
    *pptree = new Polytree;
    (*pptree)->poly = polyhedron;
    (*pptree)->tree = tree;
    
    *ierr = 0;
    
  }
  
  void polyhedron_closest (const Polytree *ptree, const d3 *query, d3 *near){
    Point query_point(query->x,query->y,query->z);
    
    Point closest = ptree->tree->closest_point(query_point);
    
    near->x = closest.x();
    near->y = closest.y();
    near->z = closest.z();
  }
  
  bool polyhedron_inside(const Polytree *ptree, const d3 *query, const d3 *outside_ref){
    Segment seg(Point(query->x,query->y,query->z),
                Point(outside_ref->x,outside_ref->y,outside_ref->z));
    
    std::list<Object_and_primitive_id> intersections;
 
    ptree->tree->all_intersections(seg, std::back_inserter(intersections));
    

    std::vector<Point> points;

    int i = 0;
    for (auto iter = intersections.begin(); iter != intersections.end(); ++iter){
      i += 1;
      // gets intersection object
      Object_and_primitive_id op = *iter;
      CGAL::Object object = op.first;
      Point point;
      if(CGAL::assign(point,object)) {
        points.push_back(point);
      }
         
    }

    int n_dist = 0;
    // find how many of the points are distinct
    for (std::vector<Point>::size_type i = 0; i < points.size(); ++i){
      bool distinct = true;

      for (std::vector<Point>::size_type j = 0; j < i; ++j){
        Vector v = points[i] - points[j];
        distinct = ( v.squared_length() > 1e-10 ); 

        if (!distinct)  break;
      }
      if (distinct) n_dist += 1;
    }

    return n_dist%2 == 1;
  }
  
  bool polyhedron_intersects_ray(const Polytree *ptree, const d3 *origin, const d3 *vec){
    Ray ray(Point(origin->x,origin->y,origin->z),
            Vector(vec->x,vec->y,vec->z));
    try{
      return ptree->tree->do_intersect(ray);
    }
    catch (...) {
      cout << origin->x <<" "<< origin->y <<" "<< origin->z << endl;
      cout << vec->x <<" "<< vec->y <<" "<< vec->z << endl;
      return false;
    }
  }
  
  void polyhedron_bbox(const Polytree *ptree, d3 *const min, d3 *const max){
    Bbox_3 bbox = ptree->tree->bbox();
    *min = {bbox.xmin(), bbox.ymin(), bbox.zmin()};
    *max = {bbox.xmax(), bbox.ymax(), bbox.zmax()};
  }
  
  void polyhedron_finalize(Polytree **pptree){
    delete (*pptree)->tree; (*pptree)->tree = NULL;
    delete (*pptree)->poly; (*pptree)->poly = NULL;
    delete *pptree; *pptree = NULL;
  }
 
}
