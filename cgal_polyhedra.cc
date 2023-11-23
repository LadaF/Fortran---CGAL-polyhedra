
// Adapted from CGAL example (Author: Pierre Alliez) by Vladimir Fuka.
// Reworked for the new API following https://github.com/CGAL/cgal/blob/master/AABB_tree/examples/AABB_tree/AABB_polyhedron_facet_intersection_example.cpp
// by Camille Wormser, Pierre Alliez

#include <iostream>
#include <fstream>
#include <list>
#include <optional>

#include <CGAL/IO/Polyhedron_iostream.h>

#include <CGAL/Simple_cartesian.h>
#include <CGAL/AABB_tree.h>
#include <CGAL/AABB_traits.h>
#include <CGAL/Polyhedron_3.h>
#include <CGAL/AABB_face_graph_triangle_primitive.h>

typedef CGAL::Simple_cartesian<double> K;
typedef K::Point_3 Point;
typedef K::Plane_3 Plane;
typedef K::Vector_3 Vector;
typedef K::Segment_3 Segment;
typedef K::Ray_3 Ray;
typedef CGAL::Polyhedron_3<K> Polyhedron;
typedef CGAL::AABB_face_graph_triangle_primitive<Polyhedron> Primitive;
typedef CGAL::AABB_traits<K, Primitive> Traits;
typedef CGAL::AABB_tree<Traits> Tree;
typedef std::optional< Tree::Intersection_and_primitive_id<Segment>::Type > Segment_intersection;
typedef std::optional< Tree::Intersection_and_primitive_id<Plane>::Type > Plane_intersection;
typedef Tree::Primitive_id Primitive_id;

typedef CGAL::Bbox_3 Bbox_3;

typedef struct {double x,y,z;} d3;

struct Polytree {
    Polyhedron *poly=nullptr; 
    Tree *tree=nullptr;
    bool infinity_outside; //whether consider infinity as outside or inside
};

using std::cout;
using std::endl;

extern "C" {
 
  void polyhedron_from_file(Polytree  ** const pptree, 
                            const char * const fname,
                            const bool         verbose,
                            const bool         infinity_outside,
                            int * const        ierr){
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
    
     Tree *tree = new Tree(faces(*polyhedron).first, faces(*polyhedron).second, *polyhedron);
    
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
    (*pptree)->infinity_outside = infinity_outside;
    
    *ierr = 0;
    
  }
  
  void polyhedron_closest(const Polytree *ptree, const d3 *query, d3 *const near){
    Point query_point(query->x,query->y,query->z);
    
    Point closest = ptree->tree->closest_point(query_point);
    
    near->x = closest.x();
    near->y = closest.y();
    near->z = closest.z();
  }
  
  bool polyhedron_inside(const Polytree *ptree, const d3 *query, const d3 *outside_ref){
    Segment seg(Point(query->x,query->y,query->z),
                Point(outside_ref->x,outside_ref->y,outside_ref->z));
    
    std::list<Segment_intersection> intersections;
 
    ptree->tree->all_intersections(seg, std::back_inserter(intersections));
    

    std::vector<Point> points;

    int i = 0;
    for (auto iter = intersections.begin(); iter != intersections.end(); ++iter){
      i += 1;

      // gets intersection object
      auto op = *iter;

      // op is a std::optional< Tree::Intersection_and_primitive_id<Segment>::Type >
      // which is a std::optional< std::pair<CGAL::Object, Primitive_id> >
      if (op.has_value() == false) continue;
      CGAL::Object object = op->first;

      Point point;
      if(CGAL::assign(point, object)) {
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

    if (ptree->infinity_outside) {
      return n_dist%2 == 1;
    } else{
      return n_dist%2 == 0;
    }
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
