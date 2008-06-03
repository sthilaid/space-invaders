;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: glu.scm
;;
;; description: glu ffi interface
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(include "opengl-header.scm")
(include "glu-header.scm")

(c-declare #<<declare-end
#include <GL/gl.h>           
#include <GL/glu.h>
declare-end
)


(define gluBeginCurve (c-lambda (GLUnurbs* ) void "gluBeginCurve"))
(define gluBeginPolygon (c-lambda (GLUtesselator* ) void "gluBeginPolygon"))
(define gluBeginSurface (c-lambda (GLUnurbs* ) void "gluBeginSurface"))
(define gluBeginTrim (c-lambda (GLUnurbs* ) void "gluBeginTrim"))
(define gluBuild1DMipmapLevels (c-lambda (GLenum   GLint   GLsizei   GLenum   GLenum   GLint   GLint   GLint   void* ) GLint "gluBuild1DMipmapLevels"))
(define gluBuild1DMipmaps (c-lambda (GLenum   GLint   GLsizei   GLenum   GLenum   void* ) GLint "gluBuild1DMipmaps"))
(define gluBuild2DMipmapLevels (c-lambda (GLenum   GLint   GLsizei   GLsizei   GLenum   GLenum   GLint   GLint   GLint   void* ) GLint "gluBuild2DMipmapLevels"))
(define gluBuild2DMipmaps (c-lambda (GLenum   GLint   GLsizei   GLsizei   GLenum   GLenum   void* ) GLint "gluBuild2DMipmaps"))
(define gluBuild3DMipmapLevels (c-lambda (GLenum   GLint   GLsizei   GLsizei   GLsizei   GLenum   GLenum   GLint   GLint   GLint   void* ) GLint "gluBuild3DMipmapLevels"))
(define gluBuild3DMipmaps (c-lambda (GLenum   GLint   GLsizei   GLsizei   GLsizei   GLenum   GLenum   void* ) GLint "gluBuild3DMipmaps"))
(define gluCheckExtension (c-lambda (GLubyte*   GLubyte* ) GLboolean "gluCheckExtension"))
(define gluCylinder (c-lambda (GLUquadric*   GLdouble   GLdouble   GLdouble   GLint   GLint ) void "gluCylinder"))
(define gluDeleteNurbsRenderer (c-lambda (GLUnurbs* ) void "gluDeleteNurbsRenderer"))
(define gluDeleteQuadric (c-lambda (GLUquadric* ) void "gluDeleteQuadric"))
(define gluDeleteTess (c-lambda (GLUtesselator* ) void "gluDeleteTess"))
(define gluDisk (c-lambda (GLUquadric*   GLdouble   GLdouble   GLint   GLint ) void "gluDisk"))
(define gluEndCurve (c-lambda (GLUnurbs* ) void "gluEndCurve"))
(define gluEndPolygon (c-lambda (GLUtesselator* ) void "gluEndPolygon"))
(define gluEndSurface (c-lambda (GLUnurbs* ) void "gluEndSurface"))
(define gluEndTrim (c-lambda (GLUnurbs* ) void "gluEndTrim"))
(define gluErrorString (c-lambda (GLenum) GLubyte* "gluErrorString"))
(define gluGetNurbsProperty (c-lambda (GLUnurbs*   GLenum   GLfloat* ) void "gluGetNurbsProperty"))
(define gluGetString (c-lambda (GLenum) GLubyte* "gluGetString"))
(define gluGetTessProperty (c-lambda (GLUtesselator*   GLenum   GLdouble* ) void "gluGetTessProperty"))
(define gluLoadSamplingMatrices (c-lambda (GLUnurbs*   GLfloat*   GLfloat*   GLint* ) void "gluLoadSamplingMatrices"))
(define gluLookAt (c-lambda (GLdouble   GLdouble   GLdouble   GLdouble   GLdouble   GLdouble   GLdouble   GLdouble   GLdouble ) void "gluLookAt"))
(define gluNewNurbsRenderer (c-lambda () GLUnurbs* "gluNewNurbsRenderer"))
(define gluNewQuadric (c-lambda () GLUquadric* "gluNewQuadric"))
(define gluNewTess (c-lambda () GLUtesselator* "gluNewTess"))
(define gluNextContour (c-lambda (GLUtesselator*   GLenum ) void "gluNextContour"))
(define gluNurbsCallback (c-lambda (GLUnurbs*   GLenum   _GLUfuncptr ) void "gluNurbsCallback"))
(define gluNurbsCallbackData (c-lambda (GLUnurbs*   GLvoid* ) void "gluNurbsCallbackData"))
(define gluNurbsCallbackDataEXT (c-lambda (GLUnurbs*   GLvoid* ) void "gluNurbsCallbackDataEXT"))
(define gluNurbsCurve (c-lambda (GLUnurbs*   GLint   GLfloat*   GLint   GLfloat*   GLint   GLenum ) void "gluNurbsCurve"))
(define gluNurbsProperty (c-lambda (GLUnurbs*   GLenum   GLfloat ) void "gluNurbsProperty"))
(define gluNurbsSurface (c-lambda (GLUnurbs*   GLint   GLfloat*   GLint   GLfloat*   GLint   GLint   GLfloat*   GLint   GLint   GLenum ) void "gluNurbsSurface"))
(define gluOrtho2D (c-lambda (GLdouble   GLdouble   GLdouble   GLdouble ) void "gluOrtho2D"))
(define gluPartialDisk (c-lambda (GLUquadric*   GLdouble   GLdouble   GLint   GLint   GLdouble   GLdouble ) void "gluPartialDisk"))
(define gluPerspective (c-lambda (GLdouble   GLdouble   GLdouble   GLdouble ) void "gluPerspective"))
(define gluPickMatrix (c-lambda (GLdouble   GLdouble   GLdouble   GLdouble   GLint* ) void "gluPickMatrix"))
(define gluProject (c-lambda (GLdouble   GLdouble   GLdouble   GLdouble*   GLdouble*   GLint*   GLdouble*   GLdouble*   GLdouble* ) GLint "gluProject"))
(define gluPwlCurve (c-lambda (GLUnurbs*   GLint   GLfloat*   GLint   GLenum ) void "gluPwlCurve"))
(define gluQuadricCallback (c-lambda (GLUquadric*   GLenum   _GLUfuncptr ) void "gluQuadricCallback"))
(define gluQuadricDrawStyle (c-lambda (GLUquadric*   GLenum ) void "gluQuadricDrawStyle"))
(define gluQuadricNormals (c-lambda (GLUquadric*   GLenum ) void "gluQuadricNormals"))
(define gluQuadricOrientation (c-lambda (GLUquadric*   GLenum ) void "gluQuadricOrientation"))
(define gluQuadricTexture (c-lambda (GLUquadric*   GLboolean ) void "gluQuadricTexture"))
(define gluScaleImage (c-lambda (GLenum   GLsizei   GLsizei   GLenum   void* GLsizei   GLsizei   GLenum   GLvoid* ) GLint "gluScaleImage"))
(define gluSphere (c-lambda (GLUquadric*   GLdouble   GLint   GLint ) void "gluSphere"))
(define gluTessBeginContour (c-lambda (GLUtesselator* ) void "gluTessBeginContour"))
(define gluTessBeginPolygon (c-lambda (GLUtesselator*   GLvoid* ) void "gluTessBeginPolygon"))
(define gluTessCallback (c-lambda (GLUtesselator*   GLenum   _GLUfuncptr ) void "gluTessCallback"))
(define gluTessEndContour (c-lambda (GLUtesselator* ) void "gluTessEndContour"))
(define gluTessEndPolygon (c-lambda (GLUtesselator* ) void "gluTessEndPolygon"))
(define gluTessNormal (c-lambda (GLUtesselator*   GLdouble   GLdouble   GLdouble ) void "gluTessNormal"))
(define gluTessProperty (c-lambda (GLUtesselator*   GLenum   GLdouble ) void "gluTessProperty"))
(define gluTessVertex (c-lambda (GLUtesselator*   GLdouble*   GLvoid* ) void "gluTessVertex"))
(define gluUnProject (c-lambda (GLdouble   GLdouble   GLdouble   GLdouble*   GLdouble*   GLint*   GLdouble*   GLdouble*   GLdouble* ) GLint "gluUnProject"))
(define gluUnProject4 (c-lambda (GLdouble   GLdouble   GLdouble   GLdouble   GLdouble*   GLdouble*   GLint*   GLdouble   GLdouble   GLdouble*   GLdouble*   GLdouble*   GLdouble* ) GLint "gluUnProject4"))
