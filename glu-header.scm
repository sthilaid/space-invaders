;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: glu-header.scm
;;
;; description: Constant and type declarations for glu.scm
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(c-declare
#<<end
#include <GL/gl.h>
#include <GL/glu.h>
end
)

(c-define-type _GLUfuncptr "_GLUfuncptr")
(c-define-type GLUnurbs (struct "GLUnurbs"))
(c-define-type GLUquadric (struct "GLUquadric"))
(c-define-type GLUtesselator (struct "GLUtesselator"))
(c-define-type GLUnurbsObj GLUnurbs)
(c-define-type GLUquadricObj GLUquadric)
(c-define-type GLUtesselatorObj GLUtesselator)
(c-define-type  GLUtriangulatorObj GLUtesselator)

(c-define-type GLUnurbs* (pointer GLUnurbs))
(c-define-type GLUquadric* (pointer GLUquadric))
(c-define-type GLUtesselator* (pointer GLUtesselator))
(c-define-type GLUnurbsObj* (pointer GLUnurbsObj))
(c-define-type GLUquadricObj* (pointer GLUquadricObj))
(c-define-type GLUtesselatorObj* (pointer GLUtesselatorObj))
(c-define-type GLUtriangulatorObj* (pointer GLUtriangulatorObj))
(c-define-type void* (pointer void))


(define GLU_EXT_object_space_tess ((c-lambda () int "___result = GLU_EXT_object_space_tess;")))
(define GLU_EXT_nurbs_tessellator ((c-lambda () int "___result = GLU_EXT_nurbs_tessellator;")))

;/* Boolean* /
(define GLU_FALSE ((c-lambda () int "___result = GLU_FALSE;")))
(define GLU_TRUE ((c-lambda () int "___result = GLU_TRUE;")))

;/* Version* /
(define GLU_VERSION_1_1 ((c-lambda () int "___result = GLU_VERSION_1_1;")))
(define GLU_VERSION_1_2 ((c-lambda () int "___result = GLU_VERSION_1_2;")))
(define GLU_VERSION_1_3 ((c-lambda () int "___result = GLU_VERSION_1_3;")))

;/* StringName* /
(define GLU_VERSION ((c-lambda () int "___result = GLU_VERSION;")))
(define GLU_EXTENSIONS ((c-lambda () int "___result = GLU_EXTENSIONS;")))

;/* ErrorCode* /
(define GLU_INVALID_ENUM ((c-lambda () int "___result = GLU_INVALID_ENUM;")))
(define GLU_INVALID_VALUE ((c-lambda () int "___result = GLU_INVALID_VALUE;")))
(define GLU_OUT_OF_MEMORY ((c-lambda () int "___result = GLU_OUT_OF_MEMORY;")))
(define GLU_INCOMPATIBLE_GL_VERSION ((c-lambda () int "___result = GLU_INCOMPATIBLE_GL_VERSION;")))
(define GLU_INVALID_OPERATION ((c-lambda () int "___result = GLU_INVALID_OPERATION;")))

;; /* NurbsDisplay* /
;; /*      GLU_FILL* /
(define GLU_OUTLINE_POLYGON ((c-lambda () int "___result = GLU_OUTLINE_POLYGON;")))
(define GLU_OUTLINE_PATCH ((c-lambda () int "___result = GLU_OUTLINE_PATCH;")))

;/* NurbsCallback* /
(define GLU_NURBS_ERROR ((c-lambda () int "___result = GLU_NURBS_ERROR;")))
(define GLU_ERROR ((c-lambda () int "___result = GLU_ERROR;")))
(define GLU_NURBS_BEGIN ((c-lambda () int "___result = GLU_NURBS_BEGIN;")))
(define GLU_NURBS_BEGIN_EXT ((c-lambda () int "___result = GLU_NURBS_BEGIN_EXT;")))
(define GLU_NURBS_VERTEX ((c-lambda () int "___result = GLU_NURBS_VERTEX;")))
(define GLU_NURBS_VERTEX_EXT ((c-lambda () int "___result = GLU_NURBS_VERTEX_EXT;")))
(define GLU_NURBS_NORMAL ((c-lambda () int "___result = GLU_NURBS_NORMAL;")))
(define GLU_NURBS_NORMAL_EXT ((c-lambda () int "___result = GLU_NURBS_NORMAL_EXT;")))
(define GLU_NURBS_COLOR ((c-lambda () int "___result = GLU_NURBS_COLOR;")))
(define GLU_NURBS_COLOR_EXT ((c-lambda () int "___result = GLU_NURBS_COLOR_EXT;")))
(define GLU_NURBS_TEXTURE_COORD ((c-lambda () int "___result = GLU_NURBS_TEXTURE_COORD;")))
(define GLU_NURBS_TEX_COORD_EXT ((c-lambda () int "___result = GLU_NURBS_TEX_COORD_EXT;")))
(define GLU_NURBS_END ((c-lambda () int "___result = GLU_NURBS_END;")))
(define GLU_NURBS_END_EXT ((c-lambda () int "___result = GLU_NURBS_END_EXT;")))
(define GLU_NURBS_BEGIN_DATA ((c-lambda () int "___result = GLU_NURBS_BEGIN_DATA;")))
(define GLU_NURBS_BEGIN_DATA_EXT ((c-lambda () int "___result = GLU_NURBS_BEGIN_DATA_EXT;")))
(define GLU_NURBS_VERTEX_DATA ((c-lambda () int "___result = GLU_NURBS_VERTEX_DATA;")))
(define GLU_NURBS_VERTEX_DATA_EXT ((c-lambda () int "___result = GLU_NURBS_VERTEX_DATA_EXT;")))
(define GLU_NURBS_NORMAL_DATA ((c-lambda () int "___result = GLU_NURBS_NORMAL_DATA;")))
(define GLU_NURBS_NORMAL_DATA_EXT ((c-lambda () int "___result = GLU_NURBS_NORMAL_DATA_EXT;")))
(define GLU_NURBS_COLOR_DATA ((c-lambda () int "___result = GLU_NURBS_COLOR_DATA;")))
(define GLU_NURBS_COLOR_DATA_EXT ((c-lambda () int "___result = GLU_NURBS_COLOR_DATA_EXT;")))
(define GLU_NURBS_TEXTURE_COORD_DATA ((c-lambda () int "___result = GLU_NURBS_TEXTURE_COORD_DATA;")))
(define GLU_NURBS_TEX_COORD_DATA_EXT ((c-lambda () int "___result = GLU_NURBS_TEX_COORD_DATA_EXT;")))
(define GLU_NURBS_END_DATA ((c-lambda () int "___result = GLU_NURBS_END_DATA;")))
(define GLU_NURBS_END_DATA_EXT ((c-lambda () int "___result = GLU_NURBS_END_DATA_EXT;")))

;/* NurbsError* /
(define GLU_NURBS_ERROR1 ((c-lambda () int "___result = GLU_NURBS_ERROR1;")))
(define GLU_NURBS_ERROR2 ((c-lambda () int "___result = GLU_NURBS_ERROR2;")))
(define GLU_NURBS_ERROR3 ((c-lambda () int "___result = GLU_NURBS_ERROR3;")))
(define GLU_NURBS_ERROR4 ((c-lambda () int "___result = GLU_NURBS_ERROR4;")))
(define GLU_NURBS_ERROR5 ((c-lambda () int "___result = GLU_NURBS_ERROR5;")))
(define GLU_NURBS_ERROR6 ((c-lambda () int "___result = GLU_NURBS_ERROR6;")))
(define GLU_NURBS_ERROR7 ((c-lambda () int "___result = GLU_NURBS_ERROR7;")))
(define GLU_NURBS_ERROR8 ((c-lambda () int "___result = GLU_NURBS_ERROR8;")))
(define GLU_NURBS_ERROR9 ((c-lambda () int "___result = GLU_NURBS_ERROR9;")))
(define GLU_NURBS_ERROR10 ((c-lambda () int "___result = GLU_NURBS_ERROR10;")))
(define GLU_NURBS_ERROR11 ((c-lambda () int "___result = GLU_NURBS_ERROR11;")))
(define GLU_NURBS_ERROR12 ((c-lambda () int "___result = GLU_NURBS_ERROR12;")))
(define GLU_NURBS_ERROR13 ((c-lambda () int "___result = GLU_NURBS_ERROR13;")))
(define GLU_NURBS_ERROR14 ((c-lambda () int "___result = GLU_NURBS_ERROR14;")))
(define GLU_NURBS_ERROR15 ((c-lambda () int "___result = GLU_NURBS_ERROR15;")))
(define GLU_NURBS_ERROR16 ((c-lambda () int "___result = GLU_NURBS_ERROR16;")))
(define GLU_NURBS_ERROR17 ((c-lambda () int "___result = GLU_NURBS_ERROR17;")))
(define GLU_NURBS_ERROR18 ((c-lambda () int "___result = GLU_NURBS_ERROR18;")))
(define GLU_NURBS_ERROR19 ((c-lambda () int "___result = GLU_NURBS_ERROR19;")))
(define GLU_NURBS_ERROR20 ((c-lambda () int "___result = GLU_NURBS_ERROR20;")))
(define GLU_NURBS_ERROR21 ((c-lambda () int "___result = GLU_NURBS_ERROR21;")))
(define GLU_NURBS_ERROR22 ((c-lambda () int "___result = GLU_NURBS_ERROR22;")))
(define GLU_NURBS_ERROR23 ((c-lambda () int "___result = GLU_NURBS_ERROR23;")))
(define GLU_NURBS_ERROR24 ((c-lambda () int "___result = GLU_NURBS_ERROR24;")))
(define GLU_NURBS_ERROR25 ((c-lambda () int "___result = GLU_NURBS_ERROR25;")))
(define GLU_NURBS_ERROR26 ((c-lambda () int "___result = GLU_NURBS_ERROR26;")))
(define GLU_NURBS_ERROR27 ((c-lambda () int "___result = GLU_NURBS_ERROR27;")))
(define GLU_NURBS_ERROR28 ((c-lambda () int "___result = GLU_NURBS_ERROR28;")))
(define GLU_NURBS_ERROR29 ((c-lambda () int "___result = GLU_NURBS_ERROR29;")))
(define GLU_NURBS_ERROR30 ((c-lambda () int "___result = GLU_NURBS_ERROR30;")))
(define GLU_NURBS_ERROR31 ((c-lambda () int "___result = GLU_NURBS_ERROR31;")))
(define GLU_NURBS_ERROR32 ((c-lambda () int "___result = GLU_NURBS_ERROR32;")))
(define GLU_NURBS_ERROR33 ((c-lambda () int "___result = GLU_NURBS_ERROR33;")))
(define GLU_NURBS_ERROR34 ((c-lambda () int "___result = GLU_NURBS_ERROR34;")))
(define GLU_NURBS_ERROR35 ((c-lambda () int "___result = GLU_NURBS_ERROR35;")))
(define GLU_NURBS_ERROR36 ((c-lambda () int "___result = GLU_NURBS_ERROR36;")))
(define GLU_NURBS_ERROR37 ((c-lambda () int "___result = GLU_NURBS_ERROR37;")))

;/* NurbsProperty* /
(define GLU_AUTO_LOAD_MATRIX ((c-lambda () int "___result = GLU_AUTO_LOAD_MATRIX;")))
(define GLU_CULLING ((c-lambda () int "___result = GLU_CULLING;")))
(define GLU_SAMPLING_TOLERANCE ((c-lambda () int "___result = GLU_SAMPLING_TOLERANCE;")))
(define GLU_DISPLAY_MODE ((c-lambda () int "___result = GLU_DISPLAY_MODE;")))
(define GLU_PARAMETRIC_TOLERANCE ((c-lambda () int "___result = GLU_PARAMETRIC_TOLERANCE;")))
(define GLU_SAMPLING_METHOD ((c-lambda () int "___result = GLU_SAMPLING_METHOD;")))
(define GLU_U_STEP ((c-lambda () int "___result = GLU_U_STEP;")))
(define GLU_V_STEP ((c-lambda () int "___result = GLU_V_STEP;")))
(define GLU_NURBS_MODE ((c-lambda () int "___result = GLU_NURBS_MODE;")))
(define GLU_NURBS_MODE_EXT ((c-lambda () int "___result = GLU_NURBS_MODE_EXT;")))
(define GLU_NURBS_TESSELLATOR ((c-lambda () int "___result = GLU_NURBS_TESSELLATOR;")))
(define GLU_NURBS_TESSELLATOR_EXT ((c-lambda () int "___result = GLU_NURBS_TESSELLATOR_EXT;")))
(define GLU_NURBS_RENDERER ((c-lambda () int "___result = GLU_NURBS_RENDERER;")))
(define GLU_NURBS_RENDERER_EXT ((c-lambda () int "___result = GLU_NURBS_RENDERER_EXT;")))

;/* NurbsSampling* /
(define GLU_OBJECT_PARAMETRIC_ERROR ((c-lambda () int "___result = GLU_OBJECT_PARAMETRIC_ERROR;")))
(define GLU_OBJECT_PARAMETRIC_ERROR_EXT ((c-lambda () int "___result = GLU_OBJECT_PARAMETRIC_ERROR_EXT;")))
(define GLU_OBJECT_PATH_LENGTH ((c-lambda () int "___result = GLU_OBJECT_PATH_LENGTH;")))
(define GLU_OBJECT_PATH_LENGTH_EXT ((c-lambda () int "___result = GLU_OBJECT_PATH_LENGTH_EXT;")))
(define GLU_PATH_LENGTH ((c-lambda () int "___result = GLU_PATH_LENGTH;")))
(define GLU_PARAMETRIC_ERROR ((c-lambda () int "___result = GLU_PARAMETRIC_ERROR;")))
(define GLU_DOMAIN_DISTANCE ((c-lambda () int "___result = GLU_DOMAIN_DISTANCE;")))

;/* NurbsTrim* /
(define GLU_MAP1_TRIM_2 ((c-lambda () int "___result = GLU_MAP1_TRIM_2;")))
(define GLU_MAP1_TRIM_3 ((c-lambda () int "___result = GLU_MAP1_TRIM_3;")))

;/* QuadricDrawStyle* /
(define GLU_POINT ((c-lambda () int "___result = GLU_POINT;")))
(define GLU_LINE ((c-lambda () int "___result = GLU_LINE;")))
(define GLU_FILL ((c-lambda () int "___result = GLU_FILL;")))
(define GLU_SILHOUETTE ((c-lambda () int "___result = GLU_SILHOUETTE;")))

;; /* QuadricCallback* /
;; /*      GLU_ERROR* /

;/* QuadricNormal* /
(define GLU_SMOOTH ((c-lambda () int "___result = GLU_SMOOTH;")))
(define GLU_FLAT ((c-lambda () int "___result = GLU_FLAT;")))
(define GLU_NONE ((c-lambda () int "___result = GLU_NONE;")))

;/* QuadricOrientation* /
(define GLU_OUTSIDE ((c-lambda () int "___result = GLU_OUTSIDE;")))
(define GLU_INSIDE ((c-lambda () int "___result = GLU_INSIDE;")))

;/* TessCallback* /
(define GLU_TESS_BEGIN ((c-lambda () int "___result = GLU_TESS_BEGIN;")))
(define GLU_BEGIN ((c-lambda () int "___result = GLU_BEGIN;")))
(define GLU_TESS_VERTEX ((c-lambda () int "___result = GLU_TESS_VERTEX;")))
(define GLU_VERTEX ((c-lambda () int "___result = GLU_VERTEX;")))
(define GLU_TESS_END ((c-lambda () int "___result = GLU_TESS_END;")))
(define GLU_END ((c-lambda () int "___result = GLU_END;")))
(define GLU_TESS_ERROR ((c-lambda () int "___result = GLU_TESS_ERROR;")))
(define GLU_TESS_EDGE_FLAG ((c-lambda () int "___result = GLU_TESS_EDGE_FLAG;")))
(define GLU_EDGE_FLAG ((c-lambda () int "___result = GLU_EDGE_FLAG;")))
(define GLU_TESS_COMBINE ((c-lambda () int "___result = GLU_TESS_COMBINE;")))
(define GLU_TESS_BEGIN_DATA ((c-lambda () int "___result = GLU_TESS_BEGIN_DATA;")))
(define GLU_TESS_VERTEX_DATA ((c-lambda () int "___result = GLU_TESS_VERTEX_DATA;")))
(define GLU_TESS_END_DATA ((c-lambda () int "___result = GLU_TESS_END_DATA;")))
(define GLU_TESS_ERROR_DATA ((c-lambda () int "___result = GLU_TESS_ERROR_DATA;")))
(define GLU_TESS_EDGE_FLAG_DATA ((c-lambda () int "___result = GLU_TESS_EDGE_FLAG_DATA;")))
(define GLU_TESS_COMBINE_DATA ((c-lambda () int "___result = GLU_TESS_COMBINE_DATA;")))

;/* TessContour* /
(define GLU_CW ((c-lambda () int "___result = GLU_CW;")))
(define GLU_CCW ((c-lambda () int "___result = GLU_CCW;")))
(define GLU_INTERIOR ((c-lambda () int "___result = GLU_INTERIOR;")))
(define GLU_EXTERIOR ((c-lambda () int "___result = GLU_EXTERIOR;")))
(define GLU_UNKNOWN ((c-lambda () int "___result = GLU_UNKNOWN;")))

;/* TessProperty* /
(define GLU_TESS_WINDING_RULE ((c-lambda () int "___result = GLU_TESS_WINDING_RULE;")))
(define GLU_TESS_BOUNDARY_ONLY ((c-lambda () int "___result = GLU_TESS_BOUNDARY_ONLY;")))
(define GLU_TESS_TOLERANCE ((c-lambda () int "___result = GLU_TESS_TOLERANCE;")))

;/* TessError* /
(define GLU_TESS_ERROR1 ((c-lambda () int "___result = GLU_TESS_ERROR1;")))
(define GLU_TESS_ERROR2 ((c-lambda () int "___result = GLU_TESS_ERROR2;")))
(define GLU_TESS_ERROR3 ((c-lambda () int "___result = GLU_TESS_ERROR3;")))
(define GLU_TESS_ERROR4 ((c-lambda () int "___result = GLU_TESS_ERROR4;")))
(define GLU_TESS_ERROR5 ((c-lambda () int "___result = GLU_TESS_ERROR5;")))
(define GLU_TESS_ERROR6 ((c-lambda () int "___result = GLU_TESS_ERROR6;")))
(define GLU_TESS_ERROR7 ((c-lambda () int "___result = GLU_TESS_ERROR7;")))
(define GLU_TESS_ERROR8 ((c-lambda () int "___result = GLU_TESS_ERROR8;")))
(define GLU_TESS_MISSING_BEGIN_POLYGON ((c-lambda () int "___result = GLU_TESS_MISSING_BEGIN_POLYGON;")))
(define GLU_TESS_MISSING_BEGIN_CONTOUR ((c-lambda () int "___result = GLU_TESS_MISSING_BEGIN_CONTOUR;")))
(define GLU_TESS_MISSING_END_POLYGON ((c-lambda () int "___result = GLU_TESS_MISSING_END_POLYGON;")))
(define GLU_TESS_MISSING_END_CONTOUR ((c-lambda () int "___result = GLU_TESS_MISSING_END_CONTOUR;")))
(define GLU_TESS_COORD_TOO_LARGE ((c-lambda () int "___result = GLU_TESS_COORD_TOO_LARGE;")))
(define GLU_TESS_NEED_COMBINE_CALLBACK ((c-lambda () int "___result = GLU_TESS_NEED_COMBINE_CALLBACK;")))

;/* TessWinding* /
(define GLU_TESS_WINDING_ODD ((c-lambda () int "___result = GLU_TESS_WINDING_ODD;")))
(define GLU_TESS_WINDING_NONZERO ((c-lambda () int "___result = GLU_TESS_WINDING_NONZERO;")))
(define GLU_TESS_WINDING_POSITIVE ((c-lambda () int "___result = GLU_TESS_WINDING_POSITIVE;")))
(define GLU_TESS_WINDING_NEGATIVE ((c-lambda () int "___result = GLU_TESS_WINDING_NEGATIVE;")))
(define GLU_TESS_WINDING_ABS_GEQ_TWO ((c-lambda () int "___result = GLU_TESS_WINDING_ABS_GEQ_TWO;")))


(define GLU_TESS_MAX_COORD ((c-lambda () int "___result = GLU_TESS_MAX_COORD;")))
