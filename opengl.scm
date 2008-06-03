;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: opengl.scm
;;
;; description: Opengl 1.1 ffi interface
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "opengl-header.scm")

(c-declare #<<end
#include <GL/gl.h>
end
)


;; /*
;;  * Miscellaneous
;;  */

(define glClearIndex (c-lambda (GLfloat) void "glClearIndex"))

(define glClearColor (c-lambda( GLclampf GLclampf GLclampf GLclampf ) void "glClearColor"))

(define glClear (c-lambda( GLbitfield ) void "glClear"))

(define glIndexMask (c-lambda( GLuint ) void "glIndexMask"))

(define glColorMask (c-lambda( GLboolean GLboolean GLboolean GLboolean ) void "glColorMask"))

(define glAlphaFunc (c-lambda ( GLenum GLclampf ) void "glAlphaFunc"))

(define glBlendFunc (c-lambda ( GLenum GLenum ) void "glBlendFunc"))

(define glLogicOp (c-lambda ( GLenum ) void "glLogicOp"))

(define glCullFace (c-lambda ( GLenum ) void "glCullFace"))

(define glFrontFace (c-lambda ( GLenum ) void "glFrontFace"))

(define glPointSize (c-lambda ( GLfloat ) void "glPointSize"))

(define glLineWidth (c-lambda ( GLfloat ) void "glLineWidth"))

(define glLineStipple (c-lambda ( GLint GLushort ) void "glLineStipple"))

(define glPolygonMode (c-lambda ( GLenum GLenum ) void "glPolygonMode"))

(define glPolygonOffset (c-lambda ( GLfloat GLfloat ) void "glPolygonOffset"))

(define glPolygonStipple (c-lambda ( GLubyte* ) void "glPolygonStipple"))

(define glGetPolygonStipple (c-lambda ( GLubyte* ) void "glGetPolygonStipple"))

(define glEdgeFlag (c-lambda ( GLboolean ) void "glEdgeFlag"))

(define glEdgeFlagv (c-lambda ( GLboolean* ) void "glEdgeFlagv"))

(define glScissor (c-lambda ( GLint GLint GLsizei GLsizei ) void "glScissor"))

(define glClipPlane (c-lambda ( GLenum GLdouble* ) void "glClipPlane"))

(define glGetClipPlane (c-lambda ( GLenum GLdouble* ) void "glGetClipPlane"))

(define glDrawBuffer (c-lambda ( GLenum ) void "glDrawBuffer"))

(define glReadBuffer (c-lambda ( GLenum ) void "glReadBuffer"))

(define glEnable (c-lambda ( GLenum ) void "glEnable"))

(define glDisable (c-lambda ( GLenum ) void "glDisable"))

(define glIsEnabled (c-lambda ( GLenum ) GLboolean "glIsEnabled"))

(define glEnableClientState (c-lambda ( GLenum ) void "glEnableClientState"))

(define glDisableClientState (c-lambda ( GLenum ) void "glDisableClientState"))

(define glGetBooleanv (c-lambda ( GLenum GLboolean* ) void "glGetBooleanv"))

(define glGetDoublev (c-lambda ( GLenum GLdouble* ) void "glGetDoublev"))

(define glGetFloatv (c-lambda ( GLenum GLfloat* ) void "glGetFloatv"))

(define glGetIntegerv (c-lambda ( GLenum GLint* ) void "glGetIntegerv"))


(define glPushAttrib (c-lambda ( GLbitfield ) void "glPushAttrib"))

(define glPopAttrib (c-lambda ( ) void "glPopAttrib"))


(define glPushClientAttrib (c-lambda ( GLbitfield ) void "glPushClientAttrib"))

(define glPopClientAttrib (c-lambda ( ) void "glPopClientAttrib"))

(define glRenderMode (c-lambda ( GLenum ) GLint "glRenderMode"))

(define glGetError (c-lambda ( ) GLenum "glGetError"))

;(define glGetString (c-lambda ( GLenum ) GLubyte* "glGetString"))

(define glFinish (c-lambda ( ) void "glFinish"))

(define glFlush (c-lambda ( ) void "glFlush"))

(define glHint (c-lambda ( GLenum GLenum ) void "glHint"))


;; /*
;;  * Depth Buffer
;;  */

(define glClearDepth (c-lambda ( GLclampd ) void "glClearDepth"))

(define glDepthFunc (c-lambda ( GLenum ) void "glDepthFunc"))

(define glDepthMask (c-lambda ( GLboolean ) void "glDepthMask"))

(define glDepthRange (c-lambda ( GLclampd GLclampd ) void "glDepthRange"))


;; /*
;;  * Accumulation Buffer
;;  */

(define glClearAccum (c-lambda ( GLfloat GLfloat GLfloat GLfloat ) void "glClearAccum"))

(define glAccum (c-lambda ( GLenum GLfloat ) void "glAccum"))


;; /*
;;  * Transformation
;;  */

(define glMatrixMode (c-lambda ( GLenum ) void "glMatrixMode"))

(define glOrtho (c-lambda ( GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble )
          void "glOrtho"))

(define glFrustum (c-lambda ( GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble )
          void "glFrustum"))

(define glViewport (c-lambda ( GLint GLint GLsizei GLsizei ) void "glViewport"))

(define glPushMatrix (c-lambda () void "glPushMatrix"))

(define glPopMatrix (c-lambda () void "glPopMatrix"))

(define glLoadIdentity (c-lambda () void "glLoadIdentity"))

(define glLoadMatrixd (c-lambda ( GLdouble* ) void "glLoadMatrixd"))
(define glLoadMatrixf (c-lambda ( GLfloat* ) void "glLoadMatrixf"))

(define glMultMatrixd (c-lambda ( GLdouble* ) void "glMultMatrixd"))
(define glMultMatrixf (c-lambda ( GLfloat* ) void "glMultMatrixf"))

(define glRotated (c-lambda ( GLdouble GLdouble GLdouble GLdouble ) void "glRotated"))
(define glRotatef (c-lambda ( GLfloat GLfloat GLfloat GLfloat ) void "glRotatef"))

(define glScaled (c-lambda ( GLdouble GLdouble GLdouble ) void "glScaled"))
(define glScalef (c-lambda ( GLfloat GLfloat GLfloat ) void "glScalef"))
(define glTranslated (c-lambda ( GLdouble GLdouble GLdouble ) void "glTranslated"))
(define glTranslatef (c-lambda ( GLfloat GLfloat GLfloat ) void "glTranslatef"))


;; /*
;;  * Display Lists
;;  */

(define glIsList (c-lambda ( GLuint ) GLboolean "glIsList"))

(define glDeleteLists (c-lambda ( GLuint GLsizei ) void "glDeleteLists"))

(define glGenLists (c-lambda ( GLsizei ) GLuint "glGenLists"))

(define glNewList (c-lambda ( GLuint GLenum ) void "glNewList"))

(define glEndList (c-lambda () void "glEndList"))

(define glCallList (c-lambda ( GLuint ) void "glCallList"))

(define glCallLists (c-lambda ( GLsizei GLenum GLvoid* ) void "glCallLists"))

(define glListBase (c-lambda ( GLuint ) void "glListBase"))


;; /*
;;  * Drawing Functions
;;  */

(define glBegin (c-lambda ( GLenum ) void "glBegin"))

(define glEnd (c-lambda () void "glEnd"))


(define glVertex2d (c-lambda ( GLdouble GLdouble ) void "glVertex2d"))
(define glVertex2f (c-lambda ( GLfloat GLfloat ) void "glVertex2f"))
(define glVertex2i (c-lambda ( GLint GLint ) void "glVertex2i"))
(define glVertex2s (c-lambda ( GLshort GLshort ) void "glVertex2s"))

(define glVertex3d (c-lambda ( GLdouble GLdouble GLdouble ) void "glVertex3d"))
(define glVertex3f (c-lambda ( GLfloat GLfloat GLfloat ) void "glVertex3f"))
(define glVertex3i (c-lambda ( GLint GLint GLint ) void "glVertex3i"))
(define glVertex3s (c-lambda ( GLshort GLshort GLshort ) void "glVertex3s"))

(define glVertex4d (c-lambda ( GLdouble GLdouble GLdouble GLdouble ) void "glVertex4d"))
(define glVertex4f (c-lambda ( GLfloat GLfloat GLfloat GLfloat ) void "glVertex4f"))
(define glVertex4i (c-lambda ( GLint GLint GLint GLint ) void "glVertex4i"))
(define glVertex4s (c-lambda ( GLshort GLshort GLshort GLshort ) void "glVertex4s"))

(define glVertex2dv (c-lambda ( GLdouble* ) void "glVertex2dv"))
(define glVertex2fv (c-lambda ( GLfloat* ) void "glVertex2fv"))
(define glVertex2iv (c-lambda ( GLint* ) void "glVertex2iv"))
(define glVertex2sv (c-lambda ( GLshort* ) void "glVertex2sv"))

(define glVertex3dv (c-lambda ( GLdouble* ) void "glVertex3dv"))
(define glVertex3fv (c-lambda ( GLfloat* ) void "glVertex3fv"))
(define glVertex3iv (c-lambda ( GLint* ) void "glVertex3iv"))
(define glVertex3sv (c-lambda ( GLshort* ) void "glVertex3sv"))

(define glVertex4dv (c-lambda ( GLdouble* ) void "glVertex4dv"))
(define glVertex4fv (c-lambda ( GLfloat* ) void "glVertex4fv"))
(define glVertex4iv (c-lambda ( GLint* ) void "glVertex4iv"))
(define glVertex4sv (c-lambda ( GLshort* ) void "glVertex4sv"))


(define glNormal3b (c-lambda ( GLbyte GLbyte GLbyte ) void "glNormal3b"))
(define glNormal3d (c-lambda ( GLdouble GLdouble GLdouble ) void "glNormal3d"))
(define glNormal3f (c-lambda ( GLfloat GLfloat GLfloat ) void "glNormal3f"))
(define glNormal3i (c-lambda ( GLint GLint GLint ) void "glNormal3i"))
(define glNormal3s (c-lambda ( GLshort GLshort GLshort ) void "glNormal3s"))

(define glNormal3bv (c-lambda ( GLbyte* ) void "glNormal3bv"))
(define glNormal3dv (c-lambda ( GLdouble* ) void "glNormal3dv"))
(define glNormal3fv (c-lambda ( GLfloat* ) void "glNormal3fv"))
(define glNormal3iv (c-lambda ( GLint* ) void "glNormal3iv"))
(define glNormal3sv (c-lambda ( GLshort* ) void "glNormal3sv"))


(define glIndexd (c-lambda ( GLdouble ) void "glIndexd"))
(define glIndexf (c-lambda ( GLfloat ) void "glIndexf"))
(define glIndexi (c-lambda ( GLint ) void "glIndexi"))
(define glIndexs (c-lambda ( GLshort ) void "glIndexs"))
(define glIndexub (c-lambda ( GLubyte ) void "glIndexub"))

(define glIndexdv (c-lambda ( GLdouble* ) void "glIndexdv"))
(define glIndexfv (c-lambda ( GLfloat* ) void "glIndexfv"))
(define glIndexiv (c-lambda ( GLint* ) void "glIndexiv"))
(define glIndexsv (c-lambda ( GLshort* ) void "glIndexsv"))
(define glIndexubv (c-lambda ( GLubyte* ) void "glIndexubv"))

(define glColor3b (c-lambda ( GLbyte GLbyte GLbyte ) void "glColor3b"))
(define glColor3d (c-lambda ( GLdouble GLdouble GLdouble ) void "glColor3d"))
(define glColor3f (c-lambda ( GLfloat GLfloat GLfloat ) void "glColor3f"))
(define glColor3i (c-lambda ( GLint GLint GLint ) void "glColor3i"))
(define glColor3s (c-lambda ( GLshort GLshort GLshort ) void "glColor3s"))
(define glColor3ub (c-lambda ( GLubyte GLubyte GLubyte ) void "glColor3ub"))
(define glColor3ui (c-lambda ( GLuint GLuint GLuint ) void "glColor3ui"))
(define glColor3us (c-lambda ( GLushort GLushort GLushort ) void "glColor3us"))

(define glColor4b (c-lambda ( GLbyte GLbyte GLbyte GLbyte ) void "glColor4b"))
(define glColor4d (c-lambda ( GLdouble GLdouble GLdouble GLdouble ) void "glColor4d"))
(define glColor4f (c-lambda ( GLfloat GLfloat GLfloat GLfloat ) void "glColor4f"))
(define glColor4i (c-lambda ( GLint GLint GLint GLint ) void "glColor4i"))
(define glColor4s (c-lambda ( GLshort GLshort GLshort GLshort ) void "glColor4s"))
(define glColor4ub (c-lambda ( GLubyte GLubyte GLubyte GLubyte ) void "glColor4ub"))
(define glColor4ui (c-lambda ( GLuint GLuint GLuint GLuint ) void "glColor4ui"))
(define glColor4us (c-lambda ( GLushort GLushort GLushort GLushort ) void "glColor4us"))


(define glColor3bv (c-lambda ( GLbyte* ) void "glColor3bv"))
(define glColor3dv (c-lambda ( GLdouble* ) void "glColor3dv"))
(define glColor3fv (c-lambda ( GLfloat* ) void "glColor3fv"))
(define glColor3iv (c-lambda ( GLint* ) void "glColor3iv"))
(define glColor3sv (c-lambda ( GLshort* ) void "glColor3sv"))
(define glColor3ubv (c-lambda ( GLubyte* ) void "glColor3ubv"))
(define glColor3uiv (c-lambda ( GLuint* ) void "glColor3uiv"))
(define glColor3usv (c-lambda ( GLushort* ) void "glColor3usv"))

(define glColor4bv (c-lambda ( GLbyte* ) void "glColor4bv"))
(define glColor4dv (c-lambda ( GLdouble* ) void "glColor4dv"))
(define glColor4fv (c-lambda ( GLfloat* ) void "glColor4fv"))
(define glColor4iv (c-lambda ( GLint* ) void "glColor4iv"))
(define glColor4sv (c-lambda ( GLshort* ) void "glColor4sv"))
(define glColor4ubv (c-lambda ( GLubyte* ) void "glColor4ubv"))
(define glColor4uiv (c-lambda ( GLuint* ) void "glColor4uiv"))
(define glColor4usv (c-lambda ( GLushort* ) void "glColor4usv"))


(define glTexCoord1d (c-lambda ( GLdouble ) void "glTexCoord1d"))
(define glTexCoord1f (c-lambda ( GLfloat ) void "glTexCoord1f"))
(define glTexCoord1i (c-lambda ( GLint ) void "glTexCoord1i"))
(define glTexCoord1s (c-lambda ( GLshort ) void "glTexCoord1s"))

(define glTexCoord2d (c-lambda ( GLdouble GLdouble ) void "glTexCoord2d"))
(define glTexCoord2f (c-lambda ( GLfloat GLfloat ) void "glTexCoord2f"))
(define glTexCoord2i (c-lambda ( GLint GLint ) void "glTexCoord2i"))
(define glTexCoord2s (c-lambda ( GLshort GLshort ) void "glTexCoord2s"))

(define glTexCoord3d (c-lambda ( GLdouble GLdouble GLdouble ) void "glTexCoord3d"))
(define glTexCoord3f (c-lambda ( GLfloat GLfloat GLfloat ) void "glTexCoord3f"))
(define glTexCoord3i (c-lambda ( GLint GLint GLint ) void "glTexCoord3i"))
(define glTexCoord3s (c-lambda ( GLshort GLshort GLshort ) void "glTexCoord3s"))

(define glTexCoord4d (c-lambda ( GLdouble GLdouble GLdouble GLdouble ) void "glTexCoord4d"))
(define glTexCoord4f (c-lambda ( GLfloat GLfloat GLfloat GLfloat ) void "glTexCoord4f"))
(define glTexCoord4i (c-lambda ( GLint GLint GLint GLint ) void "glTexCoord4i"))
(define glTexCoord4s (c-lambda ( GLshort GLshort GLshort GLshort ) void "glTexCoord4s"))

(define glTexCoord1dv (c-lambda ( GLdouble* ) void "glTexCoord1dv"))
(define glTexCoord1fv (c-lambda ( GLfloat* ) void "glTexCoord1fv"))
(define glTexCoord1iv (c-lambda ( GLint* ) void "glTexCoord1iv"))
(define glTexCoord1sv (c-lambda ( GLshort* ) void "glTexCoord1sv"))

(define glTexCoord2dv (c-lambda ( GLdouble* ) void "glTexCoord2dv"))
(define glTexCoord2fv (c-lambda ( GLfloat* ) void "glTexCoord2fv"))
(define glTexCoord2iv (c-lambda ( GLint* ) void "glTexCoord2iv"))
(define glTexCoord2sv (c-lambda ( GLshort* ) void "glTexCoord2sv"))

(define glTexCoord3dv (c-lambda ( GLdouble* ) void "glTexCoord3dv"))
(define glTexCoord3fv (c-lambda ( GLfloat* ) void "glTexCoord3fv"))
(define glTexCoord3iv (c-lambda ( GLint* ) void "glTexCoord3iv"))
(define glTexCoord3sv (c-lambda ( GLshort* ) void "glTexCoord3sv"))

(define glTexCoord4dv (c-lambda ( GLdouble* ) void "glTexCoord4dv"))
(define glTexCoord4fv (c-lambda ( GLfloat* ) void "glTexCoord4fv"))
(define glTexCoord4iv (c-lambda ( GLint* ) void "glTexCoord4iv"))
(define glTexCoord4sv (c-lambda ( GLshort* ) void "glTexCoord4sv"))


(define glRasterPos2d (c-lambda ( GLdouble GLdouble ) void "glRasterPos2d"))
(define glRasterPos2f (c-lambda ( GLfloat GLfloat ) void "glRasterPos2f"))
(define glRasterPos2i (c-lambda ( GLint GLint ) void "glRasterPos2i"))
(define glRasterPos2s (c-lambda ( GLshort GLshort ) void "glRasterPos2s"))

(define glRasterPos3d (c-lambda ( GLdouble GLdouble GLdouble ) void "glRasterPos3d"))
(define glRasterPos3f (c-lambda ( GLfloat GLfloat GLfloat ) void "glRasterPos3f"))
(define glRasterPos3i (c-lambda ( GLint GLint GLint ) void "glRasterPos3i"))
(define glRasterPos3s (c-lambda ( GLshort GLshort GLshort ) void "glRasterPos3s"))

(define glRasterPos4d (c-lambda ( GLdouble GLdouble GLdouble GLdouble ) void "glRasterPos4d"))
(define glRasterPos4f (c-lambda ( GLfloat GLfloat GLfloat GLfloat ) void "glRasterPos4f"))
(define glRasterPos4i (c-lambda ( GLint GLint GLint GLint ) void "glRasterPos4i"))
(define glRasterPos4s (c-lambda ( GLshort GLshort GLshort GLshort ) void "glRasterPos4s"))

(define glRasterPos2dv (c-lambda ( GLdouble* ) void "glRasterPos2dv"))
(define glRasterPos2fv (c-lambda ( GLfloat* ) void "glRasterPos2fv"))
(define glRasterPos2iv (c-lambda ( GLint* ) void "glRasterPos2iv"))
(define glRasterPos2sv (c-lambda ( GLshort* ) void "glRasterPos2sv"))

(define glRasterPos3dv (c-lambda ( GLdouble* ) void "glRasterPos3dv"))
(define glRasterPos3fv (c-lambda ( GLfloat* ) void "glRasterPos3fv"))
(define glRasterPos3iv (c-lambda ( GLint* ) void "glRasterPos3iv"))
(define glRasterPos3sv (c-lambda ( GLshort* ) void "glRasterPos3sv"))

(define glRasterPos4dv (c-lambda ( GLdouble* ) void "glRasterPos4dv"))
(define glRasterPos4fv (c-lambda ( GLfloat* ) void "glRasterPos4fv"))
(define glRasterPos4iv (c-lambda ( GLint* ) void "glRasterPos4iv"))
(define glRasterPos4sv (c-lambda ( GLshort* ) void "glRasterPos4sv"))


(define glRectd (c-lambda ( GLdouble GLdouble GLdouble GLdouble ) void "glRectd"))
(define glRectf (c-lambda ( GLfloat GLfloat GLfloat GLfloat ) void "glRectf"))
(define glRecti (c-lambda ( GLint GLint GLint GLint ) void "glRecti"))
(define glRects (c-lambda ( GLshort GLshort GLshort GLshort ) void "glRects"))


(define glRectdv (c-lambda ( GLdouble* GLdouble* ) void "glRectdv"))
(define glRectfv (c-lambda ( GLfloat* GLfloat* ) void "glRectfv"))
(define glRectiv (c-lambda ( GLint* GLint* ) void "glRectiv"))
(define glRectsv (c-lambda ( GLshort* GLshort* ) void "glRectsv"))


;; /*
;;  * Vertex Arrays  (1.1)
;;  */

(define glVertexPointer (c-lambda ( GLint GLenum GLsizei GLvoid* ) void "glVertexPointer"))

(define glNormalPointer (c-lambda ( GLenum GLsizei GLvoid* ) void "glNormalPointer"))

(define glColorPointer (c-lambda ( GLint GLenum GLsizei GLvoid* ) void "glColorPointer"))

(define glIndexPointer (c-lambda ( GLenum GLsizei GLvoid* ) void "glIndexPointer"))

(define glTexCoordPointer (c-lambda ( GLint GLenum GLsizei GLvoid* ) void "glTexCoordPointer"))

(define glEdgeFlagPointer (c-lambda ( GLsizei GLvoid* ) void "glEdgeFlagPointer"))

(define glGetPointerv (c-lambda ( GLenum GLvoid**) void "glGetPointerv"))

(define glArrayElement (c-lambda ( GLint ) void "glArrayElement"))

(define glDrawArrays (c-lambda ( GLenum GLint GLsizei ) void "glDrawArrays"))

(define glDrawElements (c-lambda ( GLenum GLsizei GLenum GLvoid* ) void "glDrawElements"))

(define glInterleavedArrays (c-lambda ( GLenum GLsizei GLvoid* ) void "glInterleavedArrays"))

;; /*
;;  * Lighting
;;  */

(define glShadeModel (c-lambda ( GLenum ) void "glShadeModel"))

(define glLightf (c-lambda ( GLenum GLenum GLfloat ) void "glLightf"))
(define glLighti (c-lambda ( GLenum GLenum GLint ) void "glLighti"))
(define glLightfv (c-lambda ( GLenum GLenum GLfloat* ) void "glLightfv"))
(define glLightiv (c-lambda ( GLenum GLenum GLint* ) void "glLightiv"))

(define glGetLightfv (c-lambda ( GLenum GLenum GLfloat* ) void "glGetLightfv"))
(define glGetLightiv (c-lambda ( GLenum GLenum GLint* ) void "glGetLightiv"))

(define glLightModelf (c-lambda ( GLenum GLfloat ) void "glLightModelf"))
(define glLightModeli (c-lambda ( GLenum GLint ) void "glLightModeli"))
(define glLightModelfv (c-lambda ( GLenum GLfloat* ) void "glLightModelfv"))
(define glLightModeliv (c-lambda ( GLenum GLint* ) void "glLightModeliv"))

(define glMaterialf (c-lambda ( GLenum GLenum GLfloat ) void "glMaterialf"))
(define glMateriali (c-lambda ( GLenum GLenum GLint ) void "glMateriali"))
(define glMaterialfv (c-lambda ( GLenum GLenum GLfloat* ) void "glMaterialfv"))
(define glMaterialiv (c-lambda ( GLenum GLenum GLint* ) void "glMaterialiv"))

(define glGetMaterialfv (c-lambda ( GLenum GLenum GLfloat* ) void "glGetMaterialfv"))
(define glGetMaterialiv (c-lambda ( GLenum GLenum GLint* ) void "glGetMaterialiv"))

(define glColorMaterial (c-lambda ( GLenum GLenum ) void "glColorMaterial"))


;; /*
;;  * Raster functions
;;  */

(define glPixelZoom (c-lambda ( GLfloat GLfloat ) void "glPixelZoom"))

(define glPixelStoref (c-lambda ( GLenum GLfloat ) void "glPixelStoref"))
(define glPixelStorei (c-lambda ( GLenum GLint ) void "glPixelStorei"))

(define glPixelTransferf (c-lambda ( GLenum GLfloat ) void "glPixelTransferf"))
(define glPixelTransferi (c-lambda ( GLenum GLint ) void "glPixelTransferi"))

(define glPixelMapfv (c-lambda ( GLenum GLsizei GLfloat* ) void "glPixelMapfv"))
(define glPixelMapuiv (c-lambda ( GLenum GLsizei GLuint* ) void "glPixelMapuiv"))
(define glPixelMapusv (c-lambda ( GLenum GLsizei GLushort* ) void "glPixelMapusv"))

(define glGetPixelMapfv (c-lambda ( GLenum GLfloat* ) void "glGetPixelMapfv"))
(define glGetPixelMapuiv (c-lambda ( GLenum GLuint* ) void "glGetPixelMapuiv"))
(define glGetPixelMapusv (c-lambda ( GLenum GLushort* ) void "glGetPixelMapusv"))

(define glBitmap (c-lambda ( GLsizei GLsizei GLfloat GLfloat GLfloat GLfloat GLubyte* ) void "glBitmap"))

(define glReadPixels (c-lambda ( GLint GLint GLsizei GLsizei GLenum GLenum GLvoid* ) void "glReadPixels"))

(define glDrawPixels (c-lambda ( GLsizei GLsizei GLenum GLenum GLvoid* ) void "glDrawPixels"))

(define glCopyPixels (c-lambda ( GLint GLint GLsizei GLsizei GLenum ) void "glCopyPixels"))

/*
 * Stenciling
 */
(define glStencilFunc (c-lambda ( GLenum GLint GLuint ) void "glStencilFunc"))

(define glStencilMask (c-lambda ( GLuint ) void "glStencilMask"))

(define glStencilOp (c-lambda ( GLenum GLenum GLenum ) void "glStencilOp"))

(define glClearStencil (c-lambda ( GLint ) void "glClearStencil"))



;; /*
;;  * Texture mapping
;;  */

(define glTexGend (c-lambda ( GLenum GLenum GLdouble ) void "glTexGend"))
(define glTexGenf (c-lambda ( GLenum GLenum GLfloat ) void "glTexGenf"))
(define glTexGeni (c-lambda ( GLenum GLenum GLint ) void "glTexGeni"))

(define glTexGendv (c-lambda ( GLenum GLenum GLdouble* ) void "glTexGendv"))
(define glTexGenfv (c-lambda ( GLenum GLenum GLfloat* ) void "glTexGenfv"))
(define glTexGeniv (c-lambda ( GLenum GLenum GLint* ) void "glTexGeniv"))

(define glGetTexGendv (c-lambda ( GLenum GLenum GLdouble* ) void "glGetTexGendv"))
(define glGetTexGenfv (c-lambda ( GLenum GLenum GLfloat* ) void "glGetTexGenfv"))
(define glGetTexGeniv (c-lambda ( GLenum GLenum GLint* ) void "glGetTexGeniv"))


(define glTexEnvf (c-lambda ( GLenum GLenum GLfloat ) void "glTexEnvf"))
(define glTexEnvi (c-lambda ( GLenum GLenum GLint ) void "glTexEnvi"))

(define glTexEnvfv (c-lambda ( GLenum GLenum GLfloat* ) void "glTexEnvfv"))
(define glTexEnviv (c-lambda ( GLenum GLenum GLint* ) void "glTexEnviv"))

(define glGetTexEnvfv (c-lambda ( GLenum GLenum GLfloat* ) void "glGetTexEnvfv"))
(define glGetTexEnviv (c-lambda ( GLenum GLenum GLint* ) void "glGetTexEnviv"))


(define glTexParameterf (c-lambda ( GLenum GLenum GLfloat ) void "glTexParameterf"))
(define glTexParameteri (c-lambda ( GLenum GLenum GLint ) void "glTexParameteri"))

(define glTexParameterfv (c-lambda ( GLenum GLenum GLfloat* ) void "glTexParameterfv"))
(define glTexParameteriv (c-lambda ( GLenum GLenum GLint* ) void "glTexParameteriv"))

(define glGetTexParameterfv (c-lambda ( GLenum GLenum GLfloat* ) void "glGetTexParameterfv"))
(define glGetTexParameteriv (c-lambda ( GLenum GLenum GLint* ) void "glGetTexParameteriv"))

(define glGetTexLevelParameterfv (c-lambda ( GLenum GLint GLenum GLfloat* ) void "glGetTexLevelParameterfv"))
(define glGetTexLevelParameteriv (c-lambda ( GLenum GLint GLenum GLint* ) void "glGetTexLevelParameteriv"))


(define glTexImage1D (c-lambda ( GLenum GLint GLint GLsizei GLint GLenum GLenum GLvoid* ) void "glTexImage1D"))

(define glTexImage2D (c-lambda ( GLenum GLint GLint GLsizei GLsizei GLint GLenum GLenum GLvoid* ) void "glTexImage2D"))

(define glGetTexImage (c-lambda ( GLenum GLint GLenum GLenum GLvoid* ) void "glGetTexImage"))


/* 1.1 functions* /

(define glGenTextures (c-lambda ( GLsizei GLuint* ) void "glGenTextures"))

(define glDeleteTextures (c-lambda ( GLsizei GLuint* ) void "glDeleteTextures"))

(define glBindTexture (c-lambda ( GLenum GLuint ) void "glBindTexture"))

(define glPrioritizeTextures (c-lambda ( GLsizei GLuint* GLclampf* ) void "glPrioritizeTextures"))

(define glAreTexturesResident (c-lambda ( GLsizei GLuint* GLboolean* ) GLboolean "glAreTexturesResident"))

(define glIsTexture (c-lambda ( GLuint ) GLboolean "glIsTexture"))


(define glTexSubImage1D (c-lambda ( GLenum GLint GLint GLsizei GLenum GLenum GLvoid* ) void "glTexSubImage1D"))


(define glTexSubImage2D (c-lambda ( GLenum GLint GLint GLint GLsizei GLsizei GLenum GLenum GLvoid* ) void "glTexSubImage2D"))


(define glCopyTexImage1D (c-lambda ( GLenum GLint GLenum GLint GLint GLsizei GLint ) void "glCopyTexImage1D"))


(define glCopyTexImage2D (c-lambda ( GLenum GLint GLenum GLint GLint GLsizei GLsizei GLint ) void "glCopyTexImage2D"))


(define glCopyTexSubImage1D (c-lambda ( GLenum GLint GLint GLint GLint GLsizei ) void "glCopyTexSubImage1D"))


(define glCopyTexSubImage2D (c-lambda ( GLenum GLint GLint GLint GLint GLint GLsizei GLsizei ) void "glCopyTexSubImage2D"))


;; /*
;;  * Evaluators
;;  */

(define glMap1d (c-lambda ( GLenum GLdouble GLdouble GLint GLint GLdouble* ) void "glMap1d"))
(define glMap1f (c-lambda ( GLenum GLfloat GLfloat GLint GLint GLfloat* ) void "glMap1f"))

(define glMap2d (c-lambda ( GLenum GLdouble GLdouble GLint GLint GLdouble GLdouble GLint GLint GLdouble* ) void "glMap2d"))
(define glMap2f (c-lambda ( GLenum GLfloat GLfloat GLint GLint GLfloat GLfloat GLint GLint GLfloat* ) void "glMap2f"))

(define glGetMapdv (c-lambda ( GLenum GLenum GLdouble* ) void "glGetMapdv"))
(define glGetMapfv (c-lambda ( GLenum GLenum GLfloat* ) void "glGetMapfv"))
(define glGetMapiv (c-lambda ( GLenum GLenum GLint* ) void "glGetMapiv"))

(define glEvalCoord1d (c-lambda ( GLdouble ) void "glEvalCoord1d"))
(define glEvalCoord1f (c-lambda ( GLfloat ) void "glEvalCoord1f"))

(define glEvalCoord1dv (c-lambda ( GLdouble* ) void "glEvalCoord1dv"))
(define glEvalCoord1fv (c-lambda ( GLfloat* ) void "glEvalCoord1fv"))

(define glEvalCoord2d (c-lambda ( GLdouble GLdouble ) void "glEvalCoord2d"))
(define glEvalCoord2f (c-lambda ( GLfloat GLfloat ) void "glEvalCoord2f"))

(define glEvalCoord2dv (c-lambda ( GLdouble* ) void "glEvalCoord2dv"))
(define glEvalCoord2fv (c-lambda ( GLfloat* ) void "glEvalCoord2fv"))

(define glMapGrid1d (c-lambda ( GLint GLdouble GLdouble ) void "glMapGrid1d"))
(define glMapGrid1f (c-lambda ( GLint GLfloat GLfloat ) void "glMapGrid1f"))

(define glMapGrid2d (c-lambda ( GLint GLdouble GLdouble GLint GLdouble GLdouble ) void "glMapGrid2d"))
(define glMapGrid2f (c-lambda ( GLint GLfloat GLfloat GLint GLfloat GLfloat ) void "glMapGrid2f"))

(define glEvalPoint1 (c-lambda ( GLint ) void "glEvalPoint1"))

(define glEvalPoint2 (c-lambda ( GLint GLint ) void "glEvalPoint2"))

(define glEvalMesh1 (c-lambda ( GLenum GLint GLint ) void "glEvalMesh1"))

(define glEvalMesh2 (c-lambda ( GLenum GLint GLint GLint GLint ) void "glEvalMesh2"))


;; /*
;;  * Fog
;;  */

(define glFogf (c-lambda ( GLenum GLfloat ) void "glFogf"))

(define glFogi (c-lambda ( GLenum GLint ) void "glFogi"))

(define glFogfv (c-lambda ( GLenum GLfloat* ) void "glFogfv"))

(define glFogiv (c-lambda ( GLenum GLint* ) void "glFogiv"))


;; /*
;;  * Selection and Feedback
;;  */

(define glFeedbackBuffer (c-lambda ( GLsizei GLenum GLfloat* ) void "glFeedbackBuffer"))

(define glPassThrough (c-lambda ( GLfloat ) void "glPassThrough"))

(define glSelectBuffer (c-lambda ( GLsizei GLuint* ) void "glSelectBuffer"))

(define glInitNames (c-lambda () void "glInitNames"))

(define glLoadName (c-lambda ( GLuint ) void "glLoadName"))

(define glPushName (c-lambda ( GLuint ) void "glPushName"))

(define glPopName (c-lambda () void "glPopName"))
