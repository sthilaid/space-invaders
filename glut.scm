;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: glut.scm
;;
;; description: glut ffi interface
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "opengl-header.scm")
(include "glu-header.scm")
(include "glut-header.scm")

(c-declare #<<declare-end
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
declare-end
)



; os dependent macros:

(define GLUT_STROKE_ROMAN ((c-lambda () void* "___result_voidstar = GLUT_STROKE_ROMAN;")))
(define GLUT_STROKE_MONO_ROMAN ((c-lambda () void* "___result_voidstar = GLUT_STROKE_MONO_ROMAN;")))
(define GLUT_BITMAP_9_BY_15 ((c-lambda () void* "___result_voidstar = GLUT_BITMAP_9_BY_15;")))
(define GLUT_BITMAP_8_BY_13 ((c-lambda () void* "___result_voidstar = GLUT_BITMAP_8_BY_13;")))
(define GLUT_BITMAP_TIMES_ROMAN_10 ((c-lambda () void* "___result_voidstar = GLUT_BITMAP_TIMES_ROMAN_10;")))
(define GLUT_BITMAP_TIMES_ROMAN_24 ((c-lambda () void* "___result_voidstar = GLUT_BITMAP_TIMES_ROMAN_24;")))
(define GLUT_BITMAP_HELVETICA_10 ((c-lambda () void* "___result_voidstar = GLUT_BITMAP_HELVETICA_10;")))
(define GLUT_BITMAP_HELVETICA_12 ((c-lambda () void* "___result_voidstar = GLUT_BITMAP_HELVETICA_12;")))
(define GLUT_BITMAP_HELVETICA_18 ((c-lambda () void* "___result_voidstar = GLUT_BITMAP_HELVETICA_18;")))


;; /*
;;  * Initialization see fglut_init.c
;;  */
(define glutInit (c-lambda ( int*  nonnull-char-string-list ) void "glutInit"))
(define glutInitWindowPosition (c-lambda ( int int ) void "glutInitWindowPosition"))
(define glutInitWindowSize (c-lambda ( int int ) void "glutInitWindowSize"))
(define glutInitDisplayMode (c-lambda ( unsigned-int ) void "glutInitDisplayMode"))
(define glutInitDisplayString (c-lambda ( char-string ) void "glutInitDisplayString"))

;; /*
;;  * Process loop see freeglut_main.c
;;  */
(define glutMainLoop (c-lambda ( ) void "glutMainLoop"))

;; /*
;;  * Window management see freeglut_window.c
;;  */
(define glutCreateWindow (c-lambda ( char-string ) int "glutCreateWindow"))
(define glutCreateSubWindow (c-lambda ( int int int int int ) int "glutCreateSubWindow"))
(define glutDestroyWindow (c-lambda ( int ) void "glutDestroyWindow"))
(define glutSetWindow (c-lambda ( int ) void "glutSetWindow"))
(define glutGetWindow (c-lambda ( ) int "glutGetWindow"))
(define glutSetWindowTitle (c-lambda ( char-string ) void "glutSetWindowTitle"))
(define glutSetIconTitle (c-lambda ( char-string ) void "glutSetIconTitle"))
(define glutReshapeWindow (c-lambda ( int int ) void "glutReshapeWindow"))
(define glutPositionWindow (c-lambda ( int int ) void "glutPositionWindow"))
(define glutShowWindow (c-lambda ( ) void "glutShowWindow"))
(define glutHideWindow (c-lambda ( ) void "glutHideWindow"))
(define glutIconifyWindow (c-lambda ( ) void "glutIconifyWindow"))
(define glutPushWindow (c-lambda ( ) void "glutPushWindow"))
(define glutPopWindow (c-lambda ( ) void "glutPopWindow"))
(define glutFullScreen (c-lambda ( ) void "glutFullScreen"))

;; /*
;;  * Display-connected see freeglut_display.c
;;  */
(define glutPostWindowRedisplay (c-lambda ( int ) void "glutPostWindowRedisplay"))
(define glutPostRedisplay (c-lambda ( ) void "glutPostRedisplay"))
(define glutSwapBuffers (c-lambda ( ) void "glutSwapBuffers"))

;; /*
;;  * Mouse cursor see freeglut_cursor.c
;;  */
(define glutWarpPointer (c-lambda ( int int ) void "glutWarpPointer"))
(define glutSetCursor (c-lambda ( int ) void "glutSetCursor"))

;; /*
;;  * Overlay see freeglut_overlay.c
;;  */
(define glutEstablishOverlay (c-lambda ( ) void "glutEstablishOverlay"))
(define glutRemoveOverlay (c-lambda ( ) void "glutRemoveOverlay"))
(define glutUseLayer (c-lambda ( GLenum ) void "glutUseLayer"))
(define glutPostOverlayRedisplay (c-lambda ( ) void "glutPostOverlayRedisplay"))
(define glutPostWindowOverlayRedisplay (c-lambda ( int ) void "glutPostWindowOverlayRedisplay"))
(define glutShowOverlay (c-lambda ( ) void "glutShowOverlay"))
(define glutHideOverlay (c-lambda ( ) void "glutHideOverlay"))

;; /*
;;  * Menu see freeglut_menu.c
;;  */
(define glutCreateMenu (c-lambda ( (function (int) void) ) int "glutCreateMenu"))
(define glutDestroyMenu (c-lambda ( int ) void "glutDestroyMenu"))
(define glutGetMenu (c-lambda ( ) int "glutGetMenu"))
(define glutSetMenu (c-lambda ( int ) void "glutSetMenu"))
(define glutAddMenuEntry (c-lambda ( char-string int ) void "glutAddMenuEntry"))
(define glutAddSubMenu (c-lambda ( char-string int ) void "glutAddSubMenu"))
(define glutChangeToMenuEntry (c-lambda ( int char-string int ) void "glutChangeToMenuEntry"))
(define glutChangeToSubMenu (c-lambda ( int char-string int ) void "glutChangeToSubMenu"))
(define glutRemoveMenuItem (c-lambda ( int ) void "glutRemoveMenuItem"))
(define glutAttachMenu (c-lambda ( int ) void "glutAttachMenu"))
(define glutDetachMenu (c-lambda ( int ) void "glutDetachMenu"))

;; /*
;;  * Global callback see freeglut_callbacks.c
;;  */

(define glutTimerFunc (c-lambda ( unsigned-int (function (int) void) int ) void "glutTimerFunc"))
(define glutIdleFunc (c-lambda ( (function () void) ) void "glutIdleFunc"))

;; /*
;;  * Window-specific callback see freeglut_callbacks.c
;;  */
(define glutKeyboardFunc (c-lambda ( (function (unsigned-char int int) void) ) void "glutKeyboardFunc"))
(define glutSpecialFunc (c-lambda ( (function (int int int) void) ) void "glutSpecialFunc"))
(define glutReshapeFunc (c-lambda ( (function (int int) void) ) void "glutReshapeFunc"))
(define glutVisibilityFunc (c-lambda ( (function (int) void) ) void "glutVisibilityFunc"))
(define glutDisplayFunc (c-lambda ( (function () void) ) void "glutDisplayFunc"))
(define glutMouseFunc (c-lambda ( (function (int int int int) void) ) void "glutMouseFunc"))
(define glutMotionFunc (c-lambda ( (function (int int) void ) ) void "glutMotionFunc"))
(define glutPassiveMotionFunc (c-lambda ( (function (int int) void) ) void "glutPassiveMotionFunc"))
(define glutEntryFunc (c-lambda ( (function (int) void) ) void "glutEntryFunc"))

(define glutKeyboardUpFunc (c-lambda ( (function (unsigned-char int int) void) ) void "glutKeyboardUpFunc"))
(define glutSpecialUpFunc (c-lambda ( (function (int int int) void) ) void "glutSpecialUpFunc"))
(define glutJoystickFunc (c-lambda ( (function (unsigned-int int int int) void) int ) void "glutJoystickFunc"))
(define glutMenuStateFunc (c-lambda ( (function (int) void) ) void "glutMenuStateFunc"))
(define glutMenuStatusFunc (c-lambda ( (function (int int int) void) ) void "glutMenuStatusFunc"))
(define glutOverlayDisplayFunc (c-lambda ( (function () void) ) void "glutOverlayDisplayFunc"))
(define glutWindowStatusFunc (c-lambda ( (function (int) void) ) void "glutWindowStatusFunc"))

(define glutSpaceballMotionFunc (c-lambda ( (function (int int int) void) ) void "glutSpaceballMotionFunc"))
(define glutSpaceballRotateFunc (c-lambda ( (function (int int int) void) ) void "glutSpaceballRotateFunc"))
(define glutSpaceballButtonFunc (c-lambda ( (function (int int) void) ) void "glutSpaceballButtonFunc"))
(define glutButtonBoxFunc (c-lambda ( (function (int int) void) ) void "glutButtonBoxFunc"))
(define glutDialsFunc (c-lambda ( (function (int int) void) ) void "glutDialsFunc"))
(define glutTabletMotionFunc (c-lambda ( (function (int int) void) ) void "glutTabletMotionFunc"))
(define glutTabletButtonFunc (c-lambda ( (function (int int int int) void) ) void "glutTabletButtonFunc"))

;; /*
;;  * State setting and retrieval see freeglut_state.c
;;  */
(define glutGet (c-lambda ( GLenum ) int "glutGet"))
(define glutDeviceGet (c-lambda ( GLenum ) int "glutDeviceGet"))
(define glutGetModifiers (c-lambda ( ) int "glutGetModifiers"))
(define glutLayerGet (c-lambda ( GLenum ) int "glutLayerGet"))

;; /*
;;  * Font see freeglut_font.c
;;  */
(define glutBitmapCharacter (c-lambda ( void* int ) void "glutBitmapCharacter"))
(define glutBitmapWidth (c-lambda ( void* int ) int "glutBitmapWidth"))
(define glutStrokeCharacter (c-lambda ( void* int ) void "glutStrokeCharacter"))
(define glutStrokeWidth (c-lambda ( void* int ) int "glutStrokeWidth"))
(define glutBitmapLength (c-lambda ( void* (pointer unsigned-char) ) int "glutBitmapLength"))
(define glutStrokeLength (c-lambda ( void* (pointer unsigned-char) ) int "glutStrokeLength"))

;; /*
;;  * Geometry see freeglut_geometry.c
;;  */
(define glutWireCube (c-lambda ( GLdouble ) void "glutWireCube"))
(define glutSolidCube (c-lambda ( GLdouble ) void "glutSolidCube"))
(define glutWireSphere (c-lambda ( GLdouble GLint GLint ) void "glutWireSphere"))
(define glutSolidSphere (c-lambda ( GLdouble GLint GLint ) void "glutSolidSphere"))
(define glutWireCone (c-lambda ( GLdouble GLdouble GLint GLint ) void "glutWireCone"))
(define glutSolidCone (c-lambda ( GLdouble GLdouble GLint GLint ) void "glutSolidCone"))

(define glutWireTorus (c-lambda ( GLdouble GLdouble GLint GLint ) void "glutWireTorus"))
(define glutSolidTorus (c-lambda ( GLdouble GLdouble GLint GLint ) void "glutSolidTorus"))
(define glutWireDodecahedron (c-lambda ( ) void "glutWireDodecahedron"))
(define glutSolidDodecahedron (c-lambda ( ) void "glutSolidDodecahedron"))
(define glutWireOctahedron (c-lambda ( ) void "glutWireOctahedron"))
(define glutSolidOctahedron (c-lambda ( ) void "glutSolidOctahedron"))
(define glutWireTetrahedron (c-lambda ( ) void "glutWireTetrahedron"))
(define glutSolidTetrahedron (c-lambda ( ) void "glutSolidTetrahedron"))
(define glutWireIcosahedron (c-lambda ( ) void "glutWireIcosahedron"))
(define glutSolidIcosahedron (c-lambda ( ) void "glutSolidIcosahedron"))

;; /*
;;  * Teapot rendering found in freeglut_teapot.c
;;  */
(define glutWireTeapot (c-lambda ( GLdouble ) void "glutWireTeapot"))
(define glutSolidTeapot (c-lambda ( GLdouble ) void "glutSolidTeapot"))

;; /*
;;  * Game mode see freeglut_gamemode.c
;;  */
(define glutGameModeString (c-lambda ( char-string ) void "glutGameModeString"))
(define glutEnterGameMode (c-lambda ( ) int "glutEnterGameMode"))
(define glutLeaveGameMode (c-lambda ( ) void "glutLeaveGameMode"))
(define glutGameModeGet (c-lambda ( GLenum ) int "glutGameModeGet"))

;; /*
;;  * Video resize see freeglut_videoresize.c
;;  */
(define glutVideoResizeGet (c-lambda ( GLenum ) int "glutVideoResizeGet"))
(define glutSetupVideoResizing (c-lambda ( ) void "glutSetupVideoResizing"))
(define glutStopVideoResizing (c-lambda ( ) void "glutStopVideoResizing"))
(define glutVideoResize (c-lambda ( int int int int ) void "glutVideoResize"))
(define glutVideoPan (c-lambda ( int int int int ) void "glutVideoPan"))

;; /*
;;  * Colormap see freeglut_misc.c
;;  */
(define glutSetColor (c-lambda ( int GLfloat GLfloat GLfloat ) void "glutSetColor"))
(define glutGetColor (c-lambda ( int int ) GLfloat "glutGetColor"))
(define glutCopyColormap (c-lambda ( int ) void "glutCopyColormap"))

;; /*
;;  * Misc keyboard and joystick see freeglut_misc.c
;;  */
(define glutIgnoreKeyRepeat (c-lambda ( int ) void "glutIgnoreKeyRepeat"))
(define glutSetKeyRepeat (c-lambda ( int ) void "glutSetKeyRepeat"))
(define glutForceJoystickFunc (c-lambda ( ) void "glutForceJoystickFunc"))

;; /*
;;  * Misc see freeglut_misc.c
;;  */
(define glutExtensionSupported (c-lambda ( char-string ) int "glutExtensionSupported"))
(define glutReportErrors (c-lambda ( ) void "glutReportErrors"))
