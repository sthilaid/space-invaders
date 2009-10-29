;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: glu-header.scm
;;
;; description: Constant and type declarations for glut.scm
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (c-declare "#define _STDCALL_SUPPORTED")
; (c-declare "#define _M_IX86")
(c-declare "#include \"glut.h\"")

(c-define-type int* (pointer int))


;; /*
;;  * The freeglut and GLUT API versions
;;  */
;; (define FREEGLUT ((c-lambda () int "___result = FREEGLUT;")))
;; (define VERSION ((c-lambda () int "___result = VERSION;")))
;; ;(define 0 ((c-lambda () int "___result = 0;")))
;; (define IMPLEMENTATION ((c-lambda () int "___result = IMPLEMENTATION;")))


;; /*
;;  * GLUT API macro definitions -- the special key codes:
;;  */
(define GLUT_KEY_F1 ((c-lambda () int "___result = GLUT_KEY_F1;")))
(define GLUT_KEY_F2 ((c-lambda () int "___result = GLUT_KEY_F2;")))
(define GLUT_KEY_F3 ((c-lambda () int "___result = GLUT_KEY_F3;")))
(define GLUT_KEY_F4 ((c-lambda () int "___result = GLUT_KEY_F4;")))
(define GLUT_KEY_F5 ((c-lambda () int "___result = GLUT_KEY_F5;")))
(define GLUT_KEY_F6 ((c-lambda () int "___result = GLUT_KEY_F6;")))
(define GLUT_KEY_F7 ((c-lambda () int "___result = GLUT_KEY_F7;")))
(define GLUT_KEY_F8 ((c-lambda () int "___result = GLUT_KEY_F8;")))
(define GLUT_KEY_F9 ((c-lambda () int "___result = GLUT_KEY_F9;")))
(define GLUT_KEY_F10 ((c-lambda () int "___result = GLUT_KEY_F10;")))
(define GLUT_KEY_F11 ((c-lambda () int "___result = GLUT_KEY_F11;")))
(define GLUT_KEY_F12 ((c-lambda () int "___result = GLUT_KEY_F12;")))
(define GLUT_KEY_LEFT ((c-lambda () int "___result = GLUT_KEY_LEFT;")))
(define GLUT_KEY_UP ((c-lambda () int "___result = GLUT_KEY_UP;")))
(define GLUT_KEY_RIGHT ((c-lambda () int "___result = GLUT_KEY_RIGHT;")))
(define GLUT_KEY_DOWN ((c-lambda () int "___result = GLUT_KEY_DOWN;")))
(define GLUT_KEY_PAGE_UP ((c-lambda () int "___result = GLUT_KEY_PAGE_UP;")))
(define GLUT_KEY_PAGE_DOWN ((c-lambda () int "___result = GLUT_KEY_PAGE_DOWN;")))
(define GLUT_KEY_HOME ((c-lambda () int "___result = GLUT_KEY_HOME;")))
(define GLUT_KEY_END ((c-lambda () int "___result = GLUT_KEY_END;")))
(define GLUT_KEY_INSERT ((c-lambda () int "___result = GLUT_KEY_INSERT;")))

;; /*
;;  * GLUT API macro definitions -- mouse state definitions
;;  */
(define GLUT_LEFT_BUTTON ((c-lambda () int "___result = GLUT_LEFT_BUTTON;")))
(define GLUT_MIDDLE_BUTTON ((c-lambda () int "___result = GLUT_MIDDLE_BUTTON;")))
(define GLUT_RIGHT_BUTTON ((c-lambda () int "___result = GLUT_RIGHT_BUTTON;")))
(define GLUT_DOWN ((c-lambda () int "___result = GLUT_DOWN;")))
(define GLUT_UP ((c-lambda () int "___result = GLUT_UP;")))
(define GLUT_LEFT ((c-lambda () int "___result = GLUT_LEFT;")))
(define GLUT_ENTERED ((c-lambda () int "___result = GLUT_ENTERED;")))

;; /*
;;  * GLUT API macro definitions -- the display mode definitions
;;  */
(define GLUT_RGB ((c-lambda () int "___result = GLUT_RGB;")))
(define GLUT_RGBA ((c-lambda () int "___result = GLUT_RGBA;")))
(define GLUT_INDEX ((c-lambda () int "___result = GLUT_INDEX;")))
(define GLUT_SINGLE ((c-lambda () int "___result = GLUT_SINGLE;")))
(define GLUT_DOUBLE ((c-lambda () int "___result = GLUT_DOUBLE;")))
(define GLUT_ACCUM ((c-lambda () int "___result = GLUT_ACCUM;")))
(define GLUT_ALPHA ((c-lambda () int "___result = GLUT_ALPHA;")))
(define GLUT_DEPTH ((c-lambda () int "___result = GLUT_DEPTH;")))
(define GLUT_STENCIL ((c-lambda () int "___result = GLUT_STENCIL;")))
(define GLUT_MULTISAMPLE ((c-lambda () int "___result = GLUT_MULTISAMPLE;")))
(define GLUT_STEREO ((c-lambda () int "___result = GLUT_STEREO;")))
(define GLUT_LUMINANCE ((c-lambda () int "___result = GLUT_LUMINANCE;")))

;; /*
;;  * GLUT API macro definitions -- windows and menu related definitions
;;  */
(define GLUT_MENU_NOT_IN_USE ((c-lambda () int "___result = GLUT_MENU_NOT_IN_USE;")))
(define GLUT_MENU_IN_USE ((c-lambda () int "___result = GLUT_MENU_IN_USE;")))
(define GLUT_NOT_VISIBLE ((c-lambda () int "___result = GLUT_NOT_VISIBLE;")))
(define GLUT_VISIBLE ((c-lambda () int "___result = GLUT_VISIBLE;")))
(define GLUT_HIDDEN ((c-lambda () int "___result = GLUT_HIDDEN;")))
(define GLUT_FULLY_RETAINED ((c-lambda () int "___result = GLUT_FULLY_RETAINED;")))
(define GLUT_PARTIALLY_RETAINED ((c-lambda () int "___result = GLUT_PARTIALLY_RETAINED;")))
(define GLUT_FULLY_COVERED ((c-lambda () int "___result = GLUT_FULLY_COVERED;")))

;; /*
;;  * GLUT API macro definitions -- the glutGet parameters
;;  */
(define GLUT_WINDOW_X ((c-lambda () int "___result = GLUT_WINDOW_X;")))
(define GLUT_WINDOW_Y ((c-lambda () int "___result = GLUT_WINDOW_Y;")))
(define GLUT_WINDOW_WIDTH ((c-lambda () int "___result = GLUT_WINDOW_WIDTH;")))
(define GLUT_WINDOW_HEIGHT ((c-lambda () int "___result = GLUT_WINDOW_HEIGHT;")))
(define GLUT_WINDOW_BUFFER_SIZE ((c-lambda () int "___result = GLUT_WINDOW_BUFFER_SIZE;")))
(define GLUT_WINDOW_STENCIL_SIZE ((c-lambda () int "___result = GLUT_WINDOW_STENCIL_SIZE;")))
(define GLUT_WINDOW_DEPTH_SIZE ((c-lambda () int "___result = GLUT_WINDOW_DEPTH_SIZE;")))
(define GLUT_WINDOW_RED_SIZE ((c-lambda () int "___result = GLUT_WINDOW_RED_SIZE;")))
(define GLUT_WINDOW_GREEN_SIZE ((c-lambda () int "___result = GLUT_WINDOW_GREEN_SIZE;")))
(define GLUT_WINDOW_BLUE_SIZE ((c-lambda () int "___result = GLUT_WINDOW_BLUE_SIZE;")))
(define GLUT_WINDOW_ALPHA_SIZE ((c-lambda () int "___result = GLUT_WINDOW_ALPHA_SIZE;")))
(define GLUT_WINDOW_ACCUM_RED_SIZE ((c-lambda () int "___result = GLUT_WINDOW_ACCUM_RED_SIZE;")))
(define GLUT_WINDOW_ACCUM_GREEN_SIZE ((c-lambda () int "___result = GLUT_WINDOW_ACCUM_GREEN_SIZE;")))
(define GLUT_WINDOW_ACCUM_BLUE_SIZE ((c-lambda () int "___result = GLUT_WINDOW_ACCUM_BLUE_SIZE;")))
(define GLUT_WINDOW_ACCUM_ALPHA_SIZE ((c-lambda () int "___result = GLUT_WINDOW_ACCUM_ALPHA_SIZE;")))
(define GLUT_WINDOW_DOUBLEBUFFER ((c-lambda () int "___result = GLUT_WINDOW_DOUBLEBUFFER;")))
(define GLUT_WINDOW_RGBA ((c-lambda () int "___result = GLUT_WINDOW_RGBA;")))
(define GLUT_WINDOW_PARENT ((c-lambda () int "___result = GLUT_WINDOW_PARENT;")))
(define GLUT_WINDOW_NUM_CHILDREN ((c-lambda () int "___result = GLUT_WINDOW_NUM_CHILDREN;")))
(define GLUT_WINDOW_COLORMAP_SIZE ((c-lambda () int "___result = GLUT_WINDOW_COLORMAP_SIZE;")))
(define GLUT_WINDOW_NUM_SAMPLES ((c-lambda () int "___result = GLUT_WINDOW_NUM_SAMPLES;")))
(define GLUT_WINDOW_STEREO ((c-lambda () int "___result = GLUT_WINDOW_STEREO;")))
(define GLUT_WINDOW_CURSOR ((c-lambda () int "___result = GLUT_WINDOW_CURSOR;")))

(define GLUT_SCREEN_WIDTH ((c-lambda () int "___result = GLUT_SCREEN_WIDTH;")))
(define GLUT_SCREEN_HEIGHT ((c-lambda () int "___result = GLUT_SCREEN_HEIGHT;")))
(define GLUT_SCREEN_WIDTH_MM ((c-lambda () int "___result = GLUT_SCREEN_WIDTH_MM;")))
(define GLUT_SCREEN_HEIGHT_MM ((c-lambda () int "___result = GLUT_SCREEN_HEIGHT_MM;")))
(define GLUT_MENU_NUM_ITEMS ((c-lambda () int "___result = GLUT_MENU_NUM_ITEMS;")))
(define GLUT_DISPLAY_MODE_POSSIBLE ((c-lambda () int "___result = GLUT_DISPLAY_MODE_POSSIBLE;")))
(define GLUT_INIT_WINDOW_X ((c-lambda () int "___result = GLUT_INIT_WINDOW_X;")))
(define GLUT_INIT_WINDOW_Y ((c-lambda () int "___result = GLUT_INIT_WINDOW_Y;")))
(define GLUT_INIT_WINDOW_WIDTH ((c-lambda () int "___result = GLUT_INIT_WINDOW_WIDTH;")))
(define GLUT_INIT_WINDOW_HEIGHT ((c-lambda () int "___result = GLUT_INIT_WINDOW_HEIGHT;")))
(define GLUT_INIT_DISPLAY_MODE ((c-lambda () int "___result = GLUT_INIT_DISPLAY_MODE;")))
(define GLUT_ELAPSED_TIME ((c-lambda () int "___result = GLUT_ELAPSED_TIME;")))
(define GLUT_WINDOW_FORMAT_ID ((c-lambda () int "___result = GLUT_WINDOW_FORMAT_ID;")))
;; (define GLUT_INIT_STATE ((c-lambda () int "___result = GLUT_INIT_STATE;")))

;; /*
;;  * GLUT API macro definitions -- the glutDeviceGet parameters
;;  */
(define GLUT_HAS_KEYBOARD ((c-lambda () int "___result = GLUT_HAS_KEYBOARD;")))
(define GLUT_HAS_MOUSE ((c-lambda () int "___result = GLUT_HAS_MOUSE;")))
(define GLUT_HAS_SPACEBALL ((c-lambda () int "___result = GLUT_HAS_SPACEBALL;")))
(define GLUT_HAS_DIAL_AND_BUTTON_BOX ((c-lambda () int "___result = GLUT_HAS_DIAL_AND_BUTTON_BOX;")))
(define GLUT_HAS_TABLET ((c-lambda () int "___result = GLUT_HAS_TABLET;")))
(define GLUT_NUM_MOUSE_BUTTONS ((c-lambda () int "___result = GLUT_NUM_MOUSE_BUTTONS;")))
(define GLUT_NUM_SPACEBALL_BUTTONS ((c-lambda () int "___result = GLUT_NUM_SPACEBALL_BUTTONS;")))
(define GLUT_NUM_BUTTON_BOX_BUTTONS ((c-lambda () int "___result = GLUT_NUM_BUTTON_BOX_BUTTONS;")))
(define GLUT_NUM_DIALS ((c-lambda () int "___result = GLUT_NUM_DIALS;")))
(define GLUT_NUM_TABLET_BUTTONS ((c-lambda () int "___result = GLUT_NUM_TABLET_BUTTONS;")))
(define GLUT_DEVICE_IGNORE_KEY_REPEAT ((c-lambda () int "___result = GLUT_DEVICE_IGNORE_KEY_REPEAT;")))
(define GLUT_DEVICE_KEY_REPEAT ((c-lambda () int "___result = GLUT_DEVICE_KEY_REPEAT;")))
(define GLUT_HAS_JOYSTICK ((c-lambda () int "___result = GLUT_HAS_JOYSTICK;")))
(define GLUT_OWNS_JOYSTICK ((c-lambda () int "___result = GLUT_OWNS_JOYSTICK;")))
(define GLUT_JOYSTICK_BUTTONS ((c-lambda () int "___result = GLUT_JOYSTICK_BUTTONS;")))
(define GLUT_JOYSTICK_AXES ((c-lambda () int "___result = GLUT_JOYSTICK_AXES;")))
(define GLUT_JOYSTICK_POLL_RATE ((c-lambda () int "___result = GLUT_JOYSTICK_POLL_RATE;")))

;; /*
;;  * GLUT API macro definitions -- the glutLayerGet parameters
;;  */
(define GLUT_OVERLAY_POSSIBLE ((c-lambda () int "___result = GLUT_OVERLAY_POSSIBLE;")))
(define GLUT_LAYER_IN_USE ((c-lambda () int "___result = GLUT_LAYER_IN_USE;")))
(define GLUT_HAS_OVERLAY ((c-lambda () int "___result = GLUT_HAS_OVERLAY;")))
(define GLUT_TRANSPARENT_INDEX ((c-lambda () int "___result = GLUT_TRANSPARENT_INDEX;")))
(define GLUT_NORMAL_DAMAGED ((c-lambda () int "___result = GLUT_NORMAL_DAMAGED;")))
(define GLUT_OVERLAY_DAMAGED ((c-lambda () int "___result = GLUT_OVERLAY_DAMAGED;")))

;; /*
;;  * GLUT API macro definitions -- the glutVideoResizeGet parameters
;;  */
(define GLUT_VIDEO_RESIZE_POSSIBLE ((c-lambda () int "___result = GLUT_VIDEO_RESIZE_POSSIBLE;")))
(define GLUT_VIDEO_RESIZE_IN_USE ((c-lambda () int "___result = GLUT_VIDEO_RESIZE_IN_USE;")))
(define GLUT_VIDEO_RESIZE_X_DELTA ((c-lambda () int "___result = GLUT_VIDEO_RESIZE_X_DELTA;")))
(define GLUT_VIDEO_RESIZE_Y_DELTA ((c-lambda () int "___result = GLUT_VIDEO_RESIZE_Y_DELTA;")))
(define GLUT_VIDEO_RESIZE_WIDTH_DELTA ((c-lambda () int "___result = GLUT_VIDEO_RESIZE_WIDTH_DELTA;")))
(define GLUT_VIDEO_RESIZE_HEIGHT_DELTA ((c-lambda () int "___result = GLUT_VIDEO_RESIZE_HEIGHT_DELTA;")))
(define GLUT_VIDEO_RESIZE_X ((c-lambda () int "___result = GLUT_VIDEO_RESIZE_X;")))
(define GLUT_VIDEO_RESIZE_Y ((c-lambda () int "___result = GLUT_VIDEO_RESIZE_Y;")))
(define GLUT_VIDEO_RESIZE_WIDTH ((c-lambda () int "___result = GLUT_VIDEO_RESIZE_WIDTH;")))
(define GLUT_VIDEO_RESIZE_HEIGHT ((c-lambda () int "___result = GLUT_VIDEO_RESIZE_HEIGHT;")))

;; /*
;;  * GLUT API macro definitions -- the glutUseLayer parameters
;;  */
(define GLUT_NORMAL ((c-lambda () int "___result = GLUT_NORMAL;")))
(define GLUT_OVERLAY ((c-lambda () int "___result = GLUT_OVERLAY;")))

;; /*
;;  * GLUT API macro definitions -- the glutGetModifiers parameters
;;  */
(define GLUT_ACTIVE_SHIFT ((c-lambda () int "___result = GLUT_ACTIVE_SHIFT;")))
(define GLUT_ACTIVE_CTRL ((c-lambda () int "___result = GLUT_ACTIVE_CTRL;")))
(define GLUT_ACTIVE_ALT ((c-lambda () int "___result = GLUT_ACTIVE_ALT;")))

;; /*
;;  * GLUT API macro definitions -- the glutSetCursor parameters
;;  */
(define GLUT_CURSOR_RIGHT_ARROW ((c-lambda () int "___result = GLUT_CURSOR_RIGHT_ARROW;")))
(define GLUT_CURSOR_LEFT_ARROW ((c-lambda () int "___result = GLUT_CURSOR_LEFT_ARROW;")))
(define GLUT_CURSOR_INFO ((c-lambda () int "___result = GLUT_CURSOR_INFO;")))
(define GLUT_CURSOR_DESTROY ((c-lambda () int "___result = GLUT_CURSOR_DESTROY;")))
(define GLUT_CURSOR_HELP ((c-lambda () int "___result = GLUT_CURSOR_HELP;")))
(define GLUT_CURSOR_CYCLE ((c-lambda () int "___result = GLUT_CURSOR_CYCLE;")))
(define GLUT_CURSOR_SPRAY ((c-lambda () int "___result = GLUT_CURSOR_SPRAY;")))
(define GLUT_CURSOR_WAIT ((c-lambda () int "___result = GLUT_CURSOR_WAIT;")))
(define GLUT_CURSOR_TEXT ((c-lambda () int "___result = GLUT_CURSOR_TEXT;")))
(define GLUT_CURSOR_CROSSHAIR ((c-lambda () int "___result = GLUT_CURSOR_CROSSHAIR;")))
(define GLUT_CURSOR_UP_DOWN ((c-lambda () int "___result = GLUT_CURSOR_UP_DOWN;")))
(define GLUT_CURSOR_LEFT_RIGHT ((c-lambda () int "___result = GLUT_CURSOR_LEFT_RIGHT;")))
(define GLUT_CURSOR_TOP_SIDE ((c-lambda () int "___result = GLUT_CURSOR_TOP_SIDE;")))
(define GLUT_CURSOR_BOTTOM_SIDE ((c-lambda () int "___result = GLUT_CURSOR_BOTTOM_SIDE;")))
(define GLUT_CURSOR_LEFT_SIDE ((c-lambda () int "___result = GLUT_CURSOR_LEFT_SIDE;")))
(define GLUT_CURSOR_RIGHT_SIDE ((c-lambda () int "___result = GLUT_CURSOR_RIGHT_SIDE;")))
(define GLUT_CURSOR_TOP_LEFT_CORNER ((c-lambda () int "___result = GLUT_CURSOR_TOP_LEFT_CORNER;")))
(define GLUT_CURSOR_TOP_RIGHT_CORNER ((c-lambda () int "___result = GLUT_CURSOR_TOP_RIGHT_CORNER;")))
(define GLUT_CURSOR_BOTTOM_RIGHT_CORNER ((c-lambda () int "___result = GLUT_CURSOR_BOTTOM_RIGHT_CORNER;")))
(define GLUT_CURSOR_BOTTOM_LEFT_CORNER ((c-lambda () int "___result = GLUT_CURSOR_BOTTOM_LEFT_CORNER;")))
(define GLUT_CURSOR_INHERIT ((c-lambda () int "___result = GLUT_CURSOR_INHERIT;")))
(define GLUT_CURSOR_NONE ((c-lambda () int "___result = GLUT_CURSOR_NONE;")))
(define GLUT_CURSOR_FULL_CROSSHAIR ((c-lambda () int "___result = GLUT_CURSOR_FULL_CROSSHAIR;")))

;; /*
;;  * GLUT API macro definitions -- RGB color component specification definitions
;;  */
(define GLUT_RED ((c-lambda () int "___result = GLUT_RED;")))
(define GLUT_GREEN ((c-lambda () int "___result = GLUT_GREEN;")))
(define GLUT_BLUE ((c-lambda () int "___result = GLUT_BLUE;")))

;; /*
;;  * GLUT API macro definitions -- additional keyboard and joystick definitions
;;  */
(define GLUT_KEY_REPEAT_OFF ((c-lambda () int "___result = GLUT_KEY_REPEAT_OFF;")))
(define GLUT_KEY_REPEAT_ON ((c-lambda () int "___result = GLUT_KEY_REPEAT_ON;")))
(define GLUT_KEY_REPEAT_DEFAULT ((c-lambda () int "___result = GLUT_KEY_REPEAT_DEFAULT;")))

(define GLUT_JOYSTICK_BUTTON_A ((c-lambda () int "___result = GLUT_JOYSTICK_BUTTON_A;")))
(define GLUT_JOYSTICK_BUTTON_B ((c-lambda () int "___result = GLUT_JOYSTICK_BUTTON_B;")))
(define GLUT_JOYSTICK_BUTTON_C ((c-lambda () int "___result = GLUT_JOYSTICK_BUTTON_C;")))
(define GLUT_JOYSTICK_BUTTON_D ((c-lambda () int "___result = GLUT_JOYSTICK_BUTTON_D;")))

;; /*
;;  * GLUT API macro definitions -- game mode definitions
;;  */
(define GLUT_GAME_MODE_ACTIVE ((c-lambda () int "___result = GLUT_GAME_MODE_ACTIVE;")))
(define GLUT_GAME_MODE_POSSIBLE ((c-lambda () int "___result = GLUT_GAME_MODE_POSSIBLE;")))
(define GLUT_GAME_MODE_WIDTH ((c-lambda () int "___result = GLUT_GAME_MODE_WIDTH;")))
(define GLUT_GAME_MODE_HEIGHT ((c-lambda () int "___result = GLUT_GAME_MODE_HEIGHT;")))
(define GLUT_GAME_MODE_PIXEL_DEPTH ((c-lambda () int "___result = GLUT_GAME_MODE_PIXEL_DEPTH;")))
(define GLUT_GAME_MODE_REFRESH_RATE ((c-lambda () int "___result = GLUT_GAME_MODE_REFRESH_RATE;")))
(define GLUT_GAME_MODE_DISPLAY_CHANGED ((c-lambda () int "___result = GLUT_GAME_MODE_DISPLAY_CHANGED;")))

