(include "glu-header.scm")


(c-define-type int* (pointer int))


;; /*
;;  * The freeglut and GLUT API versions
;;  */
(define FREEGLUT 1)
(define VERSION 4)
;(define 0 1)
(define IMPLEMENTATION 13)


;; /*
;;  * GLUT API macro definitions -- the special key codes:
;;  */
(define GLUT_KEY_F1 #x0001)
(define GLUT_KEY_F2 #x0002)
(define GLUT_KEY_F3 #x0003)
(define GLUT_KEY_F4 #x0004)
(define GLUT_KEY_F5 #x0005)
(define GLUT_KEY_F6 #x0006)
(define GLUT_KEY_F7 #x0007)
(define GLUT_KEY_F8 #x0008)
(define GLUT_KEY_F9 #x0009)
(define GLUT_KEY_F10 #x000A)
(define GLUT_KEY_F11 #x000B)
(define GLUT_KEY_F12 #x000C)
(define GLUT_KEY_LEFT #x0064)
(define GLUT_KEY_UP #x0065)
(define GLUT_KEY_RIGHT #x0066)
(define GLUT_KEY_DOWN #x0067)
(define GLUT_KEY_PAGE_UP #x0068)
(define GLUT_KEY_PAGE_DOWN #x0069)
(define GLUT_KEY_HOME #x006A)
(define GLUT_KEY_END #x006B)
(define GLUT_KEY_INSERT #x006C)

;; /*
;;  * GLUT API macro definitions -- mouse state definitions
;;  */
(define GLUT_LEFT_BUTTON #x0000)
(define GLUT_MIDDLE_BUTTON #x0001)
(define GLUT_RIGHT_BUTTON #x0002)
(define GLUT_DOWN #x0000)
(define GLUT_UP #x0001)
(define GLUT_LEFT #x0000)
(define GLUT_ENTERED #x0001)

;; /*
;;  * GLUT API macro definitions -- the display mode definitions
;;  */
(define GLUT_RGB #x0000)
(define GLUT_RGBA #x0000)
(define GLUT_INDEX #x0001)
(define GLUT_SINGLE #x0000)
(define GLUT_DOUBLE #x0002)
(define GLUT_ACCUM #x0004)
(define GLUT_ALPHA #x0008)
(define GLUT_DEPTH #x0010)
(define GLUT_STENCIL #x0020)
(define GLUT_MULTISAMPLE #x0080)
(define GLUT_STEREO #x0100)
(define GLUT_LUMINANCE #x0200)

;; /*
;;  * GLUT API macro definitions -- windows and menu related definitions
;;  */
(define GLUT_MENU_NOT_IN_USE #x0000)
(define GLUT_MENU_IN_USE #x0001)
(define GLUT_NOT_VISIBLE #x0000)
(define GLUT_VISIBLE #x0001)
(define GLUT_HIDDEN #x0000)
(define GLUT_FULLY_RETAINED #x0001)
(define GLUT_PARTIALLY_RETAINED #x0002)
(define GLUT_FULLY_COVERED #x0003)

;; /*
;;  * GLUT API macro definitions -- the glutGet parameters
;;  */
(define GLUT_WINDOW_X #x0064)
(define GLUT_WINDOW_Y #x0065)
(define GLUT_WINDOW_WIDTH #x0066)
(define GLUT_WINDOW_HEIGHT #x0067)
(define GLUT_WINDOW_BUFFER_SIZE #x0068)
(define GLUT_WINDOW_STENCIL_SIZE #x0069)
(define GLUT_WINDOW_DEPTH_SIZE #x006A)
(define GLUT_WINDOW_RED_SIZE #x006B)
(define GLUT_WINDOW_GREEN_SIZE #x006C)
(define GLUT_WINDOW_BLUE_SIZE #x006D)
(define GLUT_WINDOW_ALPHA_SIZE #x006E)
(define GLUT_WINDOW_ACCUM_RED_SIZE #x006F)
(define GLUT_WINDOW_ACCUM_GREEN_SIZE #x0070)
(define GLUT_WINDOW_ACCUM_BLUE_SIZE #x0071)
(define GLUT_WINDOW_ACCUM_ALPHA_SIZE #x0072)
(define GLUT_WINDOW_DOUBLEBUFFER #x0073)
(define GLUT_WINDOW_RGBA #x0074)
(define GLUT_WINDOW_PARENT #x0075)
(define GLUT_WINDOW_NUM_CHILDREN #x0076)
(define GLUT_WINDOW_COLORMAP_SIZE #x0077)
(define GLUT_WINDOW_NUM_SAMPLES #x0078)
(define GLUT_WINDOW_STEREO #x0079)
(define GLUT_WINDOW_CURSOR #x007A)

(define GLUT_SCREEN_WIDTH #x00C8)
(define GLUT_SCREEN_HEIGHT #x00C9)
(define GLUT_SCREEN_WIDTH_MM #x00CA)
(define GLUT_SCREEN_HEIGHT_MM #x00CB)
(define GLUT_MENU_NUM_ITEMS #x012C)
(define GLUT_DISPLAY_MODE_POSSIBLE #x0190)
(define GLUT_INIT_WINDOW_X #x01F4)
(define GLUT_INIT_WINDOW_Y #x01F5)
(define GLUT_INIT_WINDOW_WIDTH #x01F6)
(define GLUT_INIT_WINDOW_HEIGHT #x01F7)
(define GLUT_INIT_DISPLAY_MODE #x01F8)
(define GLUT_ELAPSED_TIME #x02BC)
(define GLUT_WINDOW_FORMAT_ID #x007B)
(define GLUT_INIT_STATE #x007C)

;; /*
;;  * GLUT API macro definitions -- the glutDeviceGet parameters
;;  */
(define GLUT_HAS_KEYBOARD #x0258)
(define GLUT_HAS_MOUSE #x0259)
(define GLUT_HAS_SPACEBALL #x025A)
(define GLUT_HAS_DIAL_AND_BUTTON_BOX #x025B)
(define GLUT_HAS_TABLET #x025C)
(define GLUT_NUM_MOUSE_BUTTONS #x025D)
(define GLUT_NUM_SPACEBALL_BUTTONS #x025E)
(define GLUT_NUM_BUTTON_BOX_BUTTONS #x025F)
(define GLUT_NUM_DIALS #x0260)
(define GLUT_NUM_TABLET_BUTTONS #x0261)
(define GLUT_DEVICE_IGNORE_KEY_REPEAT #x0262)
(define GLUT_DEVICE_KEY_REPEAT #x0263)
(define GLUT_HAS_JOYSTICK #x0264)
(define GLUT_OWNS_JOYSTICK #x0265)
(define GLUT_JOYSTICK_BUTTONS #x0266)
(define GLUT_JOYSTICK_AXES #x0267)
(define GLUT_JOYSTICK_POLL_RATE #x0268)

;; /*
;;  * GLUT API macro definitions -- the glutLayerGet parameters
;;  */
(define GLUT_OVERLAY_POSSIBLE #x0320)
(define GLUT_LAYER_IN_USE #x0321)
(define GLUT_HAS_OVERLAY #x0322)
(define GLUT_TRANSPARENT_INDEX #x0323)
(define GLUT_NORMAL_DAMAGED #x0324)
(define GLUT_OVERLAY_DAMAGED #x0325)

;; /*
;;  * GLUT API macro definitions -- the glutVideoResizeGet parameters
;;  */
(define GLUT_VIDEO_RESIZE_POSSIBLE #x0384)
(define GLUT_VIDEO_RESIZE_IN_USE #x0385)
(define GLUT_VIDEO_RESIZE_X_DELTA #x0386)
(define GLUT_VIDEO_RESIZE_Y_DELTA #x0387)
(define GLUT_VIDEO_RESIZE_WIDTH_DELTA #x0388)
(define GLUT_VIDEO_RESIZE_HEIGHT_DELTA #x0389)
(define GLUT_VIDEO_RESIZE_X #x038A)
(define GLUT_VIDEO_RESIZE_Y #x038B)
(define GLUT_VIDEO_RESIZE_WIDTH #x038C)
(define GLUT_VIDEO_RESIZE_HEIGHT #x038D)

;; /*
;;  * GLUT API macro definitions -- the glutUseLayer parameters
;;  */
(define GLUT_NORMAL #x0000)
(define GLUT_OVERLAY #x0001)

;; /*
;;  * GLUT API macro definitions -- the glutGetModifiers parameters
;;  */
(define GLUT_ACTIVE_SHIFT #x0001)
(define GLUT_ACTIVE_CTRL #x0002)
(define GLUT_ACTIVE_ALT #x0004)

;; /*
;;  * GLUT API macro definitions -- the glutSetCursor parameters
;;  */
(define GLUT_CURSOR_RIGHT_ARROW #x0000)
(define GLUT_CURSOR_LEFT_ARROW #x0001)
(define GLUT_CURSOR_INFO #x0002)
(define GLUT_CURSOR_DESTROY #x0003)
(define GLUT_CURSOR_HELP #x0004)
(define GLUT_CURSOR_CYCLE #x0005)
(define GLUT_CURSOR_SPRAY #x0006)
(define GLUT_CURSOR_WAIT #x0007)
(define GLUT_CURSOR_TEXT #x0008)
(define GLUT_CURSOR_CROSSHAIR #x0009)
(define GLUT_CURSOR_UP_DOWN #x000A)
(define GLUT_CURSOR_LEFT_RIGHT #x000B)
(define GLUT_CURSOR_TOP_SIDE #x000C)
(define GLUT_CURSOR_BOTTOM_SIDE #x000D)
(define GLUT_CURSOR_LEFT_SIDE #x000E)
(define GLUT_CURSOR_RIGHT_SIDE #x000F)
(define GLUT_CURSOR_TOP_LEFT_CORNER #x0010)
(define GLUT_CURSOR_TOP_RIGHT_CORNER #x0011)
(define GLUT_CURSOR_BOTTOM_RIGHT_CORNER #x0012)
(define GLUT_CURSOR_BOTTOM_LEFT_CORNER #x0013)
(define GLUT_CURSOR_INHERIT #x0064)
(define GLUT_CURSOR_NONE #x0065)
(define GLUT_CURSOR_FULL_CROSSHAIR #x0066)

;; /*
;;  * GLUT API macro definitions -- RGB color component specification definitions
;;  */
(define GLUT_RED #x0000)
(define GLUT_GREEN #x0001)
(define GLUT_BLUE #x0002)

;; /*
;;  * GLUT API macro definitions -- additional keyboard and joystick definitions
;;  */
(define GLUT_KEY_REPEAT_OFF #x0000)
(define GLUT_KEY_REPEAT_ON #x0001)
(define GLUT_KEY_REPEAT_DEFAULT #x0002)

(define GLUT_JOYSTICK_BUTTON_A #x0001)
(define GLUT_JOYSTICK_BUTTON_B #x0002)
(define GLUT_JOYSTICK_BUTTON_C #x0004)
(define GLUT_JOYSTICK_BUTTON_D #x0008)

;; /*
;;  * GLUT API macro definitions -- game mode definitions
;;  */
(define GLUT_GAME_MODE_ACTIVE #x0000)
(define GLUT_GAME_MODE_POSSIBLE #x0001)
(define GLUT_GAME_MODE_WIDTH #x0002)
(define GLUT_GAME_MODE_HEIGHT #x0003)
(define GLUT_GAME_MODE_PIXEL_DEPTH #x0004)
(define GLUT_GAME_MODE_REFRESH_RATE #x0005)
(define GLUT_GAME_MODE_DISPLAY_CHANGED #x0006)

