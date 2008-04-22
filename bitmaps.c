
#ifndef SPACE_INVADERS_BITMAPS
#define SPACE_INVADERS_BITMAPS
#include <GL/gl.h>

typedef struct{
  GLsizei width;
  GLsizei height;
  GLfloat xorig;
  GLfloat yorig;
  GLfloat xmove;
  GLfloat ymove;
  GLubyte* pointer;} bitmap;

GLubyte easy0_bitmap[16] = {0x30, 0xC0,
                            0x66, 0x60,
                            0x39, 0xC0,
                            0xFF, 0xF0,
                            0xE6, 0x70,
                            0xFF, 0xF0,
                            0x7F, 0xE0,
                            0x0F, 0x00};
bitmap easy0 = {12, 8, 0., 0., 13., 0., easy0_bitmap};

GLubyte easy1_bitmap[16] = {0xC0, 0x30,
                            0x36, 0xC0,
                            0x19, 0x80,
                            0xFF, 0xF0,
                            0xE6, 0x70,
                            0xFF, 0xF0,
                            0x7F, 0xE0,
                            0x0F, 0x00};
bitmap easy1 = {12, 8, 0., 0., 13., 0., easy1_bitmap};

GLubyte medium0_bitmap[16] = {0x1B, 0x00,
                              0xA0, 0xA0,
                              0xBF, 0xA0,
                              0xFF, 0xE0,
                              0x6E, 0xC0,
                              0x3F, 0x80,
                              0x11, 0x00,
                              0x20, 0x80};
bitmap medium0 = {11, 8, 0., 0., 12., 0., medium0_bitmap};

GLubyte medium1_bitmap[16] = {0x40, 0x40,
                              0x20, 0x80,
                              0x7F, 0xC0,
                              0xFF, 0xE0,
                              0xEE, 0xE0,
                              0xBF, 0xA0,
                              0x91, 0x20,
                              0x20, 0x80};
bitmap medium1 = {11, 8, 0., 0., 12., 0., medium1_bitmap};

GLubyte hard0_bitmap[8] = {0xA5,
                           0x5A,
                           0x24,
                           0xFF,
                           0xDB,
                           0x7E,
                           0x3C,
                           0x18};
bitmap hard0 = {8, 8, 0., 0., 9., 0., hard0_bitmap};

GLubyte hard1_bitmap[8] = {0x42,
                           0x81,
                           0x5A,
                           0xFF,
                           0xDB,
                           0x7E,
                           0x3C,
                           0x18};
bitmap hard1 = {8, 8, 0., 0., 9., 0., hard1_bitmap};


GLubyte player_bitmap[16] = {0xFF, 0xF8,
                             0xFF, 0xF8,
                             0xFF, 0xF8,
                             0xFF, 0xF8,
                             0x7F, 0xF0,
                             0x07, 0x00,
                             0x07, 0x00,
                             0x02, 0x00};
bitmap player0 = {13, 8, 0., 0., 14., 0., player_bitmap};
bitmap player1 = {13, 8, 0., 0., 14., 0., player_bitmap};



GLubyte f_raster[24] = {
   0xc0, 0x00, 0xc0, 0x00, 0xc0, 0x00, 0xc0, 0x00, 0xc0, 0x00,
   0xff, 0x00, 0xff, 0x00, 0xc0, 0x00, 0xc0, 0x00, 0xc0, 0x00,
   0xff, 0xc0, 0xff, 0xc0};

bitmap f_bit = {10, 12, 0.0, 0.0, 11.0, 0.0, f_raster};


#endif
