
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

GLubyte easy1_bitmap[16] = {0x30, 0xC0,
                            0x66, 0x60,
                            0x39, 0xC0,
                            0xFF, 0xF0,
                            0xE6, 0x70,
                            0xFF, 0xF0,
                            0x7F, 0xE0,
                            0x0F, 0x00};
bitmap easy1 = {12, 8, 0., 0., 12., 0., easy1_bitmap};

GLubyte easy2_bitmap[16] = {0xC0, 0x30,
                            0x36, 0xC0,
                            0x19, 0x80,
                            0xFF, 0xF0,
                            0xE6, 0x70,
                            0xFF, 0xF0,
                            0x7F, 0xE0,
                            0x0F, 0x00};

bitmap easy2 = {12, 8, 0., 0., 12., 0., easy2_bitmap};


GLubyte f_raster[24] = {
   0xc0, 0x00, 0xc0, 0x00, 0xc0, 0x00, 0xc0, 0x00, 0xc0, 0x00,
   0xff, 0x00, 0xff, 0x00, 0xc0, 0x00, 0xc0, 0x00, 0xc0, 0x00,
   0xff, 0xc0, 0xff, 0xc0};

bitmap f_bit = {10, 12, 0.0, 0.0, 11.0, 0.0, f_raster};


#endif
