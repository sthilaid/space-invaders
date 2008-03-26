
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

GLubyte easy1_bitmap[14] = {0x30, 0xC0,
                            0x66, 0x60,
                            0x39, 0xC0,
                            0xFF, 0xF0,
                            0xD6, 0x70,
                            0xFF, 0xF0,
                            0x7F, 0xD0};
bitmap easy1 = {12, 8, 0., 0., 12., 8., easy1_bitmap};

GLubyte easy2_bitmap[14] = {0x7F, 0xD0,
                            0xFF, 0xF0,
                            0xD6, 0x70,
                            0xFF, 0xF0,
                            0x39, 0xC0,
                            0x66, 0x60,
                            0x30, 0xC0};

bitmap easy2 = {12, 8, 0., 0., 12., 8., easy2_bitmap};


#endif
