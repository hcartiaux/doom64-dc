#ifndef __IMGTYPES_H
#define __IMGTYPES_H

#include <stdint.h>

typedef struct {
	uint8_t __attribute__((aligned(4))) R;
	uint8_t G;
	uint8_t B;
// [4] for padding/alignment
	uint8_t pad;
} RGBTriple;

typedef struct {
	int32_t  __attribute__((aligned(4))) size;
	RGBTriple *table;
	uint32_t pad;
} RGBPalette;

typedef struct {
	int32_t  __attribute__((aligned(4))) width;
	int32_t height;
	RGBTriple *pixels;
} RGBImage;

typedef struct {
	int32_t  __attribute__((aligned(4))) width;
	int32_t height;
	uint8_t *pixels;
} PalettizedImage;

#endif // __IMGTYPES_H
