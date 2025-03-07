#define REINDEX_FLATS 1
#define GENERATE_BUMP_WAD 0

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include "imgtypes.h"

#include "lumpinfo.h"
#include "newlumpslist.h"
#if GENERATE_BUMP_WAD
#include "textures.h"
#endif
#include "newd64pals.h"
#include "nonenemy_sheet.h"
#define ORIGINAL_DOOM64_WAD_SIZE 6101168

#define TEXNUM(i) ((i) - 968)

void DecodeD64(unsigned char* input, unsigned char* output); // 8002DFA0
unsigned char* encoder__EncodeD64(unsigned char* input, int inputlen, int* size);
unsigned char *encode(unsigned char *input, int inputlen, int *size);
int decodedsize(unsigned char *input);
void decode(unsigned char *input, unsigned char *output);

uint8_t *expand_4to8(uint8_t *src, int width, int height);
void unscramble(uint8_t *img, int width, int height, int tileheight, int compressed);
void convert(char *infn, char *outfn);

RGBPalette sawgpal;
RGBPalette pungpal;
RGBPalette pisgpal;
RGBPalette sht1pal;
RGBPalette sht2pal;
RGBPalette chggpal;
RGBPalette rockpal;
RGBPalette plaspal;
RGBPalette bfggpal;
RGBPalette lasrpal;

void init_gunpals(void)
{
	sawgpal.size = 256;
	pungpal.size = 256;
	pisgpal.size = 256;
	sht1pal.size = 256;
	sht2pal.size = 256;
	chggpal.size = 256;
	rockpal.size = 256;
	plaspal.size = 256;
	bfggpal.size = 256;
	lasrpal.size = 256;
	sawgpal.table = (RGBTriple *)malloc(256*sizeof(RGBTriple));
	pungpal.table = (RGBTriple *)malloc(256*sizeof(RGBTriple));
	pisgpal.table = (RGBTriple *)malloc(256*sizeof(RGBTriple));
	sht1pal.table = (RGBTriple *)malloc(256*sizeof(RGBTriple));
	sht2pal.table = (RGBTriple *)malloc(256*sizeof(RGBTriple));
	chggpal.table = (RGBTriple *)malloc(256*sizeof(RGBTriple));
	rockpal.table = (RGBTriple *)malloc(256*sizeof(RGBTriple));
	plaspal.table = (RGBTriple *)malloc(256*sizeof(RGBTriple));
	bfggpal.table = (RGBTriple *)malloc(256*sizeof(RGBTriple));
	lasrpal.table = (RGBTriple *)malloc(256*sizeof(RGBTriple));
}

void free_gunpals(void)
{
	free(sawgpal.table);
	free(pungpal.table);
	free(pisgpal.table);
	free(sht1pal.table);
	free(sht2pal.table);
	free(chggpal.table);
	free(rockpal.table);
	free(plaspal.table);
	free(bfggpal.table);
	free(lasrpal.table);
}

// https://stackoverflow.com/a/466242
static inline uint32_t np2(uint32_t v)
{
	v--;
	v |= v >> 1;
	v |= v >> 2;
	v |= v >> 4;
	v |= v >> 8;
	v |= v >> 16;
	v++;
	return v;
}

// https://github.com/KallistiOS/KallistiOS/blob/master/kernel/arch/dreamcast/hardware/pvr/pvr_texture.c
// adapted from pvr_txr_load_ex
/* Linear/iterative twiddling algorithm from Marcus' tatest */
#define TWIDTAB(x) ( (x&1)|((x&2)<<1)|((x&4)<<2)|((x&8)<<3)|((x&16)<<4)| \
                     ((x&32)<<5)|((x&64)<<6)|((x&128)<<7)|((x&256)<<8)|((x&512)<<9) )
#define TWIDOUT(x, y) ( TWIDTAB((y)) | (TWIDTAB((x)) << 1) )

#define MIN(a, b) ( (a)<(b)? (a):(b) )

void load_twid(void *dst, void *src, uint32_t w, uint32_t h)
{
	uint32_t x, y, yout, min, mask;

	min = MIN(w, h);
	mask = min - 1;

	uint8_t * pixels;
	uint16_t * vtex;
	pixels = (uint8_t *) src;
	vtex = (uint16_t*)dst;

	for (y = 0; y < h; y += 2) {
		yout = y;

		for (x = 0; x < w; x++) {
			vtex[TWIDOUT((yout & mask) / 2, x & mask) +
				(x / min + yout / min)*min * min / 2] =
				pixels[y * w + x] | (pixels[(y + 1) * w + x] << 8);
		}
	}
}

typedef struct
{
	int filepos;
	int size;
	char name[8];
} lumpinfo_t;

typedef struct
{
	char identification[4];
	int numlumps;
	int infotableofs;
} wadinfo_t;

wadinfo_t wadfileptr;
int infotableofs;
int numlumps;
lumpinfo_t *lumpinfo;

typedef struct {
	short compressed;
	short numpal;
	short width;
	short height;
	uint8_t data[0];
} gfxN64_t;

typedef struct {
	short id;
	short numpal;
	short wshift;
	short hshift;
	uint8_t data[0];
} textureN64_t;

typedef struct
{
	unsigned short  tiles;      // 0
	short           compressed; // 2
	unsigned short  cmpsize;    // 4
	short           xoffs;      // 6
	short           yoffs;      // 8
	unsigned short  width;      // 10
	unsigned short  height;     // 12
	unsigned short  tileheight; // 14
	uint8_t		data[0];	// 16 - all of the sprite data itself
} spriteN64_t;

typedef struct
{
	unsigned short  width;      // 0
	unsigned short  height;     // 2
	short           xoffs;      // 4
	short           yoffs;      // 6
	uint8_t		data[0];	// 8 - all of the sprite data itself
} spriteDC_t;


char identifier[4] = {'P','W','A','D'};
uint8_t *doom64wad;

#define LUMPDATASZ (256*1024)
uint8_t lumpdata[LUMPDATASZ];

short SwapShort(short dat)
{
	return ((((dat << 8) | (dat >> 8 & 0xff)) << 16) >> 16);
}

RGBPalette *fromDoom64Palette(short *data, int count);

RGBImage *fromDoom64Sprite(uint8_t *data, int w, int h, RGBPalette *pal);
RGBImage *fromDoom64Texture(uint8_t *data, int w, int h, RGBPalette *pal);

PalettizedImage *Palettize(RGBImage *image, RGBPalette *palette);

void Resize(PalettizedImage *PalImg, int nw, int nh);

short texIds[503] = {0};
short texWs[503] = {0};
short texHs[503] = {0};

PalettizedImage *allImages[966];
spriteDC_t *allSprites[966 + 355];

// this is used to see if `fromDoom64Sprite` is processing a card key or a dart
int last_lump = -1;

#if REINDEX_FLATS

textureN64_t *allN64Textures[503];

void process_flats(void) {
	memset(allN64Textures, 0, sizeof(textureN64_t *) * 503);

	RGBPalette *texPal = (RGBPalette *)malloc(sizeof(RGBPalette));
	texPal->size = 256;
	texPal->table = (RGBTriple *)malloc(256 * sizeof(RGBTriple));
	for (int i=0;i<256;i++) {
		texPal->table[i].R = PALTEXCONV[i][0];
		texPal->table[i].G = PALTEXCONV[i][1];
		texPal->table[i].B = PALTEXCONV[i][2];
	}

	for (int i=970;i<1460;i++) {
		int compressed = 0;
		int copylen = 8;
		char name[9];

		memset(name, 0, 9);

		if (strlen(lumpinfo[i].name) < 8) {
			copylen = strlen(lumpinfo[i].name);
		}

		memcpy(name, lumpinfo[i].name, copylen);

		if (name[0] & 0x80) {
			compressed = 1;
		}

		name[0] &= 0x7f;

		uint8_t *tmpdata = malloc(lumpinfo[i+1].filepos - lumpinfo[i].filepos);

		if (NULL == tmpdata) {
			fprintf(stderr, "Could not allocate tmpdata for lump %d.\n", i);
			exit(-1);
		}

		memcpy(tmpdata, doom64wad + lumpinfo[i].filepos, lumpinfo[i+1].filepos - lumpinfo[i].filepos);
		memset(lumpdata,0,LUMPDATASZ);

		if (compressed) {
			DecodeD64(tmpdata, lumpdata);
		} else {
			memcpy(lumpdata, tmpdata, lumpinfo[i].size);
		}

		free(tmpdata);

		textureN64_t *txr = (textureN64_t *)lumpdata;
		short id = SwapShort(txr->id);
		texIds[TEXNUM(i)] = id;
		short numpal = SwapShort(txr->numpal);
		short wshift = SwapShort(txr->wshift);
		texWs[TEXNUM(i)] = wshift;
		short hshift = SwapShort(txr->hshift);
		texHs[TEXNUM(i)] = hshift;
		unsigned width = (1 << wshift);
		unsigned height = (1 << hshift);
		// size -- 4bpp
		unsigned size = (width * height) >> 1;
		// pixels start here
		void *src = (void *)txr + sizeof(textureN64_t);

/* Texture C307B has 5 palettes
Texture SMONF has 5 palettes
Texture SPACEAZ has 5 palettes
Texture STRAKB has 5 palettes
Texture STRAKR has 5 palettes
Texture STRAKY has 5 palettes
Texture CASFL98 has 5 palettes
Texture CTEL has 8 palettes
Texture SPORTA has 9 palettes */

		if (numpal > 1) {
			continue;
		}

		if (name[0] == '?' || ((name[0] == 'B') && (name[2] == 'A'))) {
			// these are the stupid blank ones
			continue;
		}

		unsigned k;
		// all that remains are single-palette textures
		// Flip nibbles per byte
		uint8_t *src8 = (uint8_t *)src;
		unsigned mask = width >> 3;
		for (k = 0; k < size; k++) {
			uint8_t tmp = src8[k];
			src8[k] = (tmp >> 4);
			src8[k] |= ((tmp & 0xf) << 4);
		}
		size >>= 2;
		// Flip each sets of dwords based on texture width
		int *src32 = (int *)(src);
		for (k = 0; k < size; k += 2) {
			int x1;
			int x2;
			if (k & mask) {
				x1 = *(int *)(src32 + k);
				x2 = *(int *)(src32 + k + 1);
				*(int *)(src32 + k) = x2;
				*(int *)(src32 + k + 1) = x1;
			}
		}

		short *p = (short *)(src + (uintptr_t)((width * height) >> 1));

		RGBPalette *curPal;
		RGBImage *curImg;
		curPal = fromDoom64Palette(p, 16);
		curImg = fromDoom64Texture(src, width, height, curPal);
		PalettizedImage *palImg = Palettize(curImg, texPal);

		allN64Textures[TEXNUM(i)] = (textureN64_t *)malloc(sizeof(textureN64_t) + (palImg->width*palImg->height));
		allN64Textures[TEXNUM(i)]->id = SwapShort(texIds[TEXNUM(i)]);
		allN64Textures[TEXNUM(i)]->numpal = SwapShort(0x0001);
		allN64Textures[TEXNUM(i)]->wshift = SwapShort(texWs[TEXNUM(i)]);
		allN64Textures[TEXNUM(i)]->hshift = SwapShort(texHs[TEXNUM(i)]);
		load_twid(allN64Textures[TEXNUM(i)]->data, palImg->pixels, palImg->width, palImg->height);

		free(palImg->pixels);
		free(palImg);
		free(curImg->pixels);
		free(curImg);
		free(curPal->table);
		free(curPal);
	}
	free(texPal->table);
	free(texPal);
}
#endif

int main (int argc, char **argv) {
	char output_paths[1024];
	char *path_to_rom = argv[1];
	char *output_directory = argv[2];

	doom64wad = (uint8_t *)malloc(ORIGINAL_DOOM64_WAD_SIZE);
	if (NULL == doom64wad) {
		fprintf(stderr, "Could not allocate 6101168 bytes for original Doom 64 WAD.\n");
		exit(-1);
	}

	FILE *z64_fd = fopen(argv[1], "rb"); // doom64.z64
	if (NULL == z64_fd) {
		fprintf(stderr, "Could not open Doom 64 ROM for reading.\n");
		exit(-1);
	}

	init_gunpals();
	memset(allImages, 0, sizeof(PalettizedImage *) * 966);

	RGBPalette *nonEnemyPal = (RGBPalette *)malloc(sizeof(RGBPalette));
	if (NULL == nonEnemyPal) {
		fprintf(stderr, "Could not allocate common non-enemy palette.\n");
		exit(-1);
	}
	nonEnemyPal->size = 256;
	nonEnemyPal->table = (RGBTriple *)malloc(256 * sizeof(RGBTriple));
	if (NULL == nonEnemyPal->table) {
		fprintf(stderr, "Could not allocate color table for common non-enemy palette.\n");
		exit(-1);
	}
	for (int ci=0;ci<256;ci++) {
		nonEnemyPal->table[ci].R = D64NONENEMY[ci][0];
		nonEnemyPal->table[ci].G = D64NONENEMY[ci][1];
		nonEnemyPal->table[ci].B = D64NONENEMY[ci][2];
	}

	RGBPalette *enemyPal = (RGBPalette *)malloc(sizeof(RGBPalette));
	if (NULL == enemyPal) {
		fprintf(stderr, "Could not allocate common enemy palette.\n");
		exit(-1);
	}
	enemyPal->size = 256;
	enemyPal->table = (RGBTriple *)malloc(256 * sizeof(RGBTriple));
	if (NULL == enemyPal->table) {
		fprintf(stderr, "Could not allocate color table for common enemy palette.\n");
		exit(-1);
	}
	for (int ci=0;ci<256;ci++) {
		enemyPal->table[ci].R = D64MONSTER[ci][0];
		enemyPal->table[ci].G = D64MONSTER[ci][1];
		enemyPal->table[ci].B = D64MONSTER[ci][2];
	}

	int z64_seek_rv = fseek(z64_fd, 408848, SEEK_SET);
	if (-1 == z64_seek_rv) {
		fprintf(stderr, "Could not seek to IWAD in Doom 64 ROM: %s\n", strerror(errno));
		exit(-1);
	}
	size_t z64_total_read = 0;
	size_t z64_wad_rv = fread(doom64wad, 1, ORIGINAL_DOOM64_WAD_SIZE, z64_fd);
	if (-1 == z64_wad_rv) {
		fprintf(stderr, "Could not read IWAD from Doom 64 WAD: %s\n", strerror(errno));
		exit(-1);
	}
	z64_total_read += z64_wad_rv;
	while (z64_total_read < ORIGINAL_DOOM64_WAD_SIZE) {
		z64_wad_rv = fread(doom64wad + z64_total_read, 1, ORIGINAL_DOOM64_WAD_SIZE - z64_total_read, z64_fd);
		if (-1 == z64_wad_rv) {
			fprintf(stderr, "Could not read IWAD from Doom 64 WAD: %s\n", strerror(errno));
			exit(-1);
		}
		z64_total_read += z64_wad_rv;
	}
	int z64_close = fclose(z64_fd);
	if (0 != z64_close) {
		fprintf(stderr, "Error closing Doom 64 ROM: %s\n", strerror(errno));
		exit(-1);
	}

	memcpy(&wadfileptr, doom64wad, sizeof(wadinfo_t));

	if (strncasecmp(wadfileptr.identification, "IWAD", 4)) {
		fprintf(stderr, "invalid iwad id %c %c %c %c\n",
			wadfileptr.identification[0],
			wadfileptr.identification[1],
			wadfileptr.identification[2],
			wadfileptr.identification[3]
		);
		exit(-1);
	}

	numlumps = wadfileptr.numlumps;
	lumpinfo = (lumpinfo_t *)malloc(numlumps * sizeof(lumpinfo_t));
	if (NULL == lumpinfo) {
		fprintf(stderr, "Could not allocate lumpinfo.\n");
		exit(-1);
	}
	infotableofs = wadfileptr.infotableofs;

	memcpy((void*)lumpinfo, doom64wad + infotableofs, numlumps * sizeof(lumpinfo_t));

#if REINDEX_FLATS
		process_flats();
#endif

	// 1 - 346 are all 16 color sprites (6 of them are uncompressed lumps)
	for (int i=1;i<347;i++) {
		char name[9];
		memset(name, 0, 9);
		memcpy(name, lumpinfo[i].name, 8);
		name[0] &= 0x7f;

		last_lump = i;

		memset(lumpdata, 0, LUMPDATASZ);

		if (!(lumpinfo[i].name[0] & 0x80)) {
			memcpy(lumpdata, doom64wad + lumpinfo[i].filepos, lumpinfo[i].size);
		} else {
			uint8_t *tmpdata = malloc(lumpinfo[i+1].filepos - lumpinfo[i].filepos);
			if (NULL == tmpdata) {
				fprintf(stderr, "Could not allocate tmpdata for lump %d.\n", i);
				exit(-1);
			}
			memcpy(tmpdata, doom64wad + lumpinfo[i].filepos, lumpinfo[i+1].filepos - lumpinfo[i].filepos);
			decode(tmpdata, lumpdata);
			free(tmpdata);
		}

		spriteN64_t *sprite = (spriteN64_t *)lumpdata;

		short compressed = SwapShort(sprite->compressed);
		unsigned short cmpsize = (unsigned short)SwapShort(sprite->cmpsize);
		unsigned short width = (unsigned short)SwapShort(sprite->width);
		unsigned short height = (unsigned short)SwapShort(sprite->height);
		short xoffs = (short)SwapShort(sprite->xoffs);
		short yoffs = (short)SwapShort(sprite->yoffs);
		unsigned short tileheight = (unsigned short)SwapShort(sprite->tileheight);
		uint8_t *src = (uint8_t *)((uintptr_t)lumpdata + sizeof(spriteN64_t));

		void *paldata;
		RGBPalette *curPal;
		RGBImage *curImg;

		width = (width + 15) & ~15;
		uint8_t *expandedimg = expand_4to8(src, width, height);
		unscramble(expandedimg, width, height, tileheight, compressed);
		paldata = (void*)sprite + sizeof(spriteN64_t) + cmpsize;
		curPal = fromDoom64Palette((short *)paldata, 16);
		curImg = fromDoom64Sprite(expandedimg, width, height, curPal);
		free(expandedimg);

		allSprites[i] = (spriteDC_t *)malloc(sizeof(spriteDC_t));

		if (NULL == allSprites[i]) {
			fprintf(stderr, "Could not allocate sprite for lump %d.\n", i);
			exit(-1);
		}

		allSprites[i]->xoffs = xoffs;
		allSprites[i]->yoffs = yoffs;
		allSprites[i]->width = width;
		allSprites[i]->height = height;
		PalettizedImage *palImg;
		palImg = Palettize(curImg, nonEnemyPal);
		allImages[i] = palImg;
		free(curImg->pixels);
		free(curImg);
		free(curPal->table);
		free(curPal);
	}

	// 347 - 923 are 256 color monster sprites except for entries starting with PAL
	// 925 - 965 are 256 color psprite weapon sprites
	// all of these sprites are compressed
	for (int i=347;i<966;i++) {
		char name[9];
		memset(name, 0, 9);
		memcpy(name, lumpinfo[i].name, 8);
		name[0] &= 0x7f;

		// EXTERNAL MONSTER PALETTES, skip these
		if (!memcmp("PAL", name, 3))
			continue;

		last_lump = i;

		// these use external palettes
		// SARG two pals 347 348		2
		// PLAY three pals 395 396 397		2 + 3
		// TROO two pals 448 449		5 + 2
		// BOSS two pals 518 519		7 + 2
		// FATT one pal 566			9 + 1
		// SKUL one pal 618			10 + 1
		// PAIN one pal 659			11 + 1
		// BSPI one pal 688			12 + 1
		// POSS two pals 725 726		13 + 2
		// HEAD one pal 776			15 + 1
		// CYBR one pal 818			16 + 1
		// RECT one pal 876			17 + 1 == 18

		uint8_t *tmpdata = malloc(lumpinfo[i+1].filepos - lumpinfo[i].filepos);
		if (NULL == tmpdata) {
			fprintf(stderr, "Could not allocate tmpdata for lump %d.\n", i);
			exit(-1);
		}
		memcpy(tmpdata, doom64wad + lumpinfo[i].filepos, lumpinfo[i+1].filepos - lumpinfo[i].filepos);
		memset(lumpdata,0,LUMPDATASZ);
		decode(tmpdata, lumpdata);

		spriteN64_t *sprite = (spriteN64_t *)lumpdata;
		unsigned short tiles = (unsigned short)SwapShort(sprite->tiles) << 1;
		short compressed = SwapShort(sprite->compressed);
		unsigned short cmpsize = (unsigned short)SwapShort(sprite->cmpsize);
		short xoffs = SwapShort(sprite->xoffs);
		short yoffs = SwapShort(sprite->yoffs);
		unsigned short width = (unsigned short)SwapShort(sprite->width);
		unsigned short height = (unsigned short)SwapShort(sprite->height);
		unsigned short tileheight = (unsigned short)SwapShort(sprite->tileheight);
		uint8_t *src = (uint8_t *)((uintptr_t)lumpdata + sizeof(spriteN64_t));

		void *paldata;
		int usePal1 = 0;
		int usePal2 = 0;

		width = (width + 7) & ~7;
		unscramble(src, width, height, tileheight, compressed);

		// monster sprites
		if ((cmpsize & 1) && (i > 348) && (i < 924)) {
			char first4[8];
			memset(first4, 0, 8);
			memcpy(first4, lumpinfo[i].name, 4);
			first4[0] &= 0x7f;

			RGBPalette *curPal = (RGBPalette *)malloc(sizeof(RGBPalette));
			if (NULL == curPal) {
				fprintf(stderr, "Could not allocate curPal for lump %d.\n", i);
				exit(-1);
			}
			curPal->size = 256;
			curPal->table = (RGBTriple *)malloc(256 * sizeof(RGBTriple));
			if (NULL == curPal->table) {
				fprintf(stderr, "Could not allocate curPal color table for lump %d.\n", i);
				exit(-1);
			}

			RGBPalette *altPal = (RGBPalette *)malloc(sizeof(RGBPalette));
			if (NULL == altPal) {
				fprintf(stderr, "Could not allocate altPal for lump %d.\n", i);
				exit(-1);
			}
			altPal->size = 256;
			altPal->table = (RGBTriple *)malloc(256 * sizeof(RGBTriple));
			if (NULL == altPal->table) {
				fprintf(stderr, "Could not allocate altPal color table for lump %d.\n", i);
				exit(-1);
			}

			RGBPalette *altPal2 = (RGBPalette *)malloc(sizeof(RGBPalette));
			if (NULL == altPal2) {
				fprintf(stderr, "Could not allocate altPal2 for lump %d.\n", i);
				exit(-1);
			}
			altPal2->size = 256;
			altPal2->table = (RGBTriple *)malloc(256 * sizeof(RGBTriple));
			if (NULL == altPal2->table) {
				fprintf(stderr, "Could not allocate altPal2 color table for lump %d.\n", i);
				exit(-1);
			}

			for (int ci=0;ci<256;ci++) {
				if (!memcmp(first4, "SARG", 4)) {
					usePal1 = 1;
					curPal->table[ci].R = PALSARG0[ci][0];
					curPal->table[ci].G = PALSARG0[ci][1];
					curPal->table[ci].B = PALSARG0[ci][2];
					altPal->table[ci].R = PALSARG1[ci][0];
					altPal->table[ci].G = PALSARG1[ci][1];
					altPal->table[ci].B = PALSARG1[ci][2];
				} else if (!memcmp(first4, "PLAY", 4)) {
					usePal1 = 1;
					usePal2 = 1;
					curPal->table[ci].R = PALPLAY0[ci][0];
					curPal->table[ci].G = PALPLAY0[ci][1];
					curPal->table[ci].B = PALPLAY0[ci][2];
					altPal->table[ci].R = PALPLAY1[ci][0];
					altPal->table[ci].G = PALPLAY1[ci][1];
					altPal->table[ci].B = PALPLAY1[ci][2];
					altPal2->table[ci].R = PALPLAY2[ci][0];
					altPal2->table[ci].G = PALPLAY2[ci][1];
					altPal2->table[ci].B = PALPLAY2[ci][2];
				} else if (!memcmp(first4, "TROO", 4)) {
					usePal1 = 1;
					curPal->table[ci].R = PALTROO0[ci][0];
					curPal->table[ci].G = PALTROO0[ci][1];
					curPal->table[ci].B = PALTROO0[ci][2];
					altPal->table[ci].R = PALTROO1[ci][0];
					altPal->table[ci].G = PALTROO1[ci][1];
					altPal->table[ci].B = PALTROO1[ci][2];
				} else if (!memcmp(first4, "BOSS", 4)) {
					usePal1 = 1;
					curPal->table[ci].R = PALBOSS0[ci][0];
					curPal->table[ci].G = PALBOSS0[ci][1];
					curPal->table[ci].B = PALBOSS0[ci][2];
					altPal->table[ci].R = PALBOSS1[ci][0];
					altPal->table[ci].G = PALBOSS1[ci][1];
					altPal->table[ci].B = PALBOSS1[ci][2];
				} else if (!memcmp(first4, "FATT", 4)) {
					curPal->table[ci].R = PALFATT0[ci][0];
					curPal->table[ci].G = PALFATT0[ci][1];
					curPal->table[ci].B = PALFATT0[ci][2];
				} else if (!memcmp(first4, "SKUL", 4)) {
					curPal->table[ci].R = PALSKUL0[ci][0];
					curPal->table[ci].G = PALSKUL0[ci][1];
					curPal->table[ci].B = PALSKUL0[ci][2];
				} else if (!memcmp(first4, "PAIN", 4)) {
					curPal->table[ci].R = PALPAIN0[ci][0];
					curPal->table[ci].G = PALPAIN0[ci][1];
					curPal->table[ci].B = PALPAIN0[ci][2];
				} else if (!memcmp(first4, "BSPI", 4)) {
					curPal->table[ci].R = PALBSPI0[ci][0];
					curPal->table[ci].G = PALBSPI0[ci][1];
					curPal->table[ci].B = PALBSPI0[ci][2];
				} else if (!memcmp(first4, "POSS", 4)) {
					usePal1 = 1;
					curPal->table[ci].R = PALPOSS0[ci][0];
					curPal->table[ci].G = PALPOSS0[ci][1];
					curPal->table[ci].B = PALPOSS0[ci][2];
					altPal->table[ci].R = PALPOSS1[ci][0];
					altPal->table[ci].G = PALPOSS1[ci][1];
					altPal->table[ci].B = PALPOSS1[ci][2];
				} else if (!memcmp(first4, "HEAD", 4)) {
					curPal->table[ci].R = PALHEAD0[ci][0];
					curPal->table[ci].G = PALHEAD0[ci][1];
					curPal->table[ci].B = PALHEAD0[ci][2];
				} else if (!memcmp(first4, "CYBR", 4)) {
					curPal->table[ci].R = PALCYBR0[ci][0];
					curPal->table[ci].G = PALCYBR0[ci][1];
					curPal->table[ci].B = PALCYBR0[ci][2];
				} else if (!memcmp(first4, "RECT", 4)) {
					curPal->table[ci].R = PALRECT0[ci][0];
					curPal->table[ci].G = PALRECT0[ci][1];
					curPal->table[ci].B = PALRECT0[ci][2];
				}
			}

			int wp2 = np2(width);
			if(wp2 < 8) wp2 = 8;
			int hp2 = np2(height);
			if(hp2 < 8) hp2 = 8;

			RGBImage *curImg = fromDoom64Sprite(src, width, height, curPal);
			PalettizedImage *palImg = Palettize(curImg, enemyPal);
			Resize(palImg, wp2, hp2);

			RGBImage *altImg;
			PalettizedImage *altPalImg;
			if (usePal1) {
				altImg = fromDoom64Sprite(src, width, height, altPal);
				altPalImg = Palettize(altImg, enemyPal);
				Resize(altPalImg, wp2, hp2);
			}

			RGBImage *altImg2;
			PalettizedImage *altPalImg2;
			if (usePal2) {
				altImg2 = fromDoom64Sprite(src, width, height, altPal2);
				altPalImg2 = Palettize(altImg2, enemyPal);
				Resize(altPalImg2, wp2, hp2);
			}

			allSprites[i] = (spriteDC_t *)malloc(sizeof(spriteDC_t) + (wp2*hp2));
			if (NULL == allSprites[i]) {
				fprintf(stderr, "Could not allocate sprite for lump %d.\n", i);
				exit(-1);
			}

			allSprites[i]->xoffs = xoffs;
			allSprites[i]->yoffs = yoffs;
			allSprites[i]->width = wp2;
			allSprites[i]->height = hp2;
			load_twid(allSprites[i]->data, palImg->pixels, wp2, hp2);
			free(palImg->pixels);
			free(palImg);

			char fullname[9];
			memset(fullname, 0, 9);
			memcpy(fullname, lumpinfo[i].name, 8);
			fullname[0] &= 0x7f;
			int altlumpnum = -1;
			int altlumpnum2 = -1;

			if (!memcmp(fullname, "SARG", 4)) {
				fullname[1] = 'P';
				fullname[2] = 'E';
				fullname[3] = 'C';
				altlumpnum = W_GetNumForName(fullname);
			} else if (!memcmp(fullname, "TROO", 4)) {
				fullname[0] = 'N';
				fullname[1] = 'I';
				fullname[2] = 'T';
				fullname[3] = 'E';
				altlumpnum = W_GetNumForName(fullname);
			} else if (!memcmp(fullname, "POSS", 4)) {
				fullname[0] = 'Z';
				fullname[1] = 'O';
				fullname[2] = 'M';
				fullname[3] = 'B';
				altlumpnum = W_GetNumForName(fullname);
			} else if (!memcmp(fullname, "BOSS", 4)) {
				fullname[1] = 'A';
				fullname[2] = 'R';
				fullname[3] = 'O';
				altlumpnum = W_GetNumForName(fullname);
			}  else if (!memcmp(fullname, "PLAY", 4)) {
				fullname[2] = 'Y';
				fullname[3] = '1';
				altlumpnum = W_GetNumForName(fullname);
				fullname[2] = 'Y';
				fullname[3] = '2';
				altlumpnum2 = W_GetNumForName(fullname);
			}

			if (altlumpnum != -1) {
				allSprites[altlumpnum] = (spriteDC_t *)malloc(sizeof(spriteDC_t) + (wp2*hp2));
				if (NULL == allSprites[altlumpnum]) {
					fprintf(stderr, "Could not allocate sprite for alt lump %d.\n", altlumpnum);
					exit(-1);
				}
				allSprites[altlumpnum]->xoffs = (xoffs);
				allSprites[altlumpnum]->yoffs = (yoffs);
				allSprites[altlumpnum]->width = (wp2);
				allSprites[altlumpnum]->height = (hp2);
				load_twid(allSprites[altlumpnum]->data, altPalImg->pixels, wp2, hp2);
				free(altPalImg->pixels);
				free(altPalImg);
				free(altImg->pixels);
				free(altImg);
			}

			if (altlumpnum2 != -1) {
				allSprites[altlumpnum2] = (spriteDC_t *)malloc(sizeof(spriteDC_t) + (wp2*hp2));
				if (NULL == allSprites[altlumpnum2]) {
					fprintf(stderr, "Could not allocate sprite for alt2 lump %d.\n", altlumpnum2);
					exit(-1);
				}
				allSprites[altlumpnum2]->xoffs = (xoffs);
				allSprites[altlumpnum2]->yoffs = (yoffs);
				allSprites[altlumpnum2]->width = (wp2);
				allSprites[altlumpnum2]->height = (hp2);
				load_twid(allSprites[altlumpnum2]->data, altPalImg2->pixels, wp2, hp2);
				free(altPalImg2->pixels);
				free(altPalImg2);
				free(altImg2->pixels);
				free(altImg2);
			}

			free(curImg->pixels);
			free(curImg);
			free(curPal->table);
			free(curPal);
			free(altPal->table);
			free(altPal);
			free(altPal2->table);
			free(altPal2);
		} else {
			// the weapons are in this block
			RGBPalette *curPal;
			RGBPalette *tmpPal;
			if (!(memcmp(name,"SAWGA0",6))) {
				paldata = (void*)sprite + sizeof(spriteN64_t) + (cmpsize);
				tmpPal = fromDoom64Palette((short *)paldata, 256);
				for (int n=0;n<256;n++) {
					sawgpal.table[n].R = tmpPal->table[n].R;
					sawgpal.table[n].G = tmpPal->table[n].G;
					sawgpal.table[n].B = tmpPal->table[n].B;
				}
				free(tmpPal->table);
				free(tmpPal);
			} else if (!(memcmp(name, "PUNGA0", 6))) {
				paldata = (void*)sprite + sizeof(spriteN64_t) + (cmpsize);
				tmpPal = fromDoom64Palette((short *)paldata, 256);
				for (int n=0;n<256;n++) {
					pungpal.table[n].R = tmpPal->table[n].R;
					pungpal.table[n].G = tmpPal->table[n].G;
					pungpal.table[n].B = tmpPal->table[n].B;
				}
				free(tmpPal->table);
				free(tmpPal);
			} else if (!(memcmp(name, "PISGA0", 6))) {
				paldata = (void*)sprite + sizeof(spriteN64_t) + (cmpsize);
				tmpPal = fromDoom64Palette((short *)paldata, 256);
				for (int n=0;n<256;n++) {
					pisgpal.table[n].R = tmpPal->table[n].R;
					pisgpal.table[n].G = tmpPal->table[n].G;
					pisgpal.table[n].B = tmpPal->table[n].B;
				}
				free(tmpPal->table);
				free(tmpPal);
			} else if (!(memcmp(name, "SHT1A0", 6))) {
				paldata = (void*)sprite + sizeof(spriteN64_t) + (cmpsize);
				tmpPal = fromDoom64Palette((short *)paldata, 256);
				for (int n=0;n<256;n++) {
					sht1pal.table[n].R = tmpPal->table[n].R;
					sht1pal.table[n].G = tmpPal->table[n].G;
					sht1pal.table[n].B = tmpPal->table[n].B;
				}
				free(tmpPal->table);
				free(tmpPal);
			} else if (!(memcmp(name, "SHT2A0", 6))) {
				paldata = (void*)sprite + sizeof(spriteN64_t) + (cmpsize);
				tmpPal = fromDoom64Palette((short *)paldata, 256);
				for (int n=0;n<256;n++) {
					sht2pal.table[n].R = tmpPal->table[n].R;
					sht2pal.table[n].G = tmpPal->table[n].G;
					sht2pal.table[n].B = tmpPal->table[n].B;
				}
				free(tmpPal->table);
				free(tmpPal);
			} else if (!(memcmp(name, "CHGGA0", 6))) {
				paldata = (void*)sprite + sizeof(spriteN64_t) + (cmpsize);
				tmpPal = fromDoom64Palette((short *)paldata, 256);
				for (int n=0;n<256;n++) {
					chggpal.table[n].R = tmpPal->table[n].R;
					chggpal.table[n].G = tmpPal->table[n].G;
					chggpal.table[n].B = tmpPal->table[n].B;
				}
				free(tmpPal->table);
				free(tmpPal);
			} else if (!(memcmp(name, "ROCKA0", 6))) {
				paldata = (void*)sprite + sizeof(spriteN64_t) + (cmpsize);
				tmpPal = fromDoom64Palette((short *)paldata, 256);
				for (int n=0;n<256;n++) {
					rockpal.table[n].R = tmpPal->table[n].R;
					rockpal.table[n].G = tmpPal->table[n].G;
					rockpal.table[n].B = tmpPal->table[n].B;
				}
				free(tmpPal->table);
				free(tmpPal);
			} else if (!(memcmp(name, "PLASA0", 6))) {
				paldata = (void*)sprite + sizeof(spriteN64_t) + (cmpsize);
				tmpPal = fromDoom64Palette((short *)paldata, 256);
				for (int n=0;n<256;n++) {
					plaspal.table[n].R = tmpPal->table[n].R;
					plaspal.table[n].G = tmpPal->table[n].G;
					plaspal.table[n].B = tmpPal->table[n].B;
				}
				free(tmpPal->table);
				free(tmpPal);
			} else if (!(memcmp(name, "BFGGA0", 6))) {
				paldata = (void*)sprite + sizeof(spriteN64_t) + (cmpsize);
				tmpPal = fromDoom64Palette((short *)paldata, 256);
				for (int n=0;n<256;n++) {
					bfggpal.table[n].R = tmpPal->table[n].R;
					bfggpal.table[n].G = tmpPal->table[n].G;
					bfggpal.table[n].B = tmpPal->table[n].B;
				}
				free(tmpPal->table);
				free(tmpPal);
			} else if (!(memcmp(name, "LASRA0", 6))) {
				paldata = (void*)sprite + sizeof(spriteN64_t) + (cmpsize);
				tmpPal = fromDoom64Palette((short *)paldata, 256);
				for (int n=0;n<256;n++) {
					lasrpal.table[n].R = tmpPal->table[n].R;
					lasrpal.table[n].G = tmpPal->table[n].G;
					lasrpal.table[n].B = tmpPal->table[n].B;
				}
				free(tmpPal->table);
				free(tmpPal);
			}

			if (!(memcmp(name,"SAWG",4))) {
				curPal = &sawgpal;
			} else if (!(memcmp(name, "PUNG", 4))) {
				curPal = &pungpal;
			} else if (!(memcmp(name, "PISG", 4))) {
				curPal = &pisgpal;
			} else if (!(memcmp(name, "SHT1", 4))) {
				curPal = &sht1pal;
			} else if (!(memcmp(name, "SHT2", 4))) {
				curPal = &sht2pal;
			} else if (!(memcmp(name, "CHGG", 4))) {
				curPal = &chggpal;
			} else if (!(memcmp(name, "ROCK", 4))) {
				curPal = &rockpal;
			} else if (!(memcmp(name, "PLAS", 4))) {
				curPal = &plaspal;
			} else if (!(memcmp(name, "BFGG", 4))) {
				curPal = &bfggpal;
			} else if (!(memcmp(name, "LASR", 4))) {
				curPal = &lasrpal;
			}

			allSprites[i] = (spriteDC_t *)malloc(sizeof(spriteDC_t));
			if (NULL == allSprites[i]) {
				fprintf(stderr, "Could not allocate sprite for lump %d.\n", i);
				exit(-1);
			}

			allSprites[i]->xoffs = xoffs;
			allSprites[i]->yoffs = yoffs;
			allSprites[i]->width = width;
			allSprites[i]->height = height;

			RGBImage *curImg = fromDoom64Sprite(src, width, height, curPal);
			PalettizedImage *palImg = Palettize(curImg, nonEnemyPal);
			allImages[i] = palImg;
			free(curImg->pixels);
			free(curImg);
		}
		free(tmpdata);
	}

	// anything that is a sprite and not a monster gets put in a single 1024^2 texture
	// it is laid out according to a pre-generated sprite sheet
	// the layout data is in `nonenemy_sheet.h`
	uint8_t *ne_sheet = (uint8_t *)malloc((1024*1024));
	if (NULL == ne_sheet) {
		fprintf(stderr, "Could not allocate 1024*1024 sprite sheet buffer.\n");
		exit(-1);
	}
	memset(ne_sheet, 0, (1024*1024));
	// the sprite sheet for all non-enemy sprites contains 388 entries
	// these encompass all of the decorations/items/psprite weapons
	for (int i=0;i<388;i++) {
		int lumpnum = nonenemy_sheet[i][0];
		int x = nonenemy_sheet[i][1];
		int y = nonenemy_sheet[i][2];
		int w = nonenemy_sheet[i][3];
		int h = nonenemy_sheet[i][4];

		PalettizedImage *lumpImg = allImages[lumpnum];
		if (!lumpImg) {
			fprintf(stderr, "missing image for lump %d\n", lumpnum);
			exit(-1);
		}

		for (int k=y;k<y+h;k++) {
			for (int j=x;j<x+w;j++) {
				ne_sheet[(k*1024) + j] = lumpImg->pixels[((k-y)*w) + (j-x)];
			}
		}

		free(lumpImg->pixels);
		free(lumpImg);
	}

	// this buffer will contain a twiddled copy of the sprite sheet
	uint8_t *twid_sheet = (uint8_t *)malloc((1024*1024));
	if (NULL == twid_sheet) {
		fprintf(stderr, "Could not allocate 1024*1024 TWIDDLED sprite sheet buffer.\n");
		exit(-1);
	}
	memset(twid_sheet, 0, (1024*1024));
	load_twid(twid_sheet, ne_sheet, 1024, 1024);

	sprintf(output_paths, "%s/tex/non_enemy.tex", output_directory);
	FILE *sheet_fd = fopen(output_paths, "wb");
	if (NULL == sheet_fd) {
		fprintf(stderr, "Could not open file for writing twiddled sprite sheet.\n");
		exit(-1);
	}

	size_t twidsheet_total = 0;
	size_t twidsheet_write = fwrite(twid_sheet, 1, (1024*1024), sheet_fd);
	if (-1 == twidsheet_write) {
		fprintf(stderr, "Could not write to twiddled sprite sheet file: %s\n", strerror(errno));
		exit(-1);
	}
	twidsheet_total += twidsheet_write;
	while(twidsheet_total < 1024*1024) {
		twidsheet_write = fwrite(twid_sheet + twidsheet_total, 1, (1024*1024) - twidsheet_total, sheet_fd);
		if (-1 == twidsheet_write) {
			fprintf(stderr, "Could not write to twiddled sprite sheet file: %s\n", strerror(errno));
			exit(-1);
		}
		twidsheet_total += twidsheet_write;
	}

	int sheetclose = fclose(sheet_fd);
	if (0 != sheetclose) {
		fprintf(stderr, "Error closing twiddled sprite sheet file: %s\n", strerror(errno));
		exit(-1);
	}

	free(ne_sheet);
	free(twid_sheet);

	/*
	 * HERE BEGINS FIXWAD CODE adapted
	 */
	sprintf(output_paths, "%s/pow2.wad", output_directory);
	FILE *fd = fopen(output_paths, "wb");
	if (NULL == fd) {
		fprintf(stderr, "Could not open file for writing Dreamcast Doom 64 IWAD.\n");
		exit(-1);
	}

	for(int i=0;i<4;i++) {
		size_t idwrite = fwrite(&identifier[i], 1, 1, fd);
		if (-1 == idwrite) {
			fprintf(stderr, "Error writing identifier to Dreamcast Doom 64 IWAD: %s\n", strerror(errno));
			exit(-1);
		}
	}

	// total without maps
	numlumps -= 33;

	size_t numlumwrite = fwrite(&numlumps, 1, 4, fd);
	if (-1 == numlumwrite) {
		fprintf(stderr, "Error writing total lump count to Dreamcast Doom 64 IWAD: %s\n", strerror(errno));
		exit(-1);
	}

	// total with maps
	numlumps += 33;

	int lastofs = 4 + 4 + 4 + 4;

	for (int i=0;i<numlumps;i++) {
		if (i == 0) continue;
		if ((i > 1488) && (i < 1522)) continue;

#if REINDEX_FLATS
		// any flat that used a single palette is written to wad as compressed, 8bpp, twiddled, D64-encoded
		else if (i > 969 && i < 1460 && allN64Textures[TEXNUM(i)]) {
			uint8_t *outbuf;
			int outlen;
			int wp2 = 1 << SwapShort(allN64Textures[TEXNUM(i)]->wshift);
			int hp2 = 1 << SwapShort(allN64Textures[TEXNUM(i)]->hshift);
			outbuf = encoder__EncodeD64((void *)allN64Textures[TEXNUM(i)], sizeof(textureN64_t) + (wp2*hp2), &outlen);
			free(outbuf);
			int orig_size = outlen;
			int padded_size = (orig_size + 7) & ~7;
			lastofs = lastofs + padded_size;
		}
#endif

		// non enemy sprites are written to wad as uncompressed, header-only
		else if ((i > 0) && ((i < 347) || ((i > 923) && (i < 966)))) {
			lumpinfo[i].name[0] &= 0x7f;
			int padded_size = (sizeof(spriteDC_t) + 7) & ~7;
			lastofs = lastofs + padded_size;
		}

		// any flat that used multiple palettes is written to wad as "pass-through"
		// palettes are written to wad as "pass-through"
		// SYMBOLS/STATUS/SFONT are written to wad as "pass-through"
		// demos are written to wad as "pass-through"
	 	else if ((i > 965) || ((names[i][0] == 'P') && (names[i][1] == 'A') && (names[i][2] == 'L')) ) {
			int orig_size = lumpinfo[i].size;
			if (lumpinfo[i].name[0] & 0x80) {
				orig_size = lumpinfo[i+1].filepos - lumpinfo[i].filepos;
			}
			int padded_size = (orig_size + 7) & ~7;
			lastofs = lastofs + padded_size;
		}
		
		// enemy sprites are encoded as a new, smaller spriteDC_t type with header AND data,
		//		8bpp, common palette, compressed and Jaguar encoded
		else {
			uint8_t *outbuf;
			int outlen;
			int wp2 = (allSprites[i]->width);
			int hp2 = (allSprites[i]->height);
			outbuf = encode((void *)allSprites[i], sizeof(spriteDC_t) + (wp2*hp2), &outlen);
			free(outbuf);
			int orig_size = outlen;
			int padded_size = (orig_size + 7) & ~7;
			lastofs = lastofs + padded_size;
		}
	}

	size_t infotabofswrite = fwrite(&lastofs, 1, 4, fd);
	if (-1 == infotabofswrite) {
		fprintf(stderr, "Error writing info table offset to Dreamcast Doom 64 IWAD: %s\n", strerror(errno));
		exit(-1);
	}
	size_t padwrite = fwrite(&lastofs, 1, 4, fd);
	if (-1 == padwrite) {
		fprintf(stderr, "Error writing pad to Dreamcast Doom 64 IWAD: %s\n", strerror(errno));
		exit(-1);
	}

	lastofs = 16;

	for (int i=0;i<numlumps;i++) {
		if (((i < 349) || (i > 923)) || ((names[i][0] == 'P') && (names[i][1] == 'A') && (names[i][2] == 'L'))) {
			int data_size;
			int orig_size = lumpinfo[i].size;
			data_size = orig_size;

			if ((i > 0) && ((i < 347) || ((i > 923) && (i<966)))) {
				if (allSprites[i]) {
					int outlen;
					int fileLen;
					int origLen = sizeof(spriteDC_t);
					int padded_size = (origLen + 7) & ~7;
					fwrite((void*)allSprites[i], padded_size, 1, fd);
					lumpinfo[i].name[0] &= 0x7f;
					lumpinfo[i].filepos = lastofs;
					lumpinfo[i].size = origLen;
					lastofs = lastofs + padded_size;
				} else {
					fprintf(stderr, "missing sprite %d\n", i);
					exit(-1);
				}
			}

			else if ((i > 1488) && (i < 1522)) {
				if (lumpinfo[i].name[0] & 0x80) {
					data_size = lumpinfo[i+1].filepos - lumpinfo[i].filepos;
				}
				memset(lumpdata, 0, LUMPDATASZ);
				memcpy(lumpdata, doom64wad + lumpinfo[i].filepos, data_size);
				data_size = (data_size + 3) & ~3;
				unsigned char *mapdata = malloc(orig_size);
				DecodeD64(lumpdata, mapdata);
				char mapname[9];
				memset(mapname,0,9);
				memcpy(mapname,lumpinfo[i].name,8);
				mapname[0] = 'm';
				mapname[1] = 'a';
				mapname[2] = 'p';
				sprintf(output_paths, "%s/maps/%s.wad", output_directory, mapname);
				FILE *map_fd = fopen(output_paths, "wb");
				fwrite(mapdata, 1, orig_size, map_fd);
				fclose(map_fd);
				free(mapdata);
			}

#if REINDEX_FLATS
			else if (i > 969 && i < 1460 && allN64Textures[TEXNUM(i)]) {
				uint8_t *outbuf;
				int outlen;
				int wp2 = 1 << SwapShort(allN64Textures[TEXNUM(i)]->wshift);
				int hp2 = 1 << SwapShort(allN64Textures[TEXNUM(i)]->hshift);
				int fileLen;
				int origLen = sizeof(textureN64_t) + (wp2*hp2);
				lumpinfo[i].name[0] |= 0x80;
				outbuf = encoder__EncodeD64((void *)allN64Textures[TEXNUM(i)], origLen, &outlen);
				fileLen = outlen;
				int orig_size = fileLen;
				int padded_size = (orig_size + 7) & ~7;
				memset(lumpdata, 0, LUMPDATASZ);
				memcpy(lumpdata, outbuf, orig_size);
				free(outbuf);
				fwrite(lumpdata, 1, padded_size, fd);
				lumpinfo[i].filepos = lastofs;
				lumpinfo[i].size = origLen;
				lastofs = lastofs + padded_size;
			}
#endif

			else {
				if (lumpinfo[i].name[0] & 0x80) {
					data_size = lumpinfo[i+1].filepos - lumpinfo[i].filepos;
				}
				memset(lumpdata, 0, LUMPDATASZ);
				memcpy(lumpdata, doom64wad + lumpinfo[i].filepos, data_size);
				data_size = (data_size + 7) & ~7;
				fwrite(lumpdata, 1, data_size, fd);
				lumpinfo[i].filepos = lastofs;
				lumpinfo[i].size = orig_size;
				lastofs = lastofs + data_size;
			}
		} else {
			if (allSprites[i]) {
				uint8_t *outbuf;
				int outlen;
				int wp2 = (allSprites[i]->width);
				int hp2 = (allSprites[i]->height);
				int fileLen;
				int origLen = sizeof(spriteDC_t) + (wp2*hp2);
				outbuf = encode((void *)allSprites[i], origLen, &outlen);
				fileLen = outlen;
				int orig_size = fileLen;
				int padded_size = (orig_size + 7) & ~7;
				memset(lumpdata, 0, LUMPDATASZ);
				memcpy(lumpdata, outbuf, orig_size);
				free(outbuf);
				fwrite(lumpdata, 1, padded_size, fd);
				lumpinfo[i].filepos = lastofs;
				lumpinfo[i].size = origLen;
				lastofs = lastofs + padded_size;
			} else {
				fprintf(stderr, "missing sprite %d\n", i);
				exit(-1);
			}
		}
	}

	for (int i=0;i<numlumps;i++) {
		if ((i > 1488) && (i < 1522)) continue;

		fwrite((void*)(&lumpinfo[i]), 1, sizeof(lumpinfo_t), fd);
	}
	fclose(fd);

#define NUMALTLUMPS 311
	char pwad[4] = {'P','W','A','D'};
	sprintf(output_paths, "%s/alt.wad", output_directory);
	FILE *alt_fd = fopen(output_paths, "wb");
	for (int i=0;i<4;i++) {
		fwrite(&pwad[i], 1, 1, alt_fd);
	}
	int numaltlumps = NUMALTLUMPS;
	fwrite(&numaltlumps, 1, 4, alt_fd);
	lastofs = 4 + 4 + 4 + 4;
	lumpinfo_t *altlumpinfo = (lumpinfo_t *)malloc(numaltlumps * sizeof(lumpinfo_t));
	if (NULL == altlumpinfo) {
		fprintf(stderr, "Could not allocate alternate lump info.\n");
		exit(-1);
	}

	for (int i=0;i<numaltlumps;i++) {
		memset(altlumpinfo[i].name, 0, 8);
		memcpy(altlumpinfo[i].name, newlumps[i], strlen(newlumps[i]));
		if (newlumps[i][0] != 'S' || (newlumps[i][0] == 'S' && newlumps[i][1] != '2')) {
			altlumpinfo[i].name[0] |= 0x80;
		}
		altlumpinfo[i].filepos = lastofs;
		if(newlumps[i][0] == 'S' && newlumps[i][1] == '2') {
			altlumpinfo[i].size = 0;
		} else {
			int altlumpnum = W_GetNumForName(newlumps[i]);

			uint8_t *outbuf;
			int outlen;
			int wp2 = (allSprites[altlumpnum]->width);
			int hp2 = (allSprites[altlumpnum]->height);
			int fileLen;
			int origLen = sizeof(spriteDC_t) + (wp2*hp2);
			outbuf = encode((void *)allSprites[altlumpnum], origLen, &outlen);
			free(outbuf);
			fileLen = outlen;
			int orig_size = fileLen;
			int padded_size = (orig_size + 7) & ~7;
			altlumpinfo[i].size = origLen;
			lastofs = lastofs + padded_size;
		}
	}

	fwrite(&lastofs, 1, 4, alt_fd);
	// pad
	fwrite(&lastofs, 1, 4, alt_fd);

	for (int i = 0; i < numaltlumps; i++) {
		if (altlumpinfo[i].size) {
			int altlumpnum = W_GetNumForName(newlumps[i]);

			uint8_t *outbuf;
			int outlen;
			int wp2 = (allSprites[altlumpnum]->width);
			int hp2 = (allSprites[altlumpnum]->height);
			int fileLen;
			int origLen = sizeof(spriteDC_t) + (wp2*hp2);
			outbuf = encode((void *)allSprites[altlumpnum], origLen, &outlen);
			fileLen = outlen;
			int orig_size = fileLen;
			int padded_size = (orig_size + 7) & ~7;

			memset(lumpdata, 0, LUMPDATASZ);
			memcpy(lumpdata, outbuf, orig_size);
			free(outbuf);
			fwrite(lumpdata, 1, padded_size, alt_fd);
		}
	}

	for (int i = 0; i < numaltlumps; i++) {
		fwrite((void*)(&altlumpinfo[i]), 1, sizeof(lumpinfo_t), alt_fd);
	}
	fclose(alt_fd);


#if GENERATE_BUMP_WAD
// leaving this in for the sake of showing how this file was generated
// the file itself is in the repo, so this is commented out

// COMPRESSED BUMPMAPS

// a folder full of 3d xyz normal maps gets processed by dchemigen
// outputting a set of compressed 2d normal maps into bump folder
// the following code creates a wad from the bump folder contents

	sprintf(output_paths, "%s/bump.wad", output_directory);
	FILE *bump_fd = fopen(output_paths, "wb");
	for(int i=0;i<4;i++) {
		fwrite(&pwad[i], 1, 1, bump_fd);
	}
	int numbumplumps = NUMTEXLUMPS;
	fwrite(&numbumplumps, 1, 4, bump_fd);
	lastofs = 4 + 4 + 4;
	lumpinfo_t *bumplumpinfo = (lumpinfo_t *)malloc(numbumplumps * sizeof(lumpinfo_t));
	if (NULL == bumplumpinfo) {
		fprintf(stderr, "Could not allocate bumpmap lump info.\n");
		exit(-1);
	}

	for (int i=0;i<numbumplumps;i++) {
		memset(bumplumpinfo[i].name, 0, 8);
		memcpy(bumplumpinfo[i].name, texture_strings[i], 8);
		bumplumpinfo[i].filepos = lastofs;
		if(texture_strings[i][0] == 'T' && texture_strings[i][1] == '_') {
			bumplumpinfo[i].size = 0;
		} else {
			int bumplumpnum = W_GetTexNumForName(texture_strings[i]);
			char c_fn[256];
			sprintf(c_fn, "%s/bump/%s_NRM.comp", output_directory, texture_strings[i]);
			FILE *c_fd = fopen(c_fn, "rb");

			if (c_fd) {
				fseek(c_fd, 0, SEEK_END);
				int fileLen = ftell(c_fd);
				fclose(c_fd);

				int orig_size = fileLen;
				int padded_size = (orig_size + 3) & ~3;
				bumplumpinfo[i].size = orig_size;
				lastofs = lastofs + padded_size;
			} else {
				bumplumpinfo[i].size = 0;
			}
		}
	}

	fwrite(&lastofs, 1, 4, bump_fd);

	for (int i = 0; i < numbumplumps; i++) {
		if (bumplumpinfo[i].size != 0) {
			int bumplumpnum = W_GetTexNumForName(texture_strings[i]);

			char c_fn[256];
			sprintf(c_fn, "%s/bump/%s_NRM.comp", output_directory, texture_strings[i]);
			FILE *c_fd = fopen(c_fn, "rb");
			fseek(c_fd, 0, SEEK_END);
			int fileLen = ftell(c_fd);
			fseek(c_fd, 0, SEEK_SET);
			uint8_t *compbuf = malloc(fileLen);
			fread(compbuf, fileLen, 1, c_fd);
			fclose(c_fd);

			int orig_size = fileLen;
			int padded_size = (orig_size + 3) & ~3;

			memset(lumpdata, 0, LUMPDATASZ);
			memcpy(lumpdata, compbuf, orig_size);
			free(compbuf);
			fwrite(lumpdata, 1, padded_size, bump_fd);
		}
	}

	for (int i = 0; i < numbumplumps; i++) {
		fwrite((void*)(&bumplumpinfo[i]), 1, sizeof(lumpinfo_t), bump_fd);
	}
	fclose(bump_fd);
#endif

	free(doom64wad);

	int nd_map_starts[7] = {
		13043956,
		13297524,
		13657976,
		13969740,
		14364988,
		14726176,
		14941448
	};

	int nd_map_sizes[7] = {
		253568,
		360452,
		311764,
		395248,
		361188,
		215272,
		61996
	};

	// dump lost levels if doom64.wad present
	if (argc != 4) {
		goto the_end;
	}

#define NIGHTDIVE_WAD_SIZE 15103212
	FILE *nd_fd = fopen(argv[3], "rb"); // doom64.wad
	if (NULL == nd_fd) {
		fprintf(stderr, "Could not open specified Nightdive Doom 64 WAD for reading.\n");
		exit(-1);
	}
	for (int mid = 0; mid < 7; mid++) {
		char *mapwad = malloc(nd_map_sizes[mid]);
		int nd_seek_rv = fseek(nd_fd, nd_map_starts[mid], SEEK_SET);
		if (-1 == nd_seek_rv) {
			fprintf(stderr, "Could not seek to map WAD in Nightdive Doom 64 WAD: %s\n", strerror(errno));
			exit(-1);
		}

		size_t nd_total_read = 0;
		size_t nd_wad_rv = fread(mapwad, 1, nd_map_sizes[mid], nd_fd);
		if (-1 == nd_wad_rv) {
			fprintf(stderr, "Could not read map WAD from Nightdive Doom 64 WAD: %s\n", strerror(errno));
			exit(-1);
		}

		nd_total_read += nd_wad_rv;
		while (nd_total_read < nd_map_sizes[mid]) {
			nd_wad_rv = fread(mapwad + nd_total_read, 1,nd_map_sizes[mid] - nd_total_read, nd_fd);
			if (-1 == nd_wad_rv) {
				fprintf(stderr, "Could not read map WAD from Nightdive Doom 64 WAD: %s\n", strerror(errno));
				exit(-1);
			}
			nd_total_read += nd_wad_rv;
		}
		char nmapfn[256];
		sprintf(nmapfn, "map%d_nd.wad", 34+mid);
		char nmapfn2[256];
		sprintf(nmapfn2, "%s/maps/map%d.wad", output_directory, 34+mid);
		FILE *nmap_fd = fopen(nmapfn, "w+b");
		if (NULL == nmap_fd) {
			fprintf(stderr, "Could not open map WAD for writing.\n");
			exit(-1);
		}
		fwrite(mapwad, 1, nd_map_sizes[mid], nmap_fd);
		fclose(nmap_fd);
		convert(nmapfn, nmapfn2);
		free(mapwad);
	}
	int nd_close = fclose(nd_fd);
	if (0 != nd_close) {
		fprintf(stderr, "Error closing Nightdive Doom 64 WAD: %s\n", strerror(errno));
		exit(-1);
	}

the_end:

	for (int i=0;i<503;i++) {
		if (allN64Textures[i])
			free(allN64Textures[i]);
	}

	for (int i=0;i<966+355;i++) {
		if (allSprites[i])
			free(allSprites[i]);
	}

	free_gunpals();
	free(enemyPal->table);
	free(enemyPal);
	free(nonEnemyPal->table);
	free(nonEnemyPal);
	free(altlumpinfo);
	free(lumpinfo);
	return 0;
}
