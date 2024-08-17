#include <stdio.h>
#include <stdint.h>

#define MAXCHAR ((char)0x7f)
#define MAXSHORT ((short)0x7fff)
#define MAXINT	((int)0x7fffffff)	/* max pos 32-bit int */
#define MAXLONG ((long)0x7fffffff)

#define MINCHAR ((char)0x80)
#define MINSHORT ((short)0x8000)
#define MININT 	((int)0x80000000)	/* max negative 32-bit integer */
#define MINLONG ((long)0x80000000)

#ifndef NULL
#define	NULL	0
#endif

/*=======*/
/* TYPES */
/*=======*/

typedef unsigned char byte;
typedef unsigned long long int u64;
typedef struct {
    int dec_bit_count;
    int dec_bit_buffer;
    int enc_bit_count;
    int enc_bit_buffer;
    byte* ostart;
    byte* output;
    byte* istart;
    byte* input;
} buffers_t;

typedef struct {
    int offset;
    int incrBit;
    int unk1;
    int type;
} encodeArgs_t;

/*=========*/
/* GLOBALS */
/*=========*/

static short ShiftTable[6] = { 4, 6, 8, 10, 12, 14 }; // 8005D8A0

static int offsetTable[12];      // 800B2250
static int offsetMaxSize, windowSize; // 800b2280 , 800b2284
static encodeArgs_t encArgs;    // 800b2288

#define HASH_SIZE (1 << 14)
static short* hashtable;    // 800B2298
static short* hashtarget;   // 800B229C
static short* hashNext;     // 800B22A0
static short* hashPrev;     // 800B22A4

static short DecodeTable[2516]; // 800B22A8
static short array01[1258];     // 800B3660

static buffers_t buffers;       // 800B4034
//static byte* window;            // 800B4054

static u64 __attribute__((aligned(32))) windowBuf[8192];
static byte *window = (byte*)windowBuf;          // 800B4054

static int OVERFLOW_READ;       // 800B4058
static int OVERFLOW_WRITE;      // 800B405C

/*
============================================================================

DECODE BASED ROUTINES

============================================================================
*/

/*
========================
=
= GetOutputSize
=
========================
*/
int GetOutputSize(void) // [GEC] New
{
    return (int)((uintptr_t)buffers.output - (uintptr_t)buffers.ostart);
}

/*
========================
=
= GetReadSize
=
========================
*/

int GetReadSize(void) // [GEC] New
{
    return (int)((uintptr_t)buffers.input - (uintptr_t)buffers.istart);
}

/*
========================
=
= ReadByte -> Old GetDecodeByte
=
========================
*/

static int ReadByte(void) // 8002D1D0
{
    if ((int)((uintptr_t)buffers.input - (uintptr_t)buffers.istart) >= OVERFLOW_READ)
        return -1;

    return *buffers.input++;
}

/*
========================
=
= WriteByte -> Old WriteOutput
=
========================
*/

static void WriteByte(byte outByte) // 8002D214
{
    if ((int)(buffers.output - buffers.ostart) >= OVERFLOW_WRITE) {
		fprintf(stderr, "Overflowed output buffer");
    }

    *buffers.output++ = outByte;
}

/*
========================
=
= ReadBinary -> old DecodeScan
=
========================
*/

static int ReadBinary(void) // 8002D2F4
{
    int resultbyte;

    resultbyte = buffers.dec_bit_count;

    buffers.dec_bit_count = (resultbyte - 1);
    if ((resultbyte < 1))
    {
        resultbyte = ReadByte();

        buffers.dec_bit_buffer = resultbyte;
        buffers.dec_bit_count = 7;
    }

    resultbyte = (0 < (buffers.dec_bit_buffer & 0x80));
    buffers.dec_bit_buffer = (buffers.dec_bit_buffer << 1);

    return resultbyte;
}

/*
========================
=
= ReadCodeBinary -> old RescanByte
=
========================
*/

static int ReadCodeBinary(int byte) // 8002D3B8
{
    int shift;
    int i;
    int resultbyte;

    resultbyte = 0;
    i = 0;
    shift = 1;

    if (byte <= 0)
        return resultbyte;

    do
    {
        if (ReadBinary() != 0)
            resultbyte |= shift;

        i++;
        shift = (shift << 1);
    } while (i != byte);

    return resultbyte;
}

/*
========================
=
= InitTables -> old InitDecodeTable
=
========================
*/

static void InitTables(void) // 8002D468
{
    int evenVal, oddVal, incrVal, i;

    short* curArray;
    short* incrTbl;
    short* evenTbl;
    short* oddTbl;

    int* Tbl1, * Tbl2;

    encArgs.incrBit = 3;
    encArgs.unk1 = 0;
    encArgs.type = 0;

    buffers.dec_bit_count = 0;
    buffers.dec_bit_buffer = 0;
    buffers.enc_bit_count = 0;
    buffers.enc_bit_buffer = 0;

    curArray = &array01[(0 + 2)];
    incrTbl = &DecodeTable[(1258 + 2)];

    incrVal = 2;

    do {
        *incrTbl++ = (short)(incrVal / 2);
        *curArray++ = 1;
    } while (++incrVal < 1258);

    oddTbl = &DecodeTable[(629 + 1)];
    evenTbl = &DecodeTable[(0 + 1)];

    evenVal = 1;
    oddVal = 3;

    do
    {
        *oddTbl++ = (short)oddVal;
        oddVal += 2;

        *evenTbl++ = (short)(evenVal * 2);
        evenVal++;
    } while (evenVal < 629);

    incrVal = 0;
    i = 0;
    Tbl2 = &offsetTable[6];
    Tbl1 = &offsetTable[0];
    do {
        *Tbl1++ = incrVal;
        incrVal += (1 << (ShiftTable[i] & 0x1f));
        *Tbl2++ = incrVal - 1;
    } while (++i <= 5);

    offsetMaxSize = incrVal - 1;
    windowSize = offsetMaxSize + (64 - 1);
}

/*
========================
=
= CheckTable
=
========================
*/

static void CheckTable(int a0, int a1, int a2) // 8002D624
{
    int i;
    int idByte1;
    int idByte2;
    short* curArray;
    short* evenTbl;
    short* oddTbl;
    short* incrTbl;

    i = 0;
    evenTbl = &DecodeTable[0];
    oddTbl  = &DecodeTable[629];
    incrTbl = &DecodeTable[1258];

    idByte1 = a0;

    do {
        idByte2 = incrTbl[idByte1];

        array01[idByte2] = (array01[a1] + array01[a0]);

        a0 = idByte2;

        if (idByte2 != 1) {
            idByte1 = incrTbl[idByte2];
            idByte2 = evenTbl[idByte1];

            a1 = idByte2;

            if (a0 == idByte2) {
                a1 = oddTbl[idByte1];
            }
        }

        idByte1 = a0;
    } while (a0 != 1);

    if (array01[1] != 0x7D0) {
        return;
    }

    array01[1] >>= 1;

    curArray = &array01[2];
    do
    {
        curArray[3] >>= 1;
        curArray[2] >>= 1;
        curArray[1] >>= 1;
        curArray[0] >>= 1;
        curArray += 4;
        i += 4;
    } while (i != 1256);
}

/*
========================
=
= UpdateTables -> old DecodeByte
=
========================
*/

static void UpdateTables(int tblpos) // 8002D72C
{
    int incrIdx;
    int evenVal;
    int idByte1;
    int idByte2;
    int idByte3;
    int idByte4;

    short* evenTbl;
    short* oddTbl;
    short* incrTbl;
    short* tmpIncrTbl;

    evenTbl = &DecodeTable[0];
    oddTbl  = &DecodeTable[629];
    incrTbl = &DecodeTable[1258];

    idByte1 = (tblpos + 0x275);
    array01[idByte1] += 1;

    if (incrTbl[idByte1] != 1)
    {
        tmpIncrTbl = &incrTbl[idByte1];
        idByte2 = *tmpIncrTbl;

        if (idByte1 == evenTbl[idByte2]) {
            CheckTable(idByte1, oddTbl[idByte2], idByte1);
        }
        else {
            CheckTable(idByte1, evenTbl[idByte2], idByte1);
        }

        do
        {
            incrIdx = incrTbl[idByte2];
            evenVal = evenTbl[incrIdx];

            if (idByte2 == evenVal) {
                idByte3 = oddTbl[incrIdx];
            }
            else {
                idByte3 = evenVal;
            }

            if (array01[idByte3] < array01[idByte1])
            {
                if (idByte2 == evenVal) {
                    oddTbl[incrIdx] = (short)idByte1;
                }
                else {
                    evenTbl[incrIdx] = (short)idByte1;
                }

                evenVal = evenTbl[idByte2];

                if (idByte1 == evenVal) {
                    idByte4 = oddTbl[idByte2];
                    evenTbl[idByte2] = (short)idByte3;
                }
                else {
                    idByte4 = evenVal;
                    oddTbl[idByte2] = (short)idByte3;
                }

                incrTbl[idByte3] = (short)idByte2;

                *tmpIncrTbl = (short)incrIdx;
                CheckTable(idByte3, idByte4, idByte4);

                tmpIncrTbl = &incrTbl[idByte3];
            }

            idByte1 = *tmpIncrTbl;
            tmpIncrTbl = &incrTbl[idByte1];

            idByte2 = *tmpIncrTbl;
        } while (idByte2 != 1);
    }
}

/*
========================
=
= StartDecodeByte
=
========================
*/

static int StartDecodeByte(void) // 8002D904
{
    int lookup;
    short* evenTbl;
    short* oddTbl;

    lookup = 1;

    evenTbl = &DecodeTable[0];
    oddTbl  = &DecodeTable[629];

    while (lookup < 0x275) {
        if (ReadBinary() == 0) {
            lookup = evenTbl[lookup];
        }
        else {
            lookup = oddTbl[lookup];
        }
    }

    lookup = (lookup + -0x275);
    UpdateTables(lookup);

    return lookup;
}

/*
========================
=
= InsertNodeDirectory
= routine required for encoding
=
========================
*/

void InsertNodeDirectory(int start) // 8002D990
{
    int hashKey = ((window[start % windowSize] ^ (window[(start + 1) % windowSize] << 4)) ^ (window[(start + 2) % windowSize] << 8)) & (HASH_SIZE - 1);

    if (hashtable[hashKey] == -1) {
        hashtarget[hashKey] = start;
        hashNext[start] = -1;
    }
    else {
        hashNext[start] = hashtable[hashKey];
        hashPrev[hashtable[hashKey]] = start;
    }

    hashtable[hashKey] = start;
    hashPrev[start] = -1;
}

/*
========================
=
= DeleteNodeDirectory
= routine required for encoding
=
========================
*/

void DeleteNodeDirectory(int start) // 8002DAD0
{
    int hashKey = ((window[start % windowSize] ^ (window[(start + 1) % windowSize] << 4)) ^ (window[(start + 2) % windowSize] << 8)) & (HASH_SIZE - 1);

    if (hashtable[hashKey] == hashtarget[hashKey]) {
        hashtable[hashKey] = -1;
    }
    else {
        hashNext[hashPrev[hashtarget[hashKey]]] = -1;
        hashtarget[hashKey] = hashPrev[hashtarget[hashKey]];
    }
}

/*
========================
=
= FindMatch
= routine required for encoding
=
========================
*/


int FindMatch(int start, int count) // 8002DC0C
{
    int encodedlen;
    int offset;
    int i;
    int samelen;
    int next;
    int curr;
    int encodedpos;
    int hashKey;

    encodedlen = 0;
    if (start == windowSize) {
        start = 0;
    }

    hashKey = ((window[start % windowSize] ^ (window[(start + 1) % windowSize] << 4)) ^ (window[(start + 2) % windowSize] << 8)) & (HASH_SIZE - 1);

    offset = hashtable[hashKey];

    i = 0;
    while (offset != -1)
    {
        if (++i > count) {
            break;
        }

        if ((window[(start + encodedlen) % windowSize]) == (window[(offset + encodedlen) % windowSize]))
        {
            samelen = 0;
            curr = start;
            next = offset;

            while (window[curr] == window[next]) {
                if (samelen >= 64) {
                    break;
                }
                if (next == start) {
                    break;
                }
                if (curr == encArgs.incrBit) {
                    break;
                }
                ++samelen;
                if (++curr == windowSize) {
                    curr = 0;
                }
                if (++next == windowSize) {
                    next = 0;
                }
            }

            encodedpos = start - offset;
            if (encodedpos < 0) {
                encodedpos += windowSize;
            }
            encodedpos -= samelen;
            if ((encArgs.unk1) && (encodedpos > offsetTable[6])) {
                break;
            }

            if (encodedlen < samelen && encodedpos <= offsetMaxSize && (samelen > 3 || offsetTable[6 + (encArgs.type + 3)] >= encodedpos)) {
                encodedlen = samelen;
                encArgs.offset = encodedpos;
            }
        }

        offset = hashNext[offset]; // try next in list
    }
    return encodedlen;
}


/*
========================
=
= FUN_8002df14
= unknown Function
=
========================
*/

void FUN_8002df14(void) // 8002DF14
{
    byte byte_val;

    int i, j, k;
    byte* curPtr;
    byte* nextPtr;
    byte* next2Ptr;

    curPtr = &window[0];

    k = 0;
    j = 0;
    i = 1;
    do
    {
        nextPtr = &window[j];
        if (curPtr[0] == 10)
        {
            j = i;
            if (nextPtr[0] == curPtr[1])
            {
                next2Ptr = &window[i + 1];
                do
                {
                    nextPtr++;
                    byte_val = *next2Ptr++;
                    k++;
                } while (*nextPtr == byte_val);
            }
        }
        curPtr++;
        i++;
    } while (i != 67);

    if (k >= 16) {
        encArgs.unk1 = 1;
    }
}


/*
========================
=
= DecodeD64
=
= Exclusive Doom 64
=
========================
*/

void DecodeD64(unsigned char* input, unsigned char* output) // 8002DFA0
{
    int copyPos, storePos;
    int dec_byte, resc_byte;
    int incrBit, copyCnt, shiftPos, j;

    InitTables();

    OVERFLOW_READ = MAXINT;
    OVERFLOW_WRITE = MAXINT;

    incrBit = 0;

    buffers.input = buffers.istart = input;
    buffers.output = buffers.ostart = output;

    dec_byte = StartDecodeByte();

    while (dec_byte != 256)
    {
        if (dec_byte < 256)
        {
            /*  Decode the data directly using binary data code */

            WriteByte((byte)(dec_byte & 0xff));
            window[incrBit] = (byte)dec_byte;

            /*  Resets the count once the memory limit is exceeded in allocPtr,
                so to speak resets it at startup for reuse */
            incrBit += 1;
            if (incrBit == windowSize) {
                incrBit = 0;
            }
        }
        else
        {
            /*  Decode the data using binary data code,
                a count is obtained for the repeated data,
                positioning itself in the root that is being stored in allocPtr previously. */

            /*  A number is obtained from a range from 0 to 5,
                necessary to obtain a shift value in the ShiftTable*/
            shiftPos = (dec_byte + -257) / 62;

            /*  Get a count number for data to copy */
            copyCnt = (dec_byte - (shiftPos * 62)) + -254;

            /*  To start copying data, you receive a position number
                that you must sum with the position of table tableVar01 */
            resc_byte = ReadCodeBinary(ShiftTable[shiftPos]);

            /*  with this formula the exact position is obtained
                to start copying previously stored data */
            copyPos = incrBit - ((offsetTable[shiftPos] + resc_byte) + copyCnt);

            if (copyPos < 0) {
                copyPos += windowSize;
            }

            storePos = incrBit;

            for (j = 0; j < copyCnt; j++)
            {
                /* write the copied data */
                WriteByte(window[copyPos]);

                /* save copied data at current position in memory allocPtr */
                window[storePos] = window[copyPos];

                storePos++; /* advance to next allocPtr memory block to store */
                copyPos++;  /* advance to next allocPtr memory block to copy */

                /* reset the position of storePos once the memory limit is exceeded */
                if (storePos == windowSize) {
                    storePos = 0;
                }

                /* reset the position of copyPos once the memory limit is exceeded */
                if (copyPos == windowSize) {
                    copyPos = 0;
                }
            }

            /*  Resets the count once the memory limit is exceeded in allocPtr,
                so to speak resets it at startup for reuse */
            incrBit += copyCnt;
            if (incrBit >= windowSize) {
                incrBit -= windowSize;
            }
        }

        dec_byte = StartDecodeByte();
    }
}
