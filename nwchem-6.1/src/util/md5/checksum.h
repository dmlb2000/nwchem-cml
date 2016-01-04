/*
 $Id: checksum.h 19707 2010-10-29 17:59:36Z d3y133 $
 */
#ifndef CHECKSUM_H
#define CHECkSUM_H

extern void checksum_init(void);

extern void checksum_update(int len, const void *buf);

extern void checksum_final(char sum[33]);

extern void checksum_simple(int len, const void *buf, char sum[33]);

#endif
