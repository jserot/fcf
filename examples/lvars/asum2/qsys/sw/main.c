#include <stdio.h>
#include "alt_types.h"
#include "system.h"
#include "io.h"
#include "sys/alt_timestamp.h"

#define GCD_CC_CTL 0   // Control/status register
#define GCD_CC_ARG1 1
#define GCD_CC_ARG2 2
#define GCD_CC_RESULT 3

unsigned long ticks_per_us;

void test(alt_u32 arg1, alt_u32 arg2, int wait)
{
  alt_u32 r1, r2;

  unsigned long t, t1;
  r1 = IORD(GCD_CC_0_BASE, GCD_CC_CTL); // Reading control/status reg returns rdy value
  t = alt_timestamp()/ticks_per_us;
  printf("t=%lu us: read rdy=%lu\n", t, r1);

  IOWR(GCD_CC_0_BASE, GCD_CC_ARG1, arg1);
  IOWR(GCD_CC_0_BASE, GCD_CC_ARG2, arg2);
  r1 = IORD(GCD_CC_0_BASE, GCD_CC_ARG1);
  r2 = IORD(GCD_CC_0_BASE, GCD_CC_ARG2);
  t = alt_timestamp()/ticks_per_us;
  printf("t=%lu us: Wrote arg1=%lu arg2=%lu\n", t, r1, r2);

  IOWR(GCD_CC_0_BASE, GCD_CC_CTL, 1);  // Writing control/status register asserts start
  t = alt_timestamp()/ticks_per_us;
  printf("t=%lu us: Asserted start\n", t);

  while ( (r1 = IORD(GCD_CC_0_BASE, GCD_CC_CTL)) == 0 ); // Wait for rdy
  t1 = alt_timestamp()/ticks_per_us;
  r2 = IORD(GCD_CC_0_BASE, GCD_CC_RESULT);
  printf("t=%lu us: rdy=%lu result=%lu duration=%lu us\n", t1, r1, r2, t1-t);
}

int main()
{
  if ( alt_timestamp_start() < 0 ) {
	printf("** No timestamp device available\n");
    return 1;
    }
  ticks_per_us = alt_timestamp_freq() / 1000000;

  test(24, 36, 4);
  test(50009, 500029, 4);

  return 0;
}

