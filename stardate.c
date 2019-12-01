/*
 *  stardate: convert between date formats
 *  by Andrew Main <zefram@fysh.org>
 *  1997-02-09, stardate [-31]8857.62
 *
 *  Stardate code is based on version 1 of the Stardates in Star Trek FAQ.
 */

/*
 * Copyright (c) 1996, 1997 Andrew Main.  All rights reserved.
 *
 * Redistribution and use, in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer
 *    in the documentation and/or other materials provided with the
 *    distribution.
 * 3. All advertising materials mentioning features or use of this
 *    software must display the following acknowledgement:
 *        This product includes software developed by Andrew Main.
 * 4. The name of Andrew Main may not be used to endorse or promote
 *    products derived from this software without specific prior
 *    written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL ANDREW MAIN BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/*
 *  Unix programmers, please excuse the occasional DOSism in this code.
 *  DOS programmers, please exuse the Unixisms.  All programmers, please
 *  excuse the ANSIisms.  This program should actually run anywhere; I've
 *  tried to make it strictly conforming C.
 */

/*
 *  This program converts between dates in five formats:
 *    - stardates
 *    - the Julian calendar (with UTC time)
 *    - the Gregorian calendar (with UTC time)
 *    - the Quadcent calendar (see the Stardates FAQ for explanation)
 *    - traditional Unix time (seconds since 1970-01-01T00:00Z)
 *  Input and output can be in any of these formats.
 *
 *  Internally, all dates are converted to an extended Unix-style date format.
 *  This consists of the number of seconds since 0001=01=01 stored in *two*
 *  longs (because more than 32 bits are required), plus an additional
 *  32 bit fraction of a second.  Each of the date formats used for I/O has a
 *  pair of functions, used for converting from/to the internal format.
 */

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef EXIT_SUCCESS
# define EXIT_SUCCESS 0
#endif
#ifndef EXIT_FAILURE
# define EXIT_FAILURE 1
#endif
#ifndef ULONG_MAX
# define ULONG_MAX 0xffffffffUL
#endif

#define ISDIGIT(c) isdigit((unsigned char)(c))
#define ISALNUM(c) isalnum((unsigned char)(c))

#define ulong unsigned long

typedef struct {
  ulong high; /* multiples of 2^32 */
  ulong low; /* range 0-(2^32-1) */
} uvlong;
#define UVLONG_DIGITS 20

typedef struct {
  uvlong sec; /* seconds since 0001=01=01; unlimited range */
  ulong frac; /* range 0-(2^32-1) */
} intdate;

void getcurdate(intdate *);
void output(intdate const *);

int sdin(char const *, intdate *);
int julin(char const *, intdate *);
int gregin(char const *, intdate *);
int qcin(char const *, intdate *);
int unixin(char const *, intdate *);

char const *sdout(intdate const *);
char const *julout(intdate const *);
char const *gregout(intdate const *);
char const *qcout(intdate const *);
char const *unixdout(intdate const *);
char const *unixxout(intdate const *);

struct format {
  char opt, sel;
  /* The `in' function takes a string, and interprets it.  It returns 0     *
   * if the string is unrecognisable, 1 if it was successfully interpreted, *
   * or 2 if the string was recognised but invalid.  The date is returned   *
   * in the intdate passed to the function.  The function should round the  *
   * fraction *up*, so that on output the downward rounding won't cause     *
   * wildly inaccurate changes of date.  This makes it theoretically        *
   * possible for a date output in a different format from the input to be  *
   * up to 1/2^32 second too late, but this is much less serious.           */
  int (*in)(char const *, intdate *);
  /* The `out' function takes an internal date, and converts it to a string *
   * for output.  The string may be in static memory.                       */
  char const *(*out)(intdate const *);
} formats[] = {
  { 's', 0, sdin,   sdout    },
  { 'j', 0, julin,  julout   },
  { 'g', 0, gregin, gregout  },
  { 'q', 0, qcin,   qcout    },
  { 'u', 0, unixin, unixdout },
  { 'x', 0, NULL,   unixxout },
  { 0, 0, NULL, NULL }
};

int sddigits = 2;

char const *progname;

int main(int argc, char **argv)
{
  struct format *f;
  int sel = 0, haderr = 0;
  char *ptr;
  intdate dt;
  if((ptr = strrchr(*argv, '/')) || (ptr = strrchr(*argv, '\\')))
    progname = ptr+1;
  else
    progname = *argv;
  if(!*progname)
    progname = "stardate";
  while(*++argv && **argv == '-')
    while(*++*argv) {
      for(f = formats; f->opt; f++)
	if(**argv == f->opt) {
	  f->sel = sel = 1;
	  goto got;
	}
      fprintf(stderr, "%s: bad option: -%c\n", progname, **argv);
      exit(EXIT_FAILURE);
      got:
      if(**argv == 's' && argv[0][1] >= '0' && argv[0][1] <= '6')
	sddigits = *++*argv - '0';
    }
  if(!sel)
    formats[0].sel = 1;
  if(!*argv) {
    getcurdate(&dt);
    output(&dt);
  } else
    do {
      int n = 0;
      for(f = formats; f->opt; f++) {
	errno = 0;
	if(f->in && (n = f->in(*argv, &dt)))
	  break;
      }
      haderr |= !(n & 1);
      if(!n)
	fprintf(stderr, "%s: date format unrecognised: %s\n", progname, *argv);
      else if(n == 1) {
	if(errno) {
	  fprintf(stderr, "%s: date is out of acceptable range: %s\n",
	      progname, *argv);
	  haderr = 1;
	} else
	  output(&dt);
      }
    } while(*++argv);
  exit(haderr ? EXIT_FAILURE : EXIT_SUCCESS);
}

void getcurdate(intdate *dt)
{
  time_t t;
  struct tm *tm;
  char utc[20];
  t = time(NULL);
  tm = gmtime(&t);
  sprintf(utc, "%04d-%02d-%02dT%02d:%02d:%02d", tm->tm_year+1900, tm->tm_mon+1,
      tm->tm_mday, tm->tm_hour, tm->tm_min, tm->tm_sec);
  gregin(utc, dt);
}

void output(intdate const *dt)
{
  struct format *f;
  int d1 = 0;
  for(f = formats; f->opt; f++)
    if(f->sel) {
      if(d1)
	putchar(' ');
      d1 = 1;
      fputs(f->out(dt), stdout);
    }
  putchar('\n');
}

#define UVLONGINIT(high, low) { (high), (low) }
#define uvlonghval(n) ((n).high)
#define uvlonglval(n) ((n).low)
uvlong uvlongmk(ulong, ulong);
int uvlongiszero(uvlong);
int uvlongle(uvlong, uvlong);
#define uvlonglt(a, b) (!uvlongle((b), (a)))
#define uvlonggt(a, b) (!uvlongle((a), (b)))
#define uvlongge(a, b) (uvlongle((b), (a)))
int uvlongeq(uvlong, uvlong);
#define uvlongne(a, b) (!uvlongeq((a), (b)))
uvlong uvlonginc(uvlong);
uvlong uvlongdec(uvlong);
uvlong uvlongadd(uvlong, uvlong);
uvlong uvlongsub(uvlong, uvlong);
uvlong uvlongmul(uvlong, ulong);
uvlong uvlongdiv(uvlong, ulong);
ulong uvlongmod(uvlong, ulong);
char const *uvlongstr(uvlong, int, int);
ulong ulstr(char const *, char **, int);
uvlong uvlstr(char const *, char **, int);

/* The length of one quadcent year, 12622780800 / 400 == 31556952 seconds. */
#define QCYEAR 31556952UL
#define STDYEAR 31536000UL

/* Definitions to help with leap years. */
static int nrmdays[12]={ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
static int lyrdays[12]={ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
#define jleapyear(y) ( !((y)%4L) )
#define gleapyear(y) ( !((y)%4L) && ( ((y)%100L) || !((y)%400L) ) )
#define jdays(y) (jleapyear(y) ? lyrdays : nrmdays)
#define gdays(y) (gleapyear(y) ? lyrdays : nrmdays)
#define xdays(gp, y) (((gp) ? gleapyear(y) : jleapyear(y)) ? lyrdays : nrmdays)

/* The date 0323-01-01 (0323*01*01) is 117609 days after the internal   *
 * epoch, 0001=01=01 (0000-12-30).  This is a difference of             *
 * 117609*86400 (0x1cb69*0x15180) == 10161417600 (0x25daaed80) seconds. */
static uvlong const qcepoch = UVLONGINIT(0x2UL, 0x5daaed80UL);

/* The length of four centuries, 146097 days of 86400 seconds, is *
 * 12622780800 (0x2f0605980) seconds.                             */
static uvlong const quadcent = UVLONGINIT(0x2UL, 0xf0605980UL);

/* The epoch for Unix time, 1970-01-01, is 719164 (0xaf93c) days after *
 * our internal epoch, 0001=01=01 (0000-12-30).  This is a difference  *
 * of 719164*86400 (0xaf93c*0x15180) == 62135769600 (0xe77949a00)      *
 * seconds.                                                            */
static uvlong const unixepoch = UVLONGINIT(0xeUL, 0x77949a00UL);

/* The epoch for stardates, 2162-01-04, is 789294 (0xc0b2e) days after *
 * the internal epoch.  This is 789294*86400 (0xc0b2e*0x15180) ==      *
 * 68195001600 (0xfe0bd2500) seconds.                                  */
static uvlong const ufpepoch = UVLONGINIT(0xfUL, 0xe0bd2500UL);

/* The epoch for TNG-style stardates, 2323-01-01, is 848094 (0xcf0de) *
 * days after the internal epoch.  This is 73275321600 (0x110f8cad00) *
 * seconds.                                                           */
static uvlong const tngepoch = UVLONGINIT(0x11UL, 0x0f8cad00UL);

struct caldate {
  uvlong year;
  int month, day;
  int hour, min, sec;
};
int readcal(struct caldate *, char const *, char);

int sdin(char const *date, intdate *dt)
{
  static uvlong const nineteen = UVLONGINIT(0UL, 19UL);
  static uvlong const twenty = UVLONGINIT(0UL, 20UL);
  uvlong nissue;
  ulong integer, frac;
  char const *cptr = date;
  char *ptr;
  int oor, negi;
  char fracbuf[7];
  if(*cptr++ != '[')
    return 0;
  if(negi = (*cptr == '-'))
    cptr++;
  if(!ISDIGIT(*cptr))
    return 0;
  errno = 0;
  nissue = uvlstr(cptr, &ptr, 10);
  oor = errno;
  if(*ptr++ != ']' || !ISDIGIT(*ptr))
    return 0;
  integer = ulstr(ptr, &ptr, 10);
  if(errno || integer > 99999UL ||
      (!negi && uvlongeq(nissue, twenty) && integer > 5005UL) ||
      ((negi || uvlonglt(nissue, twenty)) && integer > 9999UL)) {
    fprintf(stderr, "%s: integer part is out of range: %s\n", progname, date);
    return 2;
  }
  if(*ptr == '.') {
    char *b = fracbuf;
    strcpy(fracbuf, "000000");
    ptr++;
    while(*b && ISDIGIT(*ptr))
      *b++ = *ptr++;
    while(ISDIGIT(*ptr))
      ptr++;
    if(*ptr)
      return 0;
  } else if(*ptr)
    return 0;
  frac = ulstr(fracbuf, NULL, 10);
  errno = oor;
  if(negi || uvlongle(nissue, twenty)) {
    /* Pre-TNG stardate */
    uvlong f;
    if(!negi) {
      /* There are two changes in stardate rate to handle: *
       *       up to [19]7340      0.2 days/unit           *
       * [19]7340 to [19]7840     10   days/unit           *
       * [19]7840 to [20]5006      2   days/unit           *
       * we scale to the first of these.                   */
      if(uvlongeq(nissue, twenty)) {
	nissue = nineteen;
	integer += 10000UL;
	goto fiddle;
      } else if(uvlongeq(nissue, nineteen) && integer >= 7340UL) {
	fiddle:
	/* We have a stardate in the range [19]7340 to [19]15006.  First *
	 * we scale it to match the prior rate, so this range changes to *
	 * 7340 to 390640.                                               */
	integer = 7340UL + ((integer - 7340UL) * 50) + frac / (1000000UL/50);
	frac = (frac * 50UL) % 1000000UL;
	/* Next, if the stardate is greater than what was originally     *
	 * [19]7840 (now represented as 32340), it is in the 2 days/unit *
	 * range, so scale it back again.  The range affected, 32340 to  *
	 * 390640, changes to 32340 to 104000.                           */
	if(integer >= 32340UL) {
	  frac = frac/5UL + (integer%5UL) * (1000000UL/5);
	  integer = 32340UL + (integer - 32340UL) / 5;
	}
	/* The odd stardate has now been scaled to match the early stardate *
	 * type.  It could be up to [19]104000.  Fortunately this will not  *
	 * cause subsequent calculations to overflow.                       */
      }
      dt->sec = uvlongadd(ufpepoch, uvlongmul(nissue, 2000UL*86400UL));
    } else {
      /* Negative stardate.  In order to avoid underflow in some cases, we *
       * actually calculate a date one issue (2000 days) too late, and     *
       * then subtract that much as the last stage.                        */
      dt->sec = uvlongsub(ufpepoch,
	  uvlongmul(uvlongdec(nissue), 2000UL*86400UL));
    }
    dt->sec = uvlongadd(dt->sec, uvlongmk(0, (86400UL/5UL) * integer));
    /* frac is scaled such that it is in the range 0-999999, and a value *
     * of 1000000 would represent 86400/5 seconds.  We want to put frac  *
     * in the top half of a uvlong, multiply by 86400/5 and divide by    *
     * 1000000, in order to leave the uvlong containing (top half) a     *
     * number of seconds and (bottom half) a fraction.  In order to      *
     * avoid overflow, this scaling is cancelled down to a multiply by   *
     * 54 and a divide by 3125.                                          */
    f = uvlongmul(uvlongmk(frac, 0), 54UL);
    f = uvlongdiv(uvlongadd(f, uvlongmk(0, 3124UL)), 3125UL);
    dt->sec = uvlongadd(dt->sec, uvlongmk(0, uvlonghval(f)));
    dt->frac = uvlonglval(f);
    if(negi) {
      /* Subtract off the issue that was added above. */
      dt->sec = uvlongsub(dt->sec, uvlongmk(0, 2000UL*86400UL));
    }
  } else {
    uvlong t;
    /* TNG stardate */
    nissue = uvlongsub(nissue, uvlongmk(0, 21UL));
    /* Each issue is 86400*146097/4 seconds long. */
    dt->sec = uvlongadd(tngepoch, uvlongmul(nissue, (86400UL/4UL)*146097UL));
    /* 1 unit is (86400*146097/4)/100000 seconds, which isn't even. *
     * It cancels to 27*146097/125.                                 */
    t = uvlongmul(uvlongmk(0, integer), 1000000UL);
    t = uvlongadd(t, uvlongmk(0, frac));
    t = uvlongmul(t, 27UL*146097UL);
    dt->sec = uvlongadd(dt->sec, uvlongdiv(t, 125000000UL));
    t = uvlongmk(uvlongmod(t, 125000000UL), 0UL);
    t = uvlongdiv(uvlongadd(t, uvlongmk(0, 124999999UL)), 125000000UL);
    dt->frac = uvlonglval(t);
  }
  return 1;
}

int calin(char const *, intdate *, int);

int julin(char const *date, intdate *dt)
{
  return calin(date, dt, 0);
}

int gregin(char const *date, intdate *dt)
{
  return calin(date, dt, 1);
}

int calin(char const *date, intdate *dt, int gregp)
{
  struct caldate c;
  uvlong t;
  int low, cycle;
  int n = readcal(&c, date, gregp ? '-' : '=');
  if(n != 1)
    return n;
  cycle = uvlongmod(c.year, 400UL);
  if(c.day > xdays(gregp, cycle)[c.month - 1]) {
    fprintf(stderr, "%s: day is out of range: %s\n", progname, date);
    return 2;
  }
  if(low = (gregp && uvlongiszero(c.year)))
    c.year = uvlongmk(0, 399UL);
  else
    c.year = uvlongdec(c.year);
  t = uvlongmul(c.year, 365UL);
  if(gregp) {
    t = uvlongsub(t, uvlongdiv(c.year, 100UL));
    t = uvlongadd(t, uvlongdiv(c.year, 400UL));
  }
  t = uvlongadd(t, uvlongdiv(c.year, 4UL));
  n = 2*gregp + c.day - 1;
  for(c.month--; c.month--; )
    n += xdays(gregp, cycle)[c.month];
  t = uvlongadd(t, uvlongmk(0, n));
  if(low)
    t = uvlongsub(t, uvlongmk(0, 146097UL));
  t = uvlongmul(t, 86400UL);
  dt->sec = uvlongadd(t, uvlongmk(0, c.hour*3600UL + c.min*60UL + c.sec));
  dt->frac = 0;
  return 1;
}

int qcin(char const *date, intdate *dt)
{
  struct caldate c;
  uvlong secs, t, f;
  int low;
  int n = readcal(&c, date, '*');
  if(n != 1)
    return n;
  if(c.day > nrmdays[c.month - 1]) {
    fprintf(stderr, "%s: day is out of range: %s\n", progname, date);
    return 2;
  }
  if(low = uvlonglt(c.year, uvlongmk(0, 323UL)))
    c.year = uvlongadd(c.year, uvlongmk(0, 400UL - 323UL));
  else
    c.year = uvlongsub(c.year, uvlongmk(0, 323UL));
  secs = uvlongadd(qcepoch, uvlongmul(c.year, QCYEAR));
  for(n = c.day - 1, c.month--; c.month--; )
    n += nrmdays[c.month];
  t = uvlongmk(0, n * 86400UL + c.hour * 3600UL + c.min * 60UL + c.sec);
  t = uvlongmul(t, QCYEAR);
  f = uvlongmk(uvlongmod(t, STDYEAR), STDYEAR - 1);
  secs = uvlongadd(secs, uvlongdiv(t, STDYEAR));
  if(low)
    secs = uvlongsub(secs, quadcent);
  dt->sec = secs;
  dt->frac = uvlonglval(uvlongdiv(f, STDYEAR));
  return 1;
}

int readcal(struct caldate *c, char const *date, char sep)
{
  int oor;
  ulong ul;
  char *ptr;
  char const *pos = date;
  if(!ISDIGIT(*pos))
    return 0;
  while(ISDIGIT(*++pos));
  if(*pos++ != sep || !ISDIGIT(*pos))
    return 0;
  while(ISDIGIT(*++pos));
  if(*pos++ != sep || !ISDIGIT(*pos))
    return 0;
  while(ISDIGIT(*++pos));
  if(*pos) {
    if((*pos != 'T' && *pos != 't') || !ISDIGIT(*++pos)) {
      badtime:
      fprintf(stderr, "%s: malformed time of day: %s\n", progname, date);
      return 2;
    }
    while(ISDIGIT(*++pos));
    if(*pos++ != ':' || !ISDIGIT(*pos))
      goto badtime;
    while(ISDIGIT(*++pos));
    if(*pos) {
      if(*pos++ != ':' || !ISDIGIT(*pos))
	goto badtime;
      while(ISDIGIT(*++pos));
      if(*pos)
	goto badtime;
    }
  }
  errno = 0;
  c->year = uvlstr(date, &ptr, 10);
  oor = errno;
  errno = 0;
  ul = ulstr(ptr+1, &ptr, 10);
  if(errno || !ul || ul > 12UL) {
    fprintf(stderr, "%s: month is out of range: %s\n", progname, date);
    return 2;
  }
  c->month = ul;
  ul = ulstr(ptr+1, &ptr, 10);
  if(errno || !ul || ul > 31UL) {
    fprintf(stderr, "%s: day is out of range: %s\n", progname, date);
    return 2;
  }
  c->day = ul;
  if(!*ptr) {
    c->hour = c->min = c->sec = 0;
    errno = oor;
    return 1;
  }
  ul = ulstr(ptr+1, &ptr, 10);
  if(errno || ul > 23UL) {
    fprintf(stderr, "%s: hour is out of range: %s\n", progname, date);
    return 2;
  }
  c->hour = ul;
  ul = ulstr(ptr+1, &ptr, 10);
  if(errno || ul > 59UL) {
    fprintf(stderr, "%s: minute is out of range: %s\n", progname, date);
    return 2;
  }
  c->min = ul;
  if(!*ptr) {
    c->sec = 0;
    errno = oor;
    return 1;
  }
  ul = ulstr(ptr+1, &ptr, 10);
  if(errno || ul > 59UL) {
    fprintf(stderr, "%s: second is out of range: %s\n", progname, date);
    return 2;
  }
  c->sec = ul;
  errno = oor;
  return 1;
}

int unixin(char const *date, intdate *dt)
{
  char const *pos = date+1;
  int radix = 10, neg;
  char *ptr;
  uvlong mag;
  if(date[0] != 'u' && date[0] != 'U')
    return 0;
  pos += neg = *pos == '-';
  if(pos[0] == '0' && (pos[1] == 'x' || pos[1] == 'X')) {
    pos += 2;
    radix = 16;
  }
  if(!ISALNUM(*pos)) {
    bad:
    fprintf(stderr, "%s: malformed Unix date: %s\n", progname, date);
    return 2;
  }
  mag = uvlstr(pos, &ptr, radix);
  if(*ptr)
    goto bad;
  dt->sec = (neg ? uvlongsub : uvlongadd)(unixepoch, mag);
  dt->frac = 0;
  return 1;
}

char const *tngsdout(intdate const *);

char const *sdout(intdate const *dt)
{
  int nissue, integer;
  uvlong frac;
  static char ret[18];
  if(uvlongle(tngepoch, dt->sec))
    return tngsdout(dt);
  if(uvlonglt(dt->sec, ufpepoch)) {
    /* Negative stardate */
    uvlong diff = uvlongdec(uvlongsub(ufpepoch, dt->sec));
    ulong nsecs = 2000UL*86400UL - 1 - uvlongmod(diff, 2000UL * 86400UL);
    nissue = -1 - uvlonglval(uvlongdiv(diff, 2000UL * 86400UL));
    integer = nsecs / (86400UL/5);
    frac = uvlongmul(uvlongmk(nsecs % (86400UL/5), dt->frac), 50UL);
  } else if(uvlonglt(dt->sec, tngepoch)) {
    /* Positive stardate */
    uvlong diff = uvlongsub(dt->sec, ufpepoch);
    ulong nsecs = uvlongmod(diff, 2000UL * 86400UL);
    nissue = uvlonglval(uvlongdiv(diff, 2000UL * 86400UL));
    if(nissue < 19 || (nissue == 19 && nsecs < 7340UL * (86400UL/5))) {
      /* TOS era */
      integer = nsecs / (86400UL/5);
      frac = uvlongmul(uvlongmk(nsecs % (86400UL/5), dt->frac), 50UL);
    } else {
      /* Film era */
      nsecs += (nissue - 19) * 2000UL*86400UL;
      nissue = 19;
      nsecs -= 7340UL * (86400UL/5);
      if(nsecs >= 5000UL*86400UL) {
	/* Late film era */
	nsecs -= 5000UL*86400UL;
	integer = 7840 + nsecs/(86400UL*2);
	if(integer >= 10000) {
	  integer -= 10000;
	  nissue++;
	}
	frac = uvlongmul(uvlongmk(nsecs % (86400UL*2), dt->frac), 5UL);
      } else {
	/* Early film era */
	integer = 7340 + nsecs/(86400UL*10);
	frac = uvlongmk(nsecs % (86400UL*10), dt->frac);
      }
    }
  }
  sprintf(ret, "[%d]%04d", nissue, integer);
  if(sddigits) {
    char *ptr = strchr(ret, 0);
    /* At this point, frac is a fractional part of a unit, in the range *
     * 0 to (2^32 * 864000)-1.  In order to represent this as a 6-digit *
     * decimal fraction, we need to scale this.  Mathematically, we     *
     * need to multiply by 1000000 and divide by (2^32 * 864000).  But  *
     * multiplying by 1000000 would cause overflow.  Cancelling the two *
     * values yields an algorithm of multiplying by 125 and dividing by *
     * (2^32*108).                                                      */
    frac = uvlongdiv(uvlongmul(frac, 125UL), 108UL);
    sprintf(ptr, ".%06lu", uvlonghval(frac));
    ptr[sddigits + 1] = 0;
  }
  return ret;
}

char const *tngsdout(intdate const *dt)
{
  static char ret[UVLONG_DIGITS + 15];
  uvlong h, l;
  ulong nsecs;
  uvlong diff = uvlongsub(dt->sec, tngepoch);
  /* 1 issue is 86400*146097/4 seconds long, which just fits in 32 bits. */
  uvlong nissue = uvlongadd(uvlongmk(0, 21UL),
      uvlongdiv(diff, (86400UL/4)*146097UL));
  nsecs = uvlongmod(diff, (86400UL/4)*146097UL);
  /* 1 unit is (86400*146097/4)/100000 seconds, which isn't even. *
   * It cancels to 27*146097/125.  For a six-figure fraction,     *
   * divide that by 1000000.                                      */
  h = uvlongmul(uvlongmk(0, nsecs), 125000000UL);
  l = uvlongmul(uvlongmk(0, dt->frac), 125000000UL);
  h = uvlongadd(h, uvlongmk(0, uvlonghval(l)));
  h = uvlongdiv(h, 27UL*146097UL);
  sprintf(ret, "[%s]%05lu", uvlongstr(nissue, 10, 1),
      uvlonglval(uvlongdiv(h, 1000000UL)));
  if(sddigits) {
    char *ptr = strchr(ret, 0);
    sprintf(ptr, ".%06lu", uvlongmod(h, 1000000UL));
    ptr[sddigits + 1] = 0;
  }
  return ret;
}

char const *calout(intdate const *, int);

char const *julout(intdate const *dt)
{
  return calout(dt, 0);
}

char const *gregout(intdate const *dt)
{
  return calout(dt, 1);
}

char const *docalout(char, int, int, uvlong, int, ulong);

char const *calout(intdate const *dt, int gregp)
{
  ulong tod = uvlongmod(dt->sec, 86400UL);
  uvlong year, days = uvlongdiv(dt->sec, 86400UL);
  /* We need the days number to be days since an xx01.01.01 to get the *
   * leap year cycle right.  For the Julian calendar, it is already    *
   * so (0001=01=01).  But for the Gregorian calendar, the epoch is    *
   * 0000-12-30, so we must add on 400 years minus 2 days.  The year   *
   * number gets corrected below.                                      */
  if(gregp)
    days = uvlongadd(days, uvlongmk(0, 146095UL));
  /* Approximate the year number, underestimating but only by a limited *
   * amount.  days/366 is a first approximation, but it goes out by 1   *
   * day every non-leap year, and so will be a full year out after 366  *
   * non-leap years.  In the Julian calendar, we get 366 non-leap years *
   * every 488 years, so adding (days/366)/487 corrects for this.  In   *
   * the Gregorian calendar, it is not so simple: we get 400 years      *
   * every 146097 days, and then add on days/366 within that set of 400 *
   * years.                                                             */
  if(gregp)
    year = uvlongadd(uvlongmul(uvlongdiv(days, 146097UL), 400UL),
	uvlongmk(0, uvlongmod(days, 146097UL) / 366UL));
  else
    year = uvlongadd(uvlongdiv(days, 366UL), uvlongdiv(days, 366UL * 487UL));
  /* We then adjust the number of days remaining to match this *
   * approximation of the year.  Note that this approximation  *
   * will never be more than two years off the correct date,   *
   * so the number of days left no longer needs to be stored   *
   * in a uvlong.                                              */
  if(gregp)
    days = uvlongsub(uvlongadd(days, uvlongdiv(year, 100UL)),
	uvlongdiv(year, 400UL));
  days = uvlongsub(days,
      uvlongadd(uvlongmul(year, 365UL), uvlongdiv(year, 4UL)));
  /* Now correct the year to an actual year number (see notes above). */
  if(gregp)
    year = uvlongsub(year, uvlongmk(0, 399UL));
  else
    year = uvlonginc(year);
  return docalout(gregp ? '-' : '=', gregp, uvlongmod(year, 400UL),
      year, uvlonglval(days), tod);
}

char const *docalout(char sep, int gregp, int cycle,
    uvlong year, int ndays, ulong tod)
{
  int nmonth = 0;
  int hr, min, sec;
  static char ret[UVLONG_DIGITS + 16];
  /* Walk through the months, fixing the year, and as a side effect *
   * calculating the month number and day of the month.             */
  while(ndays >= xdays(gregp, cycle)[nmonth]) {
    ndays -= xdays(gregp, cycle)[nmonth];
    if(++nmonth == 12) {
      nmonth = 0;
      year = uvlonginc(year);
      cycle++;
    }
  }
  ndays++;
  nmonth++;
  /* Now sort out the time of day. */
  hr = tod / 3600;
  tod %= 3600;
  min = tod / 60;
  sec = tod % 60;
  sprintf(ret, "%s%c%02d%c%02dT%02d:%02d:%02d",
      uvlongstr(year, 10, 4), sep, nmonth, sep, ndays, hr, min, sec);
  return ret;
}

char const *qcout(intdate const *dt)
{
  uvlong secs = dt->sec;
  ulong nsec;
  uvlong year, h, l;
  int low;
  if(low = uvlonglt(secs, qcepoch))
    secs = uvlongadd(secs, quadcent);
  secs = uvlongsub(secs, qcepoch);
  nsec = uvlongmod(secs, QCYEAR);
  secs = uvlongdiv(secs, QCYEAR);
  if(low)
    year = uvlongsub(secs, uvlongmk(0, 400 - 323));
  else
    year = uvlongadd(secs, uvlongmk(0, 323));
  /* We need to translate the nsec:dt->frac value (real seconds up to *
   * 31556952:0) into quadcent seconds.  This can be done by          *
   * multiplying by 146000 and dividing by 146097.  Normally this     *
   * would overflow, so we do this in two parts.                      */
  h = uvlongmk(0, nsec);
  l = uvlongmk(0, dt->frac);
  h = uvlongmul(h, 146000);
  l = uvlongmul(l, 146000);
  h = uvlongadd(h, uvlongmk(0, uvlonghval(l)));
  nsec = uvlonglval(uvlongdiv(h, 146097));
  return docalout('*', 0, 1, year, nsec / 86400, nsec % 86400UL);
}

char const *unixout(intdate const *, int, char const *);

char const *unixdout(intdate const *dt)
{
  return unixout(dt, 10, "");
}

char const *unixxout(intdate const *dt)
{
  return unixout(dt, 16, "0x");
}

char const *unixout(intdate const *dt, int radix, char const *prefix)
{
  static char ret[UVLONG_DIGITS + 3];
  char const *sgn;
  uvlong mag;
  if(uvlongle(unixepoch, dt->sec)) {
    sgn = "";
    mag = uvlongsub(dt->sec, unixepoch);
  } else {
    sgn = "-";
    mag = uvlongsub(unixepoch, dt->sec);
  }
  sprintf(ret, "U%s%s%s", sgn, prefix, uvlongstr(mag, radix, 1));
  return ret;
}

uvlong uvlongmk(ulong h, ulong l)
{
  uvlong r;
  r.high = h;
  r.low = l;
  return r;
}

int uvlongiszero(uvlong n)
{
  return !n.high && !n.low;
}

int uvlongle(uvlong a, uvlong b)
{
  return a.high < b.high || (a.high == b.high && a.low <= b.low);
}

int uvlongeq(uvlong a, uvlong b)
{
  return a.low == b.low && a.high == b.high;
}

uvlong uvlonginc(uvlong n)
{
  n.low++;
  if(!(n.low &= 0xffffffffUL)) {
    n.high = (n.high+1) & 0xffffffffUL;
    if(!n.high)
      errno = ERANGE;
  }
  return n;
}

uvlong uvlongdec(uvlong n)
{
  if(!n.low) {
    if(!n.high) {
      n.high = 0xffffffffUL;
      errno = ERANGE;
    } else
      n.high--;
    n.low = 0xffffffffUL;
  } else
    n.low--;
  return n;
}

uvlong uvlongadd(uvlong a, uvlong b)
{
  uvlong r;
  r.low = (a.low + b.low) & 0xffffffffUL;
  r.high = (a.high + b.high + (r.low < a.low)) & 0xffffffffUL;
  if(r.high < a.high || r.high < b.high)
    errno = ERANGE;
  return r;
}

uvlong uvlongsub(uvlong a, uvlong b)
{
  uvlong r;
  r.high = (a.high - b.high - (a.low < b.low)) & 0xffffffffUL;
  if(b.high + (a.low < b.low) > a.high)
    errno = ERANGE;
  r.low = (a.low - b.low) & 0xffffffffUL;
  return r;
}

uvlong uvlongmul(uvlong vl, ulong mul)
{
  uvlong r;
  unsigned ml = mul & 0xffffU, mh = mul >> 16;
  unsigned ll = vl.low & 0xffffU, lh = vl.low >> 16;
  ulong rl = ll * ml;
  ulong rm = (rl >> 16) + ll*mh + lh*ml;
  rl &= 0xffffU;
  r.high = (vl.high*mul + lh*mh + (rm >> 16)) & 0xffffffffUL;
  if(vl.high*mul/mul != vl.high || r.high < vl.high*mul || r.high < lh*mh)
    errno = ERANGE;
  rm &= 0xffffU;
  r.low = (rm << 16) | rl;
  return r;
}

uvlong uvlongdiv(uvlong n, ulong div)
{
  uvlong r, m;
  ulong b = 0x80000000UL;
  r.high = n.high / div;
  r.low = 0;
  n.high %= div;
  m.high = div;
  m.low = 0;
  while(n.high) {
    m.low = (m.low >> 1) | ((m.high&1) << 31);
    m.high >>= 1;
    if(uvlongle(m, n)) {
      n = uvlongsub(n, m);
      r.low |= b;
    }
    b >>= 1;
  }
  r.low |= n.low / div;
  return r;
}

ulong uvlongmod(uvlong n, ulong div)
{
  uvlong m;
  n.high %= div;
  m.high = div;
  m.low = 0;
  while(n.high) {
    m.low = (m.low >> 1) | ((m.high&1) << 31);
    m.high >>= 1;
    if(m.high < n.high || (m.high == n.high && m.low <= n.low))
      n = uvlongsub(n, m);
  }
  return n.low % div;
}

static char const xdigits[] = "0123456789abcdef";
static char const cdigits[] = "0123456789ABCDEF";

char const *uvlongstr(uvlong n, int radix, int min)
{
  static char ret[UVLONG_DIGITS + 1];
  char *pos = ret + UVLONG_DIGITS, *end = pos - min;
  *pos = 0;
  while(n.high || n.low) {
    *--pos = xdigits[uvlongmod(n, radix)];
    n = uvlongdiv(n, radix);
  }
  while(pos > end)
    *--pos = '0';
  return pos;
}

ulong ulstr(char const *str, char **ptr, int radix)
{
  int oerrno = errno;
  ulong n = 0;
  for(; *str; str++) {
    char const *d;
    int v;
    if((d = strchr(xdigits, *str)))
      v = d - xdigits;
    else if((d = strchr(cdigits, *str)))
      v = d - cdigits;
    else
      break;
    if(n > (ULONG_MAX-v) / radix)
      oerrno = ERANGE;
    n = n*radix + v;
  }
  errno = oerrno;
  if(ptr)
    *ptr = (char *)str;
  return n;
}

uvlong uvlstr(char const *str, char **ptr, int radix)
{
  int oerrno = errno;
  static char const cdigits[] = "0123456789ABCDEF";
  uvlong n = UVLONGINIT(0, 0);
  for(; *str; str++) {
    char const *d;
    int v;
    if((d = strchr(xdigits, *str)))
      v = d - xdigits;
    else if((d = strchr(cdigits, *str)))
      v = d - cdigits;
    else
      break;
    errno = 0;
    n = uvlongadd(uvlongmul(n, radix), uvlongmk(0, v));
    if(errno)
      oerrno = errno;
  }
  errno = oerrno;
  if(ptr)
    *ptr = (char *)str;
  return n;
}
