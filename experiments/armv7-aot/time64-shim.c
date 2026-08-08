// Supplies the glibc __*_time64 symbols for 32-bit ARM builds against an
// older glibc.
//
// The problem: .NET 10's linux-arm NativeAOT runtime references 22 symbols
// that glibc only grew around 2.34, as part of the 2038 work. 32-bit
// platforms had to gain 64-bit-timestamp variants of every time-related call,
// and .NET links against those. Plenty of armv7 hardware still in service ships
// an older glibc (2.31 is common), which squeezes us from both ends: we can't
// link against the old glibc, and a binary linked against a new one won't
// start there.
//
// The fix: define them ourselves, forwarding to each one's 32-bit
// counterpart, converting structs at the boundary.
//
// THE CATCH, stated plainly: this reintroduces the 2038 problem. Any absolute
// timestamp passing through here is narrowed to a 32-bit time_t and will wrap
// on 19 January 2038. That is a deliberate trade for reaching older embedded
// hardware that will never get a glibc update, not something to put on a
// general build. Durations (sleeps, timeouts) are unaffected, since they're
// small either way.
//
// Only linked into linux-arm builds; see the NativeLibrary item in Cli.fsproj.

#define _GNU_SOURCE

#include <time.h>
#include <errno.h>
#include <stdint.h>
#include <stddef.h>
#include <fcntl.h>
#include <pthread.h>
#include <sys/ioctl.h>
#include <sys/prctl.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>

// glibc's 64-bit time structs on a 32-bit platform.
struct dark_timespec64 {
  int64_t tv_sec;
  int32_t tv_nsec;
  int32_t _pad;
};

struct dark_timeval64 {
  int64_t tv_sec;
  int64_t tv_usec;
};

struct dark_rusage64 {
  struct dark_timeval64 ru_utime;
  struct dark_timeval64 ru_stime;
  long ru_maxrss, ru_ixrss, ru_idrss, ru_isrss;
  long ru_minflt, ru_majflt, ru_nswap;
  long ru_inblock, ru_oublock;
  long ru_msgsnd, ru_msgrcv;
  long ru_nsignals, ru_nvcsw, ru_nivcsw;
};

// Clamp rather than wrap. A value past 32-bit range is already meaningless
// here; saturating beats silently going negative.
static time_t narrow_sec(int64_t s) {
  if (s > (int64_t)INT32_MAX) return (time_t)INT32_MAX;
  if (s < (int64_t)INT32_MIN) return (time_t)INT32_MIN;
  return (time_t)s;
}

static void ts_narrow(const struct dark_timespec64 *in, struct timespec *out) {
  out->tv_sec = narrow_sec(in->tv_sec);
  out->tv_nsec = in->tv_nsec;
}

static void ts_widen(const struct timespec *in, struct dark_timespec64 *out) {
  out->tv_sec = (int64_t)in->tv_sec;
  out->tv_nsec = (int32_t)in->tv_nsec;
  out->_pad = 0;
}

static void tv_widen(const struct timeval *in, struct dark_timeval64 *out) {
  out->tv_sec = (int64_t)in->tv_sec;
  out->tv_usec = (int64_t)in->tv_usec;
}

// ---------------------------------------------------------------- durations

int __nanosleep64(const struct dark_timespec64 *req,
                  struct dark_timespec64 *rem) {
  struct timespec r32, rem32;
  int rc;

  if (req == NULL) { errno = EFAULT; return -1; }
  ts_narrow(req, &r32);
  rc = nanosleep(&r32, rem ? &rem32 : NULL);
  // On EINTR nanosleep reports the unslept remainder and the caller resumes
  // from it, so this has to come back faithfully.
  if (rem != NULL) ts_widen(&rem32, rem);
  return rc;
}

int __clock_nanosleep_time64(clockid_t clk, int flags,
                             const struct dark_timespec64 *req,
                             struct dark_timespec64 *rem) {
  struct timespec r32, rem32;
  int rc;

  if (req == NULL) return EFAULT;
  ts_narrow(req, &r32);
  rc = clock_nanosleep(clk, flags, &r32, rem ? &rem32 : NULL);
  if (rem != NULL) ts_widen(&rem32, rem);
  return rc;
}

int __pthread_cond_timedwait64(pthread_cond_t *cond, pthread_mutex_t *mutex,
                               const struct dark_timespec64 *abstime) {
  struct timespec t32;
  if (abstime == NULL) return EINVAL;
  ts_narrow(abstime, &t32);
  return pthread_cond_timedwait(cond, mutex, &t32);
}

// ------------------------------------------------------------- reading time

int __clock_gettime64(clockid_t clk, struct dark_timespec64 *tp) {
  struct timespec t32;
  int rc;
  if (tp == NULL) { errno = EFAULT; return -1; }
  rc = clock_gettime(clk, &t32);
  if (rc == 0) ts_widen(&t32, tp);
  return rc;
}

int __gettimeofday64(struct dark_timeval64 *tv, void *tz) {
  struct timeval t32;
  int rc = gettimeofday(&t32, tz);
  if (rc == 0 && tv != NULL) tv_widen(&t32, tv);
  return rc;
}

int64_t __time64(int64_t *t) {
  time_t now = time(NULL);
  if (t != NULL) *t = (int64_t)now;
  return (int64_t)now;
}

// ------------------------------------------------------- calendar breakdown

struct tm *__gmtime64(const int64_t *t) {
  time_t t32;
  if (t == NULL) return NULL;
  t32 = narrow_sec(*t);
  return gmtime(&t32);
}

struct tm *__gmtime64_r(const int64_t *t, struct tm *out) {
  time_t t32;
  if (t == NULL) return NULL;
  t32 = narrow_sec(*t);
  return gmtime_r(&t32, out);
}

struct tm *__localtime64(const int64_t *t) {
  time_t t32;
  if (t == NULL) return NULL;
  t32 = narrow_sec(*t);
  return localtime(&t32);
}

struct tm *__localtime64_r(const int64_t *t, struct tm *out) {
  time_t t32;
  if (t == NULL) return NULL;
  t32 = narrow_sec(*t);
  return localtime_r(&t32, out);
}

int64_t __mktime64(struct tm *tm) {
  return (int64_t)mktime(tm);
}

int64_t __timegm64(struct tm *tm) {
  return (int64_t)timegm(tm);
}

// --------------------------------------------------------------- file times

int __utimensat64(int dirfd, const char *path,
                  const struct dark_timespec64 times[2], int flags) {
  struct timespec t32[2];
  if (times == NULL) return utimensat(dirfd, path, NULL, flags);
  ts_narrow(&times[0], &t32[0]);
  ts_narrow(&times[1], &t32[1]);
  return utimensat(dirfd, path, t32, flags);
}

int __futimens64(int fd, const struct dark_timespec64 times[2]) {
  struct timespec t32[2];
  if (times == NULL) return futimens(fd, NULL);
  ts_narrow(&times[0], &t32[0]);
  ts_narrow(&times[1], &t32[1]);
  return futimens(fd, t32);
}

// ------------------------------------------------------------------ sockets

// SO_RCVTIMEO / SO_SNDTIMEO carry a struct timeval, so those two need
// converting. Everything else is passed straight through: the option value
// has no time in it and the 64-bit entry point exists only because glibc
// versions the whole call.
int __setsockopt64(int fd, int level, int optname,
                   const void *optval, socklen_t optlen) {
  if (level == SOL_SOCKET &&
      (optname == SO_RCVTIMEO || optname == SO_SNDTIMEO) &&
      optval != NULL && optlen == sizeof(struct dark_timeval64)) {
    const struct dark_timeval64 *in = optval;
    struct timeval t32;
    t32.tv_sec = narrow_sec(in->tv_sec);
    t32.tv_usec = (suseconds_t)in->tv_usec;
    return setsockopt(fd, level, optname, &t32, sizeof(t32));
  }
  return setsockopt(fd, level, optname, optval, optlen);
}

int __getsockopt64(int fd, int level, int optname,
                   void *optval, socklen_t *optlen) {
  if (level == SOL_SOCKET &&
      (optname == SO_RCVTIMEO || optname == SO_SNDTIMEO) &&
      optval != NULL && optlen != NULL &&
      *optlen == sizeof(struct dark_timeval64)) {
    struct timeval t32;
    socklen_t len32 = sizeof(t32);
    int rc = getsockopt(fd, level, optname, &t32, &len32);
    if (rc == 0) {
      struct dark_timeval64 *out = optval;
      tv_widen(&t32, out);
      *optlen = sizeof(*out);
    }
    return rc;
  }
  return getsockopt(fd, level, optname, optval, optlen);
}

// SO_TIMESTAMP control messages would carry a timeval, but we never enable
// that socket option, so a straight forward is correct here.
ssize_t __recvmsg64(int fd, struct msghdr *msg, int flags) {
  return recvmsg(fd, msg, flags);
}

ssize_t __sendmsg64(int fd, const struct msghdr *msg, int flags) {
  return sendmsg(fd, msg, flags);
}

// --------------------------------------------------------------------- stat

// glibc's internal __stat64_t64. Field order and types copied from
// bits/struct_stat_time64_helper.h in glibc 2.35; the 2.31 sysroot we link
// against has no such struct, which is the whole reason we're here. Natural
// alignment reproduces glibc's padding (notably four bytes before st_blocks),
// so no explicit padding fields are needed.
struct dark_stat64_t64 {
  uint64_t st_dev;
  uint64_t st_ino;
  uint32_t st_mode;
  uint32_t st_nlink;
  uint32_t st_uid;
  uint32_t st_gid;
  uint64_t st_rdev;
  int64_t st_size;
  long st_blksize;
  int64_t st_blocks;
  struct dark_timespec64 st_atim;
  struct dark_timespec64 st_mtim;
  struct dark_timespec64 st_ctim;
};

// Verified against glibc 2.35's armhf headers: 112 bytes, with st_blocks at
// 56 and the three timespecs at 64/80/96. Worth the assert. The header's
// trailing __glibc_reserved4/5 fields live in a branch that isn't taken under
// __USE_XOPEN2K8, and including them made this struct 120 bytes, which
// overran the caller's buffer and tripped the stack protector at runtime.
_Static_assert(sizeof(struct dark_stat64_t64) == 112,
               "struct __stat64_t64 must be 112 bytes on 32-bit ARM");

static void stat_widen(const struct stat64 *in, struct dark_stat64_t64 *out) {
  out->st_dev = in->st_dev;
  out->st_ino = in->st_ino;
  out->st_mode = in->st_mode;
  out->st_nlink = in->st_nlink;
  out->st_uid = in->st_uid;
  out->st_gid = in->st_gid;
  out->st_rdev = in->st_rdev;
  out->st_size = in->st_size;
  out->st_blksize = in->st_blksize;
  out->st_blocks = in->st_blocks;
  ts_widen(&in->st_atim, &out->st_atim);
  ts_widen(&in->st_mtim, &out->st_mtim);
  ts_widen(&in->st_ctim, &out->st_ctim);
}

int __stat64_time64(const char *path, struct dark_stat64_t64 *buf) {
  struct stat64 st;
  int rc = stat64(path, &st);
  if (rc == 0 && buf != NULL) stat_widen(&st, buf);
  return rc;
}

int __lstat64_time64(const char *path, struct dark_stat64_t64 *buf) {
  struct stat64 st;
  int rc = lstat64(path, &st);
  if (rc == 0 && buf != NULL) stat_widen(&st, buf);
  return rc;
}

int __fstat64_time64(int fd, struct dark_stat64_t64 *buf) {
  struct stat64 st;
  int rc = fstat64(fd, &st);
  if (rc == 0 && buf != NULL) stat_widen(&st, buf);
  return rc;
}

int __fstatat64_time64(int dirfd, const char *path,
                       struct dark_stat64_t64 *buf, int flags) {
  struct stat64 st;
  int rc = fstatat64(dirfd, path, &st, flags);
  if (rc == 0 && buf != NULL) stat_widen(&st, buf);
  return rc;
}

// ------------------------------------------------------------ miscellaneous

int __getrusage64(int who, struct dark_rusage64 *out) {
  struct rusage r32;
  int rc;
  if (out == NULL) { errno = EFAULT; return -1; }
  rc = getrusage(who, &r32);
  if (rc != 0) return rc;

  tv_widen(&r32.ru_utime, &out->ru_utime);
  tv_widen(&r32.ru_stime, &out->ru_stime);
  out->ru_maxrss = r32.ru_maxrss;   out->ru_ixrss = r32.ru_ixrss;
  out->ru_idrss = r32.ru_idrss;     out->ru_isrss = r32.ru_isrss;
  out->ru_minflt = r32.ru_minflt;   out->ru_majflt = r32.ru_majflt;
  out->ru_nswap = r32.ru_nswap;     out->ru_inblock = r32.ru_inblock;
  out->ru_oublock = r32.ru_oublock; out->ru_msgsnd = r32.ru_msgsnd;
  out->ru_msgrcv = r32.ru_msgrcv;   out->ru_nsignals = r32.ru_nsignals;
  out->ru_nvcsw = r32.ru_nvcsw;     out->ru_nivcsw = r32.ru_nivcsw;
  return 0;
}

// These three are variadic in glibc, but every argument that matters arrives
// in a register the same way, so a fixed third parameter forwards correctly
// for the commands .NET actually issues. The 64-bit entry points exist
// because a few of their commands take timestamps (F_SETLKW, timerfd ioctls);
// we don't use those.
int __fcntl_time64(int fd, int cmd, void *arg) {
  return fcntl(fd, cmd, arg);
}

int __ioctl_time64(int fd, unsigned long request, void *arg) {
  return ioctl(fd, request, arg);
}

int __prctl_time64(int option, unsigned long a2, unsigned long a3,
                   unsigned long a4, unsigned long a5) {
  return prctl(option, a2, a3, a4, a5);
}
