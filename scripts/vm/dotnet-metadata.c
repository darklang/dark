/* Build/test-only adaptation for Work VMs without procfs or /tmp.
 * NEVER link this into the interpreter or use it for profiling measurements.
 * Only current-process identity/start-time metadata is synthesized. Other
 * paths, processes and permission failures are delegated unchanged.
 */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>

static int metadata(const char *path, int flags) {
    if (!path) return -2;
    char expected[64];
    snprintf(expected, sizeof(expected), "/proc/%ld/stat", (long)getpid());
    int process = !strcmp(path, "/proc/self/stat") || !strcmp(path, expected);
    int boot = !strcmp(path, "/proc/stat");
    if (!process && !boot) return -2;
    if ((flags & O_ACCMODE) != O_RDONLY) { errno = EACCES; return -1; }
    char text[2048];
    size_t used = 0;
    if (boot) {
        used = snprintf(text, sizeof(text), "btime %ld\n", (long)time(NULL) - 1);
    } else {
        used = snprintf(text, sizeof(text), "%ld (dotnet) R", (long)getpid());
        for (int field = 4; field <= 52; ++field) {
            long value = field == 18 ? 20 : field == 20 || field == 22 ? 1
                : field == 23 ? 4096 : 0;
            used += snprintf(text + used, sizeof(text) - used, " %ld", value);
        }
        text[used++] = '\n';
    }
    int fd = memfd_create("work-vm-process-metadata", MFD_CLOEXEC);
    if (fd < 0) return -1;
    if (write(fd, text, used) != (ssize_t)used || lseek(fd, 0, SEEK_SET) < 0) {
        int error = errno; close(fd); errno = error; return -1;
    }
    return fd;
}

int open(const char *path, int flags, ...) {
    mode_t mode = 0;
    if ((flags & O_CREAT) || (flags & O_TMPFILE) == O_TMPFILE) {
        va_list args; va_start(args, flags); mode = va_arg(args, int); va_end(args);
    }
    int fd = metadata(path, flags);
    if (fd != -2) return fd;
    static int (*real)(const char *, int, ...);
    if (!real) real = dlsym(RTLD_NEXT, "open");
    return real(path, flags, mode);
}

int open64(const char *path, int flags, ...) {
    mode_t mode = 0;
    if ((flags & O_CREAT) || (flags & O_TMPFILE) == O_TMPFILE) {
        va_list args; va_start(args, flags); mode = va_arg(args, int); va_end(args);
    }
    int fd = metadata(path, flags);
    if (fd != -2) return fd;
    static int (*real)(const char *, int, ...);
    if (!real) real = dlsym(RTLD_NEXT, "open64");
    return real(path, flags, mode);
}

char *mkdtemp(char *template) {
    static char *(*real)(char *);
    if (!real) real = dlsym(RTLD_NEXT, "mkdtemp");
    const char *root = getenv("PORT_VM_TMPDIR");
    if (!root || strncmp(template, "/tmp/", 5)) return real(template);
    char mapped[PATH_MAX];
    int length = snprintf(mapped, sizeof(mapped), "%s%s", root, template + 4);
    if (length < 0 || (size_t)length >= sizeof(mapped)) {
        errno = ENAMETOOLONG; return NULL;
    }
    if (!real(mapped)) return NULL;
    /* CLR's caller uses its input, not the returned pointer. Other VM hooks
       map that /tmp input back to the workspace for chmod/open/etc. */
    size_t original = strlen(template);
    memcpy(template + original - 6, mapped + length - 6, 6);
    return template;
}
