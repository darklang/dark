/* Typed kernel operations for the interpreter's checked host boundary.
 * No libc, errno TLS, allocation, path resolution, or policy in this library.
 * Results use the compiler's convention: success or negative kernel errno.
 * Operation IDs are private to HostKernel.fs, never a Darklang syscall API.
 */
typedef long long i64;

static long trap(long number, long a, long b, long c, long d, long e, long f)
{
#if defined(__x86_64__)
    register long r10 __asm__("r10") = d;
    register long r8 __asm__("r8") = e;
    register long r9 __asm__("r9") = f;
    long result;
#if defined(__APPLE__)
    unsigned char error;
    __asm__ volatile("syscall; setc %1" : "=a"(result), "=qm"(error)
        : "a"(number < 0 ? (-number | 0x1000000) : (number | 0x2000000)), "D"(a), "S"(b), "d"(c),
          "r"(r10), "r"(r8), "r"(r9) : "rcx", "r11", "memory", "cc");
    return error ? -result : result;
#else
    __asm__ volatile("syscall" : "=a"(result)
        : "a"(number), "D"(a), "S"(b), "d"(c),
          "r"(r10), "r"(r8), "r"(r9) : "rcx", "r11", "memory", "cc");
    return result;
#endif
#elif defined(__aarch64__)
    register long x0 __asm__("x0") = a;
    register long x1 __asm__("x1") = b;
    register long x2 __asm__("x2") = c;
    register long x3 __asm__("x3") = d;
    register long x4 __asm__("x4") = e;
    register long x5 __asm__("x5") = f;
#if defined(__APPLE__)
    register long x16 __asm__("x16") = number;
    __asm__ volatile("svc #0x80; cneg x0, x0, cs" : "+r"(x0)
        : "r"(x1), "r"(x2), "r"(x3), "r"(x4), "r"(x5), "r"(x16)
        : "memory", "cc");
#else
    register long x8 __asm__("x8") = number;
    __asm__ volatile("svc #0" : "+r"(x0)
        : "r"(x1), "r"(x2), "r"(x3), "r"(x4), "r"(x5), "r"(x8)
        : "memory", "cc");
#endif
    return x0;
#elif defined(__arm__) && defined(__linux__)
    register long r0 __asm__("r0") = a;
    register long r1 __asm__("r1") = b;
    register long r2 __asm__("r2") = c;
    register long r3 __asm__("r3") = d;
    register long r4 __asm__("r4") = e;
    register long r5 __asm__("r5") = f;
    register long r7 __asm__("r7") = number;
    __asm__ volatile("svc #0" : "+r"(r0)
        : "r"(r1), "r"(r2), "r"(r3), "r"(r4), "r"(r5), "r"(r7)
        : "memory", "cc");
    return r0;
#else
#error Unsupported syscall architecture
#endif
}

/* Tables follow Linux uapi and XNU syscall ABIs, matching compiler lowering.
 * -1 means there is no such syscall on that platform, not a libc fallback.
 */
#if defined(__APPLE__)
static const short numbers[] = {463,3,4,6,199,470,-1,13,475,472,465,
    467,-1,474,473,131,344,54,37,20,24,-1,-1,-1,95,202,524,
    -1,153,94,189,197,198,517,500,-1,92,-1,116,240,116,-89,-1,-1,-1,230,362,363,92};
#elif defined(__x86_64__)
static const short numbers[] = {257,0,1,3,8,262,79,81,258,263,264,
    268,280,266,267,73,217,16,62,39,102,63,437,452,74,-1,-1,
    326,77,91,5,17,18,-1,318,204,-1,228,96,35,-1,-1,294,254,271,-1,-1,-1,72};
#elif defined(__aarch64__)
static const short numbers[] = {56,63,64,57,62,79,17,50,34,35,38,
    53,88,36,78,32,61,29,129,172,174,160,437,452,82,-1,-1,
    285,46,52,80,67,68,-1,278,123,-1,113,169,101,-1,-1,26,27,73,-1,-1,-1,25};
#elif defined(__arm__)
static const short numbers[] = {322,3,4,6,140,327,183,133,323,328,329,
    333,348,331,332,143,217,54,37,20,24,122,437,452,118,-1,-1,
    391,194,94,197,180,181,-1,384,242,-1,263,78,162,-1,-1,360,292,336,-1,-1,-1,55};
#endif

__attribute__((visibility("default")))
i64 dark_syscall(int operation, i64 a, i64 b, i64 c, i64 d, i64 e, i64 f)
{
    if (operation < 0 || operation >= (int)(sizeof(numbers)/sizeof(numbers[0])))
        return -22;
    long n = numbers[operation];
#if defined(__APPLE__)
    if (operation == 41) {
        /* Mach return codes are not errno. Keep that ABI private. */
        long rc = trap(n, (long)a, 0, 0, 0, 0, 0);
        return rc == 0 ? 0 : -5;
    }
#endif
    if (n < 0) return
#if defined(__APPLE__)
        -78;
#else
        -38;
#endif
#if defined(__arm__) && defined(__linux__)
    /* ARM EABI aligns 64-bit syscall arguments to an even register pair. */
    if (operation == 28)
        return trap(n, (long)a, 0, (long)b,
            (long)((unsigned long long)b >> 32), 0, 0);
    if (operation == 31 || operation == 32)
        return trap(n, (long)a, (long)b, (long)c, 0,
            (long)d, (long)((unsigned long long)d >> 32));
    if (operation == 4) {
        i64 offset = 0;
        long rc = trap(n, (long)a, (long)((unsigned long long)b >> 32),
            (long)b, (long)&offset, (long)c, 0);
        return rc < 0 ? (i64)rc : offset;
    }
#endif
    return (i64)trap(n, (long)a, (long)b, (long)c, (long)d, (long)e, (long)f);
}
