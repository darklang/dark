#include <ev.h>

int main(int arc, char *argv[])
{
    struct ev_loop *loop = ev_loop_new(EVFLAG_FORKCHECK);
    return 0;
}
