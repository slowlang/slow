#include <sys/errno.h>
#include <unistd.h>

#include <string.h>

int main(int argc, char *argv[]) {
    for (int i = 0; i < argc; i++) {
        int l = strlen(argv[i]);

        int r = write(1, argv[i], l);
        if (r < 0) {
            return 0-r;
        }
    }

    return 0;
}
