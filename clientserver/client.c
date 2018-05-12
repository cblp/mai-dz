#include <netinet/in.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#include "common.h"

int main() {
    const Request req = {.id = 42, .a = 4, .b = 25};

    int s = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

    struct sockaddr_in addr = {
        .sin_family = AF_INET,
        .sin_addr.s_addr = htonl(INADDR_LOOPBACK),
        .sin_port = htons(Port),
    };
    ASSERT_OK(connect(s, (struct sockaddr *) &addr, sizeof(addr)));
    fprintf(stderr, "connected\n");

    ASSERT(send(s, &req, sizeof(Request), 0) == sizeof(Request));
    fprintf(stderr, "request sent\n");

    Response res;
    const ssize_t reslen = recv(s, &res, sizeof(Response), 0);
    ASSERT(reslen == sizeof(Response));
    fprintf(stderr, "response: %d\n", res.r);

    close(s);
}
