#include <netinet/in.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <unistd.h>

#include "common.h"

void handle(int client) {
    Request req;
    const ssize_t reqlen = recv(client, &req, sizeof(Request), 0);
    ASSERT(reqlen == sizeof(Request));
    fprintf(stderr, "request received\n");

    Response res = {
        .id = req.id,
        .r = req.a * req.b,
    };

    ASSERT(send(client, &res, sizeof(Response), 0) == sizeof(Response));
    fprintf(stderr, "response sent\n");
}

int main(void) {
    int server = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

    struct sockaddr_in addr = {
        .sin_family = AF_INET,
        .sin_addr.s_addr = htonl(INADDR_ANY),
        .sin_port = htons(Port),
    };
    ASSERT_OK(bind(server, (struct sockaddr *) &addr, sizeof(addr)));

    ASSERT_OK(listen(server, 10));
    fprintf(stderr, "server started\n");

    fd_set active_fd_set, read_fd_set;
    FD_ZERO(&active_fd_set);
    FD_SET(server, &active_fd_set);

    for (;;) {
        read_fd_set = active_fd_set;
        ASSERT_OK(select(FD_SETSIZE, &read_fd_set, NULL, NULL, NULL) < 0);

        for (int s = 0; s < FD_SETSIZE; ++s) {
            if (FD_ISSET(s, &read_fd_set)) {
                if (s == server) {
                    /* Connection request on original socket. */
                    const int client = accept(server, NULL, NULL);
                    ASSERT(client >= 0);
                    FD_SET(client, &active_fd_set);
                } else {
                    /* Data arriving on an already-connected socket. */
                    handle(s);
                    close(s);
                    FD_CLR(s, &active_fd_set);
                    // fprintf(stderr, "connection closed\n");
                }
            }
        }
    }
}
