#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

#include "common.h"

int main(void) {
    int s = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

    struct sockaddr_in addr = {
        .sin_family = AF_INET,
        .sin_addr.s_addr = htonl(INADDR_ANY),
        .sin_port = htons(Port),
    };
    ASSERT_OK(bind(s, (struct sockaddr *) &addr, sizeof(addr)));

    ASSERT_OK(listen(s, 10));
    fprintf(stderr, "server started\n");

    for (;;) {
        int c = accept(s, NULL, NULL);
        fprintf(stderr, "connection accepted\n");

        Request req;
        const ssize_t reqlen = recv(c, &req, sizeof(Request), 0);
        ASSERT(reqlen == sizeof(Request));
        fprintf(stderr, "request received\n");

        Response res = {
            .id = req.id,
            .r = req.a * req.b,
        };

        ASSERT(
            send(c, &res, sizeof(Response), 0) == sizeof(Response)
        );
        fprintf(stderr, "response sent\n");

		close(c);
        fprintf(stderr, "connection closed\n");
    }
}
