set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Werror -pedantic")
add_executable(client client.c)
add_executable(server server.c)
