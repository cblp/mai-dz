set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Werror -pedantic")
add_executable(server server.c)
target_link_libraries(server mpi)
