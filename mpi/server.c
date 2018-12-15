#include <mpi.h>
#include <stdio.h>

int main(int argc, char ** argv) {
    MPI_Init(&argc, &argv);

    int numproc;
    MPI_Comm_size(MPI_COMM_WORLD, &numproc);
    // printf("numproc = %d\n", numproc);

    int id;
    MPI_Comm_rank(MPI_COMM_WORLD, &id);
    // printf("id = %d\n", id);

    long n = 0;
    if (id == 0) {
        n = 1000000000;
    }

    MPI_Bcast(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);

    const double a = 0, b = 1, h = 1.0 * (b - a) / n;
    double local_sum = 0;
    for (int i = id + 1; i <= n; i += numproc) {
        double x = (0.5 + i) * h;
        local_sum += 4.0 / (1 + x * x);
    }
    local_sum *= h;

    double pi;
    MPI_Reduce(
        &local_sum, &pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD
    );

    if (id == 0) {
        printf("pi = %f\n", pi);
    }

    return MPI_Finalize();
}
