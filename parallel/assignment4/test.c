#include <stdio.h>
#include <stdio.h>
#include <time.h>
#include "mpi.h"

#define TOP_BOUNDARY_VALUE 0.0
#define BOTTOM_BOUNDARY_VALUE 100.0
#define LEFT_BOUNDARY_VALUE 0.0
#define RIGHT_BOUNDARY_VALUE 100.0
#define INITIAL_CELL_VALUE 50.0

void create_snapshot(float **cells, int n_x, int n_y, int id);
float **allocate_cells(int n_x, int n_y);
void die(const char *error);

int main( argc, argv )
int  argc;
char **argv;
{
    int num_cols = (argc > 1) ? atoi(argv[1]) : 1000;  
    int num_rows = (argc > 2) ? atoi(argv[2]) : 1000;
    int iterations = (argc > 3) ? atoi(argv[3]) : 100;
	
    int rank, size;
    MPI_Init( &argc, &argv );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    float **cells[2];
    cells[0] = allocate_cells(num_cols + 2, num_rows + 2);
    cells[1] = allocate_cells(num_cols + 2, num_rows + 2);
    int cur_cells_index = 0, next_cells_index = 1;

    set_immutable_conditions(cells);

    printf( "Hello world from process %d of %d\n", rank, size );
    MPI_Finalize();
    return 0;
}

void set_immutable_conditions(float ***cells, int num_rows, int num_cols){
	int x, y, i;
	for (x = 1; x <= num_cols; x++) cells[0][0][x] = cells[1][0][x] = TOP_BOUNDARY_VALUE;
	for (x = 1; x <= num_cols; x++) cells[0][num_rows + 1][x] = cells[1][num_rows + 1][x] = BOTTOM_BOUNDARY_VALUE;
	for (y = 1; y <= num_rows; y++) cells[0][y][0] = cells[1][y][0] = LEFT_BOUNDARY_VALUE;
	for (y = 1; y <= num_rows; y++) cells[0][y][num_cols + 1] = cells[1][y][num_cols + 1] = RIGHT_BOUNDARY_VALUE;
	
}

float **allocate_cells(int num_cols, int num_rows) {
	float **array = (float **) malloc(num_rows * sizeof(float *));
	if (array == NULL) die("Error allocating array!\n");
	
	array[0] = (float *) malloc(num_rows * num_cols * sizeof(float));
	if (array[0] == NULL) die("Error allocating array!\n");

	int i;
	for (i = 1; i < num_rows; i++) {
		array[i] = array[0] + (i * num_cols);
	}

	return array;
}

void die(const char *error) {
	printf("%s", error);
	exit(1);
}
