// This program simulates the flow of heat through a two-dimensional plate.
// The number of grid cells used to model the plate as well as the number of
//  iterations to simulate can be specified on the command-line as follows:
//  ./heated_plate_sequential <columns> <rows> <iterations>
// For example, to execute with a 500 x 500 grid for 250 iterations, use:
//  ./heated_plate_sequential 500 500 250


#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <mpi.h>
#include <math.h>
#include <stdbool.h>

// Define the immutable boundary conditions and the inital cell value
#define TOP_BOUNDARY_VALUE 0.0
#define BOTTOM_BOUNDARY_VALUE 100.0
#define LEFT_BOUNDARY_VALUE 0.0
#define RIGHT_BOUNDARY_VALUE 100.0
#define INITIAL_CELL_VALUE 50.0


// Function prototypes
void print_cells(float **cells, int n_x, int n_y);
void initialize_cells(float **cells);
void create_snapshot(float **cells, int n_x, int n_y, int id);
float **allocate_cells(int n_x, int n_y);
void die(const char *error);
void compute (float ***cells, int iterations);
	
int nrank, size, nprocs, rows_per_proc, cols_per_proc, num_rows, num_cols;

bool handles_top_boundary(){
	if(nrank < (int)sqrt (nprocs))
	return true;
	return false;
}

bool handles_left_boundary(){
	if((nrank % (int)sqrt(nprocs)) == 0)
	return true;
	return false;
}

bool handles_bottom_boundary(){
	if (nrank >= (nprocs - (int)sqrt(nprocs)))
	return true;
	return false;
}

bool handles_right_boundary(){
	if (((nrank +1) % (int)sqrt(nprocs)) == 0);
	return true;
	return false;
}

void set_immutable_boundaries(float ***cells){
	int x, y;
	/*if(handles_top_boundary){
		for (x = 0; x < cols_per_proc; x++) cells[0][0][x] = cells[1][0][x] = TOP_BOUNDARY_VALUE;
	}
	if(handles_left_boundary){
		for (y = 0; y < rows_per_proc; y++) cells[0][y][0] = cells[1][y][0] = LEFT_BOUNDARY_VALUE;
	}
	if(handles_bottom_boundary){
		for (x = 0; x < cols_per_proc; x++) cells[0][num_rows + 1][x] = cells[1][num_rows + 1][x] = BOTTOM_BOUNDARY_VALUE;
	}
	if(handles_right_boundary){
		for (y = 0; y < rows_per_proc; y++) cells[0][y][num_cols + 1] = cells[1][y][num_cols + 1] = RIGHT_BOUNDARY_VALUE;
	}*/
	
	if(nrank < (int)sqrt (nprocs))
		for (x = 0; x < cols_per_proc; x++) cells[0][0][x] = cells[1][0][x] = TOP_BOUNDARY_VALUE;

	if((nrank % (int)sqrt(nprocs)) == 0)
		for (y = 0; y < rows_per_proc; y++) cells[0][y][0] = cells[1][y][0] = LEFT_BOUNDARY_VALUE;

	if (nrank >= ((int)nprocs - sqrt(nprocs)))
		for (x = 0; x < cols_per_proc; x++) cells[0][cols_per_proc - 1][x] = cells[1][rows_per_proc - 1][x] = BOTTOM_BOUNDARY_VALUE;

	if (((nrank +1) % (int)sqrt(nprocs)) == 0)
		for (y = 0; y < rows_per_proc; y++) cells[0][y][cols_per_proc - 1] = cells[1][y][cols_per_proc - 1] = RIGHT_BOUNDARY_VALUE;

}

void set_rows_cols_per_proc(){
   	rows_per_proc = num_rows/((int)sqrt(nprocs));
   	cols_per_proc = num_cols/(sqrt(nprocs));
}

void allocate_grid(float ***cells){
	/*if(handles_top_boundary){
		rows_per_proc+=1;
	}
	else if(handles_bottom_boundary){
		rows_per_proc+=1;
	}
	if(handles_left_boundary){
		cols_per_proc+=1;
	}else if(handles_right_boundary){
		cols_per_proc+=1;
	}*/

	if(nrank < (int)sqrt (nprocs))
		rows_per_proc+=1;

	if((nrank % (int)sqrt(nprocs)) == 0)
		cols_per_proc+=1;

	if (nrank >= ((int)nprocs - sqrt(nprocs)))
		rows_per_proc+=1;

	if (((nrank +1) % (int)sqrt(nprocs)) == 0)
		cols_per_proc+=1;

//printf("no of rows and cols is %d, %d", rows_per_proc, cols_per_proc);
	cells[0] = allocate_cells(cols_per_proc, rows_per_proc);
	cells[1] = allocate_cells(cols_per_proc, rows_per_proc);
}

void initialize_cells(float **cells) {
	//why does rows per sec need -1. Check this
	int x, y, x_start = 0, y_start = 0, x_end = rows_per_proc -1 , y_end = cols_per_proc -1;
	if(nrank < (int)sqrt (nprocs))
		y_start=1;

	if((nrank % (int)sqrt(nprocs)) == 0)
		x_start=1;

	if (nrank >= ((int)nprocs - sqrt(nprocs)))
		y_end-=1;

	if (((nrank +1) % (int)sqrt(nprocs)) == 0)
		x_end-=1;

	/*if(handles_top_boundary){
		y_start=1;
	}
	if(handles_left_boundary){
		x_start=1;
	}
	if(handles_bottom_boundary){
		y_end-=1;
	}
	if(handles_right_boundary){
		x_end-=1;
	}
*/
	for (y = y_start; y < y_end; y++) {
		for (x = x_start; x < x_end; x++) {
			cells[y][x] = INITIAL_CELL_VALUE;
		}
	}
}

int main(int argc, char **argv) {

	MPI_Init( &argc, &argv );
	MPI_Comm_size( MPI_COMM_WORLD, &nprocs );
	MPI_Comm_rank( MPI_COMM_WORLD, &nrank );
	time_t start_time = time(NULL);

	num_cols = (argc > 1) ? atoi(argv[1]) : 1000;
	num_rows = (argc > 2) ? atoi(argv[2]) : 1000;
	int iterations = (argc > 3) ? atoi(argv[3]) : 100;
		
	set_rows_cols_per_proc();

/*	if(nrank < (int)sqrt (nprocs))
	printf ("Rank %d handles topi\n", nrank);

	if((nrank % (int)sqrt(nprocs)) == 0)
	printf ("Rank %d handles left\n", nrank);

	if (nrank >= ((int)nprocs - sqrt(nprocs)))
	printf ("Rank %d handles bottom\n", nrank);

	if (((nrank +1) % (int)sqrt(nprocs)) == 0)
	printf ("Rank %d handles right\n", nrank);
*/
	//printf("Grid: %dx%d, Iterations: %d\n", num_cols, num_rows, iterations);
	
	float **cells[2];
	allocate_grid(cells);
	initialize_cells(cells[0]);
	initialize_cells(cells[1]);
printf("no of rows and cols is %d, %d for rank %d", rows_per_proc, cols_per_proc, nrank);

	set_immutable_boundaries(cells);
	compute(cells, iterations);
	// Output a snapshot of the final state of the plate
	int final_cells = (iterations % 2 == 0) ? 0 : 1;
//	create_snapshot(cells[final_cells], num_cols, num_rows, iterations);

	// Compute and output the execution time
	time_t end_time = time(NULL);
	printf("\nExecution time: %d seconds\n", (int) difftime(end_time, start_time));
	MPI_Finalize();

	return 0;
}

void compute (float ***cells, int iterations){
	int cur_cells_index = 0, next_cells_index = 1;
	int x,y,i;	
	for (i = 0; i < iterations; i++) {
		//Check this why is it -2
		for (y = 1; y <= rows_per_proc - 2; y++) {
			for (x = 1; x <= cols_per_proc - 1; x++) {
				// The new value of this cell is the average of the old values of this cell's four neighbors
				cells[next_cells_index][y][x] = (cells[cur_cells_index][y][x - 1]  +
						cells[cur_cells_index][y][x + 1]  +
						cells[cur_cells_index][y - 1][x]  +
						cells[cur_cells_index][y + 1][x]) * 0.25;
			}
		}

		// Swap the two arrays
		cur_cells_index = next_cells_index;
		next_cells_index = !cur_cells_index;

		// Print the current progress
		printf("Iteration: %d / %d\n", i + 1, iterations);
	}

}
// Allocates and returns a pointer to a 2D array of floats
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


// Creates a snapshot of the current state of the cells in PPM format.
// The plate is scaled down so the image is at most 1,000 x 1,000 pixels.
// This function assumes the existence of a boundary layer, which is not
//  included in the snapshot (i.e., it assumes that valid array indices
//  are [1..num_rows][1..num_cols]).
void create_snapshot(float **cells, int num_cols, int num_rows, int id) {
	int scale_x, scale_y;
	scale_x = scale_y = 1;

	// Figure out if we need to scale down the snapshot (to 1,000 x 1,000)
	//  and, if so, how much to scale down
	if (num_cols > 1000) {
		if ((num_cols % 1000) == 0) scale_x = num_cols / 1000;
		else {
			die("Cannot create snapshot for x-dimensions >1,000 that are not multiples of 1,000!\n");
			return;
		}
	}
	if (num_rows > 1000) {
		if ((num_rows % 1000) == 0) scale_y = num_rows / 1000;
		else {
			printf("Cannot create snapshot for y-dimensions >1,000 that are not multiples of 1,000!\n");
			return;
		}
	}

	// Open/create the file
	char text[255];
	sprintf(text, "snapshot.%d.ppm", id);
	FILE *out = fopen(text, "w");
	// Make sure the file was created
	if (out == NULL) {
		printf("Error creating snapshot file!\n");
		return;
	}

	// Write header information to file
	// P3 = RGB values in decimal (P6 = RGB values in binary)
	fprintf(out, "P3 %d %d 100\n", num_cols / scale_x, num_rows / scale_y);

	// Precompute the value needed to scale down the cells
	float inverse_cells_per_pixel = 1.0 / ((float) scale_x * scale_y);

	// Write the values of the cells to the file
	int x, y, i, j;
	for (y = 1; y <= num_rows; y += scale_y) {
		for (x = 1; x <= num_cols; x += scale_x) {
			float sum = 0.0;
			for (j = y; j < y + scale_y; j++) {
				for (i = x; i < x + scale_x; i++) {
					sum += cells[j][i];
				}
			}
			// Write out the average value of the cells we just visited
			int average = (int) (sum * inverse_cells_per_pixel);
			fprintf(out, "%d 0 %d\t", average, 100 - average);
		}
		fwrite("\n", sizeof(char), 1, out);
	}

	// Close the file
	fclose(out);
}


// Prints the specified error message and then exits
void die(const char *error) {
	printf("%s", error);
	exit(1);
}
