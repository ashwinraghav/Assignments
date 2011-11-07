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
	
int nrank, size, nprocs, rows_per_proc, cols_per_proc, num_rows, num_cols, iterations;

bool contains_top_boundary(){
	if(nrank < ((int)sqrt(nprocs)))	return true;
	return false;
}

bool contains_left_boundary(){
	return((nrank % ((int)sqrt(nprocs))) == 0);
}

bool contains_bottom_boundary(){
	return (nrank >= (nprocs - (int)sqrt(nprocs)));
}

bool contains_right_boundary(){
	return (((nrank +1) % ((int)sqrt(nprocs))) == 0);
}

void set_immutable_boundaries(float ***cells){
	int x, y;
	
	if(contains_top_boundary())
		for (x = 0; x < cols_per_proc; x++) cells[0][0][x] = cells[1][0][x] = TOP_BOUNDARY_VALUE;
	if(contains_left_boundary())
		for (y = 0; y < rows_per_proc; y++) cells[0][y][0] = cells[1][y][0] = LEFT_BOUNDARY_VALUE;

	if (contains_bottom_boundary())
		for (x = 0; x < cols_per_proc; x++) cells[0][cols_per_proc - 1][x] = cells[1][rows_per_proc - 1][x] = BOTTOM_BOUNDARY_VALUE;
	if (contains_right_boundary())
		for (y = 0; y < rows_per_proc; y++) cells[0][y][cols_per_proc - 1] = cells[1][y][cols_per_proc - 1] = RIGHT_BOUNDARY_VALUE;

}

void set_rows_cols_per_proc(){
   	rows_per_proc = num_rows/((int)sqrt(nprocs));
   	cols_per_proc = num_cols/(sqrt(nprocs));
}

void allocate_grid(float ***cells){
	cells[0] = allocate_cells(cols_per_proc + 2 , rows_per_proc + 2);
	cells[1] = allocate_cells(cols_per_proc + 2 , rows_per_proc + 2);
}

void initialize_cells(float **cells) {
	int x, y, x_start = 0, y_start = 0, x_end = rows_per_proc , y_end = cols_per_proc;

	if(contains_top_boundary())
		y_start=1;

	if(contains_left_boundary())
		x_start=1;

	if (contains_bottom_boundary())
		y_end-=1;

	if (contains_right_boundary())
		x_end-=1;

	for (y = y_start; y < y_end; y++) {
		for (x = x_start; x < x_end; x++) {
			cells[y][x] = INITIAL_CELL_VALUE;
		}
	}
}

void collate_for_display(float ***cells, float **final_cells){
	int i, j, k;
	for(k=0; k < nprocs; k++){
		for(i=0; i < rows_per_proc; i++){
			for (j=0;j<cols_per_proc;j++){
				int actual_row = i + (rows_per_proc * (int)(k/sqrt(nprocs)));
				int actual_col = j + (cols_per_proc * (int)(k/sqrt(nprocs)));
				// change final_cells[0][0] = cells[k][i][j];
			}
		}
	} 
}

void gather_all_cells(float ***cells, float **cells_to_render){
	int i;
	MPI_Status status;
	for (i = 0; i < nprocs; i++){
		MPI_Recv(cells[0], cols_per_proc, MPI_FLOAT, i, iterations + 1, MPI_COMM_WORLD, &status);
	}
	collate_for_display(cells, cells_to_render);
}

void set_immutable_boundaries_for_final_cells(float ***cells, int num_rows, int num_cols){
        int x, y, i;
        for (x = 1; x <= num_cols; x++) cells[0][0][x] = TOP_BOUNDARY_VALUE;
        for (x = 1; x <= num_cols; x++) cells[0][num_rows + 1][x] = BOTTOM_BOUNDARY_VALUE;
        for (y = 1; y <= num_rows; y++) cells[0][y][0] = LEFT_BOUNDARY_VALUE;
        for (y = 1; y <= num_rows; y++) cells[0][y][num_cols + 1] = RIGHT_BOUNDARY_VALUE;
}

int main(int argc, char **argv) {
	MPI_Init( &argc, &argv );
	MPI_Comm_size( MPI_COMM_WORLD, &nprocs );
	MPI_Comm_rank( MPI_COMM_WORLD, &nrank );
	time_t start_time;
	if (nrank ==0){
		start_time = time(NULL);
	}
	num_cols = (argc > 1) ? atoi(argv[1]) : 1000;
	num_rows = (argc > 2) ? atoi(argv[2]) : 1000;
	iterations = (argc > 3) ? atoi(argv[3]) : 1000;
	//printf("Grid: %dx%d, Iterations: %d\n", num_cols, num_rows, iterations);
	float **cells[2];
	
	set_rows_cols_per_proc();
	allocate_grid(cells);
	initialize_cells(cells[0]);
	initialize_cells(cells[1]);
	set_immutable_boundaries(cells);
	//printf("no of rows and cols is %d, %d for rank %d", rows_per_proc, cols_per_proc, nrank);


	compute(cells, iterations);

	if(nrank==0){
		int i;
		float **gathered_cells[nprocs];
		float **cells_to_render[1];
		cells_to_render[0] = allocate_cells(num_cols+2, num_rows+2);
		for(i = 0; i < nprocs ; i++){gathered_cells[i] = allocate_cells(num_cols+2, num_rows+2);}
		set_immutable_boundaries_for_final_cells(cells_to_render, num_rows, num_cols);
		gather_all_cells(gathered_cells, cells_to_render[0]);
		// Output a snapshot of the final state of the plate
		create_snapshot(cells_to_render[0], num_cols, num_rows, iterations);
		// Compute and output the execution time
		time_t end_time = time(NULL);
		printf("\nExecution time: %d seconds\n", (int) difftime(end_time, start_time));
	}

	MPI_Finalize();

	return 0;
}

void send_bottom_row(float ***cells, int next_cells_index, int i){
	MPI_Request request;
	if (!contains_bottom_boundary()){
		MPI_Isend((void*)cells[next_cells_index][rows_per_proc], cols_per_proc -1, MPI_FLOAT, nrank + (int)sqrt(nprocs), i-1, MPI_COMM_WORLD, &request);
	}	
}
void receive_top_row(float ***cells, int next_cells_index, int i){
	MPI_Status status;
	if(!contains_top_boundary()){
		MPI_Recv(cells[next_cells_index][0], cols_per_proc -1, MPI_FLOAT, nrank - sqrt(nprocs), i-1, MPI_COMM_WORLD, &status);
	}
}

void send_top_row(float ***cells, int next_cells_index, int i){
	MPI_Request request;
	if(!contains_top_boundary()){
		MPI_Isend(cells[next_cells_index][0], cols_per_proc -1, MPI_FLOAT, nrank - (int)sqrt(nprocs), i -1 , MPI_COMM_WORLD, &request);
	}
}

void receive_bottom_row(float ***cells, int next_cells_index, int i){
	MPI_Status status;
	if (!contains_bottom_boundary()){
		MPI_Recv(cells[next_cells_index][rows_per_proc + 1], cols_per_proc -1, MPI_FLOAT, nrank + sqrt(nprocs), i -1, MPI_COMM_WORLD, &status);
	}
}

void send_right_column(float ***cells, int next_cells_index, int i, MPI_Datatype new_type){
	MPI_Request request;
	if (!contains_right_boundary()){
		MPI_Isend(cells[next_cells_index][0] + cols_per_proc, 1, new_type, nrank + 1, i - 1 , MPI_COMM_WORLD, &request);
	}
}

void receive_left_column(float ***cells, int next_cells_index, int i, MPI_Datatype new_type){
	MPI_Status status;
	if (!contains_left_boundary()){
		MPI_Recv(cells[next_cells_index][0], 1, new_type, nrank - 1, i - 1, MPI_COMM_WORLD, &status);
	}
}

void send_left_column(float ***cells, int next_cells_index, int i, MPI_Datatype new_type){
	MPI_Request request;
	if (!contains_left_boundary()){
		MPI_Isend(cells[next_cells_index][0], 1, new_type, nrank - 1, i - 1 , MPI_COMM_WORLD, &request);
	}
}

void receive_right_column(float ***cells, int next_cells_index, int i, MPI_Datatype new_type){
	MPI_Status status;
	MPI_Request request;
	if (!contains_right_boundary()){
		MPI_Recv(cells[next_cells_index][0] + cols_per_proc + 1, 1, new_type, nrank + 1, i - 1, MPI_COMM_WORLD, &status);
	}
}

void send_all_rows_to_proc_0(float ***cells, int next_cells_index){
	printf("sending from %d\n", nrank);
	MPI_Request request;
	MPI_Isend(cells[next_cells_index][1], cols_per_proc, MPI_FLOAT, 0, iterations + 1, MPI_COMM_WORLD, &request);
}

void create_column_vector_type(MPI_Datatype *new_type){
	MPI_Type_vector(rows_per_proc, 1, sqrt(nprocs), MPI_FLOAT, new_type);
 	MPI_Type_commit(new_type);
}

void free_column_type_vector(MPI_Datatype *new_type){
	MPI_Type_free(new_type);
}


void compute (float ***cells, int iterations){
	int cur_cells_index = 0, next_cells_index = 1;
	int x,y,i,j;	

	MPI_Datatype new_type;
	create_column_vector_type(&new_type);

	for (i = 1; i <= iterations; i++) {
		send_bottom_row(cells, next_cells_index, i);
		send_top_row(cells, next_cells_index, i);
		send_right_column(cells, next_cells_index, i, new_type);
		send_left_column(cells, next_cells_index, i, new_type);
		receive_top_row(cells, next_cells_index, i);
		receive_bottom_row(cells, next_cells_index, i);
		receive_left_column(cells, next_cells_index, i, new_type);
		receive_right_column(cells, next_cells_index, i, new_type);


		for (y = 1; y <= rows_per_proc; y++) {
			for (x = 1; x <= cols_per_proc; x++) {
				for(j=0;j<4;j++){
					cells[next_cells_index][y][x] = (cells[cur_cells_index][y][x - 1]  +
							cells[cur_cells_index][y][x + 1]  +
							cells[cur_cells_index][y - 1][x]  +
							cells[cur_cells_index][y + 1][x]) * 0.25;
				}
			}
		}

		// Swap the two arrays
		cur_cells_index = next_cells_index;
		next_cells_index = !cur_cells_index;

		// Print the current progress
	//	printf("Iteration: %d / %d\n", i + 1, iterations);
	}
	free_column_type_vector(&new_type);
	send_all_rows_to_proc_0(cells, next_cells_index);

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
