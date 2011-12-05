#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <cuda.h>

//the dimension is hard-coded for readability
#define SIZE 10000

#define BLOCK_SIZE 16
#define ITERATIONS 10000

#define TOP_BOUNDARY_VALUE 0.0
#define BOTTOM_BOUNDARY_VALUE 100.0
#define LEFT_BOUNDARY_VALUE 0.0
#define RIGHT_BOUNDARY_VALUE 100.0
#define INITIAL_CELL_VALUE 50.0

void create_snapshot(float **cells, int n_x, int n_y, int id);
void print_matrix(float**u);
float **allocate_cells(int num_cols, int num_rows);

/*
	kernel funtion
	accepts 2 1D arrays that will be used alternatingly in iterations
*/
__global__ void jacobi(float *x, float *y)
{
	float p, q, r, s, *d_u_new[2];
	int k, next_index=0;
	d_u_new[0] = x;
	d_u_new[1] = y;

	/*Calculate the localized target cells*/
	int tx = threadIdx.x;
	int ty = threadIdx.y;

	int i = blockIdx.x*blockDim.x + tx;
	int j = blockIdx.y*blockDim.y + ty;
	int target = i*SIZE+j;
	/*************************************/

	/*
		Each thread copied its corresponding value
 		in global memory to Shared Memory.
		All threads are synced after the copy.
	*/
	__shared__ float shared_cells[BLOCK_SIZE][BLOCK_SIZE][2];
	shared_cells[tx][ty][0] = shared_cells[tx][ty][1] = d_u_new[0][target];
	__syncthreads();
	/*************************************/


	bool is_boundary_cell =  ((target<SIZE)||
				(target%SIZE==0)||
				(target>=SIZE*(SIZE-1))||
				(target%SIZE==(SIZE-1)));
	
	for (k = 0; k < ITERATIONS; k++){
		/*boundary cells are immutable*/
		if(!is_boundary_cell)
		{
			/* if the cell required for relaxation is outside the block
			   pick from global memory, else pick from shared memory*/
			if(tx-1 < 0){
				p = d_u_new[next_index][(i - 1) * SIZE + j];
			}
			else{
				p = shared_cells[tx - 1][ty][next_index];
			}
			if(tx+1 == BLOCK_SIZE){
				q = d_u_new[next_index][(i + 1) * SIZE + j];
			}
			else{
				q = shared_cells[tx + 1][ty][next_index];
			}
			if(ty-1 < 0){
				r = d_u_new[next_index][i * SIZE + j - 1];
			}
			else{
				r = shared_cells[tx][ty - 1][next_index];
			}
			if(ty+1 == BLOCK_SIZE){
				s = d_u_new[next_index][i * SIZE + j + 1];
			}
			else{
				s = shared_cells[tx][ty + 1][next_index];
			}
			/*************************************/
			next_index = (next_index + 1) % 2;
			
			/* 
			   if the computed cell is a boundary 
			   cell, write to shared & global memory
			   else, write only to shared memory
			*/
			if((tx - 1 < 0) || (tx + 1 == BLOCK_SIZE) || (ty - 1 < 0) || (ty + 1 == BLOCK_SIZE)){
				d_u_new[next_index][target] = 0.25 * (p + q + r + s);
			}
			shared_cells[tx][ty][next_index] = 0.25 * (p + q + r + s);
			/*************************************/
			
		}
		/*synchronize after every iteration*/
		__syncthreads();
	}
	/* 
	   After iterations, each thread writes
	   the final computed value back to 
	   global memory for host accessibility
	*/
	if(!truth){
		d_u_new[0][target] = 0.25 * (p + q + r + s);
	}
}

int main()
{
	float *cells[2], *cells_gpu[2], **steady_state;
	int i,j;
	size_t size;
	float h = 1.0/SIZE;
	time_t start_time = time(NULL);

	//The 2D decomposition matrix is flattened
	size=SIZE*SIZE*sizeof(float);
	cells[0]    = (float*)malloc(size);
	cells[1]    = (float*)malloc(size);
	
	//final matrix to generate ppm
	steady_state = allocate_cells(SIZE, SIZE);
	
	//allocate global memory
	cudaMalloc(&cells_gpu[0],size);
	cudaMalloc(&cells_gpu[1],size);

	//initialize cell values on host before copy*/
	for(i=0;i<SIZE;i++)
	{
		for(j=0;j<SIZE;j++)
		{
			cells[0][i*SIZE+j] = cells[1][i*SIZE+j] = INITIAL_CELL_VALUE;
		}
	}

	for(i=0;i<SIZE;i++)
	{
		cells[0][i] = cells[1][i] = TOP_BOUNDARY_VALUE;
		cells[0][i*SIZE] = cells[1][i*SIZE] = LEFT_BOUNDARY_VALUE;
		cells[0][SIZE*(SIZE-1)+i] = cells[1][SIZE*(SIZE-1)+i] = BOTTOM_BOUNDARY_VALUE;
		cells[0][i*SIZE+SIZE-1] = cells[1][i*SIZE+SIZE-1] = RIGHT_BOUNDARY_VALUE;
	}
	/*************************************/

	//copy decomposition matrix to CUDA global memory*/
	cudaMemcpy(cells_gpu[0], cells[0], size, cudaMemcpyHostToDevice);
	cudaMemcpy(cells_gpu[1], cells[1], size, cudaMemcpyHostToDevice);
	/*************************************/

	/* 
	   regulate the dimentions of the block
	   and the grid containing the blocks
	   ****Ugly code omitted******
	   
	*/
	dim3 dimBlock(BLOCK_SIZE,BLOCK_SIZE);
	dim3 dimGrid(SIZE/BLOCK_SIZE,SIZE/BLOCK_SIZE);
	/*************************************/
	
	/*Kernel queuing*/
	jacobi<<<dimGrid,dimBlock>>>(cells_gpu[0], cells_gpu[1]);
	//note that all threads are synchronized internally
	
	/* 
	   copy final values to device from global memory
	   and unflatten the 1D array for ppm generation
	*/
	cudaMemcpy(cells[0], cells_gpu[0], size, cudaMemcpyDeviceToHost);

	for(i=0;i<SIZE;i++)
	{
		for(j=0; j < SIZE; j++){
			steady_state[i][j] = cells[0][i*SIZE+j];
		}
	}
	create_snapshot(cells, SIZE-2, SIZE-2, ITERATIONS);
	/*************************************/
	
	time_t end_time = time(NULL);
	printf("\nExecution time: %d seconds\n", (int) difftime(end_time, start_time));

	//Free all allocated memory
	free(cells[0]);
	free(cells[1]);
	cudaFree(cells_gpu[0]);
	cudaFree(cells_gpu[1]);

}

float **allocate_cells(int num_cols, int num_rows) {
	float **array = (float **) malloc(num_rows * sizeof(float *));
	
	array[0] = (float *) malloc(num_rows * num_cols * sizeof(float));

	int i;
	for (i = 1; i < num_rows; i++) {
		array[i] = array[0] + (i * num_cols);
	}

	return array;
}

void create_snapshot(float **cells, int num_cols, int num_rows, int id) {
	//Code omitted
}

//for debugging
void print_matrix(float**u)
{
	//Code omitted
}
