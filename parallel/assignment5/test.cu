#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <cuda.h>
#define SIZE 8092
#define BLOCK_SIZE 16
#define ITERATIONS 10000

#define TOP_BOUNDARY_VALUE 0.0
#define BOTTOM_BOUNDARY_VALUE 100.0
#define LEFT_BOUNDARY_VALUE 0.0
#define RIGHT_BOUNDARY_VALUE 100.0
#define INITIAL_CELL_VALUE 50.0

void create_snapshot(float **cells, int n_x, int n_y, int id);

float ratio(float**u,float ant,int iter)
{
	float tmp=0.0;
	int i,j;
	for(i=0;i<SIZE;i++)
	{
		for(j=0;j<SIZE;j++)
		{
			if(u[i][j]>tmp)
				tmp=u[i][j];
		}
	}
	if(iter%10==0)
		printf(" iter=%d ratio=%f max=%f\n",iter,tmp/ant,tmp);
	return tmp;
}

void print_matrix(float**u)
{
	int i,j;
	for(i=0;i<SIZE;i++)
	{
		for(j=0;j<SIZE;j++)
		{
			printf("%f ",u[i][j]);
		}
		printf("\n");
	}
}
__global__ void jacobi(float *x, float *y, int iteration)
{
	float p, q, r, s, *d_u_new[2];
	d_u_new[0] = x;
	d_u_new[1] = y;

	int tx = threadIdx.x;
	int ty = threadIdx.y;

	int i = blockIdx.x*blockDim.x + tx;
	int j = blockIdx.y*blockDim.y + ty;
	int k, next_index=0;

	int target = i*SIZE+j;

	__shared__ float shared_cells[BLOCK_SIZE][BLOCK_SIZE][2];
	shared_cells[tx][ty][0] = shared_cells[tx][ty][1] = d_u_new[0][target];
	__syncthreads();

	bool truth =  ((target<SIZE)||(target%SIZE==0)||(target>=SIZE*(SIZE-1))||(target%SIZE==(SIZE-1)));
	next_index = (iteration) % 2;

	if(!truth)
	{
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
		if((tx - 1 < 0) || (tx + 1 == BLOCK_SIZE) || (ty - 1 < 0) || (ty + 1 == BLOCK_SIZE)){
			d_u_new[next_index][target] = 0.25 * (p + q + r + s);
		}else{
			shared_cells[tx][ty][next_index] = 0.25 * (p + q + r + s);
		}

		//__syncthreads();
	}
	if (iteration == ITERATIONS-1){
		if(!truth){
			d_u_new[0][target] = 0.25 * (p + q + r + s);
		}
	}
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

// Sets all of the specified cells to their initial value.
int main()
{
	float *cells[2], *cells_gpu[2], **steady_state;
	int i,j;
	size_t size;
	float h = 1.0/SIZE;


	size=SIZE*SIZE*sizeof(float);
	cells[0]    = (float*)malloc(size);
	cells[1]    = (float*)malloc(size);
	
	steady_state = allocate_cells(SIZE, SIZE);
	
	cudaMalloc(&cells_gpu[0],size);
	cudaMalloc(&cells_gpu[1],size);


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

	cudaMemcpy(cells_gpu[0], cells[0], size, cudaMemcpyHostToDevice);
	cudaMemcpy(cells_gpu[1], cells[1], size, cudaMemcpyHostToDevice);

	dim3 dimBlock(BLOCK_SIZE,BLOCK_SIZE);
	dim3 dimGrid(SIZE/BLOCK_SIZE,SIZE/BLOCK_SIZE);
	
	time_t start_time = time(NULL);
	int k;
	for (k = 0; k < ITERATIONS; k++)
	{
		jacobi<<<dimGrid,dimBlock>>>(cells_gpu[0], cells_gpu[1], k);
	}
	cudaMemcpy(cells[0], cells_gpu[0], size, cudaMemcpyDeviceToHost);
	time_t end_time = time(NULL);

	for(i=0;i<SIZE;i++)
	{
		for(j=0; j < SIZE; j++){
			steady_state[i][j] = cells[0][i*SIZE+j];
		//	printf("%f ", cells[0][i*SIZE+j]);
		}
	//	printf("\n");
	}
	printf("\nExecution time: %d seconds\n", (int) difftime(end_time, start_time));
	create_snapshot(steady_state, SIZE-2, SIZE-2, ITERATIONS);

	/* Liberamos memoria */
	free(cells[0]);
	free(cells[1]);
	cudaFree(cells_gpu[0]);
	cudaFree(cells_gpu[1]);

}
void create_snapshot(float **cells, int num_cols, int num_rows, int id) {
	int scale_x, scale_y;
	scale_x = scale_y = 1;
	
	// Figure out if we need to scale down the snapshot (to 1,000 x 1,000)
	//  and, if so, how much to scale down
	if (num_cols > 1000) {
		if ((num_cols % 1000) == 0) scale_x = num_cols / 1000;
		else {
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
