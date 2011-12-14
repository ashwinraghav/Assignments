#ifndef _HOTPLATE_KERNEL_H_
#define _HOTPLATE_KERNEL_H_
// The width of external cells that surround the plate's cells
#define BOUNDARYCELLS 2
#define THREAD_BLOCK_WIDTH 128
#define THREAD_BLOCK_HEIGHT 4
#define FOR(A) for(int A=1;A<=BOUNDARYCELLS;A++)
#define SYNCHRONIZATION __syncthreads()
#define NN_MEAN(array,a,b) (array[a][b+1]+array[a+1][b]+array[a-1][b]+array[a][b-1])*0.25
#define GHOST 4
//Used several macros as opposed to functions to provide opportunities for vectorization
#define GRID_BOTTOM_BLOCK (blockIdx.y==(gridDim.y-1))
#define LEFT_THREAD (!threadIdx.x)
#define TOP_THREAD (!threadIdx.y)
#define RIGHT_THREAD(threadIdx.x==(THREAD_BLOCK_WIDTH-1))
#define BOTTOM_THREAD(threadIdx.y==(THREAD_BLOCK_HEIGHT-1)) 

#define COPY(A,B,C) shared_cells[A][B]=gplate_block_cellsInput[C];

void create_snapshot(float **cells, int n_x, int n_y, int id);
__global__ void Kernel(
		dim3 plateDims,
		float* g_plateblock_cellsInput,
		float* g_plateblock_cellsOutput, dim3 gridDims)
{
	//Executing thread's location
	dim3 myCell((blockIdx.x*blockDim.x)+threadIdx.x+BOUNDARYCELLS,
			(blockIdx.y*blockDim.y)+threadIdx.y+BOUNDARYCELLS);
	if ((myCell.x >= plateDims.x-BOUNDARYCELLS) || (myCell.y >= plateDims.y - BOUNDARYCELLS)) {
		// My location is outside of the plate boundaries
		return;
	}
	/*calculating positions with respect to 1D array and thread position on the grid*/
	int left=myCell.x-1;
	int right=myCell.x+1;
	int top=myCell.y-1;
	int bottom=myCell.y+1;
	int me=myCell.x;
	int bc2=BOUNDARYCELLS;
	int x_offset_in_block=threadIdx.x+BOUNDARYCELLS; //column
	int y_offset_in_block=threadIdx.y+BOUNDARYCELLS; //row
	bc2=bc2<<1;
	float res=0.0f;
	__shared__ float shared_cells[THREAD_BLOCK_HEIGHT+(2*BOUNDARYCELLS)][THREAD_BLOCK_WIDTH+(BOUNDARYCELLS*2)]; 

	shared_cells[y_offset_in_block][x_offset_in_block]=g_plateblock_cellsInput[myCell.y*plateDims.x+myCell.x]; //I put my value first
	//Top thread gets the required cells at the top
	if(TOP_THREAD) 
	{
		FOR(i)
		{
			top=myCell.y-i;
			int temp=y_offset_in_block-i;
			COPY(temp, xoffset_in_block, top*plateDims.x+me)
		}
		//Left thread gets ghost cells      
		if(LEFT_THREAD) 
		{
			FOR(i)
			{
				top=myCell.y-i;
				int temp=y_offset_in_block-i;
				FOR(j)
				{
					left=myCell.x-j;
					COPY(temp, xoffset_in_block, top*plateDims.x+left)
				}
			}
		}
		//unless the executing thread handles the right block
		if(!GRID_RIGHT_BLOCK) 
		{
			//Right Threads get ghost cells
			if(RIGHT_THREAD)
			{
				FOR(i)
				{
					top=myCell.y-i;
					int temp=y_offset_in_block-i;
					FOR(j)
					{
						right=myCell.x+j;
						COPY(temp, xoffset_in_block, top*plateDims.x+right)
					}
				}
			}
		}
 		//Executing thread handles grid's right block
		else
		{
			//Last thread in the right block
			if(threadIdx.x==(plateDims.x-(bc2)-1)%THREAD_BLOCK_WIDTH) 
			{
				FOR(i)
				{
					top=myCell.y-i;
					int temp=y_offset_in_block-i;
					FOR(j)
					{
						right=myCell.x+j;
						COPY(temp, xoffset_in_block + j, top*plateDims.x+right)
					}   
				}
			}
		}
	}

	
	if(!GRID_BOTTOM_BLOCK)
	{
		//bottom thread gets required bottom cells
		if(BOTTOM_THREAD)
		{
			FOR(i)
			{
				int temp=y_offset_in_block+i;
				bottom=myCell.y+i;
				COPY(temp, xoffset_in_block + j, bottom*plateDims.x+me)
			}
			//get ghost cells 
			if(LEFT_THREAD) 
			{
				FOR(i)
				{
					bottom=myCell.y+i;
					FOR(j)
					{
						left=myCell.x-j;
						COPY(y_offset_in_block+i, xoffset_in_block - j, bottom*plateDims.x+left)
					}
				}
			}
			if(GRID_RIGHT_BLOCK)
			{
				if(RIGHT_THREAD)
				{
					FOR(i)
					{
						bottom=myCell.y+i;
						int temp=y_offset_in_block+i;
						FOR(j)
						{
							COPY(temp, x_offset_in_block+j, bottom*plateDims.x+right)
						}
					}
				}
			}
			else 
			{
				if(threadIdx.x==(plateDims.x-bc2-1)%THREAD_BLOCK_WIDTH)
				{
					FOR(i)
					{
						bottom=myCell.y+i;
						FOR(j)
						{
							right=myCell.x+j;
							COPY(y_offset_in_block+i, x_offset_in_block+j, bottom*plateDims.x+right)
						}
					}
				}
			}
			/***********************/
		}
	}
	else
	{
		if(threadIdx.y==(plateDims.y-(bc2)-1)%THREAD_BLOCK_HEIGHT)
		{
			FOR(i)
				shared_cells[y_offset_in_block+i][x_offset_in_block]=g_plateblock_cellsInput[(myCell.y+i)*plateDims.x +me];
			if(LEFT_THREAD)
			{
				FOR(i)
				{
					bottom=myCell.y+i;
					FOR(j)    
					{
						left=myCell.x-j;
						COPY(y_offset_in_block+i, x_offset_in_block-j, bottom*plateDims.x+left)
					}
				}
			}
			if(!GRID_RIGHT_BLOCK) 
			{
				if(RIGHT_THREAD)
				{
					FOR(i)
					{
						bottom=myCell.y+i;
						FOR(j)
						{
							right=myCell.x+j;
							COPY(y_offset_in_block+i, x_offset_in_block+j, bottom*plateDims.x+right)
						}
					}
				}
			}
			else
			{
				if(threadIdx.x==(plateDims.x-bc2-1)%THREAD_BLOCK_WIDTH) 
				{
					FOR(i)
					{
						bottom=myCell.y+i;
						FOR(j)
							COPY(y_offset_in_block+i, x_offset_in_block+j, bottom*plateDims.x+myCell.x+j)
					}
				}
			}
		}
	}
	if(LEFT_THREAD) 
	{
		FOR(i)
			COPY(y_offset_in_block, x_offset_in_block-i, myCell.y*plateDims.x+myCell.x-i)
	}
	if(!GRID_RIGHT_BLOCK)
	{
		if(RIGHT_THREAD)
		{
			FOR(i)

				COPY(y_offset_in_block, x_offset_in_block+i, myCell.y*plateDims.x+myCell.x+i)
		}
	}
	else
	{          if(threadIdx.x==(plateDims.x-bc2-1)%THREAD_BLOCK_WIDTH) 
		{
			FOR(i)

				COPY(y_offset_in_block, x_offset_in_block+i, myCell.y*plateDims.x+myCell.x+i)
		}
	}
	SYNCHRONIZATION;
	res=NN_MEAN(shared_cells,y_offset_in_block,x_offset_in_block);
	
	//Move all the ghost cells to iteration i+1
	float abv_new_state=0.0f,blw_new_state=0.0f,left_new_state=0.0f,right_new_state=0.0f;
	float abv_new_state_array[BOUNDARYCELLS];
	
	//Hacky. Boundary blocks should not update ghosts since cells are immutable
	if(TOP_THREAD)
	{
		//update ghosts too
		if(!blockIdx.y) 
		{
			for(int i=1;i<BOUNDARYCELLS;i++)
				abv_new_state_array[i-1]=shared_cells[y_offset_in_block-i][x_offset_in_block]; 
		}
		//The first block on the grid should not update ghost cells !
		else  
		{
			for(int i=1;i<BOUNDARYCELLS;i++)
				abv_new_state_array[i-1]=(shared_cells[y_offset_in_block-i-1][x_offset_in_block] +
						shared_cells[y_offset_in_block-i][x_offset_in_block-i] +
						shared_cells[y_offset_in_block-i][x_offset_in_block+i] +
						shared_cells[y_offset_in_block][x_offset_in_block])*0.25f;
		}
		abv_new_state=abv_new_state_array[0];
	}
	if(LEFT_THREAD)
	{
		if(!blockIdx.x) 
			left_new_state=shared_cells[y_offset_in_block][x_offset_in_block-1];

		else
			left_new_state=(shared_cells[y_offset_in_block-1][x_offset_in_block-1] +
					shared_cells[y_offset_in_block][x_offset_in_block-2] +
					shared_cells[y_offset_in_block][x_offset_in_block]+
					shared_cells[1+y_offset_in_block][x_offset_in_block-1])*0.25f;

	}
	if(!GRID_RIGHT_BLOCK)
	{
		if(RIGHT_THREAD) 
			right_new_state=(shared_cells[y_offset_in_block-1][1+x_offset_in_block] +
					shared_cells[y_offset_in_block][x_offset_in_block]+
					shared_cells[y_offset_in_block][x_offset_in_block+2] +
					shared_cells[1+y_offset_in_block][1+x_offset_in_block])*0.25f;

	}
	else
	{
		if(threadIdx.x==(plateDims.x-bc2-1)%THREAD_BLOCK_WIDTH) 
			right_new_state=shared_cells[y_offset_in_block][1+x_offset_in_block];

	}
	if(!GRID_BOTTOM_BLOCK)
	{
		if(BOTTOM_THREAD) 
		{
			blw_new_state=(shared_cells[y_offset_in_block][x_offset_in_block]+
					shared_cells[1+y_offset_in_block][x_offset_in_block-1]+
					shared_cells[1+y_offset_in_block][1+x_offset_in_block] +
					shared_cells[2+y_offset_in_block][x_offset_in_block])*0.25f;
		}
	}
	else
	{
		if(threadIdx.y==(plateDims.y-bc2-1)%THREAD_BLOCK_HEIGHT) //gives the last thread
			blw_new_state=shared_cells[1+y_offset_in_block][x_offset_in_block];

	}
	/****************Relaxation*****************************/
	SYNCHRONIZATION;
	shared_cells[y_offset_in_block][x_offset_in_block]=res;
	// All boundary cells update state !
	if(TOP_THREAD)
		shared_cells[y_offset_in_block-1][x_offset_in_block]=abv_new_state; 
	if(LEFT_THREAD)
		shared_cells[y_offset_in_block][x_offset_in_block-1]=left_new_state;
	if(!GRID_RIGHT_BLOCK)
	{
		if(RIGHT_THREAD)
			shared_cells[y_offset_in_block][1+x_offset_in_block]=right_new_state;
	}
	else
	{
		if(threadIdx.x==(plateDims.x-(bc2)-1)%THREAD_BLOCK_WIDTH)
			shared_cells[y_offset_in_block][1+x_offset_in_block]=right_new_state;
	}
	if(!GRID_BOTTOM_BLOCK) {
		if(BOTTOM_THREAD)
			shared_cells[1+y_offset_in_block][x_offset_in_block]=blw_new_state;
	}
	else 
	{
		if(threadIdx.y==(plateDims.y-(bc2)-1)%THREAD_BLOCK_HEIGHT)
			shared_cells[1+y_offset_in_block][x_offset_in_block]=blw_new_state;
	}

	SYNCHRONIZATION;
	res=NN_MEAN(shared_cells,y_offset_in_block,x_offset_in_block);
	g_plateblock_cellsOutput[myCell.y*plateDims.x+me]=res;
}
#endif 

//debug function
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
	printf("Necesitamos %d Mb\n",3*size/1024/1024);
	cells[0]    = (float*)malloc(size);
	cells[1]    = (float*)malloc(size);
	
	steady_state = allocate_cells(SIZE, SIZE);
	//Allocate GPU memory
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
	//perform relaxation
	for(i=0;i<ITERATIONS/GHOST;i++)
	{
		if(i%2==0)
			jacobi<<<dimGrid,dimBlock>>>(dimBlock, cells_gpu[0], cells_gpu[1], dimGrid);
		else
			jacobi<<<dimGrid,dimBlock>>>(dimBlock, cells_gpu[1], cells_gpu[0], dimGrid);
		if(i%10==0)
			printf("iter=%d\n",i);
	}

	int final_cells = (ITERATIONS % 2 == 0) ? 1 : 0;
	cudaMemcpy(cells[0], cells_gpu[final_cells], size, cudaMemcpyDeviceToHost);

	//copy the 1D cells to a 2D array for ppm conversion
	for(i=0;i<SIZE;i++)
	{
		for(j=0; j < SIZE; j++){
			steady_state[i][j] = cells[0][i*SIZE+j];
		//	printf("%f ", cells[0][i*SIZE+j]);
		}
		//printf("\n");
	}
	time_t end_time = time(NULL);
	printf("\nExecution time: %d seconds\n", (int) difftime(end_time, start_time));
	//create_snapshot(cells, SIZE-2, SIZE-2, ITERATIONS);

	/* Liberamos memoria */
	free(cells[0]);
	free(cells[1]);
	cudaFree(cells_gpu[0]);
	cudaFree(cells_gpu[1]);

}
void create_snapshot(float **cells, int num_cols, int num_rows, int id) {
	/*code omitted*/
}
