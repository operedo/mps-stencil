#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cuda.h>
//#include <cuda_runtime.h>
#define DT double
#define TEM_CELLS 125
#define OPTIMIZED_KERNEL 1
void Check_CUDA_Error(const char *message)
    {
       cudaError_t error = cudaGetLastError();
       if(error!=cudaSuccess) {
          fprintf(stderr,"ERROR: %s: %s\n", message, cudaGetErrorString(error) );
          exit(-1);
       }
    }


__global__ void fitnessKernel(
				int slides,int cols,int rows,int tem_cells,
                int tem_slides,int tem_cols,int tem_rows,
                int *tem_cells_slides_d,int *tem_cells_cols_d,
				int *tem_cells_rows_d,
			    int npatterns, int *freq_realization_d,
				int *patternArray_d,
			    int *realization_matrix_d)
{

    int tidx=threadIdx.x;
    int tidy=threadIdx.y;
    int tidz=threadIdx.z;
    int bidx=blockIdx.x;
    int bidy=blockIdx.y;
    int bidz=blockIdx.z;
    int bdimx=blockDim.x;
    int bdimy=blockDim.y;
    int bdimz=blockDim.z;
    int idx = bidx*bdimx + tidx;
    int idy = bidy*bdimy + tidy;
    int idz = bidz*bdimz + tidz;
//    int threadIdGPU = tidx + bdimx*tidy + bdimx*bdimy*tidz;
    int icell,pos;
    int local_pattern[TEM_CELLS];
    int minn,maxx,isFound,value;

    int mult_pos_tem_cells;
    pos = 0;
    minn=0;
    maxx=npatterns-1;
    isFound=0;

    if(idx < rows && idy < cols && idz < slides){


        for (icell = 0; icell < tem_cells; icell += 5){
            local_pattern[icell] = realization_matrix_d[(idx - 1 + tem_cells_rows_d[icell]) +
                           (idy - 1 + tem_cells_cols_d[icell])*(rows)
                          +(idz - 1 + tem_cells_slides_d[icell])
                          *(rows * cols)];
            local_pattern[icell + 1] = realization_matrix_d[(idx - 1 + tem_cells_rows_d[icell + 1]) +
                           (idy - 1 + tem_cells_cols_d[icell + 1])*(rows)
                          +(idz - 1 + tem_cells_slides_d[icell + 1])
                          *(rows * cols)];
            local_pattern[icell + 2] = realization_matrix_d[(idx - 1 + tem_cells_rows_d[icell + 2]) +
                           (idy - 1 + tem_cells_cols_d[icell + 2])*(rows)
                          +(idz - 1 + tem_cells_slides_d[icell + 2])
                          *(rows * cols)];
            local_pattern[icell + 3] = realization_matrix_d[(idx - 1 + tem_cells_rows_d[icell + 3]) +
                           (idy - 1 + tem_cells_cols_d[icell + 3])*(rows)
                          +(idz - 1 + tem_cells_slides_d[icell + 3])
                          *(rows * cols)];
            local_pattern[icell + 4] = realization_matrix_d[(idx - 1 + tem_cells_rows_d[icell + 4]) +
                           (idy - 1 + tem_cells_cols_d[icell + 4])*(rows)
                          +(idz - 1 + tem_cells_slides_d[icell + 4])
                          *(rows * cols)];
        }

        while(minn<=maxx & isFound == 0){

//            pos = (int)(((float)(minn+maxx))*0.5);
            pos = ((minn+maxx)/2);
            mult_pos_tem_cells = pos*tem_cells;


            value = 0;
            icell = 0;

            while ( value == 0 & icell < tem_cells){
                if (     patternArray_d[icell + mult_pos_tem_cells] > local_pattern[icell] ){
                    value = 1;
                    maxx = pos - 1;
                }
                else if ( patternArray_d[icell + mult_pos_tem_cells] < local_pattern[icell] ){
                    value = 1;
                    minn = pos + 1;
                }

                icell = icell + 1;
            }


            if (value == 0){
                atomicSub(&freq_realization_d[pos],1);
                isFound = 1;
            }
        }
    }



}
__global__ void fitnessKernelOptimized(
				int slides,int cols,int rows,int tem_cells,
                int tem_slides,int tem_cols,int tem_rows,
                int *tem_cells_slides_d,int *tem_cells_cols_d,
				int *tem_cells_rows_d,
			    int npatterns, int *freq_realization_d,
				int *patternArray_d,
			    int *realization_matrix_d)
{

    int tidx=threadIdx.x;
    int tidy=threadIdx.y;
    int tidz=threadIdx.z;
    int bidx=blockIdx.x;
    int bidy=blockIdx.y;
    int bidz=blockIdx.z;
    int bdimx=blockDim.x;
    int bdimy=blockDim.y;
    int bdimz=blockDim.z;
    int idx = bidx*bdimx + tidx;
    int idy = bidy*bdimy + tidy;
    int idz = bidz*bdimz + tidz;
//    int threadIdGPU = tidx + bdimx*tidy + bdimx*bdimy*tidz;
    int icell,pos;
    int local_pattern[TEM_CELLS];
    int minn,maxx,isFound,value;
    int l0,l1,l2,l3,l4,l5,l6,l7,l8,l9;
    int l10,l11,l12,l13,l14,l15,l16,l17,l18,l19;
    int l20,l21,l22,l23,l24,l25,l26,l27,l28,l29;
    int l30,l31,l32,l33,l34,l35,l36,l37,l38,l39;
    int mult_pos_tem_cells;
    pos = 0;
    minn=0;
    maxx=npatterns-1;
    isFound=0;

    if(idx < rows && idy < cols && idz < slides){


        for (icell = 0; icell < tem_cells; icell += 5){
            local_pattern[icell] = realization_matrix_d[(idx - 1 + tem_cells_rows_d[icell]) +
                           (idy - 1 + tem_cells_cols_d[icell])*(rows)
                          +(idz - 1 + tem_cells_slides_d[icell])
                          *(rows * cols)];
            local_pattern[icell + 1] = realization_matrix_d[(idx - 1 + tem_cells_rows_d[icell + 1]) +
                           (idy - 1 + tem_cells_cols_d[icell + 1])*(rows)
                          +(idz - 1 + tem_cells_slides_d[icell + 1])
                          *(rows * cols)];
            local_pattern[icell + 2] = realization_matrix_d[(idx - 1 + tem_cells_rows_d[icell + 2]) +
                           (idy - 1 + tem_cells_cols_d[icell + 2])*(rows)
                          +(idz - 1 + tem_cells_slides_d[icell + 2])
                          *(rows * cols)];
            local_pattern[icell + 3] = realization_matrix_d[(idx - 1 + tem_cells_rows_d[icell + 3]) +
                           (idy - 1 + tem_cells_cols_d[icell + 3])*(rows)
                          +(idz - 1 + tem_cells_slides_d[icell + 3])
                          *(rows * cols)];
            local_pattern[icell + 4] = realization_matrix_d[(idx - 1 + tem_cells_rows_d[icell + 4]) +
                           (idy - 1 + tem_cells_cols_d[icell + 4])*(rows)
                          +(idz - 1 + tem_cells_slides_d[icell + 4])
                          *(rows * cols)];
        }
        l0 = local_pattern[0];
        l1 = local_pattern[1];
        l2 = local_pattern[2];
        l3 = local_pattern[3];
        l4 = local_pattern[4];
        l5 = local_pattern[5];
        l6 = local_pattern[6];
        l7 = local_pattern[7];
        l8 = local_pattern[8];
        l9 = local_pattern[9];
        l10 = local_pattern[10];
        l11 = local_pattern[11];
        l12 = local_pattern[12];
        l13 = local_pattern[13];
        l14 = local_pattern[14];
        l15 = local_pattern[15];
        l16 = local_pattern[16];
        l17 = local_pattern[17];
        l18 = local_pattern[18];
        l19 = local_pattern[19];
        l20 = local_pattern[20];
        l21 = local_pattern[21];
        l22 = local_pattern[22];
        l23 = local_pattern[23];
        l24 = local_pattern[24];
        l25 = local_pattern[25];
        l26 = local_pattern[26];
        l27 = local_pattern[27];
        l28 = local_pattern[28];
        l29 = local_pattern[29];
        l30 = local_pattern[30];
        l31 = local_pattern[31];
        l32 = local_pattern[32];
        l33 = local_pattern[33];
        l34 = local_pattern[34];
        l35 = local_pattern[35];
        l36 = local_pattern[36];
        l37 = local_pattern[37];
        l38 = local_pattern[38];
        l39 = local_pattern[39];

        while(minn<=maxx & isFound == 0){

//            pos = (int)(((float)(minn+maxx))*0.5);
            pos = ((minn+maxx)/2);
//            pos = (minn+maxx)<<2;

            value = 0;
            icell = 0;


            mult_pos_tem_cells = pos*tem_cells;

            while(true){
                value=patternArray_d[0 + mult_pos_tem_cells] - l0;//local_pattern[0];
                if(value!=0)break;
                value=patternArray_d[1 + mult_pos_tem_cells] - l1;//local_pattern[1];
                if(value!=0)break;
                value=patternArray_d[2 + mult_pos_tem_cells] - l2;//local_pattern[2];
                if(value!=0)break;
                value=patternArray_d[3 + mult_pos_tem_cells] - l3;//local_pattern[3];
                if(value!=0)break;
                value=patternArray_d[4 + mult_pos_tem_cells] - l4;//local_pattern[4];
                if(value!=0)break;
                value=patternArray_d[5 + mult_pos_tem_cells] - l5;//local_pattern[5];
                if(value!=0)break;
                value=patternArray_d[6 + mult_pos_tem_cells] - l6;//local_pattern[6];
                if(value!=0)break;
                value=patternArray_d[7 + mult_pos_tem_cells] - l7;//local_pattern[7];
                if(value!=0)break;
                value=patternArray_d[8 + mult_pos_tem_cells] - l8;//local_pattern[8];
                if(value!=0)break;
                value=patternArray_d[9 + mult_pos_tem_cells] - l9;//local_pattern[9];
                if(value!=0)break;
                value=patternArray_d[10 + mult_pos_tem_cells] - l10;//local_pattern[10];
                if(value!=0)break;
                value=patternArray_d[11 + mult_pos_tem_cells] - l11;//local_pattern[11];
                if(value!=0)break;
                value=patternArray_d[12 + mult_pos_tem_cells] - l12;//local_pattern[12];
                if(value!=0)break;
                value=patternArray_d[13 + mult_pos_tem_cells] - l13;//local_pattern[13];
                if(value!=0)break;
                value=patternArray_d[14 + mult_pos_tem_cells] - l14;//local_pattern[14];
                if(value!=0)break;
                value=patternArray_d[15 + mult_pos_tem_cells] - l15;//local_pattern[15];
                if(value!=0)break;
                value=patternArray_d[16 + mult_pos_tem_cells] - l16;//local_pattern[16];
                if(value!=0)break;
                value=patternArray_d[17 + mult_pos_tem_cells] - l17;//local_pattern[17];
                if(value!=0)break;
                value=patternArray_d[18 + mult_pos_tem_cells] - l18;//local_pattern[18];
                if(value!=0)break;
                value=patternArray_d[19 + mult_pos_tem_cells] - l19;//local_pattern[19];
                if(value!=0)break;
                value=patternArray_d[20 + mult_pos_tem_cells] - l20;//local_pattern[20];
                if(value!=0)break;
                value=patternArray_d[21 + mult_pos_tem_cells] - l21;//local_pattern[21];
                if(value!=0)break;
                value=patternArray_d[22 + mult_pos_tem_cells] - l22;//local_pattern[22];
                if(value!=0)break;
                value=patternArray_d[23 + mult_pos_tem_cells] - l23;//local_pattern[23];
                if(value!=0)break;
                value=patternArray_d[24 + mult_pos_tem_cells] - l24;//local_pattern[24];
                if(value!=0)break;
                value=patternArray_d[25 + mult_pos_tem_cells] - l25;//local_pattern[25];
                if(value!=0)break;
                value=patternArray_d[26 + mult_pos_tem_cells] - l26;//local_pattern[26];
                if(value!=0)break;
                value=patternArray_d[27 + mult_pos_tem_cells] - l27;//local_pattern[27];
                if(value!=0)break;
                value=patternArray_d[28 + mult_pos_tem_cells] - l28;//local_pattern[28];
                if(value!=0)break;
                value=patternArray_d[29 + mult_pos_tem_cells] - l29;//local_pattern[29];
                if(value!=0)break;
                value=patternArray_d[30 + mult_pos_tem_cells] - l30;//local_pattern[30];
                if(value!=0)break;
                value=patternArray_d[31 + mult_pos_tem_cells] - l31;//local_pattern[31];
                if(value!=0)break;
                value=patternArray_d[32 + mult_pos_tem_cells] - l32;//local_pattern[32];
                if(value!=0)break;
                value=patternArray_d[33 + mult_pos_tem_cells] - l33;//local_pattern[33];
                if(value!=0)break;
                value=patternArray_d[34 + mult_pos_tem_cells] - l34;//local_pattern[34];
                if(value!=0)break;
                value=patternArray_d[35 + mult_pos_tem_cells] - l35;//local_pattern[35];
                if(value!=0)break;
                value=patternArray_d[36 + mult_pos_tem_cells] - l36;//local_pattern[36];
                if(value!=0)break;
                value=patternArray_d[37 + mult_pos_tem_cells] - l37;//local_pattern[37];
                if(value!=0)break;
                value=patternArray_d[38 + mult_pos_tem_cells] - l38;//local_pattern[38];
                if(value!=0)break;
                value=patternArray_d[39 + mult_pos_tem_cells] - l39;//local_pattern[39];
                if(value!=0)break;
                value=patternArray_d[40 + mult_pos_tem_cells] - local_pattern[40];
                if(value!=0)break;
                value=patternArray_d[41 + mult_pos_tem_cells] - local_pattern[41];
                if(value!=0)break;
                value=patternArray_d[42 + mult_pos_tem_cells] - local_pattern[42];
                if(value!=0)break;
                value=patternArray_d[43 + mult_pos_tem_cells] - local_pattern[43];
                if(value!=0)break;
                value=patternArray_d[44 + mult_pos_tem_cells] - local_pattern[44];
                if(value!=0)break;
                value=patternArray_d[45 + mult_pos_tem_cells] - local_pattern[45];
                if(value!=0)break;
                value=patternArray_d[46 + mult_pos_tem_cells] - local_pattern[46];
                if(value!=0)break;
                value=patternArray_d[47 + mult_pos_tem_cells] - local_pattern[47];
                if(value!=0)break;
                value=patternArray_d[48 + mult_pos_tem_cells] - local_pattern[48];
                if(value!=0)break;
                value=patternArray_d[49 + mult_pos_tem_cells] - local_pattern[49];
                if(value!=0)break;
                value=patternArray_d[50 + mult_pos_tem_cells] - local_pattern[50];
                if(value!=0)break;
                value=patternArray_d[51 + mult_pos_tem_cells] - local_pattern[51];
                if(value!=0)break;
                value=patternArray_d[52 + mult_pos_tem_cells] - local_pattern[52];
                if(value!=0)break;
                value=patternArray_d[53 + mult_pos_tem_cells] - local_pattern[53];
                if(value!=0)break;
                value=patternArray_d[54 + mult_pos_tem_cells] - local_pattern[54];
                if(value!=0)break;
                value=patternArray_d[55 + mult_pos_tem_cells] - local_pattern[55];
                if(value!=0)break;
                value=patternArray_d[56 + mult_pos_tem_cells] - local_pattern[56];
                if(value!=0)break;
                value=patternArray_d[57 + mult_pos_tem_cells] - local_pattern[57];
                if(value!=0)break;
                value=patternArray_d[58 + mult_pos_tem_cells] - local_pattern[58];
                if(value!=0)break;
                value=patternArray_d[59 + mult_pos_tem_cells] - local_pattern[59];
                if(value!=0)break;
                value=patternArray_d[60 + mult_pos_tem_cells] - local_pattern[60];
                if(value!=0)break;
                value=patternArray_d[61 + mult_pos_tem_cells] - local_pattern[61];
                if(value!=0)break;
                value=patternArray_d[62 + mult_pos_tem_cells] - local_pattern[62];
                if(value!=0)break;
                value=patternArray_d[63 + mult_pos_tem_cells] - local_pattern[63];
                if(value!=0)break;
                value=patternArray_d[64 + mult_pos_tem_cells] - local_pattern[64];
                if(value!=0)break;
                value=patternArray_d[65 + mult_pos_tem_cells] - local_pattern[65];
                if(value!=0)break;
                value=patternArray_d[66 + mult_pos_tem_cells] - local_pattern[66];
                if(value!=0)break;
                value=patternArray_d[67 + mult_pos_tem_cells] - local_pattern[67];
                if(value!=0)break;
                value=patternArray_d[68 + mult_pos_tem_cells] - local_pattern[68];
                if(value!=0)break;
                value=patternArray_d[69 + mult_pos_tem_cells] - local_pattern[69];
                if(value!=0)break;
                value=patternArray_d[70 + mult_pos_tem_cells] - local_pattern[70];
                if(value!=0)break;
                value=patternArray_d[71 + mult_pos_tem_cells] - local_pattern[71];
                if(value!=0)break;
                value=patternArray_d[72 + mult_pos_tem_cells] - local_pattern[72];
                if(value!=0)break;
                value=patternArray_d[73 + mult_pos_tem_cells] - local_pattern[73];
                if(value!=0)break;
                value=patternArray_d[74 + mult_pos_tem_cells] - local_pattern[74];
                if(value!=0)break;
                value=patternArray_d[75 + mult_pos_tem_cells] - local_pattern[75];
                if(value!=0)break;
                value=patternArray_d[76 + mult_pos_tem_cells] - local_pattern[76];
                if(value!=0)break;
                value=patternArray_d[77 + mult_pos_tem_cells] - local_pattern[77];
                if(value!=0)break;
                value=patternArray_d[78 + mult_pos_tem_cells] - local_pattern[78];
                if(value!=0)break;
                value=patternArray_d[79 + mult_pos_tem_cells] - local_pattern[79];
                if(value!=0)break;
                value=patternArray_d[80 + mult_pos_tem_cells] - local_pattern[80];
                if(value!=0)break;
                value=patternArray_d[81 + mult_pos_tem_cells] - local_pattern[81];
                if(value!=0)break;
                value=patternArray_d[82 + mult_pos_tem_cells] - local_pattern[82];
                if(value!=0)break;
                value=patternArray_d[83 + mult_pos_tem_cells] - local_pattern[83];
                if(value!=0)break;
                value=patternArray_d[84 + mult_pos_tem_cells] - local_pattern[84];
                if(value!=0)break;
                value=patternArray_d[85 + mult_pos_tem_cells] - local_pattern[85];
                if(value!=0)break;
                value=patternArray_d[86 + mult_pos_tem_cells] - local_pattern[86];
                if(value!=0)break;
                value=patternArray_d[87 + mult_pos_tem_cells] - local_pattern[87];
                if(value!=0)break;
                value=patternArray_d[88 + mult_pos_tem_cells] - local_pattern[88];
                if(value!=0)break;
                value=patternArray_d[89 + mult_pos_tem_cells] - local_pattern[89];
                if(value!=0)break;
                value=patternArray_d[90 + mult_pos_tem_cells] - local_pattern[90];
                if(value!=0)break;
                value=patternArray_d[91 + mult_pos_tem_cells] - local_pattern[91];
                if(value!=0)break;
                value=patternArray_d[92 + mult_pos_tem_cells] - local_pattern[92];
                if(value!=0)break;
                value=patternArray_d[93 + mult_pos_tem_cells] - local_pattern[93];
                if(value!=0)break;
                value=patternArray_d[94 + mult_pos_tem_cells] - local_pattern[94];
                if(value!=0)break;
                value=patternArray_d[95 + mult_pos_tem_cells] - local_pattern[95];
                if(value!=0)break;
                value=patternArray_d[96 + mult_pos_tem_cells] - local_pattern[96];
                if(value!=0)break;
                value=patternArray_d[97 + mult_pos_tem_cells] - local_pattern[97];
                if(value!=0)break;
                value=patternArray_d[98 + mult_pos_tem_cells] - local_pattern[98];
                if(value!=0)break;
                value=patternArray_d[99 + mult_pos_tem_cells] - local_pattern[99];
                if(value!=0)break;
                value=patternArray_d[100 + mult_pos_tem_cells] - local_pattern[100];
                if(value!=0)break;
                value=patternArray_d[101 + mult_pos_tem_cells] - local_pattern[101];
                if(value!=0)break;
                value=patternArray_d[102 + mult_pos_tem_cells] - local_pattern[102];
                if(value!=0)break;
                value=patternArray_d[103 + mult_pos_tem_cells] - local_pattern[103];
                if(value!=0)break;
                value=patternArray_d[104 + mult_pos_tem_cells] - local_pattern[104];
                if(value!=0)break;
                value=patternArray_d[105 + mult_pos_tem_cells] - local_pattern[105];
                if(value!=0)break;
                value=patternArray_d[106 + mult_pos_tem_cells] - local_pattern[106];
                if(value!=0)break;
                value=patternArray_d[107 + mult_pos_tem_cells] - local_pattern[107];
                if(value!=0)break;
                value=patternArray_d[108 + mult_pos_tem_cells] - local_pattern[108];
                if(value!=0)break;
                value=patternArray_d[109 + mult_pos_tem_cells] - local_pattern[109];
                if(value!=0)break;
                value=patternArray_d[110 + mult_pos_tem_cells] - local_pattern[110];
                if(value!=0)break;
                value=patternArray_d[111 + mult_pos_tem_cells] - local_pattern[111];
                if(value!=0)break;
                value=patternArray_d[112 + mult_pos_tem_cells] - local_pattern[112];
                if(value!=0)break;
                value=patternArray_d[113 + mult_pos_tem_cells] - local_pattern[113];
                if(value!=0)break;
                value=patternArray_d[114 + mult_pos_tem_cells] - local_pattern[114];
                if(value!=0)break;
                value=patternArray_d[115 + mult_pos_tem_cells] - local_pattern[115];
                if(value!=0)break;
                value=patternArray_d[116 + mult_pos_tem_cells] - local_pattern[116];
                if(value!=0)break;
                value=patternArray_d[117 + mult_pos_tem_cells] - local_pattern[117];
                if(value!=0)break;
                value=patternArray_d[118 + mult_pos_tem_cells] - local_pattern[118];
                if(value!=0)break;
                value=patternArray_d[119 + mult_pos_tem_cells] - local_pattern[119];
                if(value!=0)break;
                value=patternArray_d[120 + mult_pos_tem_cells] - local_pattern[120];
                if(value!=0)break;
                value=patternArray_d[121 + mult_pos_tem_cells] - local_pattern[121];
                if(value!=0)break;
                value=patternArray_d[122 + mult_pos_tem_cells] - local_pattern[122];
                if(value!=0)break;
                value=patternArray_d[123 + mult_pos_tem_cells] - local_pattern[123];
                if(value!=0)break;
                value=patternArray_d[124 + mult_pos_tem_cells] - local_pattern[124];
                if(value!=0)break;
                atomicSub(&freq_realization_d[pos],1);
                minn = maxx + 1;
                break;
            }

            if (value > 0){
                maxx = pos -1;
            }
            else if (value < 0){
                minn = pos + 1;
            }
        }
    }
}


//
//
//        }
//}

// function called from main fortran program
extern "C" int fitnessfunctioncudawrapper_	(int *slides,int *cols,int *rows,int *tem_cells,
                                 int *tem_slides,int *tem_cols,int *tem_rows,
                                 int *tem_cells_slides,int *tem_cells_cols,int *tem_cells_rows,
				 int *npatterns, int *freq_realization, int *patternArray,
				 int *realization_matrix, DT *value)
{
    cudaSetDevice(0);

    int *realization_matrix_d;
    int *freq_realization_d;
    int *tem_cells_slides_d, *tem_cells_cols_d, *tem_cells_rows_d;
    int *patternArray_d;
//    int shared_mem_size;
    cudaEvent_t start, stop;
    float time;
    cudaEventCreate(&start);
    cudaEventCreate(&stop);
//    shared_mem_size = *npatterns*sizeof(int);
    dim3 threads(4,4,4);
    dim3 blocks( (*rows-*tem_rows + threads.x -1)/threads.x,
                    (*cols-*tem_cols + threads.y -1)/threads.y,
                    (*slides-*tem_slides + threads.z -1)/(threads.z) );
//    printf("bdimz = %d\n", blocks.z);
    // Allocate memory on GPU

    cudaMalloc( (void **)&realization_matrix_d, sizeof(int) * (*slides * *cols * *rows) );
//    cudaMemset(realization_matrix_d, 0, sizeof(int) * (*slides * *cols * *rows));
    Check_CUDA_Error("ERROR:");
    cudaMalloc( (void **)&freq_realization_d, sizeof(int) * (*npatterns) );
//    cudaMemset(freq_realization_d, 0, sizeof(int) * (*npatterns) );
    Check_CUDA_Error("ERROR:");
    cudaMalloc( (void **)&patternArray_d, sizeof(int) * (*npatterns* *tem_cells));
//    cudaMemset(patternArray_d , 0, sizeof(int) * (*npatterns* *tem_cells));
    Check_CUDA_Error("ERROR:");
    cudaMalloc( (void **)&tem_cells_slides_d, sizeof(int) * (*tem_cells) );
//    cudaMemset(tem_cells_slides_d, 0, sizeof(int) * (*tem_cells));
    Check_CUDA_Error("ERROR:");
    cudaMalloc( (void **)&tem_cells_cols_d, sizeof(int) * (*tem_cells) );
//    cudaMemset(tem_cells_cols_d, 0, sizeof(int)*(*tem_cells));
    Check_CUDA_Error("ERROR:");
    cudaMalloc( (void **)&tem_cells_rows_d, sizeof(int) * (*tem_cells) );
//    cudaMemset(tem_cells_rows_d, 0, sizeof(int)*(*tem_cells));
    Check_CUDA_Error("ERROR:");

    // copy vectors from CPU to GPU
    cudaMemcpy( realization_matrix_d, realization_matrix,
        sizeof(int) * (*slides * *cols * *rows), cudaMemcpyHostToDevice );
    Check_CUDA_Error("realization matrix host to device");
    cudaMemcpy( freq_realization_d,   freq_realization  ,
        sizeof(int) * (*npatterns), cudaMemcpyHostToDevice );
    Check_CUDA_Error("freq host to device");
    cudaMemcpy( patternArray_d,   patternArray  ,
        sizeof(int) * (*npatterns* *tem_cells), cudaMemcpyHostToDevice );
    Check_CUDA_Error("patternArray host to device");
    cudaMemcpy( tem_cells_slides_d,   tem_cells_slides  ,
        sizeof(int) * (*tem_cells), cudaMemcpyHostToDevice );
    Check_CUDA_Error("tem_cells_slides host to device");
    cudaMemcpy( tem_cells_cols_d,     tem_cells_cols    ,
        sizeof(int) * (*tem_cells), cudaMemcpyHostToDevice );
    Check_CUDA_Error("tem_cells_cols host to device");
    cudaMemcpy( tem_cells_rows_d,     tem_cells_rows    ,
        sizeof(int) * (*tem_cells), cudaMemcpyHostToDevice );
    Check_CUDA_Error("tem_cells_rows host to device");

 //   printf("value-in=%f\n",*value);
    if(!OPTIMIZED_KERNEL){
        cudaEventRecord(start, 0);
        fitnessKernel<<< blocks, threads>>>(
                    *slides,*cols,*rows,*tem_cells,
                    *tem_slides,*tem_cols,*tem_rows,
                     tem_cells_slides_d,tem_cells_cols_d,tem_cells_rows_d,
                    *npatterns, freq_realization_d,patternArray_d,
                     realization_matrix_d);
        cudaDeviceSynchronize();
        Check_CUDA_Error("fitness kernel");
        cudaEventRecord(stop, 0);
        cudaEventSynchronize(stop);
        cudaEventElapsedTime(&time, start, stop);
//        printf ("Time for the not Optimized kernel: %f ms\n", time);
        printf ("%f\n", time/1000);
    }
    else{
        cudaEventRecord(start, 0);
        fitnessKernelOptimized<<< blocks, threads>>>(
                    *slides,*cols,*rows,*tem_cells,
                    *tem_slides,*tem_cols,*tem_rows,
                     tem_cells_slides_d,tem_cells_cols_d,tem_cells_rows_d,
                    *npatterns, freq_realization_d,patternArray_d,
                     realization_matrix_d);
        cudaDeviceSynchronize();
        Check_CUDA_Error("fitness kernel");
        cudaEventRecord(stop, 0);
        cudaEventSynchronize(stop);
        cudaEventElapsedTime(&time, start, stop);
//        printf ("Time for the Optimized kernel: %f ms\n", time);
        printf ("%f\n", time/1000);
    }


    // copy vectors back from GPU to CPU
    cudaMemcpy( freq_realization, freq_realization_d,
            sizeof(int) * (*npatterns ),  cudaMemcpyDeviceToHost);
 //   Check_CUDA_Error("memcpy freq dev to host");

    *value=0.0;
    for(int i=0;i<*npatterns;i++)
        *value=*value + (DT)(freq_realization[i])*(DT)(freq_realization[i]);

//    printf("GPU value = %f\n",*value);

    // free device memory
    cudaFree(realization_matrix_d);
    Check_CUDA_Error("ERROR:");
    cudaFree(freq_realization_d  );
    Check_CUDA_Error("ERROR:");
    cudaFree(patternArray_d      );
    Check_CUDA_Error("ERROR:");
    cudaFree(tem_cells_slides_d  );
    Check_CUDA_Error("ERROR:");
    cudaFree(tem_cells_cols_d    );
    Check_CUDA_Error("ERROR:");
    cudaFree(tem_cells_rows_d    );
    Check_CUDA_Error("ERROR:");
    cudaDeviceReset();
    return 0;
}
