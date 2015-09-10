#include <stdlib.h>

int fitnessfunctionCwrapper_ (int *slides,int *cols,int *rows,int *tem_cells,
                                int *tem_slides,int *tem_cols,int *tem_rows,
                                int *tem_cells_slides,int *tem_cells_cols,int *tem_cells_rows,
				int *npatterns, int *freq_realization, int *patternArray, 
				int *realization_matrix, double *value);



int patternSearchWrapper(int tem_cells, int *localPattern, int npatterns, int *patternArray){
	int minn,maxx,pos,isFound,value,irow;
	minn=0;
	maxx=npatterns-1;
	isFound=0;
	while(minn<=maxx & isFound==0){
		pos = (int)(((float)(minn+maxx))*0.5);
	
		value = 0;
		irow = 0;
		
		while ( value == 0 && irow < tem_cells ){
		   	if (     patternArray[irow + pos*tem_cells] > localPattern[irow] )
		   		value = 1;
		   	else if ( patternArray[irow + pos*tem_cells] < localPattern[irow] )
		      		value = 2;
		   	irow = irow + 1;
		}
		
       		if (value == 0)
          		isFound = 1;
       		else{ 
			if (value == 1)
          			maxx = pos - 1;
       			else
          			minn = pos + 1;
		}
	}
    	if (isFound == 0) pos = -1;
	return pos;
} 





int fitnessfunctioncwrapper_ (int *slides,int *cols,int *rows,int *tem_cells,
                                int *tem_slides,int *tem_cols,int *tem_rows,
                                int *tem_cells_slides,int *tem_cells_cols,int *tem_cells_rows,
				int *npatterns, int *freq_realization, int *patternArray,
				int *realization_matrix, double *value) 
{
	int islide,icol,irow,icell,pos,ii;

	int localPattern[*tem_cells];

	//for(ii=0;ii<(*slides)*(*rows)*(*cols);ii++)
	//	printf("%d",realization_matrix[ii]);
	//printf("\n");


//	printf("slides=%d, cols=%d, rows=%d, tem_cells=%d, tem_slides=%d, tem_cols=%d, tem_rows=%d, npatterns=%d\n",
//				*slides,*cols,*rows,*tem_cells,
//                                *tem_slides,*tem_cols,*tem_rows,*npatterns);
//	printf("freq[0]=%d, freq[1]=%d, realiz[0]=%d, realiz[2]=%d\n",freq_realization[0],freq_realization[1],realization_matrix[0],realization_matrix[1]);

	int counter=1;
  	for(islide=-1;islide<*slides-*tem_slides;islide++){
     		for(icol=-1;icol<*cols-*tem_cols;icol++){
        		for(irow=-1;irow<*rows-*tem_rows;irow++){
           			for(icell=0;icell<*tem_cells;icell++){
              				localPattern[icell]=
					realization_matrix[
						(irow+tem_cells_rows[icell])+
                                       		(icol+tem_cells_cols[icell])*(*rows)+
                                        	(islide+tem_cells_slides[icell])*(*rows * *cols)
					];
					//printf("%d",localPattern[icell]);
					//if(counter<=2000 && counter>=1900)
					//	printf("%d",localPattern[icell]);
				}
				//if(counter<=2000 && counter>=1900)
				//	printf("\n");

           			//call patternSearch(tem_cells,localPattern,pos)
				pos=patternSearchWrapper(*tem_cells, localPattern, *npatterns, patternArray);
				//if(counter<=2000 && counter>=1900) printf("%d\n",pos+1);
				//counter++;
           			if(pos!=-1){
              				freq_realization[pos]=freq_realization[pos]-1;
           			}
			}
		}
	}
	
	*value=0.0;	
	for(ii=0;ii<*npatterns;ii++){
		*value = *value + (double)(freq_realization[ii])*(double)(freq_realization[ii]);
		if(freq_realization[ii] != 0){
			printf("ii = %d, freq = %d\n",ii,freq_realization[ii]);
		}
	}
	
	//*value=1000.0;
	return 0;
}
