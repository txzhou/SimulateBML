// Appendix B
// C code to move cars

#include <R.h>


// "white" == 0
// "red" == 1
// "blue" == 2

// "move" == 1
// "stay" == 0

void move_red(int *grid_in, int *nrows, int *ncols, int *matrix_note, int *grid_out)
{
  int ncells = *nrows * *ncols;
  int pos, pos_right;
  
  for (pos = 0; pos < ncells; pos++) {
    pos_right = (pos + *nrows) % (ncells);
    if (grid_in[pos] == 1 && grid_in[pos_right] == 0) {
      matrix_note[pos] = 1;
    }
  }
  
  for (pos = 0; pos < ncells; pos++) {
    pos_right = (pos + *nrows) % (ncells);
    if (matrix_note[pos] == 1) {
      grid_out[pos] = 0;
      grid_out[pos_right] = 1;
    }
  }
}


void move_blue(int *grid_in, int *nrows, int *ncols, int *matrix_note, int *grid_out)
{
  int ncells = *nrows * *ncols;
  int pos, pos_up; // pos_up is +1 in row number.
  
  for (pos = 0; pos < ncells; pos++) {
    pos_up = pos + 1;
    if (pos_up % *nrows == 0) {
      pos_up = pos_up - *nrows;
    }
    
    if (grid_in[pos] == 2 && grid_in[pos_up] == 0) {
      matrix_note[pos] = 1;
    }
  }
  
  for (pos = 0; pos < ncells; pos++) {
    pos_up = pos + 1;
    if (pos_up % *nrows == 0) {
      pos_up = pos_up - *nrows;
    }
    
    if (matrix_note[pos] == 1) {
      grid_out[pos] = 0;
      grid_out[pos_up] = 2;
    }
  }
}
