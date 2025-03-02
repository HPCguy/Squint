/* The MIT License

   Copyright (c) 2011 by Attractive Chaos <attractor@live.co.uk>

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*/

// This file implements an improved algorithm of Guenter Stertenbrink's suexco.c
// (http://magictour.free.fr/suexco.txt).

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>

/* For Sudoku, there are 9x9x9=729 possible choices (9 numbers to choose for
   each cell in a 9x9 grid), and 4x9x9=324 constraints with each constraint
   representing a set of choices that are mutually conflictive with each other.
   The 324 constraints are classified into 4 categories:

   1. row-column where each cell contains only one number
   2. box-number where each number appears only once in one 3x3 box
   3. row-number where each number appears only once in one row
   4. col-number where each number appears only once in one column

   Each category consists of 81 constraints. We number these constraints from 0
   to 323. In this program, for example, constraint 0 requires that the (0,0)
   cell contains only one number; constraint 81 requires that number 1 appears
   only once in the upper-left 3x3 box; constraint 162 requires that number 1
   appears only once in row 1; constraint 243 requires that number 1 appears
   only once in column 1.
   
   Noting that a constraint is a subset of choices, we may represent a
   constraint with a binary vector of 729 elements. Thus we have a 729x324
   binary matrix M with M(r,c)=1 indicating the constraint c involves choice r.
   Solving a Sudoku is reduced to finding a subset of choices such that no
   choices are present in the same constraint. This is equivalent to finding the
   minimal subset of choices intersecting all constraints, a minimum hitting
   set problem or a eqivalence of the exact cover problem.

   The 729x324 binary matrix is a sparse matrix, with each row containing 4
   non-zero elements and each column 9 non-zero elements. In practical
   implementation, we store the coordinate of non-zero elements instead of
   the binary matrix itself. We use a binary row vector to indicate the
   constraints that have not been used and use a column vector to keep the
   number of times a choice has been forbidden. When we set a choice, we will
   use up 4 constraints and forbid other choices in the 4 constraints. When we
   make wrong choices, we will find an unused constraint with all choices
   forbidden, in which case, we have to backtrack to make new choices. Once we
   understand what the 729x324 matrix represents, the backtracking algorithm
   itself is easy.

   A major difference between the algorithm implemented here and Guenter
   Stertenbrink's suexco.c lies in how to count the number of the available
   choices for each constraint. Suexco.c computes the count with a loop, while
   the algorithm here keeps the count in an array. The latter is a little more
   complex to implement as we have to keep the counts synchronized all the time,
   but it is 50-100% faster, depending on the input.
 */

// the sparse representation of the binary matrix
typedef struct {
   int r[324][9]; // M(r[c][i], c) is a non-zero element
   int c[729][4]; // M(r, c[r][j]) is a non-zero element
} sdaux_t;

// generate the sparse representation of the binary matrix
void sd_genmat(sdaux_t *a)
{
   int i, j, k, r, c, c2, r2;
   char nr[324];
   for (i = r = 0; i < 9; ++i) // generate c[729][4]
      for (j = 0; j < 9; ++j)
         for (k = 0; k < 9; ++k) { // this "9" means each cell has 9 possible numbers
            a->c[r][0] = 9 * i + j;                            // row-column constraint
            a->c[r][1] = (((i<3) ? 0 : ((i<6) ? 3 : 6)) +      // box-number constraint
                          ((j<3) ? 0 : ((j<6) ? 1 : 2))) * 9 +
                          k + 81;
            a->c[r][2] = 9 * i + k + 162;                      // row-number constraint
            a->c[r][3] = 9 * j + k + 243;                      // col-number constraint
            ++r;
         }
   for (c = 0; c < 324; ++c) nr[c] = 0;
   for (r = 0; r < 729; ++r) // generate r[][] from c[][]
      for (c2 = 0; c2 < 4; ++c2) {
         k = a->c[r][c2];
         a->r[k][nr[k]++] = r;
      }
}

// update the state vectors when we pick up choice r; v=1 for setting choice; v=-1 for reverting
int sd_update(sdaux_t *aux, char *sr, char *sc, int r, int v)
{
   int c2, min = 10, min_c = 0;
   for (c2 = 0; c2 < 4; ++c2) sc[aux->c[r][c2]] += 0x80;
   for (c2 = 0; c2 < 4; ++c2) { // update # available choices
      int r2, rr, cc2, c = aux->c[r][c2];
      if (v > 0) { // move forward
         for (r2 = 0; r2 < 9; ++r2) {
            if (sr[(rr = aux->r[c][r2])]++ != 0) continue; // update the row status
            for (cc2 = 0; cc2 < 4; ++cc2) {
               int cc = aux->c[rr][cc2];
               if (--sc[cc] < min) { // update # allowed choices
                  min = sc[cc];
                  min_c = cc; // register the minimum number
               }
            }
         }
      } else { // revert
         int *p;
         for (r2 = 0; r2 < 9; ++r2) {
            if (--sr[(rr = aux->r[c][r2])] != 0) continue; // update the row status
            p = &aux->c[rr][0]; ++sc[p[0]]; ++sc[p[1]]; ++sc[p[2]]; ++sc[p[3]]; // update the count array
         }
      }
   }
   return min<<16 | min_c; // return the col that has been modified and with the minimal available choices
}

// solve a Sudoku; _s is the standard dot/number representation
int sd_solve(sdaux_t *aux, char *_s, char *out)
{
   int i, j, r, c, r2, dir, cand, a, n = 0, min, hints = 0; // dir=1: forward; dir=-1: backtrack
   char sc[324];        // bit 1-7: # allowed choices; bit 8: the constraint has been used or not
   char sr[729];        // sr[r]: # times the row is forbidden by others;
   int cr[81], cc[81];  // cr[i]: row chosen at step i; cc[i]: col chosen at step i
   for (r = 0; r < 729; ++r) sr[r] = 0; // no row is forbidden
   for (c = 0; c < 324; ++c) sc[c] = (0<<7) | 9; // 9 allowed choices; no constraint has been used
   for (i = 0; i < 81; ++i) {
      a = (_s[i] >= '1' && _s[i] <= '9') ? (_s[i] - '1') : -1; // number from -1 to 8
      if (a >= 0) sd_update(aux, sr, sc, i * 9 + a, 1); // set the choice
      if (a >= 0) ++hints; // count the number of hints
      cr[i] = cc[i] = -1;
      out[i] = _s[i];
   }
   for (i = 0, dir = 1, cand = 10<<16, out[81] = 0; 1;) {
      while (i >= 0 && i < (81 - hints)) { // maximum 81-hints steps
         if (dir == 1) {
            min = cand>>16;
            cc[i] = cand & 0xffff;
            if (min > 1) {
               for (c = 0; c < 324; ++c) {
                  if (sc[c] < min) {
                     min = sc[c];
                     cc[i] = c; // choose the top constraint
                     if (min <= 1) break; // this is for acceleration; slower without this line
                  }
               }
            }
            if (min == 0 || min == 10) cr[i--] = dir = -1; // backtrack
         }
         c = cc[i];
         if (dir == -1 && cr[i] >= 0) sd_update(aux, sr, sc, aux->r[c][cr[i]], -1); // revert the choice
         for (r2 = cr[i] + 1; r2 < 9; ++r2) // search for the choice to make
            if (sr[aux->r[c][r2]] == 0) break; // found if the state equals 0
         if (r2 < 9) {
            cand = sd_update(aux, sr, sc, aux->r[c][r2], 1); // set the choice
            cr[i++] = r2; dir = 1; // moving forward
         } else cr[i--] = dir = -1; // backtrack
      }
      if (i < 0) break;
      for (j = 0; j < i; ++j) r = aux->r[cc[j]][cr[j]], out[r/9] = r%9 + '1'; // print
      ++n; --i; dir = -1; // backtrack
   }
   return n; // return the number of solutions
}

char *puzzle[20] = {
"..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9", // near worst case for brute-force solver (wiki)
".......12........3..23..4....18....5.6..7.8.......9.....85.....9...4.5..47...6...", // gsf's sudoku q1 (Platinum Blonde)
".2..5.7..4..1....68....3...2....8..3.4..2.5.....6...1...2.9.....9......57.4...9..", // (Cheese)
"........3..1..56...9..4..7......9.5.7.......8.5.4.2....8..2..9...35..1..6........", // (Fata Morgana)
"12.3....435....1....4........54..2..6...7.........8.9...31..5.......9.7.....6...8", // (Red Dwarf)
"1.......2.9.4...5...6...7...5.9.3.......7.......85..4.7.....6...3...9.8...2.....1", // (Easter Monster)
".......39.....1..5..3.5.8....8.9...6.7...2...1..4.......9.8..5..2....6..4..7.....", // Nicolas Juillerat's Sudoku explainer 1.2.1 (top 5)
"12.3.....4.....3....3.5......42..5......8...9.6...5.7...15..2......9..6......7..8",
"..3..6.8....1..2......7...4..9..8.6..3..4...1.7.2.....3....5.....5...6..98.....5.",
"1.......9..67...2..8....4......75.3...5..2....6.3......9....8..6...4...1..25...6.",
"..9...4...7.3...2.8...6...71..8....6....1..7.....56...3....5..1.4.....9...2...7..",
"....9..5..1.....3...23..7....45...7.8.....2.......64...9..1.....8..6......54....7", // dukuso's suexrat9 (top 1)
"4...3.......6..8..........1....5..9..8....6...7.2........1.27..5.3....4.9........", // from http://magictour.free.fr/topn87 (top 3)
"7.8...3.....2.1...5.........4.....263...8.......1...9..9.6....4....7.5...........",
"3.7.4...........918........4.....7.....16.......25..........38..9....5...2.6.....",
"........8..3...4...9..2..6.....79.......612...6.5.2.7...8...5...1.....2.4.5.....3", // dukuso's suexratt (top 1)
".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6...", // first 2 from sudoku17
".......12....35......6...7.7.....3.....4..8..1...........12.....8.....4..5....6..",
"1.......2.9.4...5...6...7...5.3.4.......6........58.4...2...6...3...9.8.7.......1", // 2 from http://www.setbb.com/phpbb/viewtopic.php?p=10478
".....1.2.3...4.5.....6....7..2.....1.8..9..3.4.....8..5....2....9..3.4....67....."
};

void draw(char *puzzle, char *out)
{
   for (int i=0; i<9; ++i) {
      printf("%.9s     %.9s\n", &puzzle[i*9], &out[i*9]);
   }
   printf("\n");
}


int main()
{
   int i;
   char out[82];
   sdaux_t *a = (sdaux_t *) calloc(1, sizeof(sdaux_t));
   sd_genmat(a);
   for (i=0; i<20; ++i) {
      sd_solve(a, puzzle[i], out);
      draw(puzzle[i], out);
   }
   free(a);
   return 0;
}
