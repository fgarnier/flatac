#include <stdio.h>
#include <stdlib.h>

/* Simulates tossing an unbiased coin n times.  Returns 
 * the number of heads.
 */
int toss(int n) {
  int nHeads = 0, i;
  for (i = 0; i < n; ++i)
    // rand() % 2 yields 0 or 1 with uniform probability
    nHeads += rand() % 2;
  return nHeads;
}

/* Perform nTrials of an nTosses coin-tossing experiment 
 * and store the results in nOccur.
 */
void performExperiment(int * nOccur, int nTosses, 
                       int nTrials) 
{
  if (!nOccur) return;
  int i;
  // Initialize nOccur.
  for (i = 0; i <= nTosses; ++i)
    nOccur[i] = 0;
  // Perform nTrials of the experiment.
  for (i = 0; i < nTrials; ++i)
    // 1. toss(nTosses) returns the outcome of one trail.
    // 2. Increment the count for that outcome.
    nOccur[toss(nTosses)]++;
}

/* Given an array of occurrence data of size sz 
 * representing the results of nTrials of an experiment, 
 * each instance of which yields an integer in the range 
 * [0, sz), prints a distribution labeled with the outcomes
 * and the percentages (as an int) of trials that yielded 
 * those outcomes.
 */
void printDistribution(int * nOccur, int sz, int nTrials) {
  if (!nOccur) return;
  int i, j;
  for (i = 0; i < sz; ++i) {
    int percent = (100 * nOccur[i]) / nTrials;
    printf("%2d %2d ", i, percent);
    for (j = 0; j < percent; ++j)
      printf("*");
    printf("\n");
  }
}

/* Prints usage. */
void printUsage() {
  printf("Usage: binomial [# tosses] [# trials]\n");
}

/* Graphs a distribution for nTrials of a coin-tossing
 * experiment, where each trial consists of nTosses coin 
 * tosses, and the number of heads is counted.  Explores 
 * the binomial distribution.
 */
int main(int argc, char ** argv) {
  // Zero'th argument: name of the executable
  // First argument: nTosses
  // Second argument: nTrials
  // If different, print usage and quit.
  if (argc != 3) {
    printUsage();
    return 0;
  }

  // Obtain the input.  Protect against malformed input.
  int nTosses, nTrials, numRead;
  numRead = sscanf(argv[1], "%d", &nTosses);
  if (numRead != 1 || nTosses <= 0) {
    printUsage();
    return 0;
  }
  numRead = sscanf(argv[2], "%d", &nTrials);
  if (numRead != 1 || nTrials <= 0) {
    printUsage();
    return 0;
  }

  // Set up the occurrence array, which maps the number of
  // heads out of nTosses to the number of trials that had
  // precisely that number of heads.
  int nOccur[nTosses+1];

  // Perform the experiment.
  performExperiment(nOccur, nTosses, nTrials);

  // Visualize the result of the experiment.
  printDistribution(nOccur, nTosses+1, nTrials);

  return 0;
}
