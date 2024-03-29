# Periodic Pattern Matching
Problem Statement: Implement Parallel Periodic Pattern Matching using MPI

## Directories and files
`testcase/`: contains python script `gen_testcase.py` for sample testcase generation  
`lab4_io.h` and `lab4_io.c`: functions to read text & pattern from file and check the correctness of the result  
`main_mpi.c`: function `main()`  
`lab4_mpi.h`: header file for the functions to be implemented  
`lab4_mpi.c`: implement the function in this file  
Refer to respective files for further details.  
**Do not change the directory structure and prototype of functions.**

## Building and Executing
If you are using `c`, compile your code as follows:
```
mpicc -lm main_mpi.c lab4_mpi.c lab4_io.c -o ppm
```
If you are using `c++`, compile your code as follows:
```
mpic++ -lm main_mpi.c lab4_mpi.cpp lab4_io.c -o ppm
```
#### Command Line Arguments
The program takes one command line arguments:
- arg: input filename (consist text and patterns)  

To run the program:
```
mpirun -np 4 ./ppm <input filename>
```
Example:
```
mpirun -np 4 ./ppm ./testcase/testcase_10000_10
```

## Generating testcases
Script `gen_testcase.py` generates testcases as per the parameters and output the generated testcase in file `testcase_<n>_<num_patterns>` in the desired format. You might need to change the values of variables `n` and `num_patterns` in the script. Read the comments in the script for more information.
```
python3 gen_testcase.py
```

## Input-Output Specifications
#### Input dataset specifications
- n : length of text
- text : text in which pattern is to be matched
- num_patterns : #patterns to be matched in the text
- m_set : lengths of patterns in pattern_set
- p_set : periods of patterns in pattern_set
- pattern_set : set of patterns to be matched

The first line of the input file contains `n` followed by `num_patterns`. The second line contains the `text`. Third and fourth lines contain length of patterns `m_set`, and period of patterns `p_set` respectively. Next `num_patterns` lines contain the patterns to be matched. All the values in one line are space separated.  
