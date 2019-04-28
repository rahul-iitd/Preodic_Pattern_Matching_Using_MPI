//#include "lab4_mpi.h"

#include <stdlib.h>
//#include "mpi.h"
#include <vector>
#include <iostream>
#include <algorithm>
#include <cmath>
#include <string.h>

using namespace std;

int* witness_arr(char* Y, int period, int m){
    int pie;
    
    if (period<=ceil(m/2.0)) pie = period;
    else pie = ceil(m/2.0);
    int* arr = (int*)malloc(sizeof(int)*(pie));
    for (int i=0; i<pie; i++) {
        if (i==0) arr[i]=(0);
        else{
            for (int j=0; j<m-i; j++) {
                if (Y[j]!=Y[j+i]){
                    arr[i]=(j);
                    break;
                }
            }
        }
    }
    return arr;
}

int duel(char* Z,int n,char* Y,int m,int* wit_arr,int i,int j){
    int k = wit_arr[j-i];
    if (j+k>n-1)return i;
    if (Z[j+k]!=Y[k])return i;
    return j;
}

char* substr(char* s,int index,int l){
    char* s_sub;
    s_sub = (char*)malloc(sizeof(char)*(l+1));
    for (int i=0; i<l; i++) {
        s_sub[i]=s[index+i];
    }
    s_sub[l] = '\0';
    return s_sub;
}

int* np_text_analysis(char* T,int n,char* P,int m,int* phi_p){
    int b;
    if ((n)%(int)ceil(m/2.0)==0) b = (n)/ceil((m)/2.0)-1;
    else b = (n)/ceil((m)/2.0);
    int* match_pos = (int*)malloc(sizeof(int)*(b+2));
    int pot_pos[b+1];
    int i;
    for (int bi= 0;bi<=b;bi++){
        i = bi*ceil((m)/2.0);
        
        if (bi==b){
            for (int j=i+1; j <= n-1; j++) {
                i = duel(T,n, P,m, phi_p, i, j);
            }
            pot_pos[bi]=i;
        }
        else{
            for (int j=i+1; j <= (bi+1)*ceil((m)/2.0)-1; j++) {
                i = duel(T,n, P,m, phi_p, i, j);
            }
            pot_pos[bi]=i;
        }
    }
    int len = 0;
    for (int i=0; i<b+1; i++) {
        if (strcmp(substr(T,pot_pos[i],m),P)==0) {
            len++;
            match_pos[len]=pot_pos[i];
        }
    }
    match_pos[0] = len;
    
    return match_pos;
}



int* P_TextAnalysis(char* T,int n,char* P,int m,int period){
    int k, s;
    int C[period][(n-m)/period+1];
    int* pos;
    char* u;
    char* v;
    int* MATCH = (int*)malloc(sizeof(int)*(n-m+1));
    
    char* P_dash = substr(P,0,2*period-1);
    
    int* phi_Pdash = witness_arr(P_dash, ceil(m/2.0), 2*period-1);
    pos = np_text_analysis(T,n, P_dash,2*period-1, phi_Pdash);
    
    
    u = substr(P,0,period);
    
    k = (int)floor(m/period);
    
    if (k*period != m) v = substr(P,k*period,m-k*period);
    
    int M[n];
    for (int i=0; i<n; i++) {
        int temp = 0;
        char* u_sq_v = (char *) malloc(1 + 2*strlen(u)+ strlen(v) );
        strcpy(u_sq_v, u);
        strcat(u_sq_v, u);
        strcat(u_sq_v, v);
        int flag = 0;
        for (int l=1; l<=pos[0]; l++) {
            if (i==pos[l]) flag=1;
        }
        
        if (flag == 1 && (strcmp(substr(T,i,2*(int)strlen(u)+ (int)strlen(v)),u_sq_v)==0)) temp = 1;
        M[i]=temp;
    }
    
    
    
    for (int i=0; i< period; i++) {
        s = (int)floor((n-1-i)/period) + 1;
        int S[s];
        for (int a=0; a<s; a++) {
            S[a]=M[i+a*period];
        }
        for (int j=0; j<s; j++) {
            C[i][j]=0;
            int count = 0;
            if (j+k-1<=s){
                for(int c=j; c<j+k-1;c++){
                    if (S[c]==1) count++;
                }
                if (count == k-1) C[i][j]=1;
            }
        }
    }
    
    for (int j= 0; j<=n-m; j++) {
        for (int i=0; i<period && i<=j; i++) {
            if ((j-i)%period==0){
                MATCH[j] = C[i][(j-i)/period];
            }
        }
    }
    
    return MATCH;
}


void periodic_pattern_matching (
                                int n,
                                char *text,
                                int num_patterns,
                                int *m_set,
                                int *p_set,
                                char **pattern_set,
                                int **match_counts,
                                int **matches)
{
    int m, period;
    vector< vector<int> > matched_indexes(num_patterns);
    match_counts[0] = (int*)malloc(sizeof(int)*num_patterns);
    
    int c =0;
    int final_arr[n];
    for (int p=0; p<num_patterns; p++) {
        m = m_set[p]; // lenght of pattren
        period = p_set[p]; // period of pattern
        int* MATCH;
        MATCH = P_TextAnalysis(text,n, pattern_set[p],m, period);
        int count = 0;
        for (int i=0; i<=n-m; i++) {
            if (MATCH[i]==1) {
                final_arr[c]=i;
                count++;
                c++;
            }
        }
        
        match_counts[0][p]=count;
        cout<<"Done: "<<p<<endl;
    }
    matches[0] = (int*)malloc(sizeof(int)*c);
    cout<<c<<endl;
    for (int i=0; i<c; i++) {
        matches[0][i] = final_arr[i];
        cout<<final_arr[i]<<" ";
    }
}

//int
//main(){
//    char* s1 = "babababababaabab";
//    char* s2 = "abababa";
//
//    int* a = P_TextAnalysis(s1, 16, s2, 7, 2);
//
//    for (int i=0; i<10; i++) {
//        cout<<a[i]<<" ";
//    }
//    return 0;
//}

