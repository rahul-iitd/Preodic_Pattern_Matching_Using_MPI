//#include "lab4_mpi.h"

#include <malloc.h>
#include "mpi.h"
#include <vector>
#include <string>
#include <iostream>
#include <algorithm>
#include <cmath>

using namespace std;

vector<int> witness_arr(string Y, int period, int m){
    int pie;
    
    if (period<=ceil(m/2.0)) pie = period;
    else pie = ceil(m/2.0);
    
    vector<int> arr;
    for (int i=0; i<pie; i++) {
        if (i==0) arr.push_back(0);
        else{
            for (int j=0; j<Y.length(); j++) {
                if (Y.at(j)!=Y.at(j+i)){
                    arr.push_back(j);
                    break;
                }
            }
        }
    }
    return arr;
}

int duel(string Z,string Y,vector<int> wit_arr,int i,int j){
    int k = wit_arr[j-i];
    if (j+k>Z.length()-1)return i;
    if (Z.at(j+k)!=Y.at(k))return i;
    return j;
}

vector<int> np_text_analysis(string T,string P,vector<int> phi_p){
    vector<int> match_pos;
    vector<int> pot_pos;
    int b;
    if ((T.length())%(int)ceil(P.length()/2.0)==0) b = (T.length())/ceil((P.length())/2.0)-1;
    else b = (T.length())/ceil((P.length())/2.0);
    int i;
    for (int bi= 0;bi<=b;bi++){
        i = bi*ceil((P.length())/2.0);
        
        if (bi==b){
            for (int j=i+1; j <= T.length()-1; j++) {
                i = duel(T, P, phi_p, i, j);
            }
            pot_pos.push_back(i);
        }
        else{
            for (int j=i+1; j <= (bi+1)*ceil((P.length())/2.0)-1; j++) {
                i = duel(T, P, phi_p, i, j);
            }
            pot_pos.push_back(i);
        }
    }
    
    for (int i=0; i<pot_pos.size(); i++) {
        if (T.substr(pot_pos[i],P.length())==P) match_pos.push_back(pot_pos[i]);
    }
    return match_pos;
}

vector<int> P_TextAnalysis(string T,string P,int period){
    int m = (int)P.length();
    int n = (int)T.length();
    int k, s;
    vector<int> pos, M;
    string u;
    string v = "";
    vector<int> MATCH(n-m+1,0);
    
    string P_dash = P.substr(0,2*period-1);
    
    vector<int> phi_Pdash = witness_arr(P_dash, ceil(m/2.0), 2*period-1);
    pos = np_text_analysis(T, P_dash, phi_Pdash);
    
    
    u = P.substr(0,period);
    
    k = (int)floor(m/period);
    
    if (k*period != m) v = P.substr(k*period,m-k*period);
    
    std::vector<int>::iterator it;
    for (int i=0; i<n; i++) {
        int temp = 0;
        string u_sq_v;
        u_sq_v.append(u);
        u_sq_v.append(u);
        u_sq_v.append(v);
        it = find(pos.begin(), pos.end(), i);
        if ((it != pos.end()) && (T.substr(i,u_sq_v.length())==u_sq_v)) temp = 1;
        M.push_back(temp);
    }
    
    
//    int C[period][(n-m)/period+100];
    vector< vector<int> > C(n);
    for (int i=0; i< period; i++) {
        s = (int)floor((M.size()-1-i)/period) + 1;
        vector<int> S;
        for (int a=0; a<s; a++) {
            S.push_back(M[i+a*period]);
        }
        for (int j=0; j<s; j++) {
            C[i].push_back(0);
            int count = 0;
            if (j+k-1<=S.size()){
                for(int c=j; c<j+k-1;c++){
                    if (S[c]==1) count++;
                }
                if (count == k-1) C[i][j]=1;
            }
        }
    }
    
    //    for (int j= 0; j<=n-m; j++) {
    //        for (int i=0; i<period; i++) {
    //            int l=0;
    //            while(true){
    //                if (j == i+l*period) MATCH[j]=C[i][l];
    //                if (i+l*period > j) break;
    //                l++;
    //            }
    //        }
    //    }
    
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
    string T(text);
    int m, period;
    vector< vector<int> > matched_indexes(num_patterns);
    
    
    
    int total_tasks, id, num_process_child, offset;
    
    MPI_Comm_rank(MPI_COMM_WORLD, &id);
    MPI_Comm_size(MPI_COMM_WORLD, &total_tasks);
    MPI_Status status;
    
    num_process_child = total_tasks - 1;
    if (num_process_child>0){
    
    int sender_id = 1;
    if (id==0){
        match_counts[0] = (int*)malloc(sizeof(int)*num_patterns);
        
        for (int i=0; i<num_patterns; i++) {
            MPI_Recv(&offset,1,MPI_INT, sender_id,0,MPI_COMM_WORLD,&status);
            //                cout<<"Offset is:"<<offset<<endl;
            vector<int> ans(offset);
            
            MPI_Recv(&ans[0],offset,MPI_INT, sender_id,0,MPI_COMM_WORLD,&status);
            
            //                cout<<endl;
            matched_indexes[i]=ans;
            sender_id = (sender_id)%num_process_child+1;
        }
        
        
        
        int sum = 0;
        for (int i=0; i<num_patterns; i++) {
            match_counts[0][i] = (int)matched_indexes[i].size();
            sum += matched_indexes[i].size();
        }
        
        matches[0] = (int*)malloc(sizeof(int)*sum);
        
        int c = 0;
        for (int i=0; i<num_patterns; i++) {
            for (int j=0; j<matched_indexes[i].size(); j++) {
                matches[0][c] = matched_indexes[i][j];
                //                cout << matched_indexes[i][j]<<" ";
                c++;
            }
            //            cout<<endl;
        }
    }
    
    if (id>0)
    {
        for (int p= id-1; p<num_patterns; p=p+num_process_child) {
            vector<int> ans,MATCH;
            string P(pattern_set[p]);
            period = p_set[p];
            MATCH = P_TextAnalysis(T, P, period);
            
            for (int i=0; i<=n-m_set[p]; i++) {
                if (MATCH[i]==1) ans.push_back(i);
            }
            offset = ans.size();
            MPI_Send(&offset,1,MPI_INT, 0,0,MPI_COMM_WORLD);
            MPI_Send(&ans[0],offset,MPI_INT, 0,0,MPI_COMM_WORLD);
        }
    }
    }
    else{
        match_counts[0] = (int*)malloc(sizeof(int)*num_patterns);
        
        
        for (int p=0; p<num_patterns; p++) {
            m = m_set[p]; // lenght of pattren
            period = p_set[p]; // period of pattern
            string P(pattern_set[p]);
            vector<int> MATCH;
            MATCH = P_TextAnalysis(T, P, period);
            
            for (int i=0; i<=n-m; i++) {
                if (MATCH[i]==1) matched_indexes[p].push_back(i);
            }
//            cout<<"Done :"<<p<<endl;
//            cout<<"Size is:"<<matched_indexes[p].size()<<endl;
            
        }
        
        int sum = 0;
        for (int i=0; i<num_patterns; i++) {
            match_counts[0][i] = (int)matched_indexes[i].size();
            sum += matched_indexes[i].size();
        }
        
        matches[0] = (int*)malloc(sizeof(int)*sum);
        
        int c = 0;
        for (int i=0; i<num_patterns; i++) {
            for (int j=0; j<matched_indexes[i].size(); j++) {
                matches[0][c] = matched_indexes[i][j];
//                cout << matched_indexes[i][j]<<" ";
                c++;
            }
//            cout<<endl;
        }
    }
    
}


