#include <cstdlib>
#include <vector>
#include <iostream>

using namespace std;

typedef vector< double > VD;
typedef vector< VD > VVD;
typedef vector< int > VI;
typedef vector< VI > VVI;
int n;
VVD mem;
VD d;
VVI c;
VVI egde;



double solve(int remaining, int current, double probsRemaining){
    if(remaining == 0)
        return 0;

    double &best = mem[remaining][current];
    if(best < 1e98)
        return best;
    for (int i = 0; i < n; i++) {
        if((1 << i) & remaining){
            double res = solve(remaining - (1 << i), i, probsRemaining - d[i]) 
                       + c[current][i]*probsRemaining;
            if(res < best){
                best = res;
                egde[remaining][current] = i;
            }
        }
    }
    return best;
}

int main(int argc, char** argv) {
    cin >> n;
    d = VD(n);
    for (int i = 0; i < n; i++) {
        cin >> d[i];
    }
    c = VVI(n, VI(n));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cin >> c[i][j];
        }
    }
    mem = VVD(1 << n, VD(n, 1e99));
    egde = VVI(1 << n, VI(n));
    
    cout << "Min. Expected Latency: " << solve((1<<n)-2, 0, 1.0 - d[0]) << endl;
    cout << "Path: ";
    int remaining = (1 << n) - 1;
    int now = 0;
    while(remaining) {
        cout << (now+1) << " ";
        remaining -= 1 << now;
        now = egde[remaining][now];
    }
    cout << endl;

    return 0;
}
