#include <cstdlib>
#include <vector>
#include <iostream>

using namespace std;

typedef vector< double > VD;
typedef vector< int > VI;
typedef vector< VI > VVI;
int n;
VD c;
VVI g;

double solve(int visited, int current){
    if(visited == 0)
        return current == 0 ? 0 : 1e99;

    double best = 1e99;
    for (int i = 0; i < n; i++) {
        if((1 << i) & visited){
            double res = solve(visited - (1 << i), i) + g[i][current]*c[current];
            best = min(best, res);
        }
    }
    return best;
}

int main(int argc, char** argv) {
    cin >> n;
    c = VD(n);
    for (int i = 0; i < n; i++) {
        cin >> c[i];
    }
    g = VVI(n, VI(n));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cin >> g[i][j];
        }
    }

    double best = 1e99;
    for (int i = 0; i < n; i++) {
        best = min(best, solve((1 << n) - 1, i));
    }

    cout << best << endl;

    return 0;
}


