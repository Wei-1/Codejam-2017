#include <cstdio>
#include "again.h"
#include "message.h"

#define MAX_NODES 20
#define MOD 1000000007

using namespace std;

typedef long long ll;

ll a[MAX_NODES], b[MAX_NODES];

int main() {
  int nodeNum = NumberOfNodes();
  int nodeBegin = (int) (MyNodeId() * GetN() / nodeNum);
  int nodeEnd = (int) ((MyNodeId() + 1) * GetN() / nodeNum);

  for(int i = nodeBegin; i < nodeEnd; i++) {
    int k = i % nodeNum;
    a[k] = (a[k] + GetA(i)) % MOD;
    b[k] = (b[k] + GetB(i)) % MOD;
  }

  if(MyNodeId() != 0) {
    for(int k = 0; k < nodeNum; k++) {
      PutLL(0, a[k]);
      PutLL(0, b[k]);
    }
    Send(0);
  } else {
    for(int k = 1; k < nodeNum; k++) {
      Receive(k);
      for(int i = 0; i < nodeNum; i++) {
        a[i] = (a[i] + GetLL(k)) % MOD;
        b[i] = (b[i] + GetLL(k)) % MOD;
      }
    }

    ll res = 0;
    for(int i = 0; i < nodeNum; i++) {
      for(int j = 0; j < nodeNum; j++) {
        if((i + j) % nodeNum > 0) {
          res = (res + (a[i] * b[j] % MOD)) % MOD;
        }
      }
    }
    printf("%lld\n", res);
  }
  return 0;
}