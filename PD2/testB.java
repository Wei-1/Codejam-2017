public class Main {
  static int MASTER_NODE = 0;
  static long SENDING_DONE = -1;
  static long LARGE_PRIME = 1000000007;

  public static void main(String[] args) {
    if (message.MyNodeId() == MASTER_NODE) {
      long result = 0;
      for (int node = 1; node < message.NumberOfNodes(); ++node) {
        while (true) {
          message.Receive(node);
          long value = message.GetLL(node);
          if (value == SENDING_DONE) {
            break;
          } else {
            result = (result + value) % LARGE_PRIME;
          }
        }
      }
      System.out.println(result);
    } else {
      long tmpresult = 0L;
      int n = (int) again.GetN();
      long[] arra = new long[n];
      long[] arrb = new long[n];
      for (int i = 0; i < n; ++i) {
        arra[i] = again.GetA((long) i);
        arrb[i] = again.GetB((long) i);
      }
      for (int i = 0; i < n; ++n) {
        for (int j = 0; j < n; ++j) {
          if ((i + j) % message.NumberOfNodes() == message.MyNodeId()) {
            long value = arra[i] * arrb[j];
            tmpresult = (tmpresult + value) % LARGE_PRIME;
          }
        }
      }
      message.PutLL(MASTER_NODE, tmpresult);
      message.Send(MASTER_NODE);
      message.PutLL(MASTER_NODE, SENDING_DONE);
      message.Send(MASTER_NODE);
    }
  }
}
