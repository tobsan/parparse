__BEGIN_PROGRAM

int main() {
    int p;
    int x;
    x = 2;
    p = 2;
    while(p < 5) {
        x = x * x;
        p = p + 1;
        while(p < 5) {
            x = x * x;
            p = p + 1;
        }
        int t;
        int s;
        s = 1;
        t = 10 * 2;
        while(p < 5) {
            x = x * x;
            p = p + 1;
        }
        int t;
        int s;
        s = 1;
        t = 10 * 2;
        while(s < t) {
            s = s+2;
            t = t+1;
        }
    }
}

__END_PROGRAM
