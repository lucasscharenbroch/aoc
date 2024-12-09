#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#define IN_BOUNDS(i, j) (i >= 0 && j >= 0 && i < n && j < m)

int gcd(int a, int b) {
    return a % b == 0 ? b : gcd(b, a % b);
}

int main() {
    FILE *file;
    char lines[1024][1024];

    file = fopen("8.input", "r");

    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    int n = 0;
    for(int i = 0; fgets(lines[i], sizeof(lines[i]), file); i++) n++;

    int m = strlen(lines[0]) - 1; // (ignore '\n')

    char mask[1024][1024]; // antinode mask
    memset(mask, 0, sizeof(mask));

    for(int i = 0; i < n; i++) {
        for(int j = 0; j < m; j++) {
            for(int k = 0; k < n; k++) {
                for(int l = 0; l < m; l++) {
                    if (i == k && j == l) continue;
                    if (lines[i][j] == '.') continue;
                    if (lines[i][j] != lines[k][l]) continue;

                    int di = k - i;
                    int dj = l - j;

                    { // part 1
                        int ip = i + 2 * di;
                        int jp = j + 2 * dj;

                        if (IN_BOUNDS(ip, jp))
                            mask[ip][jp] = 1;
                    }

                    // compute maximally fine-grained (di, dj)
                    int d;
                    while((d = gcd(di, dj)) != 1) {
                        di /= d;
                        dj /= d;
                    }

                    // scan backwards and forwards
                    for (int mul = 1; true; mul++) {
                        bool should_break = true;

                        for (int direction = -1; direction <= 1; direction += 2) {
                            int ip = i + mul * di * direction;
                            int jp = j + mul * dj * direction;
                            if (IN_BOUNDS(ip, jp)) {
                                if (mask[ip][jp] == 0) mask[ip][jp] = 2;
                                should_break = false;
                            }
                        }

                        if (should_break) break;
                    }
                }
            }
        }
    }

    int sum = 0;
    int sum2 = 0;
    for(int i = 0; i < n; i++)
        for(int j = 0; j < m; j++)
            if (mask[i][j] == 1) sum++, sum2++;
            else if (mask[i][j] == 2) sum2++;

    printf("%d\n%d\n", sum, sum2);

    return 0;
}
