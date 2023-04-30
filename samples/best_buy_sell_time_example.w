f dim(mat) {
    r (s (s mat))[0];
}

f len(list) {
    v dim(list) == 1;
    r (s list)[0];
}

f best_time_to_buy_sell(prices) {
    a best_b = 0;
    a best_s = 1;
    a best = prices[best_s]-prices[best_b];
    a j = 1;
    w (j < len(prices)) {
        i (prices[j] > prices[best_s]) {
            best_s = j;
            best = prices[best_s] - prices[best_b];
        }
        i (prices[j] < prices[best_b]) {
            best_b = j;
        }
        j = j + 1;
    }
    r best;
}

f slice(arr, ii, j) {
    a b = [0] sa [j-ii];
    a k = ii;
    w (k < j) {
        b[k-ii] = arr[k];
        k = k + 1;
    }
    r b;
}

f best_time_to_buy_sell_twice(prices) {
    a split = 3;
    a best = 0;
    w (split < len(prices)-3) {
        a left = slice(prices, 0, split);
        a right = slice(prices, split, len(prices));
        a res = best_time_to_buy_sell(left) + best_time_to_buy_sell(right);
        i (res > best) {
            best = res;
        }
        split = split + 1;
    }
    r best;
}

a d = [3,3,5,0,0,3,1,4];

p best_time_to_buy_sell_twice(d);

