f input_to_number(x) {
    i (!(s s x == [1])) {
        r 0;
    }
    a len = (s x)[0];
    a n = 0;
    a idx = 0;
    w (idx < len - 1) {
        i (!(x[idx] >= 48) O !(x[idx] <= 57)) {
            r 0;
        }
        n = n * 10;
        n = n + x[idx] - 48;
	idx = idx + 1;
    }
    r n;
}

a x = [];
a good = F;
a the_number = 0;
w (!good) {
    p "Person 1, type a number for person 2 to guess (must be between 1-100).";
    l x;
    a y = input_to_number(x);
    i (y >= 1 A y <= 100) {
        good = T;
	the_number = y;
    }
    i (!good) {
        p "Please type a number between 1-100.";
    }
}

a idx = 0;
w (idx < 200) {
    p "";
    idx = idx + 1;
}

x = [];
good = F;
p "Ok, cleared the terminal. Person 2, start guessing.";
w (!good) {
    p "Person 2, guess a number (must be between 1-100).";
    l x;
    a y = input_to_number(x);
    i (y >= 1 A y <= 100) {
        i (y < the_number) {
            p "You guessed too small.";
        }
        i (y > the_number) {
            p "You guessed too large.";
        }
        i (y == the_number) {
            p "You guessed correctly!";
	    good = T;
        }
    }
    i (!(y >= 1 A y <= 100)) {
        p "Please type a number between 1-100.";
    }
}
