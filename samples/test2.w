f id(x) {
  r x + 0.25;
}

a x = 0.7;
a y = 0.3;
i (x < 0.9) {
  p x;
  y = x;
}
p y;

a xx = 0.0;
a yy = 0.0;
a zz = id(0.25);
w (xx < 1.1) {
  xx = xx + 0.1;
  yy = yy + 0.5;
  zz = 0.7;
}
p xx + yy + zz;