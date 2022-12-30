# This file is part of weak-lang.
# weak-lang is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
# weak-lang is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
# You should have received a copy of the GNU Affero General Public License
# along with weak-lang. If not, see <https://www.gnu.org/licenses/>.

# https://adventofcode.com/2021/day/1

f dim(mat) {
    r (s (s mat))[0];
}

f len(list) {
    v dim(list) == 1;
    r (s list)[0];
}

f part_one(depths) {
    v dim(depths) == 1;
    a len = len(depths);
    v len >= 1;
    a j = 1;
    a count = 0;
    w (j < len) {
        i (depths[j] > depths[j-1]) {
            count = count + 1;
        }
        j = j + 1;
    }
    r count;
}

f part_two(depths) {
    v dim(depths) == 1;
    a len = len(depths);
    v len >= 1;
    a j = 0;
    a new_depths = [0] sa [len];
    a count = 0;
    w (j < len - 2) {
        new_depths[count] = depths[j] + depths[j+1] + depths[j+2];
        count = count + 1;
        j = j + 1;
    }
    r part_one(new_depths);
}

a d = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263];

p part_one(d); # Should print 7
p part_two(d); # Should print 5