import numpy as np
import matplotlib.pyplot as plt

rng = np.random.RandomState(42)

def gaussian(mu, sigma_left, sigma_right, bound_left, bound_right):
    while True:
        u1, u2 = rng.rand(), rng.rand()
        z = np.sqrt(-2 * np.log(u1)) * np.sin(2 * np.pi * u2)
        if rng.rand() < 0.5:
            v = mu - sigma_left * abs(z)
        else:
            v = mu + sigma_right * abs(z)
        if v < bound_left:
            continue
        if v > bound_right:
            continue
        return v

def gaussian_int(left_bound, left_anchor, middle, right_anchor, right_bound):
    assert float(left_bound).is_integer()
    assert float(right_bound).is_integer()
    assert left_bound <= left_anchor < middle < right_anchor <= right_bound
    v = gaussian(middle, middle - left_anchor, right_anchor - middle,
                 left_bound - 0.5, right_bound + 0.5)
    v = int(round(v))
    return v

class digit:
    # middle being a float: 50% below middle, 50%above middle
    #  - middle=10   mean a lots of '10'
    #  - middle=10.5 mean many '10' and many '11'
    # Distribution before rounding:
    # - 16% between left_bound and left_anchor
    # - 34% between left_anchor and middle
    # - 34% between middle and right_anchor
    # - 16% between right_anchor and right_bound
    # none below left_bound
    # none below max

    # left_bound = 0
    # left_anchor = 1
    # middle = 2.5
    # right_anchor = 3
    # right_bound = 9

    left_bound = 0
    left_anchor = 1
    middle = 3.5
    right_anchor = 5
    right_bound = 9


vs = np.asarray([
    gaussian_int(
        digit.left_bound, digit.left_anchor, digit.middle, digit.right_anchor, digit.right_bound
    )
    for _ in range(40000)
])

bins = sorted(list(set(vs.tolist() + list(range(digit.left_bound, digit.right_bound + 1)))))
print(bins)

for v in bins:
    count = (vs == v).sum()
    print(f'{v:<2} {count / vs.size:>3.0%} ({count})')


bins = bins + [bins[-1] * 2 - bins[-2]]
plt.hist(vs, bins=bins)
plt.savefig('salut.png')
