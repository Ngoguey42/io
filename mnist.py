
import numpy as np
np.set_printoptions(linewidth=300, threshold=100000)
rng = np.random.RandomState(42)


def find_countour(img):
    assert img.size > 0
    assert img.ndim == 2
    img = img.astype(bool, copy=False)

    h, w = img.shape
    h, w = h + 2, w + 2
    infos = np.zeros((h, w), 'int8')
    infos[1:-1, 1:-1] = img
    debug_unique_set = set()

    f_ = False
    t_ = True
    dirty = True
    while dirty:
        dirty = False
        for i in range(0, h - 1):
            for j in range(0, w - 1):
                br = infos[i    , j    ] != 0
                tr = infos[i - 1, j    ] != 0
                tl = infos[i - 1, j - 1] != 0
                bl = infos[i    , j - 1] != 0
                zone = [ tl, tr, bl, br ]
                if zone == [f_, t_, t_, f_]:
                    # print('add br', i, j)
                    infos[i    , j    ] = 1
                    dirty = True
                elif zone == [t_, f_, f_, t_]:
                    # print('add bl at', i, j)
                    infos[i    , j - 1] = 1
                    dirty = True

    def find_component(i0, j0):
        # print('>>> find_component', i0, j0)
        coords = []
        previj = i0 + 1, j0

        i, j = i0, j0

        count = 0
        while True:

            def follow():
                br = infos[i    , j    ] > 0
                tr = infos[i - 1, j    ] > 0
                tl = infos[i - 1, j - 1] > 0
                bl = infos[i    , j - 1] > 0
                if br:
                    infos[i    , j    ] = 2
                else:
                    infos[i    , j    ] = -1
                if tr:
                    infos[i - 1, j    ] = 2
                else:
                    infos[i - 1, j    ] = -1
                if tl:
                    infos[i - 1, j - 1] = 2
                else:
                    infos[i - 1, j - 1] = -1
                if bl:
                    infos[i    , j - 1] = 2
                else:
                    infos[i    , j - 1] = -1
                r = i, j + 1
                t = i - 1, j
                l = i, j - 1
                b = i + 1, j
                zone = [ tl, tr, bl, br ]

                # print("> {} {}  |  {} ({}, {}) ({}, {}) \n  {} {}".format(
                #     int(tl), int(tr),
                #     count, i, j, *previj,
                #     int(bl), int(br),
                # ))

                f_ = False
                t_ = True
                if previj == b:
                    if   zone == [f_, f_, f_, t_] or zone == [t_, t_, t_, f_]: return r
                    # elif zone == [f_, t_, f_, f_] or zone == [t_, f_, t_, t_]: return
                    # elif zone == [t_, f_, f_, f_] or zone == [f_, t_, t_, t_]: return
                    elif zone == [f_, f_, t_, f_] or zone == [t_, t_, f_, t_]: return l
                    # elif zone == [t_, t_, f_, f_] or zone == [f_, f_, t_, t_]: return
                    elif zone == [t_, f_, t_, f_] or zone == [f_, t_, f_, t_]: return t
                    # elif zone == [f_, t_, t_, f_]:
                    #     infos[i - 1, j - 1] = 0;
                    #     return r
                    # elif zone == [t_, f_, f_, t_]:
                    #     infos[i - 1, j    ] = 0;
                    #     return l
                    else: assert False
                elif previj == r:
                    if   zone == [f_, f_, f_, t_] or zone == [t_, t_, t_, f_]: return b
                    elif zone == [f_, t_, f_, f_] or zone == [t_, f_, t_, t_]: return t
                    # elif zone == [t_, f_, f_, f_] or zone == [f_, t_, t_, t_]: return
                    # elif zone == [f_, f_, t_, f_] or zone == [t_, t_, f_, t_]: return
                    elif zone == [t_, t_, f_, f_] or zone == [f_, f_, t_, t_]: return l
                    # elif zone == [t_, f_, t_, f_] or zone == [f_, t_, f_, t_]: return
                    # elif zone == [f_, t_, t_, f_]:
                    #     infos[i - 1, j - 1] = 0;
                    #     return b
                    # elif zone == [t_, f_, f_, t_]:
                    #     infos[i    , j - 1] = 0;
                    #     return t
                    else: assert False
                elif previj == t:
                    # if   zone == [f_, f_, f_, t_] or zone == [t_, t_, t_, f_]: return
                    if   zone == [f_, t_, f_, f_] or zone == [t_, f_, t_, t_]: return r
                    elif zone == [t_, f_, f_, f_] or zone == [f_, t_, t_, t_]: return l
                    # elif zone == [f_, f_, t_, f_] or zone == [t_, t_, f_, t_]: return
                    # elif zone == [t_, t_, f_, f_] or zone == [f_, f_, t_, t_]: return
                    elif zone == [t_, f_, t_, f_] or zone == [f_, t_, f_, t_]: return b
                    # elif zone == [f_, t_, t_, f_]:
                    #     infos[i    , j    ] = 0;
                    #     return l
                    # elif zone == [t_, f_, f_, t_]:
                    #     infos[i    , j - 1] = 0;
                    #     return r
                    else: assert False
                elif previj == l:
                    # if   zone == [f_, f_, f_, t_] or zone == [t_, t_, t_, f_]: return
                    # elif zone == [f_, t_, f_, f_] or zone == [t_, f_, t_, t_]: return
                    if   zone == [t_, f_, f_, f_] or zone == [f_, t_, t_, t_]: return t
                    elif zone == [f_, f_, t_, f_] or zone == [t_, t_, f_, t_]: return b
                    elif zone == [t_, t_, f_, f_] or zone == [f_, f_, t_, t_]: return r
                    # elif zone == [t_, f_, t_, f_] or zone == [f_, t_, f_, t_]: return
                    # elif zone == [f_, t_, t_, f_]:
                    #     infos[i    , j    ] = 0;
                    #     return t
                    # elif zone == [t_, f_, f_, t_]:
                    #     infos[i - 1, j    ] = 0;
                    #     return b
                    else: assert False
                else: assert False

            coords.append((i, j))
            assert (i, j) not in debug_unique_set, (i, j)
            debug_unique_set.add((i, j))
            count += 1
            # if count == 500: break

            (i, j), previj = follow(), (i, j)

            if i == i0 and j == j0:
                break
        return coords

    components = []
    inside = False
    debug_break = False
    for i in range(1, h - 1):
        if debug_break: break
        for j in range(1, w - 1):
            if debug_break: break
            u = infos[i, j - 1]
            v = infos[i, j]
            outside = not inside

            # print(i, j, (['outside', 'inside'][inside], u, v))

            if outside and u == 0 and v == 1:
                components.append(find_component(i, j))
                # debug_break = True
                inside = True
            elif inside and u == 2 and v == -1:
                inside = False
            elif outside and u == -1 and v == 2:
                inside = True


    # def simplify(coords):
    #     # (i0, j0), *coords = coords
    #     edges = [
    #         (i2 - i1, j2 - j1)
    #         for (i1, j1), (i2, j2) in zip(coords, coords[1:] + [coords[0]])
    #     ]
    #     yield coords[0]
    #     for edge0, ij, edge1 in zip(edges, coords[1:], edges[1:]):
    #         if edge0 != edge1:
    #             yield ij

    # components = [list(simplify(coords)) for coords in components]

    test = np.zeros((h * 2, w * 2), 'int8')
    for i in range(h):
        for j in range(w):
            if infos[i, j] != 0:
                test[i * 2 + 1, j * 2 + 1] = infos[i, j]
    for coords in components:
        for (i, j), (k, l) in zip(coords, coords[1:]):
            test[i * 2, j * 2] = 9
            test[k * 2, l * 2] = 9
            k = k + i
            l = l + j
            # test[k, l] += 8

    # print(infos)
    test[test == -1] = 5
    # print(str(test)
    #       .replace('5', ' ')
    #       .replace('0', ' ')
    #       .replace('1', '-')
    #       .replace('3', '@')
    #       .replace('2', '~')
    #       .replace('9', '*')
    #       .replace('8', '.')
    # )


    components = [
        [(i - 1, j - 1) for (i, j) in coords]
        for coords in components
    ]
    return components

imgs = np.frombuffer(open("train-images.idx3-ubyte", 'rb').read(), 'uint8', offset=16).reshape(-1, 28, 28)
labs = np.frombuffer(open("train-labels.idx1-ubyte", 'rb').read(), 'uint8', offset=8)
# imgs = np.frombuffer(open("./t10k-images.idx3-ubyte", 'rb').read(), 'uint8', offset=16).reshape(-1, 28, 28)
# labs = np.frombuffer(open("t10k-labels.idx1-ubyte", 'rb').read(), 'uint8', offset=8)
# print(imgs.shape)
# print(labs.shape)
# for i in range(4):
#     j = labs.tolist().index(i)
"""
# multi compo
test 8

# checkerboard pattern
test 18

"""

def dump(ijs):
    ijs = np.asarray(ijs[::-1]).astype(float)
    # meani = ijs[:, 0].mean()
    # meanj = ijs[:, 1].mean()
    maxi = ijs[:, 0].max()
    maxj = ijs[:, 1].max()
    mini = ijs[:, 0].min()
    minj = ijs[:, 1].min()

    # print(maxi - mini, maxj - minj)

    print()
    # print(maxi, mini, (maxi - mini), '  ', maxj, minj, (maxj - minj))
    # print('is:', np.around(ijs[:, 0].min(), 3), np.around(ijs[:, 0].mean(), 3), np.around(ijs[:, 0].max(), 3))
    # print('js:', np.around(ijs[:, 1].min(), 3), np.around(ijs[:, 1].mean(), 3), np.around(ijs[:, 1].max(), 3))

    ijs = (ijs - np.asarray([[(maxi + mini), (maxj + minj)]]) / 2)
    # print('is:', np.around(ijs[:, 0].min(), 3), np.around(ijs[:, 0].mean(), 3), np.around(ijs[:, 0].max(), 3))
    # print('js:', np.around(ijs[:, 1].min(), 3), np.around(ijs[:, 1].mean(), 3), np.around(ijs[:, 1].max(), 3))

    ijs = ijs / 14
    # print('is:', np.around(ijs[:, 0].min(), 3), np.around(ijs[:, 0].mean(), 3), np.around(ijs[:, 0].max(), 3))
    # print('js:', np.around(ijs[:, 1].min(), 3), np.around(ijs[:, 1].mean(), 3), np.around(ijs[:, 1].max(), 3))
    # ijs = (ijs - [[meani, meanj]]) / 14
    # ijs = (ijs - [[meani, meanj]]) / 10 / 100 * 9

    xys = np.c_[ijs[:, 1], -ijs[:, 0]]
    # print('xs:', np.around(xys[:, 0].min(), 3), np.around(xys[:, 0].mean(), 3), np.around(xys[:, 0].max(), 3))
    # print('ys:', np.around(xys[:, 1].min(), 3), np.around(xys[:, 1].mean(), 3), np.around(xys[:, 1].max(), 3))

    s = ', '.join(
        '[{:.5f}, {:.5f}]'.format(x, y)
        # 'Vec2({:.5f}, {:.5f})'.format(x, y)
        for x, y in xys
    )
    print('  [{}],'.format(s))

# for j in range(len(labs)):
    # input()
    # i = labs[j]

for i in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]:
# for i in [0, 1, 2, 3, 4, 5, 5, 6, 7, 8, 9]:

# for j in [11]:
# for j in [3]:
# for j in [0]:
# for j in [18]:
# for j in [8]:
# for j in [222]:
# for j in [1087]:
#     i = labs[j]
    while True:
        j = rng.randint(1000)
        if labs[j] != i:
            continue
        # print('> lab:{}, idx:{}'.format(i, j))
        img = imgs[j] > 50
        # img = .copy()
        # print(img.astype(bool).astype(int))
        components = find_countour(img)
        # print([len(c) for c in components])
        if len(components) != 1:
            continue
        dump(components[0])
        # exit()
        break
        # if len(components) != 1:
        # input()
