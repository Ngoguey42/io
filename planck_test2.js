/* TODO: Set object weights to avoid changing mouseForce on scale*/
var pl = planck, Vec2 = pl.Vec2, Math = pl.Math;
var width = 10.0 * 2
var height = width;
var digits_scale = 10
var BALL_RADIUS = width / 100 * 9
var ACTIVE_RAILS = true
var mouseForce = width * 40
var ARE_BULLETS = true

function bodies_of_world(w) {
  var arr = [];
  var b = w.getBodyList();

  while (b !== null) {
    arr.push(b)
    b = b.m_next
  }
  return arr
}

function fixtures_of_body(b) {
  var arr = [];
  var f = b.getFixtureList();
  while (f !== null) {
    arr.push(f)
    f = f.m_next
  }
  return arr
}

function gaussian(mu, sigma_left, sigma_right, bound_left, bound_right) {
  while (true) {
    u1 = Math.random()
    u2 = Math.random()
    z = Math.sqrt(-2 * Math.log(u1)) * Math.sin(2 * Math.PI * u2)
    if (Math.random() < 0.5)
      v = mu - sigma_left * Math.abs(z)
    else
      v = mu + sigma_right * Math.abs(z)
    if (v < bound_left)
      continue
    if (v > bound_right)
      continue
    console.log("> Gaussian mu/sigmas", mu, sigma_left, sigma_right, 'bounds', bound_left, bound_right, 'res:', v)
    return v
  }
}

function gaussian_int(left_bound, left_anchor, middle, right_anchor, right_bound) {
  /* middle being a float: 50% below middle, 50%above middle
   *  - middle=10   mean a lots of '10'
   *  - middle=10.5 mean many '10' and many '11'
   * Distribution before rounding:
   * - 16% between left_bound and left_anchor
   * - 34% between left_anchor and middle
   * - 34% between middle and right_anchor
   * - 16% between right_anchor and right_bound
   * none below left_bound
   * none below max */

  /* assert float(left_bound).is_integer()*/
  /* assert float(right_bound).is_integer()*/
  /* assert left_bound <= left_anchor < middle < right_anchor <= right_bound*/
  v = gaussian(middle, middle - left_anchor, right_anchor - middle,
               left_bound - 0.5, right_bound + 0.5)
  v = Math.round(v)
  return v
}

digit_coords = [

  [[-0.00704, 0.06691], [-0.01604, 0.06691], [-0.01604, 0.05791], [-0.02504, 0.05791], [-0.02504, 0.04891], [-0.03404, 0.04891], [-0.03404, 0.03991], [-0.05204, 0.03991], [-0.05204, 0.03091], [-0.06104, 0.03091], [-0.06104, 0.01291], [-0.07004, 0.01291], [-0.07004, 0.00391], [-0.07904, 0.00391], [-0.07904, -0.01409], [-0.08804, -0.01409], [-0.08804, -0.05909], [-0.07904, -0.05909], [-0.07904, -0.07709], [-0.07004, -0.07709], [-0.07004, -0.08609], [-0.06104, -0.08609], [-0.06104, -0.09509], [0.02896, -0.09509], [0.02896, -0.08609], [0.03796, -0.08609], [0.03796, -0.07709], [0.06496, -0.07709], [0.06496, -0.05909], [0.08296, -0.05909], [0.08296, -0.03209], [0.09196, -0.03209], [0.09196, 0.00391], [0.08296, 0.00391], [0.08296, 0.03091], [0.07396, 0.03091], [0.07396, 0.03991], [0.06496, 0.03991], [0.06496, 0.04891], [0.05596, 0.04891], [0.05596, 0.05791], [0.03796, 0.05791], [0.03796, 0.06691], [0.01996, 0.06691], [0.01996, 0.07591], [-0.00704, 0.07591]],
  [[0.00900, 0.07470], [0.00000, 0.07470], [0.00000, 0.03870], [-0.00900, 0.03870], [-0.00900, -0.01530], [-0.01800, -0.01530], [-0.01800, -0.06930], [-0.02700, -0.06930], [-0.02700, -0.08730], [-0.00900, -0.08730], [-0.00900, -0.07830], [0.00000, -0.07830], [0.00000, -0.03330], [0.00900, -0.03330], [0.00900, 0.01170], [0.01800, 0.01170], [0.01800, 0.06570], [0.02700, 0.06570], [0.02700, 0.09270], [0.00900, 0.09270]],
  [[-0.03955, 0.09464], [-0.04855, 0.09464], [-0.04855, 0.06764], [-0.01255, 0.06764], [-0.01255, 0.07664], [0.01445, 0.07664], [0.01445, 0.06764], [0.04145, 0.06764], [0.04145, 0.04964], [0.03245, 0.04964], [0.03245, 0.04064], [0.02345, 0.04064], [0.02345, 0.03164], [0.01445, 0.03164], [0.01445, 0.02264], [0.00545, 0.02264], [0.00545, 0.01364], [-0.04855, 0.01364], [-0.04855, 0.00464], [-0.05755, 0.00464], [-0.05755, -0.00436], [-0.06655, -0.00436], [-0.06655, -0.01336], [-0.08455, -0.01336], [-0.08455, -0.03136], [-0.09355, -0.03136], [-0.09355, -0.04036], [-0.10255, -0.04036], [-0.10255, -0.04936], [-0.09355, -0.04936], [-0.09355, -0.05836], [-0.04855, -0.05836], [-0.04855, -0.04936], [-0.03055, -0.04936], [-0.03055, -0.04036], [-0.01255, -0.04036], [-0.01255, -0.03136], [0.01445, -0.03136], [0.01445, -0.04036], [0.02345, -0.04036], [0.02345, -0.04936], [0.03245, -0.04936], [0.03245, -0.05836], [0.04145, -0.05836], [0.04145, -0.07636], [0.05945, -0.07636], [0.05945, -0.05836], [0.06845, -0.05836], [0.06845, -0.04036], [0.05945, -0.04036], [0.05945, -0.03136], [0.05045, -0.03136], [0.05045, -0.02236], [0.04145, -0.02236], [0.04145, -0.01336], [0.03245, -0.01336], [0.03245, 0.00464], [0.04145, 0.00464], [0.04145, 0.01364], [0.05045, 0.01364], [0.05045, 0.02264], [0.05945, 0.02264], [0.05945, 0.09464], [0.03245, 0.09464], [0.03245, 0.10364], [-0.03955, 0.10364]],
  [[-0.03662, 0.07014], [-0.04562, 0.07014], [-0.04562, 0.05214], [-0.05462, 0.05214], [-0.05462, 0.03414], [-0.04562, 0.03414], [-0.04562, 0.02514], [-0.03662, 0.02514], [-0.03662, 0.03414], [-0.02762, 0.03414], [-0.02762, 0.05214], [-0.00962, 0.05214], [-0.00962, 0.06114], [-0.00062, 0.06114], [-0.00062, 0.05214], [0.01738, 0.05214], [0.01738, 0.04314], [0.02638, 0.04314], [0.02638, 0.02514], [0.00838, 0.02514], [0.00838, 0.01614], [-0.02762, 0.01614], [-0.02762, 0.00714], [-0.05462, 0.00714], [-0.05462, -0.01986], [0.00838, -0.01986], [0.00838, -0.02886], [0.01738, -0.02886], [0.01738, -0.05586], [0.00838, -0.05586], [0.00838, -0.06486], [-0.00062, -0.06486], [-0.00062, -0.07386], [-0.01862, -0.07386], [-0.01862, -0.08286], [-0.05462, -0.08286], [-0.05462, -0.10086], [-0.00062, -0.10086], [-0.00062, -0.09186], [0.01738, -0.09186], [0.01738, -0.08286], [0.02638, -0.08286], [0.02638, -0.07386], [0.04438, -0.07386], [0.04438, -0.01986], [0.03538, -0.01986], [0.03538, -0.01086], [0.02638, -0.01086], [0.02638, 0.00714], [0.04438, 0.00714], [0.04438, 0.01614], [0.05338, 0.01614], [0.05338, 0.06114], [0.04438, 0.06114], [0.04438, 0.07014], [0.03538, 0.07014], [0.03538, 0.07914], [-0.03662, 0.07914]],
  [[0.04629, 0.00771], [0.01929, 0.00771], [0.01929, -0.00129], [0.00129, -0.00129], [0.00129, 0.01671], [-0.00771, 0.01671], [-0.00771, 0.05271], [-0.01671, 0.05271], [-0.01671, 0.06171], [-0.02571, 0.06171], [-0.02571, 0.05271], [-0.03471, 0.05271], [-0.03471, 0.04371], [-0.04371, 0.04371], [-0.04371, -0.01029], [-0.05271, -0.01029], [-0.05271, -0.01929], [-0.04371, -0.01929], [-0.04371, -0.02829], [-0.03471, -0.02829], [-0.03471, -0.03729], [0.03729, -0.03729], [0.03729, -0.10929], [0.07329, -0.10929], [0.07329, -0.10029], [0.08229, -0.10029], [0.08229, 0.07071], [0.04629, 0.07071]],
  [[0.01191, 0.07915], [0.00291, 0.07915], [0.00291, 0.07015], [-0.00609, 0.07015], [-0.00609, 0.06115], [-0.01509, 0.06115], [-0.01509, 0.05215], [-0.02409, 0.05215], [-0.02409, 0.04315], [-0.03309, 0.04315], [-0.03309, 0.03415], [-0.04209, 0.03415], [-0.04209, 0.02515], [-0.05109, 0.02515], [-0.05109, 0.01615], [-0.06009, 0.01615], [-0.06009, -0.00185], [-0.04209, -0.00185], [-0.04209, -0.01085], [-0.03309, -0.01085], [-0.03309, -0.00185], [-0.00609, -0.00185], [-0.00609, 0.00715], [0.03891, 0.00715], [0.03891, -0.01985], [0.02991, -0.01985], [0.02991, -0.04685], [0.01191, -0.04685], [0.01191, -0.05585], [0.00291, -0.05585], [0.00291, -0.06485], [-0.05109, -0.06485], [-0.05109, -0.05585], [-0.09609, -0.05585], [-0.09609, -0.07385], [-0.08709, -0.07385], [-0.08709, -0.08285], [-0.06009, -0.08285], [-0.06009, -0.09185], [0.01191, -0.09185], [0.01191, -0.08285], [0.02991, -0.08285], [0.02991, -0.07385], [0.03891, -0.07385], [0.03891, -0.06485], [0.04791, -0.06485], [0.04791, -0.05585], [0.05691, -0.05585], [0.05691, -0.03785], [0.06591, -0.03785], [0.06591, 0.02515], [0.04791, 0.02515], [0.04791, 0.03415], [0.02091, 0.03415], [0.02091, 0.02515], [-0.00609, 0.02515], [-0.00609, 0.03415], [0.00291, 0.03415], [0.00291, 0.04315], [0.01191, 0.04315], [0.01191, 0.05215], [0.02091, 0.05215], [0.02091, 0.06115], [0.07491, 0.06115], [0.07491, 0.07015], [0.08391, 0.07015], [0.08391, 0.08815], [0.01191, 0.08815]],
  /* [[-0.05181, 0.08270], [-0.06081, 0.08270], [-0.06081, 0.03770], [-0.05181, 0.03770], [-0.05181, 0.01970], [-0.03381, 0.01970], [-0.03381, 0.01070], [-0.02481, 0.01070], [-0.02481, 0.00170], [0.00219, 0.00170], [0.00219, -0.00730], [0.02919, -0.00730], [0.02919, -0.01630], [0.04719, -0.01630], [0.04719, -0.02530], [0.03819, -0.02530], [0.03819, -0.03430], [0.02919, -0.03430], [0.02919, -0.04330], [-0.00681, -0.04330], [-0.00681, -0.05230], [-0.05181, -0.05230], [-0.05181, -0.04330], [-0.03381, -0.04330], [-0.03381, -0.02530], [-0.06081, -0.02530], [-0.06081, -0.03430], [-0.06981, -0.03430], [-0.06981, -0.04330], [-0.07881, -0.04330], [-0.07881, -0.05230], [-0.08781, -0.05230], [-0.08781, -0.06130], [-0.07881, -0.06130], [-0.07881, -0.07030], [-0.06981, -0.07030], [-0.06981, -0.07930], [0.01119, -0.07930], [0.01119, -0.07030], [0.04719, -0.07030], [0.04719, -0.06130], [0.05619, -0.06130], [0.05619, -0.05230], [0.06519, -0.05230], [0.06519, -0.04330], [0.08319, -0.04330], [0.08319, -0.00730], [0.07419, -0.00730], [0.07419, 0.00170], [0.05619, 0.00170], [0.05619, 0.01070], [0.03819, 0.01070], [0.03819, 0.01970], [0.01119, 0.01970], [0.01119, 0.02870], [-0.01581, 0.02870], [-0.01581, 0.03770], [-0.02481, 0.03770], [-0.02481, 0.04670], [-0.03381, 0.04670], [-0.03381, 0.07370], [-0.00681, 0.07370], [-0.00681, 0.06470], [0.03819, 0.06470], [0.03819, 0.05570], [0.05619, 0.05570], [0.05619, 0.06470], [0.07419, 0.06470], [0.07419, 0.07370], [0.08319, 0.07370], [0.08319, 0.09170], [0.00219, 0.09170], [0.00219, 0.10070], [-0.05181, 0.10070]],*/
  [[0.02209, 0.08209], [0.00409, 0.08209], [0.00409, 0.07309], [-0.00491, 0.07309], [-0.00491, 0.06409], [-0.01391, 0.06409], [-0.01391, 0.05509], [-0.02291, 0.05509], [-0.02291, 0.03709], [-0.03191, 0.03709], [-0.03191, 0.02809], [-0.04091, 0.02809], [-0.04091, 0.01909], [-0.04991, 0.01909], [-0.04991, -0.00791], [-0.05891, -0.00791], [-0.05891, -0.02591], [-0.06791, -0.02591], [-0.06791, -0.07991], [-0.05891, -0.07991], [-0.05891, -0.08891], [0.01309, -0.08891], [0.01309, -0.07991], [0.02209, -0.07991], [0.02209, -0.07091], [0.03109, -0.07091], [0.03109, -0.05291], [0.04909, -0.05291], [0.04909, -0.04391], [0.05809, -0.04391], [0.05809, -0.03491], [0.06709, -0.03491], [0.06709, -0.00791], [0.05809, -0.00791], [0.05809, 0.00109], [0.04909, 0.00109], [0.04909, 0.01009], [0.02209, 0.01009], [0.02209, 0.00109], [0.00409, 0.00109], [0.00409, -0.01691], [-0.00491, -0.01691], [-0.00491, -0.02591], [-0.01391, -0.02591], [-0.01391, -0.03491], [-0.02291, -0.03491], [-0.02291, -0.06191], [-0.04091, -0.06191], [-0.04091, -0.03491], [-0.03191, -0.03491], [-0.03191, -0.02591], [-0.02291, -0.02591], [-0.02291, -0.00791], [-0.01391, -0.00791], [-0.01391, 0.02809], [-0.00491, 0.02809], [-0.00491, 0.03709], [0.01309, 0.03709], [0.01309, 0.04609], [0.02209, 0.04609], [0.02209, 0.05509], [0.03109, 0.05509], [0.03109, 0.07309], [0.04009, 0.07309], [0.04009, 0.09109], [0.02209, 0.09109]],
  [[0.00700, 0.08350], [-0.02000, 0.08350], [-0.02000, 0.07450], [-0.04700, 0.07450], [-0.04700, 0.06550], [-0.07400, 0.06550], [-0.07400, 0.02950], [-0.06500, 0.02950], [-0.06500, 0.03850], [-0.01100, 0.03850], [-0.01100, 0.04750], [0.03400, 0.04750], [0.03400, 0.02950], [0.02500, 0.02950], [0.02500, 0.02050], [0.01600, 0.02050], [0.01600, 0.01150], [0.00700, 0.01150], [0.00700, 0.00250], [-0.00200, 0.00250], [-0.00200, -0.01550], [-0.01100, -0.01550], [-0.01100, -0.02450], [-0.02000, -0.02450], [-0.02000, -0.03350], [-0.02900, -0.03350], [-0.02900, -0.04250], [-0.03800, -0.04250], [-0.03800, -0.05150], [-0.04700, -0.05150], [-0.04700, -0.06050], [-0.05600, -0.06050], [-0.05600, -0.06950], [-0.06500, -0.06950], [-0.06500, -0.07850], [-0.07400, -0.07850], [-0.07400, -0.08750], [-0.05600, -0.08750], [-0.05600, -0.07850], [-0.04700, -0.07850], [-0.04700, -0.06950], [-0.02900, -0.06950], [-0.02900, -0.06050], [-0.02000, -0.06050], [-0.02000, -0.05150], [-0.00200, -0.05150], [-0.00200, -0.04250], [0.00700, -0.04250], [0.00700, -0.03350], [0.01600, -0.03350], [0.01600, -0.02450], [0.02500, -0.02450], [0.02500, -0.00650], [0.03400, -0.00650], [0.03400, 0.00250], [0.04300, 0.00250], [0.04300, 0.01150], [0.05200, 0.01150], [0.05200, 0.02050], [0.06100, 0.02050], [0.06100, 0.02950], [0.07000, 0.02950], [0.07000, 0.04750], [0.07900, 0.04750], [0.07900, 0.06550], [0.08800, 0.06550], [0.08800, 0.07450], [0.07900, 0.07450], [0.07900, 0.08350], [0.07000, 0.08350], [0.07000, 0.09250], [0.00700, 0.09250]],
  [[-0.00242, 0.07719], [-0.02942, 0.07719], [-0.02942, 0.06819], [-0.04742, 0.06819], [-0.04742, 0.05919], [-0.05642, 0.05919], [-0.05642, 0.04119], [-0.06542, 0.04119], [-0.06542, 0.03219], [-0.05642, 0.03219], [-0.05642, 0.02319], [-0.04742, 0.02319], [-0.04742, 0.01419], [-0.03842, 0.01419], [-0.03842, 0.00519], [-0.02942, 0.00519], [-0.02942, -0.00381], [-0.02042, -0.00381], [-0.02042, -0.01281], [-0.02942, -0.01281], [-0.02942, -0.03081], [-0.02042, -0.03081], [-0.02042, -0.07581], [-0.01142, -0.07581], [-0.01142, -0.09381], [0.02458, -0.09381], [0.02458, -0.08481], [0.03358, -0.08481], [0.03358, -0.07581], [0.04258, -0.07581], [0.04258, -0.06681], [0.05158, -0.06681], [0.05158, -0.03981], [0.06058, -0.03981], [0.06058, -0.03081], [0.05158, -0.03081], [0.05158, -0.01281], [0.04258, -0.01281], [0.04258, -0.00381], [0.02458, -0.00381], [0.02458, 0.00519], [0.00658, 0.00519], [0.00658, 0.01419], [0.01558, 0.01419], [0.01558, 0.02319], [0.02458, 0.02319], [0.02458, 0.03219], [0.03358, 0.03219], [0.03358, 0.05019], [0.04258, 0.05019], [0.04258, 0.08619], [-0.00242, 0.08619]],
  [[0.00082, 0.07732], [-0.01718, 0.07732], [-0.01718, 0.06832], [-0.03518, 0.06832], [-0.03518, 0.05932], [-0.04418, 0.05932], [-0.04418, 0.05032], [-0.05318, 0.05032], [-0.05318, 0.04132], [-0.06218, 0.04132], [-0.06218, 0.02332], [-0.07118, 0.02332], [-0.07118, -0.01268], [-0.04418, -0.01268], [-0.04418, -0.02168], [-0.01718, -0.02168], [-0.01718, -0.01268], [0.00982, -0.01268], [0.00982, -0.02168], [0.00082, -0.02168], [0.00082, -0.03968], [-0.00818, -0.03968], [-0.00818, -0.06668], [-0.01718, -0.06668], [-0.01718, -0.07568], [-0.02618, -0.07568], [-0.02618, -0.09368], [0.01882, -0.09368], [0.01882, -0.08468], [0.02782, -0.08468], [0.02782, -0.05768], [0.03682, -0.05768], [0.03682, -0.03968], [0.04582, -0.03968], [0.04582, -0.01268], [0.05482, -0.01268], [0.05482, 0.00532], [0.06382, 0.00532], [0.06382, 0.05032], [0.07282, 0.05032], [0.07282, 0.07732], [0.06382, 0.07732], [0.06382, 0.08632], [0.00082, 0.08632]],

]

function arr_to_obj(xy) {
  return {x: xy[0], y: xy[1]}
}

function simplify_coords(coords) {
  coords = coords.map(arr_to_obj)
  coords = simplify(coords, 0.010)
  coords = coords.map(xy => [xy.x, xy.y])
  return coords
}

function triangulate(xys) {
  var triangles = earcut(xys.reduce((a, b) => a.concat(b), []))
  var arr = []
  for (var i = 0; i < triangles.length / 3; i++) {
    arr.push([
      xys[triangles[i * 3 + 0]],
      xys[triangles[i * 3 + 1]],
      xys[triangles[i * 3 + 2]],
    ])
  }
  return arr
}

function createRails(world) {
  var thickness = width / 2
  def = {
    friction: 0.,
    restitution: 1,
  }
  var tl = Vec2(+(width * .5 + .0), +(height * .5 + .0))
  var tr = Vec2(+(width * .5 + thickness), +(height * .5 + thickness))
  var br = Vec2(+(width * .5 + thickness), -(height * .5 + thickness))
  var bl = Vec2(+(width * .5 + .0), -(height * .5 + .0))

  var b = world.createBody({userData: {type: "rail", idx: 0}})
  var shape = pl.Polygon([tl, tr, br, bl])
  b.createFixture(shape, def)
  b.setActive(ACTIVE_RAILS)

  var tl = Vec2(-(width * .5 + .0), +(height * .5 + .0))
  var tr = Vec2(-(width * .5 + thickness), +(height * .5 + thickness))
  var br = Vec2(-(width * .5 + thickness), -(height * .5 + thickness))
  var bl = Vec2(-(width * .5 + .0), -(height * .5 + .0))

  var b = world.createBody({userData: {type: "rail", idx: 0}})
  var shape = pl.Polygon([tl, tr, br, bl])
  b.createFixture(shape, def)
  b.setActive(ACTIVE_RAILS)

  var tl = Vec2(+(width * .5 + .0), -(height * .5 + .0))
  var tr = Vec2(+(width * .5 + thickness), -(height * .5 + thickness))
  var br = Vec2(-(width * .5 + thickness), -(height * .5 + thickness))
  var bl = Vec2(-(width * .5 + .0), -(height * .5 + .0))

  var b = world.createBody({userData: {type: "rail", idx: 0}})
  var shape = pl.Polygon([tl, tr, br, bl])
  b.createFixture(shape, def)
  b.setActive(ACTIVE_RAILS)

  var tl = Vec2(+(width * .5 + .0), +(height * .5 + .0))
  var tr = Vec2(+(width * .5 + thickness), +(height * .5 + thickness))
  var br = Vec2(-(width * .5 + thickness), +(height * .5 + thickness))
  var bl = Vec2(-(width * .5 + .0), +(height * .5 + .0))

  var b = world.createBody({userData: {type: "rail", idx: 0}})
  var shape = pl.Polygon([tl, tr, br, bl])
  b.createFixture(shape, def)
  b.setActive(ACTIVE_RAILS)
}

function shapeStats(dense_coords, light_coords) {
  var minx = light_coords.reduce((a, b) => Math.min(a, b[0]), 100)
  var maxx = light_coords.reduce((a, b) => Math.max(a, b[0]), -100)
  var miny = light_coords.reduce((a, b) => Math.min(a, b[1]), 100)
  var maxy = light_coords.reduce((a, b) => Math.max(a, b[1]), -100)
  var meany = light_coords.reduce((a, b) => a + b[1], 0) / light_coords.length
  var spanx = (maxx - minx)
  var spany = (maxy - miny)
  var span = Math.max(spanx, spany)

  var coords_near_meany = dense_coords.filter(xy => Math.abs((xy[1] - meany) / spany) < 0.25 )
  var minx_near_meany = coords_near_meany.reduce((a, b) => Math.min(a, b[0]), 100)

  console.log('>>> coords counts:', dense_coords.length, light_coords.length, coords_near_meany.length)
  console.log('    stats: span/minx.../meany', span, minx_near_meany, meany)
  return [span, minx_near_meany, meany]
}

function putFixtures(b, digit, balltype) {
  var dense_coords = digit_coords[digit]
  var light_coords = simplify_coords(dense_coords)
  var fn = Vec2.scaleFn(digits_scale, digits_scale)
  var ball_fixture_def = {
    friction: 0.01,
    restitution: 0.3,
    density: 1,
  }
  if (balltype == 'sub') {
    var [span, minx, meany] = shapeStats(dense_coords, light_coords)
    var offset = span / 10 / 2
    var bar_width = span / 2 * 0.66
    var bar_height = span / 5 * 0.6
    var random_span = bar_height / 3
    function r() { return Math.random() * random_span }


    var midx = minx - offset - bar_width / 2
    var midy = meany
    b.createFixture(pl.Polygon([
      Vec2(midx + bar_width / 2 - r(), midy + bar_height / 2 - r()),
      Vec2(midx - bar_width / 2 + r(), midy + bar_height / 2 - r()),
      Vec2(midx - bar_width / 2 + r(), midy - bar_height / 2 + r()),
      Vec2(midx + bar_width / 2 - r(), midy - bar_height / 2 + r()),
    ].map(fn)), ball_fixture_def)
  }
  if (balltype == 'add') {
    var [span, minx, meany] = shapeStats(dense_coords, light_coords)
    var offset = span / 10 / 2
    var bar_width = span / 2 * 0.66
    var bar_height = span / 5 * 0.55
    var random_span = bar_height / 3
    function r() { return Math.random() * random_span }
    var midx = minx - offset - bar_width / 2
    var midy = meany

    b.createFixture(pl.Polygon([
      Vec2(midx + bar_width / 2 - r(), midy + bar_height / 2 - r()),
      Vec2(midx - bar_width / 2 + r(), midy + bar_height / 2 - r()),
      Vec2(midx - bar_width / 2 + r(), midy - bar_height / 2 + r()),
      Vec2(midx + bar_width / 2 - r(), midy - bar_height / 2 + r()),
    ].map(fn)), ball_fixture_def)
    b.createFixture(pl.Polygon([
      Vec2(midx + bar_height / 2 - r(), midy + bar_width / 2 - r()),
      Vec2(midx - bar_height / 2 + r(), midy + bar_width / 2 - r()),
      Vec2(midx - bar_height / 2 + r(), midy - bar_width / 2 + r()),
      Vec2(midx + bar_height / 2 - r(), midy - bar_width / 2 + r()),
    ].map(fn)), ball_fixture_def)
  }
  shape = triangulate(light_coords)
  for (var j = 0; j < shape.length; j++) {
    var poly = pl.Polygon([
      fn(Vec2(shape[j][0][0], shape[j][0][1])),
      fn(Vec2(shape[j][1][0], shape[j][1][1])),
      fn(Vec2(shape[j][2][0], shape[j][2][1])),
    ])
    b.createFixture(poly, ball_fixture_def)
  }

}

function attemptToPutDigit(world, idx, digit, xy, balltype) {
  const style = {
    'player': {fill: 'white', stroke: 'white'},
    'add': {fill: 'red', stroke: 'red'},
    'sub': {fill: 'blue', stroke: 'blue'},
  }[balltype]

  var b = world.createDynamicBody({
    userData: {type: 'ball', digit: digit, idx: idx, balltype: balltype, alive: true},
    linearDamping: 1.5,
    angularDamping: 10,
  });
  b.setBullet(ARE_BULLETS);
  b.setPosition({x: xy[0], y: xy[1]});
  b.render = style;
  putFixtures(b, digit, balltype)
  return b
}

function createBalls(world) {
  var kx = 4
  var ky = 3
  var spacingx = (width - kx * BALL_RADIUS * 2) / (kx + 1)
  var ball_centroid_distancex = spacingx + BALL_RADIUS * 2
  var spacingy = (height - ky * BALL_RADIUS * 2) / (ky + 1)
  var ball_centroid_distancey = spacingy + BALL_RADIUS * 2

  console.log('> Creating balls with: spacing', spacingx, 'centroid dist', ball_centroid_distancex, spacingx * (kx + 1) + BALL_RADIUS * 2 * kx)
  console.log('                       spacing', spacingy, 'centroid dist', ball_centroid_distancey, spacingy * (ky + 1) + BALL_RADIUS * 2 * ky)

  function randomDigit() {
    return gaussian_int(0, 1, 3.5, 5, 9)
    /* return gaussian_int(0, 1, 2.5, 3, 9)*/
  }

  player = attemptToPutDigit(world, 42, 0, [0, 0], 'player')
  for (var i = 0; i < 2; i++) {
    var angle = Math.random() * Math.PI * 2
    var dist = (Math.random() * 0.7 + 0.25) * (width / 2)
    var digit
    var balltype

    for (j of [0, 1, 2, 3]) {
      if (i == 0 && j % 2 == 0) {
        balltype = 'add'
        digit = Math.max(2, randomDigit())
      }
      else {
        balltype = ['add', 'sub'][Math.floor(Math.random() * 1.999)]
        digit = randomDigit()
      }
      var x = dist * Math.cos(angle + Math.PI / 2 * j)
      var y = dist * Math.sin(angle + Math.PI / 2 * j)
      attemptToPutDigit(world, 42, digit, [x, y], balltype)

    }
  }
  return player
}


pl.internal.Settings.velocityThreshold = 0;
var world = pl.World({});
createRails(world)
var g_pending = []
g_player = createBalls(world)

var post_solve_count = 0
world.on('post-solve', function(contact) {
  var f0 = contact.getFixtureA()
  var f1 = contact.getFixtureB()
  var b0 = f0.getBody()
  var b1 = f1.getBody()

  var player = null
  var other = null
  if (b0.getUserData().type == 'ball' && b0.getUserData().balltype == 'player') {
    player = b0
  }
  if (b1.getUserData().type == 'ball' && b1.getUserData().balltype == 'player') {
    player = b1
  }
  if (b0.getUserData().type == 'ball' && b0.getUserData().balltype != 'player' && b0.getUserData().alive) {
    other = b0
  }
  if (b1.getUserData().type == 'ball' && b1.getUserData().balltype != 'player' && b1.getUserData().alive) {
    other = b1
  }

  if (player && other) {
    if (other.getUserData().balltype == 'add')
      g_pending.push(other.getUserData().digit)
    else
      g_pending.push(-other.getUserData().digit)
    other.getUserData().alive = false
    console.log('player collision! pending:', g_pending)

    setTimeout(function() {
      world.destroyBody(other);
      if (g_pending.length > 0) {
        digit = g_pending.reduce((a, b) => a + b, g_player.getUserData().digit)
        c = g_player.c_position.c
        a = g_player.c_position.a
        velo = g_player.c_velocity
        g_pending = []
        world.destroyBody(g_player)
        if (digit >= 0 && digit <= 9) {
          g_player = attemptToPutDigit(world, 42, digit, [0, 0], 'player')
          g_player.setPosition(c)
          g_player.setAngularVelocity(velo.w)
          g_player.setLinearVelocity(velo.v)
        }
      }
    }, 1)
  }
  post_solve_count = post_solve_count + 1
});

world.__proto__.queryAABB2 = world.queryAABB
function _hook(aabb, callback) {
  console.log('> queryAABB, hooked')
  function my_callback(f) {
    var d = f.m_body.getUserData()
    if (d.type == 'ball' && d.balltype == 'player') {
      console.log('>>> queryAABB, callback player',)
      callback(f)
    }
    else
      console.log('>>> queryAABB, callback IGNORED',)
  }
  return world.__proto__.queryAABB2.apply(this, [aabb, my_callback])
}
world.__proto__.queryAABB = _hook

console.log('> Calling testbed')
tb = planck.testbed('8 Ball', function(testbed) {
  console.log('> Testbed callback')
  testbed.x = 0;
  testbed.y = 0;
  testbed.width = width * 2;
  testbed.height = height * 2;
  testbed.ratio = 100;
  testbed.mouseForce = mouseForce;
  return world;

});

var bodies = bodies_of_world(world)
var [b] = bodies
