
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

function pair_to_vec2(xy) {
  return {x: xy[0], y: xy[1]}
}

function simplify_coords(coords) {
  coords = coords.map(pair_to_vec2)
  coords = simplify(coords, simplification_slack)
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

  return [span, minx_near_meany, meany]
}

function shuffle(a) {
    var j, x, i;
    for (i = a.length - 1; i > 0; i--) {
        j = Math.floor(Math.random() * (i + 1));
        x = a[i];
        a[i] = a[j];
        a[j] = x;
    }
    return a;
}

function createBoolBiasedRng(k) {
  // The higher k is, the more the function behaves like a uniform sampling
  // The lower k is, the more the function behaves like a round robin function
  //
  // k=0.0001 => 0% change to have x3 `true` in a row
  // k=1 => 4%
  // k=2 => 7%
  // k=999 => 12.5%
  var balance = 0
  function boolRngBiased() {
    var left = (balance >= 0 ? -k : -k + balance)
    var right = (balance <= 0 ? k : +k + balance)
    var result = Math.random() * (right - left) + left >= 0
    console.log('> Sign RNG:', result, balance)
    balance += result ? -1 : 1
    return result
  }
  return boolRngBiased
}

function create_promise() {
  var fire
  var p = new Promise(
    (resolve, _) => fire = resolve
  )
  return [p, fire]
}
