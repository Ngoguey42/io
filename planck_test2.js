/* Proof Of Concept code, please don't judge me :( */

var pl = planck, Vec2 = pl.Vec2, Math = pl.Math;
const width = 10.0 * 2
const height = width;
const digit_scale = 1.26
const simplification_slack = 0.08
const ACTIVE_WALLS = true
const mouseForce = width * 40
/* const mouseForce = width * 100*/
const ARE_BULLETS = true
const MIN_CONTACT_STEP_DISTANCE = 20
const MAX_HP = 100

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

digit_coords = [

  {coords: [[-0.07143, 0.60714], [-0.14286, 0.60714], [-0.14286, 0.53571], [-0.21429, 0.53571], [-0.21429, 0.46429], [-0.28571, 0.46429], [-0.28571, 0.39286], [-0.35714, 0.39286], [-0.42857, 0.39286], [-0.42857, 0.32143], [-0.50000, 0.32143], [-0.50000, 0.25000], [-0.50000, 0.17857], [-0.57143, 0.17857], [-0.57143, 0.10714], [-0.64286, 0.10714], [-0.64286, 0.03571], [-0.64286, -0.03571], [-0.71429, -0.03571], [-0.71429, -0.10714], [-0.71429, -0.17857], [-0.71429, -0.25000], [-0.71429, -0.32143], [-0.71429, -0.39286], [-0.64286, -0.39286], [-0.64286, -0.46429], [-0.64286, -0.53571], [-0.57143, -0.53571], [-0.57143, -0.60714], [-0.50000, -0.60714], [-0.50000, -0.67857], [-0.42857, -0.67857], [-0.35714, -0.67857], [-0.28571, -0.67857], [-0.21429, -0.67857], [-0.14286, -0.67857], [-0.07143, -0.67857], [0.00000, -0.67857], [0.07143, -0.67857], [0.14286, -0.67857], [0.21429, -0.67857], [0.21429, -0.60714], [0.28571, -0.60714], [0.28571, -0.53571], [0.35714, -0.53571], [0.42857, -0.53571], [0.50000, -0.53571], [0.50000, -0.46429], [0.50000, -0.39286], [0.57143, -0.39286], [0.64286, -0.39286], [0.64286, -0.32143], [0.64286, -0.25000], [0.64286, -0.17857], [0.71429, -0.17857], [0.71429, -0.10714], [0.71429, -0.03571], [0.71429, 0.03571], [0.71429, 0.10714], [0.64286, 0.10714], [0.64286, 0.17857], [0.64286, 0.25000], [0.64286, 0.32143], [0.57143, 0.32143], [0.57143, 0.39286], [0.50000, 0.39286], [0.50000, 0.46429], [0.42857, 0.46429], [0.42857, 0.53571], [0.35714, 0.53571], [0.28571, 0.53571], [0.28571, 0.60714], [0.21429, 0.60714], [0.14286, 0.60714], [0.14286, 0.67857], [0.07143, 0.67857], [0.00000, 0.67857], [-0.07143, 0.67857]], pxcount: 173},

  {coords: [[0.07143, 0.64286], [0.07143, 0.57143], [0.00000, 0.57143], [0.00000, 0.50000], [0.00000, 0.42857], [0.00000, 0.35714], [0.00000, 0.28571], [-0.07143, 0.28571], [-0.07143, 0.21429], [-0.07143, 0.14286], [-0.07143, 0.07143], [-0.07143, -0.00000], [-0.07143, -0.07143], [-0.07143, -0.14286], [-0.14286, -0.14286], [-0.14286, -0.21429], [-0.14286, -0.28571], [-0.14286, -0.35714], [-0.14286, -0.42857], [-0.14286, -0.50000], [-0.14286, -0.57143], [-0.21429, -0.57143], [-0.21429, -0.64286], [-0.21429, -0.71429], [-0.14286, -0.71429], [-0.07143, -0.71429], [-0.07143, -0.64286], [0.00000, -0.64286], [0.00000, -0.57143], [0.00000, -0.50000], [0.00000, -0.42857], [0.00000, -0.35714], [0.00000, -0.28571], [0.07143, -0.28571], [0.07143, -0.21429], [0.07143, -0.14286], [0.07143, -0.07143], [0.07143, -0.00000], [0.07143, 0.07143], [0.14286, 0.07143], [0.14286, 0.14286], [0.14286, 0.21429], [0.14286, 0.28571], [0.14286, 0.35714], [0.14286, 0.42857], [0.14286, 0.50000], [0.21429, 0.50000], [0.21429, 0.57143], [0.21429, 0.64286], [0.21429, 0.71429], [0.14286, 0.71429], [0.07143, 0.71429]], pxcount: 47},

  {coords: [[-0.17857, 0.64286], [-0.25000, 0.64286], [-0.25000, 0.57143], [-0.25000, 0.50000], [-0.25000, 0.42857], [-0.17857, 0.42857], [-0.10714, 0.42857], [-0.03571, 0.42857], [0.03571, 0.42857], [0.03571, 0.50000], [0.10714, 0.50000], [0.17857, 0.50000], [0.25000, 0.50000], [0.25000, 0.42857], [0.32143, 0.42857], [0.39286, 0.42857], [0.46429, 0.42857], [0.46429, 0.35714], [0.46429, 0.28571], [0.39286, 0.28571], [0.39286, 0.21429], [0.32143, 0.21429], [0.32143, 0.14286], [0.25000, 0.14286], [0.25000, 0.07143], [0.17857, 0.07143], [0.17857, -0.00000], [0.10714, -0.00000], [0.03571, -0.00000], [-0.03571, -0.00000], [-0.10714, -0.00000], [-0.17857, -0.00000], [-0.25000, -0.00000], [-0.25000, -0.07143], [-0.32143, -0.07143], [-0.32143, -0.14286], [-0.39286, -0.14286], [-0.39286, -0.21429], [-0.46429, -0.21429], [-0.53571, -0.21429], [-0.53571, -0.28571], [-0.53571, -0.35714], [-0.60714, -0.35714], [-0.60714, -0.42857], [-0.67857, -0.42857], [-0.67857, -0.50000], [-0.60714, -0.50000], [-0.60714, -0.57143], [-0.53571, -0.57143], [-0.46429, -0.57143], [-0.39286, -0.57143], [-0.32143, -0.57143], [-0.25000, -0.57143], [-0.25000, -0.50000], [-0.17857, -0.50000], [-0.10714, -0.50000], [-0.10714, -0.42857], [-0.03571, -0.42857], [0.03571, -0.42857], [0.03571, -0.35714], [0.10714, -0.35714], [0.17857, -0.35714], [0.25000, -0.35714], [0.25000, -0.42857], [0.32143, -0.42857], [0.32143, -0.50000], [0.39286, -0.50000], [0.39286, -0.57143], [0.46429, -0.57143], [0.46429, -0.64286], [0.46429, -0.71429], [0.53571, -0.71429], [0.60714, -0.71429], [0.60714, -0.64286], [0.60714, -0.57143], [0.67857, -0.57143], [0.67857, -0.50000], [0.67857, -0.42857], [0.60714, -0.42857], [0.60714, -0.35714], [0.53571, -0.35714], [0.53571, -0.28571], [0.46429, -0.28571], [0.46429, -0.21429], [0.39286, -0.21429], [0.39286, -0.14286], [0.39286, -0.07143], [0.46429, -0.07143], [0.46429, -0.00000], [0.53571, -0.00000], [0.53571, 0.07143], [0.60714, 0.07143], [0.60714, 0.14286], [0.60714, 0.21429], [0.60714, 0.28571], [0.60714, 0.35714], [0.60714, 0.42857], [0.60714, 0.50000], [0.60714, 0.57143], [0.60714, 0.64286], [0.53571, 0.64286], [0.46429, 0.64286], [0.39286, 0.64286], [0.39286, 0.71429], [0.32143, 0.71429], [0.25000, 0.71429], [0.17857, 0.71429], [0.10714, 0.71429], [0.03571, 0.71429], [-0.03571, 0.71429], [-0.10714, 0.71429], [-0.17857, 0.71429]], pxcount: 162},

  {coords: [[-0.28571, 0.64286], [-0.35714, 0.64286], [-0.35714, 0.57143], [-0.35714, 0.50000], [-0.42857, 0.50000], [-0.42857, 0.42857], [-0.42857, 0.35714], [-0.35714, 0.35714], [-0.35714, 0.28571], [-0.28571, 0.28571], [-0.28571, 0.35714], [-0.21429, 0.35714], [-0.21429, 0.42857], [-0.21429, 0.50000], [-0.14286, 0.50000], [-0.07143, 0.50000], [-0.07143, 0.57143], [0.00000, 0.57143], [0.00000, 0.50000], [0.07143, 0.50000], [0.14286, 0.50000], [0.14286, 0.42857], [0.21429, 0.42857], [0.21429, 0.35714], [0.21429, 0.28571], [0.14286, 0.28571], [0.07143, 0.28571], [0.07143, 0.21429], [0.00000, 0.21429], [-0.07143, 0.21429], [-0.14286, 0.21429], [-0.21429, 0.21429], [-0.21429, 0.14286], [-0.28571, 0.14286], [-0.35714, 0.14286], [-0.42857, 0.14286], [-0.42857, 0.07143], [-0.42857, -0.00000], [-0.42857, -0.07143], [-0.35714, -0.07143], [-0.28571, -0.07143], [-0.21429, -0.07143], [-0.14286, -0.07143], [-0.07143, -0.07143], [0.00000, -0.07143], [0.07143, -0.07143], [0.07143, -0.14286], [0.14286, -0.14286], [0.14286, -0.21429], [0.14286, -0.28571], [0.14286, -0.35714], [0.07143, -0.35714], [0.07143, -0.42857], [0.00000, -0.42857], [0.00000, -0.50000], [-0.07143, -0.50000], [-0.14286, -0.50000], [-0.14286, -0.57143], [-0.21429, -0.57143], [-0.28571, -0.57143], [-0.35714, -0.57143], [-0.42857, -0.57143], [-0.42857, -0.64286], [-0.42857, -0.71429], [-0.35714, -0.71429], [-0.28571, -0.71429], [-0.21429, -0.71429], [-0.14286, -0.71429], [-0.07143, -0.71429], [0.00000, -0.71429], [0.00000, -0.64286], [0.07143, -0.64286], [0.14286, -0.64286], [0.14286, -0.57143], [0.21429, -0.57143], [0.21429, -0.50000], [0.28571, -0.50000], [0.35714, -0.50000], [0.35714, -0.42857], [0.35714, -0.35714], [0.35714, -0.28571], [0.35714, -0.21429], [0.35714, -0.14286], [0.35714, -0.07143], [0.28571, -0.07143], [0.28571, -0.00000], [0.21429, -0.00000], [0.21429, 0.07143], [0.21429, 0.14286], [0.28571, 0.14286], [0.35714, 0.14286], [0.35714, 0.21429], [0.42857, 0.21429], [0.42857, 0.28571], [0.42857, 0.35714], [0.42857, 0.42857], [0.42857, 0.50000], [0.42857, 0.57143], [0.35714, 0.57143], [0.35714, 0.64286], [0.28571, 0.64286], [0.28571, 0.71429], [0.21429, 0.71429], [0.14286, 0.71429], [0.07143, 0.71429], [0.00000, 0.71429], [-0.07143, 0.71429], [-0.14286, 0.71429], [-0.21429, 0.71429], [-0.28571, 0.71429]], pxcount: 127},

  {coords: [[0.25000, 0.64286], [0.25000, 0.57143], [0.25000, 0.50000], [0.25000, 0.42857], [0.25000, 0.35714], [0.25000, 0.28571], [0.25000, 0.21429], [0.17857, 0.21429], [0.10714, 0.21429], [0.03571, 0.21429], [0.03571, 0.14286], [-0.03571, 0.14286], [-0.10714, 0.14286], [-0.10714, 0.21429], [-0.10714, 0.28571], [-0.17857, 0.28571], [-0.17857, 0.35714], [-0.17857, 0.42857], [-0.17857, 0.50000], [-0.17857, 0.57143], [-0.25000, 0.57143], [-0.25000, 0.64286], [-0.32143, 0.64286], [-0.32143, 0.57143], [-0.39286, 0.57143], [-0.39286, 0.50000], [-0.46429, 0.50000], [-0.46429, 0.42857], [-0.46429, 0.35714], [-0.46429, 0.28571], [-0.46429, 0.21429], [-0.46429, 0.14286], [-0.46429, 0.07143], [-0.53571, 0.07143], [-0.53571, -0.00000], [-0.46429, -0.00000], [-0.46429, -0.07143], [-0.39286, -0.07143], [-0.39286, -0.14286], [-0.32143, -0.14286], [-0.25000, -0.14286], [-0.17857, -0.14286], [-0.10714, -0.14286], [-0.03571, -0.14286], [0.03571, -0.14286], [0.10714, -0.14286], [0.17857, -0.14286], [0.17857, -0.21429], [0.17857, -0.28571], [0.17857, -0.35714], [0.17857, -0.42857], [0.17857, -0.50000], [0.17857, -0.57143], [0.17857, -0.64286], [0.17857, -0.71429], [0.25000, -0.71429], [0.32143, -0.71429], [0.39286, -0.71429], [0.46429, -0.71429], [0.46429, -0.64286], [0.53571, -0.64286], [0.53571, -0.57143], [0.53571, -0.50000], [0.53571, -0.42857], [0.53571, -0.35714], [0.53571, -0.28571], [0.53571, -0.21429], [0.53571, -0.14286], [0.53571, -0.07143], [0.53571, -0.00000], [0.53571, 0.07143], [0.53571, 0.14286], [0.53571, 0.21429], [0.53571, 0.28571], [0.53571, 0.35714], [0.53571, 0.42857], [0.53571, 0.50000], [0.53571, 0.57143], [0.53571, 0.64286], [0.53571, 0.71429], [0.46429, 0.71429], [0.39286, 0.71429], [0.32143, 0.71429], [0.25000, 0.71429]], pxcount: 156},

  {coords: [[0.14286, 0.64286], [0.07143, 0.64286], [0.07143, 0.57143], [0.00000, 0.57143], [0.00000, 0.50000], [-0.07143, 0.50000], [-0.07143, 0.42857], [-0.14286, 0.42857], [-0.14286, 0.35714], [-0.21429, 0.35714], [-0.21429, 0.28571], [-0.28571, 0.28571], [-0.28571, 0.21429], [-0.35714, 0.21429], [-0.35714, 0.14286], [-0.42857, 0.14286], [-0.42857, 0.07143], [-0.42857, -0.00000], [-0.35714, -0.00000], [-0.28571, -0.00000], [-0.28571, -0.07143], [-0.21429, -0.07143], [-0.21429, -0.00000], [-0.14286, -0.00000], [-0.07143, -0.00000], [0.00000, -0.00000], [0.00000, 0.07143], [0.07143, 0.07143], [0.14286, 0.07143], [0.21429, 0.07143], [0.28571, 0.07143], [0.35714, 0.07143], [0.35714, -0.00000], [0.35714, -0.07143], [0.35714, -0.14286], [0.28571, -0.14286], [0.28571, -0.21429], [0.28571, -0.28571], [0.28571, -0.35714], [0.21429, -0.35714], [0.14286, -0.35714], [0.14286, -0.42857], [0.07143, -0.42857], [0.07143, -0.50000], [0.00000, -0.50000], [-0.07143, -0.50000], [-0.14286, -0.50000], [-0.21429, -0.50000], [-0.28571, -0.50000], [-0.35714, -0.50000], [-0.35714, -0.42857], [-0.42857, -0.42857], [-0.50000, -0.42857], [-0.57143, -0.42857], [-0.64286, -0.42857], [-0.71429, -0.42857], [-0.71429, -0.50000], [-0.71429, -0.57143], [-0.64286, -0.57143], [-0.64286, -0.64286], [-0.57143, -0.64286], [-0.50000, -0.64286], [-0.42857, -0.64286], [-0.42857, -0.71429], [-0.35714, -0.71429], [-0.28571, -0.71429], [-0.21429, -0.71429], [-0.14286, -0.71429], [-0.07143, -0.71429], [0.00000, -0.71429], [0.07143, -0.71429], [0.14286, -0.71429], [0.14286, -0.64286], [0.21429, -0.64286], [0.28571, -0.64286], [0.28571, -0.57143], [0.35714, -0.57143], [0.35714, -0.50000], [0.42857, -0.50000], [0.42857, -0.42857], [0.50000, -0.42857], [0.50000, -0.35714], [0.50000, -0.28571], [0.57143, -0.28571], [0.57143, -0.21429], [0.57143, -0.14286], [0.57143, -0.07143], [0.57143, -0.00000], [0.57143, 0.07143], [0.57143, 0.14286], [0.57143, 0.21429], [0.50000, 0.21429], [0.42857, 0.21429], [0.42857, 0.28571], [0.35714, 0.28571], [0.28571, 0.28571], [0.21429, 0.28571], [0.21429, 0.21429], [0.14286, 0.21429], [0.07143, 0.21429], [0.00000, 0.21429], [0.00000, 0.28571], [0.07143, 0.28571], [0.07143, 0.35714], [0.14286, 0.35714], [0.14286, 0.42857], [0.21429, 0.42857], [0.21429, 0.50000], [0.28571, 0.50000], [0.35714, 0.50000], [0.42857, 0.50000], [0.50000, 0.50000], [0.57143, 0.50000], [0.64286, 0.50000], [0.64286, 0.57143], [0.71429, 0.57143], [0.71429, 0.64286], [0.71429, 0.71429], [0.64286, 0.71429], [0.57143, 0.71429], [0.50000, 0.71429], [0.42857, 0.71429], [0.35714, 0.71429], [0.28571, 0.71429], [0.21429, 0.71429], [0.14286, 0.71429]], pxcount: 150},

  {coords: [[0.17857, 0.64286], [0.10714, 0.64286], [0.03571, 0.64286], [0.03571, 0.57143], [-0.03571, 0.57143], [-0.03571, 0.50000], [-0.10714, 0.50000], [-0.10714, 0.42857], [-0.17857, 0.42857], [-0.17857, 0.35714], [-0.17857, 0.28571], [-0.25000, 0.28571], [-0.25000, 0.21429], [-0.32143, 0.21429], [-0.32143, 0.14286], [-0.39286, 0.14286], [-0.39286, 0.07143], [-0.39286, -0.00000], [-0.39286, -0.07143], [-0.46429, -0.07143], [-0.46429, -0.14286], [-0.46429, -0.21429], [-0.53571, -0.21429], [-0.53571, -0.28571], [-0.53571, -0.35714], [-0.53571, -0.42857], [-0.53571, -0.50000], [-0.53571, -0.57143], [-0.53571, -0.64286], [-0.46429, -0.64286], [-0.46429, -0.71429], [-0.39286, -0.71429], [-0.32143, -0.71429], [-0.25000, -0.71429], [-0.17857, -0.71429], [-0.10714, -0.71429], [-0.03571, -0.71429], [0.03571, -0.71429], [0.10714, -0.71429], [0.10714, -0.64286], [0.17857, -0.64286], [0.17857, -0.57143], [0.25000, -0.57143], [0.25000, -0.50000], [0.25000, -0.42857], [0.32143, -0.42857], [0.39286, -0.42857], [0.39286, -0.35714], [0.46429, -0.35714], [0.46429, -0.28571], [0.53571, -0.28571], [0.53571, -0.21429], [0.53571, -0.14286], [0.53571, -0.07143], [0.46429, -0.07143], [0.46429, -0.00000], [0.39286, -0.00000], [0.39286, 0.07143], [0.32143, 0.07143], [0.25000, 0.07143], [0.17857, 0.07143], [0.17857, -0.00000], [0.10714, -0.00000], [0.03571, -0.00000], [0.03571, -0.07143], [0.03571, -0.14286], [-0.03571, -0.14286], [-0.03571, -0.21429], [-0.10714, -0.21429], [-0.10714, -0.28571], [-0.17857, -0.28571], [-0.17857, -0.35714], [-0.17857, -0.42857], [-0.17857, -0.50000], [-0.25000, -0.50000], [-0.32143, -0.50000], [-0.32143, -0.42857], [-0.32143, -0.35714], [-0.32143, -0.28571], [-0.25000, -0.28571], [-0.25000, -0.21429], [-0.17857, -0.21429], [-0.17857, -0.14286], [-0.17857, -0.07143], [-0.10714, -0.07143], [-0.10714, -0.00000], [-0.10714, 0.07143], [-0.10714, 0.14286], [-0.10714, 0.21429], [-0.03571, 0.21429], [-0.03571, 0.28571], [0.03571, 0.28571], [0.10714, 0.28571], [0.10714, 0.35714], [0.17857, 0.35714], [0.17857, 0.42857], [0.25000, 0.42857], [0.25000, 0.50000], [0.25000, 0.57143], [0.32143, 0.57143], [0.32143, 0.64286], [0.32143, 0.71429], [0.25000, 0.71429], [0.17857, 0.71429]], pxcount: 142},

  {coords: [[0.00000, 0.64286], [-0.07143, 0.64286], [-0.14286, 0.64286], [-0.21429, 0.64286], [-0.21429, 0.57143], [-0.28571, 0.57143], [-0.35714, 0.57143], [-0.42857, 0.57143], [-0.42857, 0.50000], [-0.50000, 0.50000], [-0.57143, 0.50000], [-0.64286, 0.50000], [-0.64286, 0.42857], [-0.64286, 0.35714], [-0.64286, 0.28571], [-0.64286, 0.21429], [-0.57143, 0.21429], [-0.57143, 0.28571], [-0.50000, 0.28571], [-0.42857, 0.28571], [-0.35714, 0.28571], [-0.28571, 0.28571], [-0.21429, 0.28571], [-0.14286, 0.28571], [-0.14286, 0.35714], [-0.07143, 0.35714], [0.00000, 0.35714], [0.07143, 0.35714], [0.14286, 0.35714], [0.21429, 0.35714], [0.21429, 0.28571], [0.21429, 0.21429], [0.14286, 0.21429], [0.14286, 0.14286], [0.07143, 0.14286], [0.07143, 0.07143], [0.00000, 0.07143], [0.00000, -0.00000], [-0.07143, -0.00000], [-0.07143, -0.07143], [-0.07143, -0.14286], [-0.14286, -0.14286], [-0.14286, -0.21429], [-0.21429, -0.21429], [-0.21429, -0.28571], [-0.28571, -0.28571], [-0.28571, -0.35714], [-0.35714, -0.35714], [-0.35714, -0.42857], [-0.42857, -0.42857], [-0.42857, -0.50000], [-0.50000, -0.50000], [-0.50000, -0.57143], [-0.57143, -0.57143], [-0.57143, -0.64286], [-0.64286, -0.64286], [-0.64286, -0.71429], [-0.57143, -0.71429], [-0.50000, -0.71429], [-0.50000, -0.64286], [-0.42857, -0.64286], [-0.42857, -0.57143], [-0.35714, -0.57143], [-0.28571, -0.57143], [-0.28571, -0.50000], [-0.21429, -0.50000], [-0.21429, -0.42857], [-0.14286, -0.42857], [-0.07143, -0.42857], [-0.07143, -0.35714], [0.00000, -0.35714], [0.00000, -0.28571], [0.07143, -0.28571], [0.07143, -0.21429], [0.14286, -0.21429], [0.14286, -0.14286], [0.14286, -0.07143], [0.21429, -0.07143], [0.21429, -0.00000], [0.28571, -0.00000], [0.28571, 0.07143], [0.35714, 0.07143], [0.35714, 0.14286], [0.42857, 0.14286], [0.42857, 0.21429], [0.50000, 0.21429], [0.50000, 0.28571], [0.50000, 0.35714], [0.57143, 0.35714], [0.57143, 0.42857], [0.57143, 0.50000], [0.64286, 0.50000], [0.64286, 0.57143], [0.57143, 0.57143], [0.57143, 0.64286], [0.50000, 0.64286], [0.50000, 0.71429], [0.42857, 0.71429], [0.35714, 0.71429], [0.28571, 0.71429], [0.21429, 0.71429], [0.14286, 0.71429], [0.07143, 0.71429], [0.00000, 0.71429]], pxcount: 128},

  {coords: [[0.00000, 0.64286], [-0.07143, 0.64286], [-0.14286, 0.64286], [-0.21429, 0.64286], [-0.21429, 0.57143], [-0.28571, 0.57143], [-0.35714, 0.57143], [-0.35714, 0.50000], [-0.42857, 0.50000], [-0.42857, 0.42857], [-0.42857, 0.35714], [-0.50000, 0.35714], [-0.50000, 0.28571], [-0.42857, 0.28571], [-0.42857, 0.21429], [-0.35714, 0.21429], [-0.35714, 0.14286], [-0.28571, 0.14286], [-0.28571, 0.07143], [-0.21429, 0.07143], [-0.21429, -0.00000], [-0.14286, -0.00000], [-0.14286, -0.07143], [-0.21429, -0.07143], [-0.21429, -0.14286], [-0.21429, -0.21429], [-0.14286, -0.21429], [-0.14286, -0.28571], [-0.14286, -0.35714], [-0.14286, -0.42857], [-0.14286, -0.50000], [-0.14286, -0.57143], [-0.07143, -0.57143], [-0.07143, -0.64286], [-0.07143, -0.71429], [0.00000, -0.71429], [0.07143, -0.71429], [0.14286, -0.71429], [0.21429, -0.71429], [0.21429, -0.64286], [0.28571, -0.64286], [0.28571, -0.57143], [0.35714, -0.57143], [0.35714, -0.50000], [0.42857, -0.50000], [0.42857, -0.42857], [0.42857, -0.35714], [0.42857, -0.28571], [0.50000, -0.28571], [0.50000, -0.21429], [0.42857, -0.21429], [0.42857, -0.14286], [0.42857, -0.07143], [0.35714, -0.07143], [0.35714, -0.00000], [0.28571, -0.00000], [0.21429, -0.00000], [0.21429, 0.07143], [0.14286, 0.07143], [0.07143, 0.07143], [0.07143, 0.14286], [0.14286, 0.14286], [0.14286, 0.21429], [0.21429, 0.21429], [0.21429, 0.28571], [0.28571, 0.28571], [0.28571, 0.35714], [0.28571, 0.42857], [0.35714, 0.42857], [0.35714, 0.50000], [0.35714, 0.57143], [0.35714, 0.64286], [0.35714, 0.71429], [0.28571, 0.71429], [0.21429, 0.71429], [0.14286, 0.71429], [0.07143, 0.71429], [0.00000, 0.71429]], pxcount: 114},

  {coords: [[0.00000, 0.64286], [-0.07143, 0.64286], [-0.14286, 0.64286], [-0.14286, 0.57143], [-0.21429, 0.57143], [-0.28571, 0.57143], [-0.28571, 0.50000], [-0.35714, 0.50000], [-0.35714, 0.42857], [-0.42857, 0.42857], [-0.42857, 0.35714], [-0.50000, 0.35714], [-0.50000, 0.28571], [-0.50000, 0.21429], [-0.57143, 0.21429], [-0.57143, 0.14286], [-0.57143, 0.07143], [-0.57143, -0.00000], [-0.57143, -0.07143], [-0.50000, -0.07143], [-0.42857, -0.07143], [-0.35714, -0.07143], [-0.35714, -0.14286], [-0.28571, -0.14286], [-0.21429, -0.14286], [-0.14286, -0.14286], [-0.14286, -0.07143], [-0.07143, -0.07143], [0.00000, -0.07143], [0.07143, -0.07143], [0.07143, -0.14286], [0.00000, -0.14286], [0.00000, -0.21429], [0.00000, -0.28571], [-0.07143, -0.28571], [-0.07143, -0.35714], [-0.07143, -0.42857], [-0.07143, -0.50000], [-0.14286, -0.50000], [-0.14286, -0.57143], [-0.21429, -0.57143], [-0.21429, -0.64286], [-0.21429, -0.71429], [-0.14286, -0.71429], [-0.07143, -0.71429], [0.00000, -0.71429], [0.07143, -0.71429], [0.14286, -0.71429], [0.14286, -0.64286], [0.21429, -0.64286], [0.21429, -0.57143], [0.21429, -0.50000], [0.21429, -0.42857], [0.28571, -0.42857], [0.28571, -0.35714], [0.28571, -0.28571], [0.35714, -0.28571], [0.35714, -0.21429], [0.35714, -0.14286], [0.35714, -0.07143], [0.42857, -0.07143], [0.42857, -0.00000], [0.42857, 0.07143], [0.50000, 0.07143], [0.50000, 0.14286], [0.50000, 0.21429], [0.50000, 0.28571], [0.50000, 0.35714], [0.50000, 0.42857], [0.57143, 0.42857], [0.57143, 0.50000], [0.57143, 0.57143], [0.57143, 0.64286], [0.50000, 0.64286], [0.50000, 0.71429], [0.42857, 0.71429], [0.35714, 0.71429], [0.28571, 0.71429], [0.21429, 0.71429], [0.14286, 0.71429], [0.07143, 0.71429], [0.00000, 0.71429]], pxcount: 164},

]

function arr_to_obj(xy) {
  return {x: xy[0], y: xy[1]}
}

function simplify_coords(coords) {
  coords = coords.map(arr_to_obj)
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

const firegrass = [
  [0.0, 0.758112491, 0.214410284, 0.233365527],
  [0.03125, 0.792973226, 0.278253976, 0.26171667],
  [0.0625, 0.824721715, 0.336893693, 0.292041952],
  [0.09375, 0.853149914, 0.392215859, 0.324355787],
  [0.125, 0.878071007, 0.444956966, 0.358634377],
  [0.15625, 0.899319896, 0.495383632, 0.394815586],
  [0.1875, 0.916753461, 0.543535858, 0.432799404],
  [0.21875, 0.930250628, 0.58933337, 0.472448816],
  [0.25, 0.939712236, 0.632628934, 0.51359092],
  [0.28125, 0.945060749, 0.673238039, 0.556018224],
  [0.3125, 0.946239785, 0.710956895, 0.599490112],
  [0.34375, 0.943213507, 0.745574177, 0.643734476],
  [0.375, 0.935965841, 0.776879188, 0.688449558],
  [0.40625, 0.9244995, 0.804667852, 0.733306049],
  [0.4375, 0.908834783, 0.828747317, 0.777949474],
  [0.46875, 0.889008036, 0.848939615, 0.822002902],
  [0.5, 0.865069663, 0.865084671, 0.865070027],
  [0.53125, 0.819531136, 0.867767935, 0.85308666],
  [0.5625, 0.772179049, 0.866735157, 0.835704507],
  [0.59375, 0.723443873, 0.862046472, 0.813149162],
  [0.625, 0.673741625, 0.853784703, 0.785687648],
  [0.65625, 0.623467383, 0.842054539, 0.753624476],
  [0.6875, 0.572988255, 0.82698154, 0.717297142],
  [0.71875, 0.52263548, 0.808710957, 0.677070968],
  [0.75, 0.472695132, 0.78740642, 0.633333147],
  [0.78125, 0.423396466, 0.763248502, 0.586485662],
  [0.8125, 0.374896113, 0.736433175, 0.536936504],
  [0.84375, 0.327254487, 0.707170194, 0.485088029],
  [0.875, 0.280396428, 0.675681414, 0.431320088],
  [0.90625, 0.234036496, 0.642199062, 0.375962865],
  [0.9375, 0.187513317, 0.60696398, 0.319247349],
  [0.96875, 0.139336649, 0.57022386, 0.261201283],
  [1.0, 0.085438707, 0.532231466, 0.201388188],
]

function createFloorClosure(canvas, maxhp) {
  function setHp(hp) {
    const frac = Math.max(0., Math.min(1., hp / maxhp))
    var i, r, g, b
    for (i = 0; i < firegrass.length; i++) {
      if (firegrass[i][0] >= frac)
        break
    }
    if (i == 0) {
      ;[_, r, g, b] = firegrass[0]
    }
    else {
      var [x0, r0, g0, b0] = firegrass[i - 1]
      var [x1, r1, g1, b1] = firegrass[i]
      var frac1 = (frac - x0) / (x1 - x0)
      var frac0 = 1 - frac1
      r = frac0 * r0 + frac1 * r1
      g = frac0 * g0 + frac1 * g1
      b = frac0 * b0 + frac1 * b1
    }
    c = (
      '#' +
      Math.round(r * 255).toString(16) +
      Math.round(g * 255).toString(16) +
      Math.round(b * 255).toString(16)
    )
    canvas.style.backgroundColor = c;
  }
  setHp(100)
  return setHp
}


function createWalls(world) {
  var thickness = width / 2
  def = {
    friction: 0.,
    restitution: 1,
  }
  var tl = Vec2(+(width * .5 + .0), +(height * .5 + .0))
  var tr = Vec2(+(width * .5 + thickness), +(height * .5 + thickness))
  var br = Vec2(+(width * .5 + thickness), -(height * .5 + thickness))
  var bl = Vec2(+(width * .5 + .0), -(height * .5 + .0))

  var b = world.createBody({userData: {type: "wall", idx: 0}})
  var shape = pl.Polygon([tl, tr, br, bl])
  b.createFixture(shape, def)
  b.setActive(ACTIVE_WALLS)
  b.render = {'fill': '#1f1f1f', 'stroke': '#1f1f1f'}

  var tl = Vec2(-(width * .5 + .0), +(height * .5 + .0))
  var tr = Vec2(-(width * .5 + thickness), +(height * .5 + thickness))
  var br = Vec2(-(width * .5 + thickness), -(height * .5 + thickness))
  var bl = Vec2(-(width * .5 + .0), -(height * .5 + .0))

  var b = world.createBody({userData: {type: "wall", idx: 0}})
  var shape = pl.Polygon([tl, tr, br, bl])
  b.createFixture(shape, def)
  b.setActive(ACTIVE_WALLS)
  b.render = {'fill': '#1f1f1f', 'stroke': '#1f1f1f'}

  var tl = Vec2(+(width * .5 + .0), -(height * .5 + .0))
  var tr = Vec2(+(width * .5 + thickness), -(height * .5 + thickness))
  var br = Vec2(-(width * .5 + thickness), -(height * .5 + thickness))
  var bl = Vec2(-(width * .5 + .0), -(height * .5 + .0))

  var b = world.createBody({userData: {type: "wall", idx: 0}})
  var shape = pl.Polygon([tl, tr, br, bl])
  b.createFixture(shape, def)
  b.setActive(ACTIVE_WALLS)
  b.render = {'fill': '#1f1f1f', 'stroke': '#1f1f1f'}

  var tl = Vec2(+(width * .5 + .0), +(height * .5 + .0))
  var tr = Vec2(+(width * .5 + thickness), +(height * .5 + thickness))
  var br = Vec2(-(width * .5 + thickness), +(height * .5 + thickness))
  var bl = Vec2(-(width * .5 + .0), +(height * .5 + .0))

  var b = world.createBody({userData: {type: "wall", idx: 0}})
  var shape = pl.Polygon([tl, tr, br, bl])
  b.createFixture(shape, def)
  b.setActive(ACTIVE_WALLS)
  b.render = {'fill': '#1f1f1f', 'stroke': '#1f1f1f'}
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

function putFixtures(b, digit, op, def) {
  var dense_coords = digit_coords[digit]['coords']
  var light_coords = simplify_coords(dense_coords)
  var fn = Vec2.scaleFn(digit_scale, digit_scale)
  if (op == 'sub') {
    var [span, minx, meany] = shapeStats(dense_coords, light_coords)
    var offset = span / 10 / 2
    var bar_width = span / 2 * 0.66
    var bar_height = span / 5 * 0.6
    var random_span = bar_height / 3
    function r() { return Math.random() * random_span }


    var midx = minx - offset - bar_width / 2
    var midy = meany
    b.createFixture(pl.Polygon([
      Vec2(midx + bar_width / 2 - r() + 0.25, midy + bar_height / 2 - r()),
      Vec2(midx - bar_width / 2 + r(), midy + bar_height / 2 - r()),
      Vec2(midx - bar_width / 2 + r(), midy - bar_height / 2 + r()),
      Vec2(midx + bar_width / 2 - r() + 0.25, midy - bar_height / 2 + r()),
    ].map(fn)), def)
  }
  if (op == 'add') {
    var [span, minx, meany] = shapeStats(dense_coords, light_coords)
    var offset = span / 10 / 2
    var bar_width = span / 2 * 0.66
    var bar_height = span / 5 * 0.55
    var random_span = bar_height / 3
    function r() { return Math.random() * random_span }
    var midx = minx - offset - bar_width / 2
    var midy = meany

    b.createFixture(pl.Polygon([
      Vec2(midx + bar_width / 2 - r() + 0.35, midy + bar_height / 2 - r()),
      Vec2(midx - bar_width / 2 + r(), midy + bar_height / 2 - r()),
      Vec2(midx - bar_width / 2 + r(), midy - bar_height / 2 + r()),
      Vec2(midx + bar_width / 2 - r() + 0.35, midy - bar_height / 2 + r()),
    ].map(fn)), def)
    b.createFixture(pl.Polygon([
      Vec2(midx + bar_height / 2 - r(), midy + bar_width / 2 - r()),
      Vec2(midx - bar_height / 2 + r(), midy + bar_width / 2 - r()),
      Vec2(midx - bar_height / 2 + r(), midy - bar_width / 2 + r()),
      Vec2(midx + bar_height / 2 - r(), midy - bar_width / 2 + r()),
    ].map(fn)), def)
  }
  shape = triangulate(light_coords)
  for (var j = 0; j < shape.length; j++) {
    var poly = pl.Polygon([
      fn(Vec2(shape[j][0][0], shape[j][0][1])),
      fn(Vec2(shape[j][1][0], shape[j][1][1])),
      fn(Vec2(shape[j][2][0], shape[j][2][1])),
    ])
    b.createFixture(poly, def)
  }

}

g_pin_idx = 0
function putPinAt(world, digit, xy, op) {
  const style = {
    'add': {fill: 'red', stroke: 'red'},
    'sub': {fill: 'blue', stroke: 'blue'},
  }[op]

  var d = {type: 'pin', digit: digit, idx: g_pin_idx, op: op,
           alive: true, last_contact: -MIN_CONTACT_STEP_DISTANCE}
  g_pin_idx += 1

  var b = world.createDynamicBody({
    userData: d,
    linearDamping: 2.5,
    angularDamping: 10,
  });
  b.setBullet(ARE_BULLETS);
  b.setPosition({x: xy[0], y: xy[1]});
  b.render = style;
  putFixtures(b, digit, op, {
    friction: 0.01,
    restitution: 0.3,
    density: 1,
  })
  b.setMassData({
    mass: digit_coords[digit].pxcount / 173,
    center: Vec2(0, 0),
    I: 1,
  })
  return b
}

function putPlayer(world, digit, xy, a) {
  const style = {fill: 'white', stroke: 'white'}
  var d = {type: 'player', digit: digit}
  var b = world.createDynamicBody({
    userData: d,
    linearDamping: 2.5,
    angularDamping: (digit == 7 ? 1.2 : 10),
    fixedRotation: (digit == 9 || digit == 6),
  });
  if (digit == 9 || digit == 6)
    a = 0
  b.setBullet(ARE_BULLETS);
  b.setPosition(xy);
  b.setAngle(a)
  b.render = style;
  putFixtures(b, digit, null, {
    friction: 0.01,
    restitution: 0.3,
    density: 1,
  })
  b.setMassData({
    mass: digit_coords[digit].pxcount / 173 * 1.5,
    center: Vec2(0, 0),
    I: 1,
  })
  return b
}

function computePopCoordinates() {
  var arr = [];
  const count = 4
  for (var i = 0; i <= count; i++) {
    for (var j = 0; j <= count; j++) {
      if (i == 0) continue
      if (j == 0) continue
      if (i == count - 1) continue
      if (j == count - 1) continue

      var x = width / (count * 2) * (1 + i) - width / 2
      var y = height / (count * 2) * (1 + j) - height / 2
      arr.push([x, y])
    }
  }
  for (var i = 0; i <= count - 1; i++) {
    for (var j = 0; j <= count - 1; j++) {
      var x = width / count * i - width / 2
      var y = height / count * j - height / 2
      arr.push([x, y])
    }
  }
  return arr
}

const POP_COORDINATES = computePopCoordinates()

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

function findPinPosition(world) {
  function isAvailable(x, y) {
    var available = true
    const where = pl.AABB(
      Vec2(x - digit_scale / 2 * 2, y - digit_scale / 2 * 2),
      Vec2(x + digit_scale / 2 * 2, y + digit_scale / 2 * 2),
    )
    world.queryAABB2(where, function(_) {
      available = false
    })
    return available
  }
  var xys = [...POP_COORDINATES]
  shuffle(xys)
  for (xy of xys) {
    var [x, y] = xy
    if (isAvailable(x, y))
      return xy
  }
  return null
}

var ga = 1
var gb = 2.5
var gc = 3

function createBoolRngBiased(k) {
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

randomIsSub = createBoolRngBiased(1)

function putPin(world, forceBigPositive) {
  function randomDigit() {
    return gaussian_int(0, ga, gb, gc, 9)
  }
  if (forceBigPositive) {
    var op = 'add'
    var digit = Math.max(2, randomDigit())
  }
  else {
    var op = ['add', 'sub'][Number(randomIsSub())]
    var digit = randomDigit()
  }
  var xy = findPinPosition(world)
  if (xy === null)
    return false
  var [x, y] = xy
  putPinAt(world, digit, [x, y], op)
  return true
}

pl.internal.Settings.velocityThreshold = 0;
var world = pl.World({});
world.__proto__.queryAABB2 = world.queryAABB
createWalls(world)
var g_pending = []
var g_player = putPlayer(world, 0, Vec2(0, 0), 0)
for (var i = 0; i < 5; i++)
  putPin(world, i <= 1)
var g_score = 0
var g_knock_ball = null
var g_hp = MAX_HP
var g_round = 0


function sumToPlayer(pending, hpdiff) {
  var digit = g_player.getUserData().digit
  var digit0 = digit
  var d_incr = 0
  var s_incr = 0

  var s_factor = pending.length

  g_hp += hpdiff

  var small_scores = []

  for (d_signed of pending) {
    d_incr += d_signed

    s_incr1 = Math.abs(d_signed) + Math.round(15 / (10 - Math.abs(d_signed))) - 1 // 1, 2, 3, 4, 6, 7, 9, 11, 15, 23
    /* s_incr0 = 1.6 ** Math.abs(d_signed) // 1, 2, 3, 5, 7, 11, 17, 27, 43, 69 */
    small_scores.push(s_incr1)

    console.log('> Eating', d_signed, '. Gaining', s_incr1, '*', s_factor)
    s_incr += s_incr1 * s_factor
  }
  digit = digit + d_incr

  setColor(g_hp)
  document.getElementById('health').innerText = (
    g_hp.toString() +
    ' hp (' +
     (hpdiff >= 0 ? '+' : '') +
    hpdiff.toString() +
    ')'
  )
  document.getElementById('eaten').innerText = (
    digit0.toString() +
    pending.map(i => {
      if (Object.is(i, -0))
        return "-0"
      else if (Object.is(i, 0))
        return "+0"
      else if (i < 0)
        return i.toString()
      else
        return '+' + i.toString()
    }).join('') +
    " = " +
    digit.toString()
  )

  c = g_player.c_position.c
  a = g_player.c_position.a
  velo = g_player.c_velocity
  world.destroyBody(g_player)
  console.log('> g_score:', g_score, 'digit:', digit)
  if (digit < 0 || digit > 9) {
    document.getElementById('score').innerText = (
      "Game over with score " + g_score.toString() + '. ' +
      (digit < 0 ? "Keep a positive number!" : "Stay below 10!") +
      " Refresh page."
    )
    return
  }
  if (g_hp < 0) {
    document.getElementById('score').innerText = (
      "Game over with score " + g_score.toString() + '.' +
      " Refresh page."
    )
    return
  }

  var ok = true
  /* for (var i = 0; i < pending.length + 1 && ok; i++) {*/
  /* for (var i = 0; i <  1 && ok; i++) {*/

  pincount = 1
  /* g_round <= 5 ? 1 :
   * g_round <= 15 ? 2 :
   * 3
     )
   */
  for (var i = 0; i < pincount && ok; i++) {
    console.log('> put new pin', i, pending.length)
    ok = putPin(world, false)
  }
  if (!ok) {
    document.getElementById('score').innerText = ("Game over with score " + g_score.toString() + ". refresh page")
    return
  }

  if (gc < 7) {
    gb += 0.5
    gc += 0.5
  }
  else if (ga < 5) {
    ga += 1
  }

  g_score += s_incr
  document.getElementById('score').innerText = (
    g_score.toString()
  + ' (+'
  + s_factor.toString()
  + ' x (' + small_scores.join('+') + ') = ' + s_incr.toString() + ')'
  )

  g_player = putPlayer(world, digit, c, a)
}

function classify(a, b) {
  adat = a.getUserData()
  bdat = b.getUserData()
  var player = null
  var wall = null
  var alive_pin = null
  var dead_pin = null

  if (bdat.type == 'player')
    player = b
  else if (bdat.type == 'wall')
    wall = b
  else if (bdat.type == 'pin' && bdat.alive === true)
    alive_pin = b
  else if (bdat.type == 'pin' && bdat.alive === false)
    dead_pin = b
  else
    console.error('unknown entity')

  if (adat.type == 'player')
    player = a
  else if (adat.type == 'wall')
    wall = a
  else if (adat.type == 'pin' && adat.alive === true)
    alive_pin = a
  else if (adat.type == 'pin' && adat.alive === false)
    dead_pin = a
  else
    console.error('unknown entity')

  return [player, wall, alive_pin, dead_pin]
}

world.on('pre-solve', function(contact) {
  var [player, wall, pin, dead_pin] = classify(contact.getFixtureA().getBody(), contact.getFixtureB().getBody())

  if (dead_pin) {
    contact.setEnabled(false)
  }
  if (pin && wall) {
    console.log('> Deactivating pin', pin.getUserData().idx)
    pin.getUserData().alive = false
    if (pin.getUserData().op == 'add')
      g_pending.push(pin.getUserData().digit)
    else
      g_pending.push(-pin.getUserData().digit)
    contact.setEnabled(false)
  }
})

function create_promise() {
  var fire
  var p = new Promise(
    (resolve, _) => fire = resolve
  )
  return [p, fire]
}

async function main(world) {
  console.log('> main')
  setColor(MAX_HP)
  while (true) { // one loop per game round
    ;[p, g_knock_ball] = create_promise() // Oh oui le javascript
    console.log('> main | wait for click')
    await p
    console.log('> main | got click')
    g_knock_ball = null

    function same_maps(a, b) {
      for (k of [...b.keys()]) {
        va = a.get(k)
        vb = b.get(k)
        if (Math.abs(va[0] - vb[0]) / width > 5 / 200)
          return false
        if (Math.abs(va[1] - vb[1]) / width > 5 / 200)
          return false
      }
      return true
    }

    function list_positions() {
      var o = new Map()
      for (b of bodies_of_world(world)) {
        ud = b.getUserData()
        if (ud && ud.type == 'player')
          o.set('player', [b.c_position.c.x, b.c_position.c.y, b.c_position.a])
        else if (ud && ud.type == 'pin')
          o.set(ud.idx, [b.c_position.c.x, b.c_position.c.y, b.c_position.a])
      }
      return o
    }
    var last_positions = list_positions()
    var positions
    var last_step = world.m_stepCount
    while (true) { // wait for end of movements
      /* console.log('> main | waiting a bit', world.m_stepCount)*/
      while (true) { // sleep for several steps
        await (new Promise(resolve => setTimeout(resolve, 1)))
        if (world.m_stepCount >= last_step + 20) {
          last_step = world.m_stepCount
          break
        }
      }
      /* console.log('> main | checking positions', world.m_stepCount)*/
      positions = list_positions()
      if (same_maps(last_positions, positions))
        break
      last_positions = positions
    }
    console.log('> main | stopping remaining velocity')
    var hpdiff = -1
    for (b of bodies_of_world(world)) {
      const ud = b.getUserData()
      b.setLinearVelocity({x: 0, y: 0})
      b.setAngularVelocity(0)
      if (ud && ud.type == 'pin' && !ud.alive)
        world.destroyBody(b)
      if (ud && ud.type == 'pin' && ud.alive)
        hpdiff -= 1
    }
    console.log('> main | apply score', g_pending)
    sumToPlayer(g_pending, hpdiff)
    g_pending = []
    g_round += 1
  }
}

var tb
var mouse = 0
var canvas
var setColor

function _hook(aabb, callback) {
  mouse = 0
  console.log('hook', aabb)
  function my_callback(f) {
    var d = f.m_body.getUserData()
    mouse = 0
    if (d.type == 'player' && g_knock_ball !== null) {
      console.log('hook:callback player')
      mouse = 1
      callback(f)
    }
    else
      console.log('hook:callback IGNORED')
  }
  return world.__proto__.queryAABB2.apply(this, [aabb, my_callback])
}
world.__proto__.queryAABB = _hook

tb = planck.testbed('8 Ball', function(testbed) {
  tb = testbed
  canvas = tb.canvas
  testbed.x = 0;
  testbed.y = 0;
  testbed.width = width * 2;
  testbed.height = height * 2;
  testbed.ratio = 100;
  testbed.mouseForce = mouseForce;
  setTimeout(main, 0, world)

  document.getElementById('health').innerText = (MAX_HP).toString() + ' hp'
  setColor = createFloorClosure(canvas, MAX_HP)

  canvas.onmouseup = function (_) {
    console.log('canvas:onmouseup', 'mouse value:', mouse, 'has-callback:', g_knock_ball !== null)
    if (mouse == 1 && g_knock_ball !== null) {
      mouse = 0
      g_knock_ball()
    }
  }

  return world;
});

var bodies = bodies_of_world(world)
var [b] = bodies
