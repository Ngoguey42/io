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

function putFixtures(b, digit, op, def) {
  var dense_coords = DIGITS_DEFN[digit]['exterior']
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
    mass: DIGITS_DEFN[digit]['sum_imgclean'] / 140.72,
    center: pair_to_vec2(DIGITS_DEFN[digit]['barycenter_imgclean']),
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
    mass: DIGITS_DEFN[digit]['sum_imgclean'] / 140.72 * 1.5,
    center: pair_to_vec2(DIGITS_DEFN[digit]['barycenter_imgclean']),
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

randomIsSub = createBoolBiasedRng(1)

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
