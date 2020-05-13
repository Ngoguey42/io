/* Proof Of Concept code, please don't judge me :( */

var pl = planck, Vec2 = pl.Vec2, Math = pl.Math;
pl.internal.Settings.velocityThreshold = 0;

const width = 10.0 * 1.7
const height = width;
const digit_scale = 1.75
const simplification_slack = 0.08
const ACTIVE_WALLS = true
const mouseForce = width * 30
/* const mouseForce = width * 100*/
const ARE_BULLETS = true
const MIN_CONTACT_STEP_DISTANCE = 20
const MAX_HP = 100

function putWalls(world) {
  var thickness = width / 4
  var def = {
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

function putDigitFixtures(b, digit, op, def) {
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

var g_pin_idx = 0
function putPinAt(world, digit, xy, op) {
  const style = {
    1: {fill: 'pink', stroke: 'pink'},
    9: {fill: 'pink', stroke: 'pink'},
    2: {fill: 'blue', stroke: 'blue'},
    8: {fill: 'blue', stroke: 'blue'},
    3: {fill: 'green', stroke: 'green'},
    7: {fill: 'green', stroke: 'green'},
    6: {fill: 'orange', stroke: 'orange'},
    4: {fill: 'orange', stroke: 'orange'},
    5: {fill: 'red', stroke: 'red'},
  }[digit]

  var d = {type: 'pin', digit: digit, idx: g_pin_idx, op: op,
           alive: true, last_contact: -MIN_CONTACT_STEP_DISTANCE}
  g_pin_idx += 1
  console.log('> New pin', d)

  var b = world.createDynamicBody({
    userData: d,
    linearDamping: 2.5,
    angularDamping: 10,
    fixedRotation: digit == 6 || digit == 9,
  });
  b.setBullet(ARE_BULLETS);
  b.setPosition({x: xy[0], y: xy[1]});
  b.render = style;
  putDigitFixtures(b, digit, op, {
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
  putDigitFixtures(b, digit, null, {
    friction: 0.01,
    restitution: 0.7,
    density: 1,
  })
  b.setMassData({
    mass: DIGITS_DEFN[digit]['sum_imgclean'] / 140.72,
    center: pair_to_vec2(DIGITS_DEFN[digit]['barycenter_imgclean']),
    I: 1,
  })
  return b
}

function computeSpawnCoordinates() {
  var arr = [];
  const count = 4
  for (var i = 0; i <= count; i++) {
    for (var j = 0; j <= count; j++) {
      /* if (i == 0) continue
       * if (j == 0) continue
       * if (i == count - 1) continue
       * if (j == count - 1) continue
       */
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

const SPAWN_COORDINATES = computeSpawnCoordinates()

function findPinPosition(world, digit) {
  ;({w, h} = DIGITS_DEFN[digit]);
  function isAvailable(x, y) {
    var available = true
    const where = pl.AABB(
      Vec2(x - digit_scale * w / 2, y - digit_scale * h / 2),
      Vec2(x + digit_scale * w / 2, y + digit_scale * h / 2),
      /* Vec2(x - digit_scale / 2 * 2, y - digit_scale / 2 * 2),
       * Vec2(x + digit_scale / 2 * 2, y + digit_scale / 2 * 2),*/
    )
    world.queryAABB2(where, function(_) {
      available = false
    })
    return available
  }
  var xys = [...SPAWN_COORDINATES]
  shuffle(xys)
  for (xy of xys) {
    var [x, y] = xy
    if (isAvailable(x, y))
      return xy
  }
  return null
}

const randomBool = createBoolBiasedRng(1)
var ga = 1
var gb = 2.5
var gc = 3

function putPin(world, digit) {
  var xy = findPinPosition(world, digit)
  if (xy === null)
    return false
  var [x, y] = xy
  putPinAt(world, digit, [x, y], null)
  return true
}

var g_player = null
var g_score = 0
var g_knock_ball = null
var g_hp = MAX_HP
var g_round = 0

function classify(a, b) {
  adat = a.getUserData()
  bdat = b.getUserData()
  var player = null
  var wall = null
  var alive_pin = null
  var dead_pin = null
  var two_pins = []

  if (bdat.type == 'player')
    player = b
  else if (bdat.type == 'wall')
    wall = b
  else if (bdat.type == 'pin' && bdat.alive === true) {
    alive_pin = b
    two_pins.push(b)
  }
  else if (bdat.type == 'pin' && bdat.alive === false)
    dead_pin = b
  else
    console.error('unknown entity')

  if (adat.type == 'player')
    player = a
  else if (adat.type == 'wall')
    wall = a
  else if (adat.type == 'pin' && adat.alive === true) {
    alive_pin = a
    two_pins.push(a)
  }
  else if (adat.type == 'pin' && adat.alive === false)
    dead_pin = a
  else
    console.error('unknown entity')

  if (two_pins.length != 2)
    two_pins = null

  return [player, wall, alive_pin, dead_pin, two_pins]
}

async function main(world, canvas) {
  console.log('> main')

  g_player = putPlayer(world, 0, Vec2(0, 0), 0)

  /* var xy = findPinPosition(world, digit)
   * if (xy === null)
   *   return false
   * var [x, y] = xy
   * putPinAt(world, digit, [x, y], null)
   */
  putWalls(world)
  const r = 0.75 * width / 2
  const a0 = Math.random() * Math.PI * 2

  var digits = Array.from({length:9},(v,k)=>k+1)
  digits.push(5)
  console.log(digits)
  shuffle(digits)

  for (var i = 0; i < 10; i++) {
    var a = a0 + Math.PI * 2 / 10 * i
    var x = r * Math.cos(a)
    var y = r * Math.sin(a)
    putPinAt(world, digits[i], [x, y], null)
  }

  while (true) { // one loop per game round
    ;[p, g_knock_ball] = create_promise()
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
    var done = true

    for (b of bodies_of_world(world)) {
      const ud = b.getUserData()
      b.setLinearVelocity({x: 0, y: 0})
      b.setAngularVelocity(0)
      if (ud && ud.type == 'pin' && ud.alive)
        done = false
    }
    if (done)
      document.getElementById('score').innerText = (
        "Game over after " + (g_round + 1) + " rounds. Refresh page."
      )
    else
      document.getElementById('score').innerText = "Round " + (g_round + 2)

    g_round += 1
  }
}

var tb
var mouse = 0
/* var canvas*/

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
      console.log('hook:callback IGNORED', d.type, g_knock_ball !== null)
  }
  return this.queryAABB2.apply(this, [aabb, my_callback])
}

planck.testbed('8 Ball', function(testbed) {

  var world = pl.World({});
  world.__proto__.queryAABB2 = world.queryAABB
  world.__proto__.queryAABB = _hook
  tb = testbed
  canvas = tb.canvas
  testbed.x = 0;
  testbed.y = 0;
  testbed.width = width * 2;
  testbed.height = height * 2;
  testbed.ratio = 100;
  testbed.mouseForce = mouseForce;
  setTimeout(main, 0, world)

  world.on('pre-solve', function(contact) {
    var [player, wall, pin, dead_pin, two_pins] = classify(contact.getFixtureA().getBody(), contact.getFixtureB().getBody())

    if (dead_pin) {
      contact.setEnabled(false)
    }
    if (two_pins) {
      var [p0, p1] = two_pins
      var ud0 = p0.getUserData()
      var ud1 = p1.getUserData()
      console.log('> collision of value', ud0.digit, ud1.digit)
      if (ud0.digit + ud1.digit == 10) {
        console.log('> Deactivating pins', ud0.idx, ud1.idx)
        ud0.alive = false
        ud1.alive = false
        contact.setEnabled(false)
        setTimeout(function () {
          console.log('> Destroying pins', ud0.idx, ud1.idx)
          world.destroyBody(p0)
          world.destroyBody(p1)
        }, 1)
    }
    }
  })

  canvas.onmouseup = function (_) {
    console.log('canvas:onmouseup', 'mouse value:', mouse, 'has-callback:', g_knock_ball !== null)
    if (mouse == 1 && g_knock_ball !== null) {
      mouse = 0
      g_knock_ball()
    }
  }

  return world;
});

/* var bodies = bodies_of_world(world)*/
/* var [b] = bodies*/
