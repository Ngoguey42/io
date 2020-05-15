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
/* const ARE_BULLETS = false*/
const ARE_BULLETS = true
const MIN_CONTACT_STEP_DISTANCE = 20
const MAX_HP = 100
const BENCHMARK = true
const USE_BALLS = false
const BENCH_LOOP_COUNT = 10

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

  if (USE_BALLS) {
    b.createFixture(pl.Circle(1), def);
  }
  else {
    var dense_coords = DIGITS_DEFN[digit]['exterior']
    var light_coords = simplify_coords(dense_coords)
    /* console.log('> Digit ', digit, 'from', dense_coords.length, 'coords to', light_coords.length, 'coords')*/
    var fn = Vec2.scaleFn(digit_scale, digit_scale)
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
  /* console.log('> New pin', d)*/

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
  /* console.log('> main')*/

  g_player = putPlayer(world, 0, Vec2(0, 0), 0)

  /* var xy = findPinPosition(world, digit)
   * if (xy === null)
   *   return false
   * var [x, y] = xy
   * putPinAt(world, digit, [x, y], null)
   */
  putWalls(world)
  const r = 0.75 * width / 2
  /* const a0 = Math.random() * Math.PI * 2*/
  const a0 = 0 * Math.PI * 2

  var digits = Array.from({length:9},(v,k)=>k+1)
  digits.push(5)
  /* console.log(digits)*/
  /* shuffle(digits)*/

  for (var i = 0; i < 10; i++) {
    var a = a0 + Math.PI * 2 / 10 * i
    var x = r * Math.cos(a)
    var y = r * Math.sin(a)
    putPinAt(world, digits[i], [x, y], null)
  }

  while (true) { // one loop per game round
    ;[p, g_knock_ball] = create_promise()
    /* console.log('> main | wait for click')*/
    await p
    /* console.log('> main | got click')*/
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
    /* console.log('> main | stopping remaining velocity')*/
    var done = true

    console.log(world.m_stepCount
              + ': ["freeze", '
              + g_player.c_position.c.x
              + ", "
              + g_player.c_position.c.y
              + '],')
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


function onPreSolveContact(contact) {
  var [player, wall, pin, dead_pin, two_pins] = classify(contact.getFixtureA().getBody(), contact.getFixtureB().getBody())

  if (dead_pin) {
    contact.setEnabled(false)
  }
  if (two_pins) {
    var [p0, p1] = two_pins
    var ud0 = p0.getUserData()
    var ud1 = p1.getUserData()
    /* console.log('> collision of value', ud0.digit, ud1.digit)*/
    if (ud0.digit + ud1.digit == 10) {
      /* console.log('> Deactivating pins', ud0.idx, ud1.idx)*/
      ud0.alive = false
      ud1.alive = false
      contact.setEnabled(false)
      if (!BENCHMARK) {
        setTimeout(function () {
          console.log(world.m_stepCount + ': ["destroy", ' + ud0.idx + ", " + ud1.idx + '],')
          world.destroyBody(p0)
          world.destroyBody(p1)
        }, 1)
      }
    }
  }
}

const events = {
123: ["move", Vec2(8189.966101694915, -4457.457627118645)],
223: ["freeze", -3.2583717709039686, 5.370508766869994],
248: ["move", Vec2(8458.22119235671, -939.902197160893)],
348: ["freeze", 2.989605675013066, 0.2059742198642078],
388: ["move", Vec2(1024.3861528739465, 2848.758127464377)],
488: ["freeze", 2.967847163431716, 6.991383162408699],
544: ["move", Vec2(-2809.9671863977383, -990.7772456671967)],
624: ["freeze", -4.891969865155296, 5.574849331193486],
719: ["move", Vec2(-6069.67233069066, -5034.288440221952)],
724: ["destroy", 6, 2],
819: ["freeze", 3.9102816060715266, 4.963682086599173],
854: ["move", Vec2(-6188.885173189342, -6739.586979483325)],
864: ["destroy", 9, 4],
954: ["freeze", -2.697437621110727, 3.8587470814325413],
1011: ["move", Vec2(6910.147524179155, -2584.3886275939394)],
1019: ["destroy", 7, 1],
1111: ["freeze", 5.8340167914163406, 5.6451718263968065],
1154: ["move", Vec2(-4272.744619052654, -2480.019468031071)],
1174: ["destroy", 8, 0],
1254: ["freeze", -4.5922095652817285, 3.115764014886168],
1300: ["move", Vec2(9441.62653689355, -3129.3305608644823)],
1420: ["freeze", 7.111925344546387, -1.2115397152983276],
1484: ["move", Vec2(-1965.7318633396485, -4547.7265287109885)],
1604: ["freeze", -5.895234301331919, 0.41081798524056573],
1636: ["move", Vec2(-1597.622237471704, 3667.5956108017076)],
1736: ["freeze", -5.736635361041712, -4.799957148184972],
1773: ["move", Vec2(2611.188547523195, -2669.373706518094)],
1873: ["freeze", 4.795966425364605, 2.288058353191983],
1917: ["move", Vec2(-3448.442641258729, 5718.958247162038)],
2037: ["freeze", 6.233844595696732, -6.729582409346531],
2083: ["move", Vec2(3244.053149073121, 5182.094278610428)],
2183: ["freeze", -1.5249097242065228, -5.388947096575334],
2240: ["move", Vec2(-6653.493793174352, -789.9025607584284)],
2254: ["destroy", 5, 3],
2360: ["freeze", 0.6719982830393725, 0.14290374267577768],
}

if (BENCHMARK) {
  for (j = 0; j < BENCH_LOOP_COUNT; j++) {
    t0 = Date.now() / 1000
    var world = pl.World({});
    world.on('pre-solve', onPreSolveContact)
    /* world.setContinuousPhysics(false)*/

    g_player = putPlayer(world, 0, Vec2(0, 0), 0)
    putWalls(world)
    const r = 0.75 * width / 2
    const a0 = 0 * Math.PI * 2
    var digits = Array.from({length:9},(v,k)=>k+1)
    digits.push(5)
    for (var i = 0; i < 10; i++) {
      var a = a0 + Math.PI * 2 / 10 * i
      var x = r * Math.cos(a)
      var y = r * Math.sin(a)
      putPinAt(world, digits[i], [x, y], null)
    }

    t1 = Date.now() / 1000
    for (i = 0; i < 3619; i++) {
      world.step(1/60);

      for (b of bodies_of_world(world))  {
        var [player, wall, pin, dead_pin, two_pins] = classify(b, b)
        if (dead_pin)
          world.destroyBody(b)
      }

      var ev = events[i]
      if (ev !== undefined) {
        if (ev[0] == 'move') {
          g_player.applyForceToCenter(ev[1], true);
        }
        else if (ev[0] == 'freeze') {
          for (b of bodies_of_world(world)) {
            b.setLinearVelocity({x: 0, y: 0})
            b.setAngularVelocity(0)
          }
          /* console.log('> freeze player at', g_player.c_position.c.x, g_player.c_position.c.y, 'was', ev[1], ev[2])*/
        }
        /* else if (ev[0] == 'destroy') {
         *   for (b of bodies_of_world(world)) {
         *     if (b.getUserData().idx == ev[1]) {
         *       console.log('> destroy ball', ev[1])
         *       world.destroyBody(b)
         *     }
         *     if (b.getUserData().idx == ev[2]) {
         *       world.destroyBody(b)
         *       console.log('> destroy ball', ev[2])
         *     }
         *   }*/
        /* }*/
      }
    }
    t2 = Date.now() / 1000

    console.log('prime', t1 - t0, 'steps', t2 - t1, 'total', t2 - t0)
  }
}
else {
  var tb
  var mouse = 0
  /* var canvas*/

  function _hook(aabb, callback) {
    mouse = 0
    /* console.log('hook', aabb)*/
    function my_callback(f) {
      var d = f.m_body.getUserData()
      mouse = 0
      if (d.type == 'player' && g_knock_ball !== null) {
        /* console.log('hook:callback player')*/
        mouse = 1
        callback(f)
      }
      /* else*/
      /* console.log('hook:callback IGNORED', d.type, g_knock_ball !== null)*/
    }
    return this.queryAABB2.apply(this, [aabb, my_callback])
  }

  planck.testbed('8 Ball', function(testbed) {

    var world = pl.World({});
    /* world.setContinuousPhysics(false)*/

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

    world.on('pre-solve', onPreSolveContact)

    canvas.onmouseup = function (_) {
      /* console.log('canvas:onmouseup', 'mouse value:', mouse, 'has-callback:', g_knock_ball !== null)*/
      if (mouse == 1 && g_knock_ball !== null) {
        mouse = 0
        g_knock_ball()
      }
    }

    return world;
  });

  /* var bodies = bodies_of_world(world)*/
  /* var [b] = bodies*/
}
