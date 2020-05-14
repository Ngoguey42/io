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
    console.log('> Digit ', digit, 'from', dense_coords.length, 'coords to', light_coords.length, 'coords')
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

    console.log('$ Kill all velocities at step', world.m_stepCount)
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

const events = {
341: ["move", Vec2(240.1662049861507,-5703.947368421052)],
441: ["kill", null],
531: ["move", Vec2(-2820.8290555793856,-6036.213400113114)],
631: ["kill", null],
715: ["move", Vec2(-1736.55385958767,-3411.6692174901973)],
815: ["kill", null],
856: ["move", Vec2(-1041.171975122699,3593.823529510891)],
956: ["kill", null],
980: ["move", Vec2(4733.654411589059,3166.799313221475)],
1080: ["kill", null],
1113: ["move", Vec2(-17.093097375925524,-1340.3992276017466)],
1193: ["kill", null],
1220: ["move", Vec2(-1129.8613039671036,308.59253228426354)],
1300: ["kill", null],
1346: ["move", Vec2(-1398.3770246544534,2974.625200066688)],
1446: ["kill", null],
1500: ["move", Vec2(-2364.7833128116185,2322.2723386013568)],
1580: ["kill", null],
1632: ["move", Vec2(7806.195502194902,-2787.3350251991046)],
1732: ["kill", null],
1776: ["move", Vec2(6427.646998463051,2943.9443420052603)],
1896: ["kill", null],
1947: ["move", Vec2(5653.20522739833,1563.7359598174446)],
2067: ["kill", null],
2101: ["move", Vec2(10620.536910455594,2466.2084211951524)],
2201: ["kill", null],
2249: ["move", Vec2(7252.811366527396,1837.5453575562017)],
2369: ["kill", null],
2447: ["move", Vec2(4552.732852694188,2912.8256820490224)],
2567: ["kill", null],
2615: ["move", Vec2(601.6774713747501,714.7106300227429)],
2695: ["kill", null],
2753: ["move", Vec2(2188.5141732617585,-3687.9861812140452)],
2853: ["kill", null],
2937: ["move", Vec2(-6318.810615206455,55.4625802640948)],
3057: ["kill", null],
3080: ["move", Vec2(-1359.0393539641766,-1059.9360569064927)],
3160: ["kill", null],
3195: ["move", Vec2(3381.648148558141,-473.88793485278825)],
3295: ["kill", null],
3369: ["move", Vec2(-7288.671960412422,1722.5066239616947)],
3489: ["kill", null],
3539: ["move", Vec2(1010.6328161897775,-2574.7708658619085)],
3619: ["kill", null],
}

if (BENCHMARK) {
  for (j = 0; j < BENCH_LOOP_COUNT; j++) {
    t0 = Date.now() / 1000
    var world = pl.World({});
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
      var ev = events[i]
      if (ev !== undefined) {
        if (ev[0] == 'move') {
          g_player.applyForceToCenter(ev[1], true);
        }
        else {
          for (b of bodies_of_world(world)) {
            b.setLinearVelocity({x: 0, y: 0})
            b.setAngularVelocity(0)
          }
        }
      }
      world.step(1/60);
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

    world.on('pre-solve', function(contact) {
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
          setTimeout(function () {
            /* console.log('> Destroying pins', ud0.idx, ud1.idx)*/
            world.destroyBody(p0)
            world.destroyBody(p1)
          }, 1)
        }
      }
    })

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
