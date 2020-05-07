var pl = planck, Vec2 = pl.Vec2, Math = pl.Math;
var width = 2.00
var height = width;
var BALL_RADIUS = width / 100 * 9
var ACTIVE_RAILS = true
var mouseForce = width * 10
var ARE_BULLETS = false

function bodies_of_world(w) {
  var arr = [];
  var b = w.getBodyList();

  while (b !== null) {
    arr.push(b)
    b = b.m_next
  }
  return arr
}

pl.internal.Settings.velocityThreshold = 0;

const EPSILON = 0.001
function createRails(world) {
  var thickness = width / 2

  var tl = Vec2(+(width * .5 + .0), +(height * .5 + .0))
  var tr = Vec2(+(width * .5 + thickness), +(height * .5 + thickness))
  var br = Vec2(+(width * .5 + thickness), -(height * .5 + thickness))
  var bl = Vec2(+(width * .5 + .0), -(height * .5 + .0))

  var b = world.createBody({userData: {type: "rail", idx: 0}})
  var shape = pl.Polygon([tl, tr, br, bl])
  b.createFixture(shape, {
    friction: 0.1,
    restitution: 0.9,
  })
  b.setActive(ACTIVE_RAILS)

  var tl = Vec2(-(width * .5 + .0), +(height * .5 + .0))
  var tr = Vec2(-(width * .5 + thickness), +(height * .5 + thickness))
  var br = Vec2(-(width * .5 + thickness), -(height * .5 + thickness))
  var bl = Vec2(-(width * .5 + .0), -(height * .5 + .0))

  var b = world.createBody({userData: {type: "rail", idx: 0}})
  var shape = pl.Polygon([tl, tr, br, bl])
  b.createFixture(shape, {
    friction: 0.1,
    restitution: 0.9,
  })
  b.setActive(ACTIVE_RAILS)

  var tl = Vec2(+(width * .5 + .0), -(height * .5 + .0))
  var tr = Vec2(+(width * .5 + thickness), -(height * .5 + thickness))
  var br = Vec2(-(width * .5 + thickness), -(height * .5 + thickness))
  var bl = Vec2(-(width * .5 + .0), -(height * .5 + .0))

  var b = world.createBody({userData: {type: "rail", idx: 0}})
  var shape = pl.Polygon([tl, tr, br, bl])
  b.createFixture(shape, {
    friction: 0.1,
    restitution: 0.9,
  })
  b.setActive(ACTIVE_RAILS)

  var tl = Vec2(+(width * .5 + .0), +(height * .5 + .0))
  var tr = Vec2(+(width * .5 + thickness), +(height * .5 + thickness))
  var br = Vec2(-(width * .5 + thickness), +(height * .5 + thickness))
  var bl = Vec2(-(width * .5 + .0), +(height * .5 + .0))

  var b = world.createBody({userData: {type: "rail", idx: 0}})
  var shape = pl.Polygon([tl, tr, br, bl])
  b.createFixture(shape, {
    friction: 0.1,
    restitution: 0.9,
  })
  b.setActive(ACTIVE_RAILS)

}

var world = pl.World({});
createRails(world)
createBalls(world)

function createBalls(world) {
  const def = [
    /* [0, {fill: 'white', stroke: 'black'}],*/
    [0, {fill: 'orange', stroke: 'black'}],
    /* [0, {fill: 'yellow', stroke: 'black'}],*/
    [1, {fill: 'black', stroke: 'white'}],
    [2, {fill: 'purple', stroke: 'white'}],
    [3, {fill: 'green', stroke: 'white'}],
    [4, {fill: 'blue', stroke: 'white'}],
    [5, {fill: 'red', stroke: 'white'}],
    [5, {fill: 'red', stroke: 'white'}],
    [6, {fill: 'blue', stroke: 'white'}],
    [7, {fill: 'green', stroke: 'white'}],
    [8, {fill: 'purple', stroke: 'white'}],
    [9, {fill: 'black', stroke: 'white'}],
  ]
  var kx = 4
  var ky = 3
  var spacingx = (width - kx * BALL_RADIUS * 2) / (kx + 1)
  var ball_centroid_distancex = spacingx + BALL_RADIUS * 2
  var spacingy = (height - ky * BALL_RADIUS * 2) / (ky + 1)
  var ball_centroid_distancey = spacingy + BALL_RADIUS * 2

  console.log('spacing', spacingx, 'centroid dist', ball_centroid_distancex, spacingx * (kx + 1) + BALL_RADIUS * 2 * kx)
  console.log('spacing', spacingy, 'centroid dist', ball_centroid_distancey, spacingy * (ky + 1) + BALL_RADIUS * 2 * ky)

  for (var i = 0; i < def.length; i++) {
    var [digit, style] = def[i]

    var x = i % kx
    var y = Math.floor(i / kx)

    var x = x * ball_centroid_distancex
    var y = y * ball_centroid_distancey

    var x = x - width / 2 + spacingx + BALL_RADIUS
    var y = y - height / 2 + spacingy + BALL_RADIUS

    var x = x * 1
    var y = y * -1

    console.log(i, digit, style, {x: x, y: y})

    var b = world.createDynamicBody({
      userData: {type: 'ball', digit: digit, idx: i},
      linearDamping: 1.5,
      angularDamping: 1
    });
    b.setBullet(ARE_BULLETS);
    b.setPosition({x: x, y: y});
    b.createFixture(pl.Circle(BALL_RADIUS), {
      friction: 0.1,
      restitution: 0.5,
      density: 1,
      userData: 'ball'
    });
    b.render = style;
  }
}

var post_solve_count = 0
world.on('post-solve', function(contact) {
  var f0 = contact.getFixtureA()
  var f1 = contact.getFixtureB()
  var b0 = f0.getBody()
  var b1 = f1.getBody()
  var d0 = b0.getUserData()
  var d1 = b1.getUserData()

  /* console.log('> post-solve', post_solve_count, d0, d1)*/

  if (d0.type == 'ball' && d1.type == 'ball' && d0.digit + d1.digit == 10) {
    setTimeout(function() {
      world.destroyBody(b0);
      world.destroyBody(b1);
    }, 1)
  }

  post_solve_count = post_solve_count + 1
});

console.log('begin testbed')
planck.testbed('8 Ball', function(testbed) {
  console.log('begin testbed callback')
  testbed.x = 0;
  testbed.y = 0;
  testbed.width = width * 3;
  testbed.height = height * 3;
  testbed.ratio = 100;
  testbed.mouseForce = mouseForce;
  return world;

});

var bodies = bodies_of_world(world)
var [b] = bodies
