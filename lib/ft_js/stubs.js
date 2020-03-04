//Provides: ft_js_float32_ba_of_ta
function ft_js_float32_ba_of_ta(arr) {
  var g=jsoo_runtime;
  if (!(arr instanceof Float32Array))
    console.log("Pas bien 0", arr)
  var ba = g.caml_ba_create_unsafe(0, 0, Array.from([arr.length]), arr);
  return ba
}

//Provides: ft_js_float32_ta_of_ba
function ft_js_float32_ta_of_ba(arr) {
  if (!(arr.kind == 0 || arr.kind == 10))
    console.log("Pas bien 10", arr)
  if (!(arr.data instanceof Float32Array))
    console.log("Pas bien 11", arr)
  return arr.data
}

//Provides: ft_js_uint8_ba_of_ta
function ft_js_uint8_ba_of_ta(arr) {
  var g=jsoo_runtime;
  if (!(arr instanceof Uint8Array))
    console.log("Pas bien 2", arr)
  var ba = g.caml_ba_create_unsafe(3, 0, Array.from([arr.length]), arr);
  return ba
}

//Provides: ft_js_uint8_ta_of_ba
function ft_js_uint8_ta_of_ba(arr) {
  if (!(arr.kind == 3 || arr.kind == 13))
    console.log("Pas bien 30", arr)
  if (!(arr.data instanceof Uint8Array))
    console.log("Pas bien 31", arr)
  return arr.data
}
