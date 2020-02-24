//Provides: ft_float32ba_of_uint8arr
function ft_float32ba_of_uint8arr(arr) {
  var g=jsoo_runtime;
  var arr = Float32Array.from(arr);
  var ba = g.caml_ba_create_unsafe(0, 0, Array.from([arr.length]), arr);
  return ba
}

//Provides: ft_uint8arr_of_float32ba
function ft_uint8arr_of_float32ba(arr) {
  /* var g=jsoo_runtime;*/
  /* var arr = */
  /* var ba = g.caml_ba_create_unsafe(0, 0, Array.from([arr.length]), arr);*/
  return Uint8Array.from(arr.data);
}
