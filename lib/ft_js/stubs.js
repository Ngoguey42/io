//Requires: caml_ba_create_unsafe
//Provides: ft_js_float32_ba_of_ta
function ft_js_float32_ba_of_ta(arr) {
  if (!(arr instanceof joo_global_object.Float32Array))
    joo_global_object.console.log("Pas bien 0", arr)
  var ba = caml_ba_create_unsafe(0, 0, Array.from([arr.length]), arr);
  return ba
}

//Provides: ft_js_float32_ta_of_ba
function ft_js_float32_ta_of_ba(arr) {
  if (!(arr.kind == 0 || arr.kind == 10))
    joo_global_object.console.log("Pas bien 10", arr)
  if (!(arr.data instanceof joo_global_object.Float32Array))
    joo_global_object.console.log("Pas bien 11", arr)
  return arr.data
}

//Requires: caml_ba_create_unsafe
//Provides: ft_js_int32_ba_of_ta
function ft_js_int32_ba_of_ta(arr) {
  if (!(arr instanceof joo_global_object.Int32Array))
    joo_global_object.console.log("Pas bien 50", arr)
  var ba = caml_ba_create_unsafe(0, 0, Array.from([arr.length]), arr);
  return ba
}

//Provides: ft_js_int32_ta_of_ba
function ft_js_int32_ta_of_ba(arr) {
  if (!(arr.kind == 0 || arr.kind == 10))
    joo_global_object.console.log("Pas bien 51", arr)
  if (!(arr.data instanceof joo_global_object.Int32Array))
    joo_global_object.console.log("Pas bien 52", arr)
  return arr.data
}

//Requires: caml_ba_create_unsafe
//Provides: ft_js_uint8_ba_of_ta
function ft_js_uint8_ba_of_ta(arr) {
  if (!(arr instanceof joo_global_object.Uint8Array))
    joo_global_object.console.log("Pas bien 2", arr)
  var ba = caml_ba_create_unsafe(3, 0, Array.from([arr.length]), arr);
  return ba
}

//Provides: ft_js_uint8_ta_of_ba
function ft_js_uint8_ta_of_ba(arr) {
  if (!(arr.kind == 3 || arr.kind == 13))
    joo_global_object.console.log("Pas bien 30", arr)
  if (!(arr.data instanceof joo_global_object.Uint8Array))
    joo_global_object.console.log("Pas bien 31", arr)
  return arr.data
}

//Provides: ft_js_create_component_class
function ft_js_create_component_class(ftJsConstructor, displayName){
  function FtJsComponent(props) {
    this.state = {}
    joo_global_object.React.Component.call(this, props)
    ftJsConstructor(this, props)
  }
  FtJsComponent.prototype = Object.create(joo_global_object.React.Component.prototype)
  FtJsComponent.displayName = displayName
  FtJsComponent.prototype.constructor = FtJsComponent
  FtJsComponent.prototype.render = function() {
      return this.ftJsRender()
  }
  FtJsComponent.prototype.componentDidMount = function() {
      this.ftJsMount()
  }
  FtJsComponent.prototype.componentDidUpdate = function() {
      this.ftJsUpdate()
  }
  FtJsComponent.prototype.componentWillUnmount = function() {
      this.ftJsUnmount()
  }
  return FtJsComponent
}
