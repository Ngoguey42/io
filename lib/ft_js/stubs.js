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

//Provides: ft_js_create_component_type
function ft_js_create_component_type(ftJsConstructor){
  function FtJsComponent(props) {
    this.state = {}
    joo_global_object.React.Component.call(this, props)
    ftJsConstructor(this, props)
  }
  FtJsComponent.prototype = Object.create(joo_global_object.React.Component.prototype)
  FtJsComponent.prototype.constructor = FtJsComponent
  FtJsComponent.prototype.render = function() {
      return this.ftJsRender(this.props.data)
  }
  FtJsComponent.prototype.componentDidMount = function() {
      return this.ftJsMount()
  }
  FtJsComponent.prototype.componentWillUnmount = function() {
      return this.ftJsUnmount()
  }
  return FtJsComponent
}

//Provides: ft_js_import
function ft_js_import(url){
  function truc(resolve, reject){
    var script = document.createElement('script');
    script.src = url;
    script.async = true;
    function onresolve(){
      resolve(window['external_global_component'])
    }
    script.onload = onresolve;
    script.onerror = reject;
    document.head.appendChild(script);
  }
  return new joo_global_object.Promise(truc)
  /* return import(name)*/
}
