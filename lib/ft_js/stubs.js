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
