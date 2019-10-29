// Node module dependencies
// Example package.json:
// {
//   "dependencies": {
//     "react": "^15.4.2",
//     "react-dom": "^15.4.2"
//   }
// }
var hgr$React_ = null;
function hgr$React() {
    if (!hgr$React_) {
        hgr$React_ = require('react');
    }
    return hgr$React_;
}

var hgr$ReactDOM_ = null;
function hgr$ReactDOM() {
    if (!hgr$ReactDOM_) {
        hgr$ReactDOM_ = require('react-dom');
    }
    return hgr$ReactDOM_;
}

// frrom babel generated code
function hgr$objectWithoutProperties(obj, keys) {
    var target = {};
    for (var i in obj) {
        if (keys.indexOf(i) >= 0) continue;
        if (!Object.prototype.hasOwnProperty.call(obj, i)) continue;
        target[i] = obj[i];
    }
    return target;
}

var hgr$ShimComponent_ = null;
// ShimComponent_ hides all React lifecycle and expose them as simple properties
// "rendered" is called with the ref to the real DOM element whenever react rerenders (even the initial rerender)
// "constructor" is called with the ref to the real DOM element on the initial rerender, before the "rendered" callback.
// "destructor" is called with the ref to the real DOM element on the initial rerender, before the "rendered" callback.
function hgr$ShimComponent() {
    if (!hgr$ShimComponent_) {
        const ReactPureComponent = hgr$React()["PureComponent"];
        class ShimComponent extends ReactPureComponent {
            static CustomProperties() {
                return ["rendered", "constructor", "destructor"];
            }

            getRef() {
                return null;
            }

            constructor(props) {
                super(props);
                this.state = {};
            }

            rerender() {
                this.forceUpdate();
            }

            componentDidMount() {
                // ref is called before componentDidMount
                // if and only if component got rendered
                // so this.ref may be null
                if (this.props['constructor'])
                    this.props['constructor'](this.getRef());
            }

            componentDidUpdate(prevProps, prevState) {
                // componentDidUpdate is not called on initial render.
                // ignore prevProps, prevState and forward to a custom callback
                if (this.props['rendered'])
                    this.props['rendered'](this.getRef());
            }

            componentWillUnmount() {
                if (this.props['destructor'])
                    this.props['destructor'](this.getRef());
            }
        }
        hgr$ShimComponent_ = ShimComponent;
    }
    return hgr$ShimComponent_;
}

var hgr$ElementComponent_ = null;
// ElementComponent is ShimComponent, with additional properties
// "elementName" is the name of the DOM element to actually render
// "elementRef" the ref to the DOM element
function hgr$ElementComponent() {
    if (!hgr$ElementComponent_) {
        const ShimComponent = hgr$ShimComponent();
        class ElementComponent extends ShimComponent {
            static CustomProperties() {
                return ShimComponent.CustomProperties().concat(["elementName", "elementRef"]);
            }

            constructor(props) {
                super(props);
                this.ref = null;
                this.onRef= this.onRef.bind(this);
            }

            onRef(ref) {
                this.ref = ref;
                // also forward to elementRef prop if set
                if (this.props.elementRef)
                    this.props.elementRef(ref);
            }

            getRef() {
                return this.ref;
            }

            rerender() {
                this.forceUpdate();
            }

            render() {
                // hide ref from ReactElement because we want to pass our handler instead onRef instead
                // hide our custom properties
                props = hgr$objectWithoutProperties(this.props, ElementComponent.CustomProperties());
                props.ref = this.onRef;
                return React.createElement(this.props.elementName, props, props.children);
            }
        }
        hgr$ElementComponent_ = ElementComponent;
    }
    return hgr$ElementComponent_;
}

// WidgetComponent is ShimComponent, with one additional property
// "render" is a custom rendering function.
var hgr$WidgetComponent_ = null;
function hgr$WidgetComponent() {
    if (!hgr$WidgetComponent_) {
        const ShimComponent = hgr$ShimComponent();
        class WidgetComponent extends ShimComponent {
            static CustomProperties() {
                return ElementComponent.CustomProperties().concat(["render"]);
            }

            constructor(props) {
                super(props);
            }

            getRef() {
                return this;
            }

            render() {
                // hide our custom properties
                props = hgr$objectWithoutProperties(this.props, WidgetComponent.CustomProperties());
                if (this.props['render'])
                    return this.props['render']();
                return null;
            }
        }
        hgr$WidgetComponent_ = WidgetComponent;
    }
    return hgr$WidgetComponent_;
}

class hgr$ReactBatcher {
    constructor() {
        this.batch = [];
    }

    batch(f) {
        this.batch.push(f);
    }

    runBatch() {
      const btch = this.batch;
      this.batch = []
      hgr$ReactDOM().unstable_batchedUpdates(() => {
        for (const f of btch){
            f();
        }
      });
    }
}

function hgr$mkReactBatcher() {
    return new hgr$ReactBatcher();
}

// Convert a list of ReactElements into a single ReactElement
function hgr$mkCombinedElements(elements) {
    if (elements && elements.constructor === Array) {
        if (elements.length === 0) {
            return null;
        }
        else if (elements.length === 1) {
            return elements[0];
        }
        else {
            return hgr$React().createElement(hgr$React().Fragment, null, elements);
        }
    }
    return null;
}

