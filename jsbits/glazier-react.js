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

var hgr$WidgetComponent_ = null;
function hgr$WidgetComponent() {
    if (!hgr$WidgetComponent_) {
        const ReactPureComponent = hgr$React()["PureComponent"];

        // The state in the Shim contains
        //   frame :: A GHCJS.Foreign.Export of the haskell state to render.
        // Inheriting from Component means every call to this.setState will result in a render.
        // Inheriting from PureComponet means a shallow comparison will be made.
        // Protect "PureComponent" from closure compiler because it's not in the official externs.
        class WidgetComponent extends ReactPureComponent {

            constructor(props) {
                super(props);
                this.state = {};
            }

            rerender() {
                this.forceUpdate();
            }

            componentDidUpdate(prevProps, prevState) {
                // componentDidUpdate is not called on initial render.
                // ignore prevProps, prevState and forward to a custom callback
                if (this.props['rendered'])
                    this.props['rendered']();
            }

            render() {
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
