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

var hgr$component_ = null;
function hgr$component() {
    if (!hgr$component_) {
        // Inheriting from Component means every call to this.setState will result in a render
        // Inheriting from PureComponet means a shallow comparison will be made
        // Protect "PureComponent" from closure compiler because it's not in the official externs
        var ReactPureComponent = hgr$React()["PureComponent"];
        class Shim extends ReactPureComponent {

            componentDidUpdate() {
                if (this.props['componentDidUpdate'])
                    this.props['componentDidUpdate'](this.state);
            }

            render() {
                if (this.props['render'])
                    return this.props['render']();
                return null;
            }
        }
        hgr$component_ = Shim;
    }
    return hgr$component_;
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
            return hgr$React().createElement('div', null, elements);
        }
    }
    return null;
}

// Combine functions into a single function
// Given two 'Callback (JSVal -> IO ())'
// return a function that calls both callbacks
function hgr$combineCallback1(a, b) {
    return function(j) {
        a(j);
        b(j);
    }
}
