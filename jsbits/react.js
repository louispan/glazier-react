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

var hgr$shimComponent_ = null;
function hgr$shimComponent() {
    if (!hgr$shimComponent_) {
        // Inheriting from Component means every call to this.setState will result in a render
        // Inheriting from PureComponet means a shallow comparison will be made
        // Protect "PureComponent" from closure compiler because it's not in the official externs
        var ReactPureComponent = hgr$React()["PureComponent"];
        class Shim extends ReactPureComponent {

            componentDidUpdate(prevProps, prevState) {
                // ignore prevProps, prevState and forward to a custom callback
                if (this.props['updated'])
                    this.props['updated']();
            }

            render() {
                if (this.props['render'])
                    return this.props['render']();
                return null;
            }
        }
        hgr$shimComponent_ = Shim;
    }
    return hgr$shimComponent_;
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
