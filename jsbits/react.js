// Node module dependencies
// Example package.json:
// {
//   "dependencies": {
//     "react": "^15.4.2",
//     "react-dom": "^15.4.2"
//   }
// }
var hgr$React_;
function hgr$React() {
    if (typeof hgr$React_ == "undefined") {
        hgr$React_ = require('react');
    }
    return hgr$React_;
}

var hgr$ReactDOM_;
function hgr$ReactDOM() {
    if (typeof hgr$ReactDOM_ == "undefined") {
        hgr$ReactDOM_ = require('react-dom');
    }
    return hgr$ReactDOM_;
}

var hgr$shimComponent_;
function hgr$shimComponent() {
    if (typeof hgr$shimComponent_ == "undefined") {
        // Inheriting from Component means every call to this.setState will result in a render
        // Inheriting from PureComponet means a shallow comparison will be made
        var React = hgr$React();
        class Shim extends React.PureComponent {

            componentDidUpdate() {
                if (this.props['componentDidUpdate'])
                    this.props['componentDidUpdate'](this.state);
            }

            render() {
                if (this.props['render'])
                    return this.props['render'](this.state);
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
