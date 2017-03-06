// This function requires that React is in scope by the time this is called.
// Using React.createClass for now since not all browsers support ES6 classes
// It is the equivalent of the following:
// import React from 'react';

// // Inheriting from Component means every call to this.setState will result in a render
// // Inheriting from PureComponet means a shallow comparison will be made
// class Shim extends React.PureComponent {

//     componentDidUpdate() {
//         if (this.props['componentDidUpdate'])
//             this.props['componentDidUpdate'](this.state);
//     }

//     render() {
//         if (this.props['render'])
//             return this.props['render'](this.state);
//         return null;
//     }
// }
// export default Shim;
function hgr$mkClass() {
    const specs = {};
    specs['componentDidUpdate'] = function() {
        if (this.props['componentDidUpdate'])
            this.props['componentDidUpdate'](this.state);
    };
    specs['render'] = function() {
        if (this.props['render'])
            return this.props['render'](this.state);
        return null;
    };
    const cl = React.createClass(specs);
    cl.prototype.isPureReactComponent = true;

    return cl;
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
            return React.createElement('div', null, elements);
        }
    }
    return null;
}
