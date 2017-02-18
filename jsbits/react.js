// This function requires that React is in scope
// isPure: bool
// name: string
// renderCb: function() { return React.createElement(...) }
// propTypes: { ... }
// defaultProps: { ... }
// state: { ... }
// Using React.createClass for now since not all browsers support ES6 classes
// function hgr$mkClass(isPure, name, render, initialState, registry, key) {
//     const specs = { 'displayName': name }
//     specs['render'] = function() {
//         if (render)
//         return null;
//     };
//     if (initialState) {
//         specs['getInitialState'] = function() {
//             return initialState;
//         };
//     }
//     spec['constructor'] = function(props) {
//         super(props);
//         registry.listen(key, function(newState) { this.setState(newState) }.bind(this));
//     }

//     const cl = React.createClass(specs);
//     // means the state will only be shallowly compared
//     if (isPure) {
//         cl.prototype.isPureReactComponent = true;
//     }
//     return cl;
// }

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
