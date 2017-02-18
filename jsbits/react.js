// This function requires that React is in scope
// isPure: bool
// name: string
// renderCb: function() { return React.createElement(...) }
// propTypes: { ... }
// defaultProps: { ... }
// state: { ... }
// Using React.createClass for now since not all browsers support ES6 classes
function hgr$mkClass(isPure, name, renderCb, defaultProps, state, propTypes) {
    const specs = { 'displayName': name }
    specs['render'] = (renderCb) ? renderCb : function() { return null; };
    if (propTypes) {
        specs['propTypes'] = propTypes;
    }
    if (defaultProps) {
        specs['getDefaultProps'] = function() { return defaultProps; };
    }
    if (state) {
        specs['getInitialState'] = function() { return state; };
    }
    const cl = React.createClass(specs);
    if (isPure) {
        cl.prototype.isPureReactComponent = true;
    }
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
