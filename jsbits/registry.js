// This is used so that the props can be assigned from Haskell
// and retrieved via Javascript.
// It is required to interoperate with foreign React components.
// A Haskell-only React app that only relies on React's dom diff will not need this.
// However, if you want reduce DOM diffing by using the React.Component.setState,
// then you'll need a registry for interoperability.
// Use this registry to create a global variable that is accessible from both haskell and html.
// The global must be 'var' and not 'const' to be visible under the 'window' global because
// a ghcjs bug which includes js-sources twice (https://github.com/ghcjs/ghcjs/issues/567)
// This global will need to be added to externs file before minimizing using closure-compiler.
// FIXME: externs file for registry
function Registry() {
    // private
    // Using closures to hide data: http://www.crockford.com/javascript/private.html
    const listeners = {};

    // privileged public

    // This is used from javascript to be called back for named triggers.
    this.listen = function(name, listener) {
        if (!listeners[name])
            listeners[name] = [];
        listeners[name].push(listener);
    };

    //  This is used from haskell to notify all javascript listeners.
    this.shout = function(name, data) {
        if (listeners[name]) {
            for (var i = 0; i < listeners[name].length; i++) {
                listeners[name][i](data);
            }
        }
    }

    this.mkName = function(names) {
        return arr.join('#');
    }
}
