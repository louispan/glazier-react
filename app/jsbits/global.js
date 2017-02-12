// This is used so that the props can be assigned from Haskell
// and retrieved via Javascript.
// It is only required to interoperate with foreign React components.
// A Haskell-only React app that only relies on React's virtualdom will not need this.
// However, if you want reduce DOM diffing by using the React.Component.setState,
// then you'll need a global for interoperability.
// This must be 'var' and not 'const' to be visible under the 'window' global because
// a ghcjs bug which includes js-sources twice (https://github.com/ghcjs/ghcjs/issues/567)
// This global will need to be added to externs file before minimizing using closure-compiler.
var h$glazier$react$todo = { listeners: {} };
h$glazier$react$todo.addListener = function(name, listener) {
    if (!this.listeners[name])
        this.listeners[name] = [];
    this.listeners[name].push(listener);
};
h$glazier$react$todo.notifyListeners = function(name, evt) {
    if (this.listeners[name]) {
        for (var i = 0; i < this.listeners[name].length; i++) {
            this.listeners[name][i](evt);
        }
    }
};
