// This is used so that the props can be assigned from Haskell
// and retrieved via Javascript.
// It is only required to interoperate with foreign React components.
// A Haskell-only React app will not need this.
// This must be 'var' and not 'const' to be visible under the 'window' global because
// a ghcjs bug which includes js-sources twice (https://github.com/ghcjs/ghcjs/issues/567)
var h$glazier$react$todo = { listeners: {} };
h$glazier$react$todo.addListener = function(name, listener) {
    if (!this.listeners[name])
        this.listeners[name] = [];
    this.listeners[name].push(listener);
};
h$glazier$react$todo.notifyListeners = function(name) {
    if (this.listeners[name]) {
        for (var i = 0; i < this.listeners[name].length; i++) {
            this.listeners[name][i]();
        }
    }
};
