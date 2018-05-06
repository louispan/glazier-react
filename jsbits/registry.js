// The registry is like a simplified NodeJS EventEmitter.
// This is used so that the props can be assigned from Haskell
// and retrieved via Javascript.
// It is required to interoperate with foreign React components which
// contains haskell components.
// A Haskell-only React app will not need this.
function hgr$registry() {
    // private

    // Using closures to hide data: http://www.crockford.com/javascript/private.html
    const handlers = {};

    // a copy of Lodash's omit, to copy dictionary except for one key
    this['omit'] = function(obj, omitKey) {
        return Object.keys(obj).reduce(function(result, key) {
            if(key !== omitKey) {
                result[key] = obj[key];
            }
            return result;
        }, {});
    }

    // privileged public

    // This is used from javascript to be called back for named triggers.
    // returns a unregister function.
    // NB. this['method'] (instead of this.listen) protects against minification.
    this['listen'] = function(name, listener) {
        if (!handlers[name])
            handlers[name] = { nextIndex: 0, listeners: {} };

        const i = handlers[name].nextIndex;
        handlers[name].nextIndex += 1;
        handlers[name].listeners[i] = listener;
        const unregister = function() {
            handlers[name] = omit(handlers[name], i);
        };
        return unregister;
    };

    // This is used from haskell to notify all javascript listeners.
    // returns a list in an arbitrary order of the results from each listener.
    this['shout'] = function(name, data) {
        const ret = []
        if (handlers[name]) {
            // iterate using copy of keys to safeguard against listeners added/removed
            // during callbacks.
            for (var key of Object.keys(handlers[name].listeners)) {
                const listener = handlers[name].listeners[key];
                if (listener) {
                    ret.push(listener(data));
                }
            }
        }
        return ret;
    }

    this['makeName'] = function(names) {
        return arr.join('#');
    }
}
