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
        // The state in the Shim contains
        //   frame :: A GHCJS.Foreign.Export of the haskell state to render.
        // Inheriting from Component means every call to this.setState will result in a render.
        // Inheriting from PureComponet means a shallow comparison will be made.
        // Protect "PureComponent" from closure compiler because it's not in the official externs.
        var ReactPureComponent = hgr$React()["PureComponent"];

        class Shim extends ReactPureComponent {

            constructor(props) {
                super(props);
                this.state = { frameNum: 0 };
                this.pendingRerender = false;
            }

            doRerender() {
                this.setState(function(prevState, props) {
                    var newFrameNum = this.safeIncrement(prevState.frameNum);
                    return {
                        frameNum: newFrameNum
                    };
                });
                this.pendingRerender = false;
            }

            rerender() {
                // batch all rerender requrests in the same callstack.
                if (!this.pendingRerender) {
                    this.pendingRerender = true;
                    var that = this;
                    window.setTimeout(function(){that.doRerender()}, 1);
                }
            }

            safeIncrement(i) {
                if (i >= Number.MAX_SAFE_INTEGER) {
                        return Number.MIN_SAFE_INTEGER
                    }
                    else {
                        return i + 1;
                }
            }

            componentDidUpdate(prevProps, prevState) {
                // componentDidUpdate is not called on initial render.
                // ignore prevProps, prevState and forward to a custom callback
                if (this.props['rendered'])
                    this.props['rendered']();
            }

            componentDidMount() {
                // Also forward to updated so it gets a callback on initial render.
                console.log("##### componentDidMount ###");
                if (this.props['mounted'])
                    this.props['mounted']();
                if (this.props['rendered'])
                    this.props['rendered']();
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
