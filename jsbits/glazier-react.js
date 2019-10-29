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

// frrom babel generated code
function hgr$objectWithoutProperties(obj, keys) {
    var target = {};
    for (var i in obj) {
        if (keys.indexOf(i) >= 0) continue;
        if (!Object.prototype.hasOwnProperty.call(obj, i)) continue;
        target[i] = obj[i];
    }
    return target;
}

var hgr$ShimComponent_ = null;
// ShimComponent_ hides all React lifecycle and expose them as simple properties
// "onRendered" is called with the ref to the real DOM element whenever react rerenders (even the initial rerender)
// "onConstruct" is called with the ref to the real DOM element on the initial rerender, before the "rendered" callback.
// "onDestruct" is called with the ref to the real DOM element on the initial rerender, before the "rendered" callback.
function hgr$ShimComponent() {
    if (!hgr$ShimComponent_) {
        const ReactPureComponent = hgr$React()["PureComponent"];
        class ShimComponent extends ReactPureComponent {
            static CustomProperties() {
                return ["onRendered", "onConstruct", "onDestruct"];
            }

            getRef() {
                return null;
            }

            constructor(props) {
                super(props);
                this.state = {};
            }

            rerender() {
                this.forceUpdate();
            }

            componentDidMount() {
                // ref is called before componentDidMount
                // if and only if component got rendered
                // so this.ref may be null
                if (this.props['onConstruct'])
                    this.props['onConstruct'](this.getRef());
            }

            componentDidUpdate(prevProps, prevState) {
                // componentDidUpdate is not called on initial render.
                // ignore prevProps, prevState and forward to a custom callback
                if (this.props['onRendered'])
                    this.props['onRendered'](this.getRef());
            }

            componentWillUnmount() {
                if (this.props['onDestruct'])
                    this.props['onDestruct'](this.getRef());
            }
        }
        hgr$ShimComponent_ = ShimComponent;
    }
    return hgr$ShimComponent_;
}

var hgr$ElementComponent_ = null;
// ElementComponent is ShimComponent, with additional properties
// "elementName" is the name of the DOM element to actually render
// "elementRef" the ref to the DOM element
function hgr$ElementComponent() {
    if (!hgr$ElementComponent_) {
        const ShimComponent = hgr$ShimComponent();
        const diff = require('diff');

        // used for calculating new start
        // Basically as we count down old start, we count up new start
        // stops when original start is zero
        const stepSelection = function(diff, old, n) {
            // data only in after:
            // if orig start < 0
            // before: foo|b*r
            // after : foo|b*ar
            // new start is < 0 -> no change
            // old start doesn't change (old string doesn't have this char to increment index)

            // if origin start > 0
            // before: foob*|r
            // after : foob*a|r
            // increment new start -> move past extra char
            // old start does't change (old string doesn't have this char to increment index)
            if (diff.added) {
                return [old, old > 0 ? n + diff.value.length : n];
            }
            // data only in before:
            // if orig start < 0
            // before: foo|b*ar
            // after : foo|b*r
            // old start is < 0 -> no change
            // new start doesn't change (new string doesn't have this char to increment index)
            //
            // if origin start > 0
            // before: foob*a|r
            // after : foob*|r
            // decrement old start -> (move past extra char)
            // new start does't change (new string doesn't have this char to increment index)
            else if (diff.removed) {
                return [old > 0 ? old - diff.value.length : 0, n];
            }
            // common data: lockstep - we want to decrement old start and increment new start
            // as long as original start is above zero
            else {
                if (old > 0)diff.value.length
                    return [old - diff.value.length, n + diff.value.length]
                else
                    return [0, n]
            }
        };

        // When using input
        // checkbox intermediate state is not settable by HTML
        // so this wrapper makes it settable.

        // Also, in GHJS, text inputs doesn't interact well as a React controlled component.
        // Eg. cursor jumps if user types quickly.

        // It is because there is a race condition with lazy event handlers setting the value,
        // So this prototype uses the React uncontrolled component
        // (using defaultValue instead of value).

        // For <input>, React uses controlled input if input.value is not null
        // and there is an onChange handler.

        // Vanilla React uncontrolled input only reads "defaultValue" once.

        // On rerender, this widget will read "props.value", and set the DOM input value in javascript.
        // So, try to avoid triggering a re-render unless absolutely necessary
        // In normal cases, you don't need to trigger a rerender since it is an
        // uncontrolled component, and will be rerendered automatically by the DOM
        // from user input.

        // This widget attempts to set the cursor position at the correct place
        // by using a diffing algorithm on the old and new value.

        class ElementComponent extends ShimComponent {
            static CustomProperties() {
                return ShimComponent.CustomProperties().concat(["elementName", "elementRef", "indeterminate"]);
            }

            constructor(props) {
                super(props);
                this.ref = null;
                this.onRef= this.onRef.bind(this);
            }

            updateInput() {
                if (this.ref && this.props.elementName.localeCompare("input")) {
                    // safe if this.props.indeterminate is undefined
                    this.ref.indeterminate = this.props.indeterminate
                    // Using uncontrolled input
                    // input stringifies "undefined" but converts null to ''
                    if (typeof this.props.value !== 'undefined') {
                        this.ref.value = this.props.value;

                        // first calculate the new selection rang
                        var s0 = this.ref.selectionStart;
                        var e0 = this.ref.selectionEnd;
                        const ds = diff.diffChars(this.ref.value, this.props.value, {ignoreCase: true});
                        var s1 = 0;
                        var e1 = 0;
                        for (d of ds) {
                            if (s0 <= 0 && e0 <= 0)
                                break;
                            [s0, s1] = stepSelection(d, s0, s1);
                            [e0, e1] = stepSelection(d, e0, e1);
                        }
                        this.ref.setSelectionRange(s1, e1);
                    }
                    else {
                        this.ref.value = '';
                    }
                }
            }

            onRef(ref) {
                this.ref = ref;
                updateInput();
                // also forward to elementRef prop if set
                if (this.props.elementRef)
                    this.props.elementRef(ref);
            }

            getRef() {
                return this.ref;
            }

            rerender() {
                updateInput();
                this.forceUpdate();
            }

            render() {
                // hide ref from ReactElement because we want to pass our handler instead onRef instead
                // hide our custom properties
                props = hgr$objectWithoutProperties(this.props, ElementComponent.CustomProperties());
                props.ref = this.onRef;
                if (this.props.elementName.localeCompare("input")) {
                    props.defaultValue = this.props.value; // this only works on the first render
                }
                return React.createElement(this.props.elementName, props, props.children);
            }
        }
        hgr$ElementComponent_ = ElementComponent;
    }
    return hgr$ElementComponent_;
}

// WidgetComponent is ShimComponent, with one additional property
// "render" is a custom rendering function.
var hgr$WidgetComponent_ = null;
function hgr$WidgetComponent() {
    if (!hgr$WidgetComponent_) {
        const ShimComponent = hgr$ShimComponent();
        class WidgetComponent extends ShimComponent {
            static CustomProperties() {
                return ElementComponent.CustomProperties().concat(["render"]);
            }

            constructor(props) {
                super(props);
            }

            getRef() {
                return this;
            }

            render() {
                // hide our custom properties
                props = hgr$objectWithoutProperties(this.props, WidgetComponent.CustomProperties());
                if (this.props['render'])
                    return this.props['render']();
                return null;
            }
        }
        hgr$WidgetComponent_ = WidgetComponent;
    }
    return hgr$WidgetComponent_;
}

class hgr$ReactBatcher {
    constructor() {
        this.batch = [];
    }

    batch(f) {
        this.batch.push(f);
    }

    runBatch() {
      const btch = this.batch;
      this.batch = []
      hgr$ReactDOM().unstable_batchedUpdates(() => {
        for (const f of btch){
            f();
        }
      });
    }
}

function hgr$mkReactBatcher() {
    return new hgr$ReactBatcher();
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
            return hgr$React().createElement(hgr$React().Fragment, null, elements);
        }
    }
    return null;
}

