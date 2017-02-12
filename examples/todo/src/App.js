const React = window.React;
const Component = window.React.Component;

import logo from './logo.svg';
import './App.css';

class App extends Component {

    // hgr$todo is a global variable from ghcjs all.js which must be loaded via  via <script> tags in the index.html
    // It is not possible to export all.js as a webpack module because Babel freezes trying to compile haskell.
    // Global variable loaded via <script> tags in the index.html are accessible from `window`.
    constructor(props) {
        super(props);
        // This component is stateful as the state is modifiable via a Haskell callback, which is setup later

        this.state = { };

        // Use a global registry to be notified when state has changed.
        window.hgr$todo.listen(
            'counterState',
            function(newCount){
                this.setState({ count: newCount })
            }.bind(this));
    }

    // At onload, the setup from Haskell main has not yet completed,
    // which means the haskell render callback or event triggers have not been registered into the global registry.
    // This mean 'onClick={ window.h$glazier$react$todo.cb }' directly in render() won't work.
    onIncrement(e) {
        if (window.hgr$todo.onIncrement) {
            window.hgr$todo.onIncrement(e);
        }
    }

    onDecrement(e) {
        if (window.hgr$todo.onDecrement) {
            window.hgr$todo.onDecrement(e);
        }
    }

    onIgnore(e) {
        if (window.hgr$todo.onIgnore) {
            window.hgr$todo.onIgnore(e);
        }
    }

    // Since renderHaskell is not a event callback, but called directly in render(),
    // the this pointer is valid.
    // My convention is callbacks called 'onXXX' are not bound with this by default.
    // and 'withXXX' are expected to have a valid this.
    renderHaskell() {
        if (window.hgr$todo.render) {
            return window.hgr$todo.render(this.state.count);
        }
        else {
            return null;
        }
    }

    render() {
        return (
            <div className="App">
                <div className="App-header">
                    <img src={logo} className="App-logo" alt="logo" />
                    <h2>Welcome to React</h2>
                </div>
                <div>
                    <h2 onClick={ this.onIncrement }>Increment</h2>
                </div>
                <div>
                    <h2 onClick={ this.onDecrement }>Decrement</h2>
                </div>
                <div>
                    <h2 onClick={ this.onIgnore }>Ignore</h2>
                </div>
                <p className="App-intro">
                    To get started, edit <code>src/App.js</code> and save to reload.
                </p>
                <div>
                    <h2>{ this.renderHaskell() }</h2>
                </div>
            </div>
        );
    }
}

export default App;
