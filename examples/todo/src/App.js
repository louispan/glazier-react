import React, {Component} from 'react';
import logo from './logo.svg';
import './App.css';

class App extends Component {

    // hgr$registry is a global variable from ghcjs all.js which must be loaded via  via <script> tags in the index.html
    // It is not possible to export all.js as a webpack module because Babel freezes trying to compile haskell.
    // Global variable loaded via <script> tags in the index.html are accessible from `window`.
    constructor(props) {
        super(props);

        // This component is stateful as the state is modifiable via a Haskell callback, which is setup later
        this.state = { };

        // Use a global registry to be notified when state has changed.
        window.hgr$registry.listen(
            'counterState',
            function(newCount){
                this.setState({ count: newCount })
            }.bind(this));
    }

    // It is a bit confusing to know which callbacks have a valid this.
    // My convention is callbacks called 'onXXX' are not bound with this by default.
    // and 'withXXX' are expected to have a valid this.
    // using object methods instead of lambdas inlined in render() mean the same
    // event handler is used.
    onIncrement(e) {
        window.hgr$registry.shout('onIncrement', e);
    }

    onDecrement(e) {
        window.hgr$registry.shout('onDecrement', e);
    }

    onIgnore(e) {
        window.hgr$registry.shout('onIgnore', e);
    }

    renderHaskell() {
        return window.hgr$combineElements(null, window.hgr$registry.shout('render', this.state.count));
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
