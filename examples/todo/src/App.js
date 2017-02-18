import React, {Component} from 'react';
import logo from './logo.svg';
import './App.css';
import {registry, mkCombinedElements} from '../build/todo.min';

class App extends Component {

    constructor(props) {
        super(props);

        // This component is stateful as the state is modifiable via a Haskell callback, which is setup later
        this.state = { };

        // Use the registry to be notified when state has changed.
        registry.listen(
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
        registry.shout('onIncrement', e);
    }

    onDecrement(e) {
        registry.shout('onDecrement', e);
    }

    onIgnore(e) {
        registry.shout('onIgnore', e);
    }

    renderHaskell() {
        return mkCombinedElements(registry.shout('render', this.state.count));
    }

    render_old() {
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
    render() {
        return mkCombinedElements(registry.shout('render', this.state.count));
    }}

export default App;
