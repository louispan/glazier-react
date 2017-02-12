const React = window.React;
const Component = window.React.Component;

import logo from './logo.svg';
import './App.css';

class App extends Component {

    // NB. h$glazier$react$todo is a global variable from ghcjs all.js
    // global variable loaded via <script> tags in the index.html are accessible from `window`.
    constructor(props) {
        super(props);
        // This component is stateful as it renders using a Haskell callback, which is setup later
        this.state = { };
        // Use a global registry to be notified when setup is compoleted
        window.h$glazier$react$todo.addListener(
            'counterState',
            function(newCount){
                this.setState({ count: newCount })
            }.bind(this));
    }
    // However, at onload, the setup from Haskell main has not yet completed,
    // so using 'onClick={ window.h$glazier$react$todo.cb }' directly in render() won't work.
    // We use a wrapper function to defer references to props of window.h$glazier$react$todo.
    onIncrement(e) {
        if (window.h$glazier$react$todo.onIncrement) {
            window.h$glazier$react$todo.onIncrement(e);
        }
    }

    onDecrement(e) {
        if (window.h$glazier$react$todo.onDecrement) {
            window.h$glazier$react$todo.onDecrement(e);
        }
    }

    onIgnore(e) {
        if (window.h$glazier$react$todo.onIgnore) {
            window.h$glazier$react$todo.onIgnore(e);
        }
    }

    renderHaskell() {
        if (window.h$glazier$react$todo.render) {
            return window.h$glazier$react$todo.render(this.state.count);
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
                    <h2 onClick={ this.handleClick }>Welcome to React</h2>
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
