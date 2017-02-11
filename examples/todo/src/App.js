const React = window.React;
const Component = window.React.Component;

import logo from './logo.svg';
import './App.css';

class App extends Component {
    // for whatever reason using onClick={ window.h$glazier$react$todo.cb }> doesn't work
    // so we need this wrapper function
    handleClick(e) {
        // h$glazier$react$todo is a global variable from ghcjs all.js
        // global variable loaded via <script> tags in the index.html are accessible from `window`.
        window.h$glazier$react$todo.cb(e);
    }

    render() {
        return (
            <div className="App">
                <div className="App-header">
                    <img src={logo} className="App-logo" alt="logo" />
                    <h2 onClick={ this.handleClick }>Welcome to React</h2>
                </div>
                <p className="App-intro">
                    To get started, edit <code>src/App.js</code> and save to reload.
                </p>
            </div>
        );
    }
}

export default App;
