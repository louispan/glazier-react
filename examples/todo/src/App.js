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
        this.state = { haskellRender: window.h$glazier$react$todo.haskellRender };
        // Use a global registry to be notified when setup is compoleted
        window.h$glazier$react$todo.addListener('haskellRender',
            function(){ this.setState({ haskellRender: window.h$glazier$react$todo.haskellRender })}.bind(this));
    }
    // However, at onload, the setup from Haskell main has not yet completed,
    // so using 'onClick={ window.h$glazier$react$todo.cb }' directly in render() won't work.
    // We use a wrapper function to defer references to props of window.h$glazier$react$todo.
    handleClick(e) {
        if (window.h$glazier$react$todo.cb) {
            window.h$glazier$react$todo.cb(e);
            console.log(e);
            console.log(e.target);
            var wack = window.React.createElement("div");
            console.log(wack);
        }
    }

    renderExtra() {
        if (this.state.haskellRender) {
            return this.state.haskellRender("hello");
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
                <h2 onClick={ this.handleClick }>Welcome to React2</h2>
                </div>
                <p className="App-intro">
                    To get started, edit <code>src/App.js</code> and save to reload.
                </p>
                <div>
                <h2>{ this.renderExtra() }</h2>
                </div>
            </div>
        );
    }
}

export default App;
