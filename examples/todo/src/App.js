import React, { Component } from 'react';
import logo from './logo.svg';
import './App.css';


class App extends Component {
    // for whatever reason using onClick={ window.h$glazier$todo.cb(e) }> doesn't work
    // so we need this wrapper function
    handleClick(e) {
        window.h$glazier$todo.cb(e);
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
