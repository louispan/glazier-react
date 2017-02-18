import React, {Component} from 'react';
import logo from './logo.svg';
import './App.css';
// import {registry, mkCombinedElements} from '../build/todo.min';
import {registry, mkCombinedElements} from '../build/todo';

class App extends Component {

    constructor(props) {
        super(props);

        // This component is stateful so that React knows to re-render when the Haskell state has changed.
        this.state = { seqNum: 0};

        // Use the registry to be notified when a re-render is required.
        registry.listen(
            'forceRender',
            // This function will ensure state is different
            function(ignore){
                this.setState(function(prevState, props) {
                    return {
                        seqNum: prevState.seqNum + 1
                    };
                })
            }.bind(this));
    }

    render() {
        return mkCombinedElements(registry.shout('renderHaskell', null));
    }}

export default App;
