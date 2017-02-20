import React from 'react';
// import {registry, mkCombinedElements} from '../build/todo.min';
import {registry, mkCombinedElements} from '../build/todo';

class App extends React.Component {

    constructor(props) {
        super(props);

        // This component is stateful so that React knows to re-render when the Haskell state has changed.
        this.state = {};

        // Use the registry to be notified when a re-render is required.
        registry.listen(
            'forceRender',
            function(newSeqNum){
                this.setState({ seqNum: newSeqNum });
            }.bind(this));
    }

    componentDidUpdate() {
        registry.shout('renderUpdated', this.state['seqNum']);
    }

    render() {
        return mkCombinedElements(registry.shout('renderHaskell', this.state['seqNum']));
    }
}

export default App;
