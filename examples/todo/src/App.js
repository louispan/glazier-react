import React from 'react';
// import {registry, mkCombinedElements} from '../build/todo.min';
import {registry, mkCombinedElements} from '../build/todo';

// Inheriting from Component means every call to this.setState will result in a render
// Inheriting from PureComponet means a shallow comparison will be made
class App extends React.PureComponent {

    constructor(props) {
        super(props);

        // This component is stateful so that React knows to re-render when the Haskell state has changed.
        this.state = {};

        // Use the registry to be notified when a re-render is required.
        registry.listen(
            'forceRender',
            function(newSeqNum){
                // console.log("forceRender", this.state);
                this.setState({ seqNum: newSeqNum });
            }.bind(this));
    }

    componentDidUpdate() {
        // console.log("renderUpdated", this.state);
        registry.shout('renderUpdated', this.state['seqNum']);
    }

    render() {
        // console.log("render", this.state);
        return mkCombinedElements(registry.shout('renderHaskell', this.state['seqNum']));
    }
}

export default App;
