import React from 'react';

// Inheriting from Component means every call to this.setState will result in a render
// Inheriting from PureComponet means a shallow comparison will be made
class Dummy extends React.PureComponent {

    constructor(props) {
        super(props);

        // This component is stateful so that React knows to re-render when the Haskell state has changed.
        this.state = {};
    }

    componentDidUpdate() {
        if (this.props['updated'])
            this.props['updated'](this.state['seqNum']);
    }

    render() {
        if (this.props['render'])
            return this.props['render'](this.state['seqNum']);
        return null;
    }
}

export default Dummy;
