import React from 'react';
import { colorToCss } from './Game';

class Square extends React.Component {
    render() {
        return (
            <div style={{ backgroundColor: colorToCss(this.props.value) }} 
            onClick={this.props.onClick}
            />
        );
    }
}

export default Square;