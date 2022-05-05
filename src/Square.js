import React from 'react';
import "./index.css";
import { colorToCss } from './Game';

class Square extends React.Component {
     
    render() {
        return (
            <div style={{backgroundColor: colorToCss(this.props.value)}} 
            className={this.props.originCell? "originCell" : undefined}
            onClick={this.props.onClick}
            />
        );
    }
}

export default Square;