import React from 'react';
import Square from './Square';

class Board extends React.Component {
    render() {
        return (
            <div className= {this.props.onOriginSelected ? "clickableBoard" : "board"}>
                {this.props.grid.map((row, i) =>
                    row.map((cell, j) =>
                        <Square
                            value={cell}
                            key={i + "." + j}
                            onClick ={() => this.props.onOriginSelected && this.props.onOriginSelected([i,j])}
                            originCell={!!this.props.origin && this.props.origin[0] === i && this.props.origin[1] === j}
                        />
                    )
                )}
            </div>
        );
    }
}

export default Board;