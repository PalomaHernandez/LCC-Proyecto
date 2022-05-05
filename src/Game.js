import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

/**
 * List of colors.
 */

const colors = ["r", "v", "p", "g", "b", "y"];  // red, violet, pink, green, blue, yellow

const grillas = [1, 2, 3];

/**
 * Returns the CSS representation of the received color.
 */

export function colorToCss(color) {
  switch (color) {
    case "r": return "#cc2a2a";
    case "v": return "#b33078";
    case "p": return "#ee9696";
    case "g": return "#3f9138";
    case "b": return "#636bd3";
    case "y": return "#e7da25";
  }
  return color;
}
class Game extends React.Component {

  pengine;

  constructor(props) {
    super(props);
    this.state = {
      turns: 0,
      grid: null,
      longitud: 1,
      history: [],
      complete: false,  // true if game is complete, false otherwise
      waiting: false,
      playing: false,
      adyacentesC: null,
      numGrid: 1,
      gridSelected: false
    };
    this.handleClick = this.handleClick.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);
  }

  handlePengineCreate(numGrid) {

    if (this.state.gridSelected || this.state.playing)
      return;

    var queryS = 'init1(Grid, LAdyacentes)';

    if (numGrid === 2) {
      queryS = 'init2(Grid, LAdyacentes)';
    }
    if (numGrid === 3) {
      queryS = 'init3(Grid, LAdyacentes)';
    }

    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grid'],
          adyacentesC: response['LAdyacentes']
        });
      }
    });
  }

  handleClick(color) {
    // No action on click if game is complete or we are waiting.
    if (this.state.complete || this.state.waiting) {
      return;
    }
    if (this.state.playing === false) {
      this.setState({
        playing: true,
        gridSelected: true
      })
    }
    // Build Prolog query to apply the color flick.
    // The query will be like:
    // flick([[g,g,b,g,v,y,p,v,b,p,v,p,v,r],
    //        [r,r,p,p,g,v,v,r,r,b,g,v,p,r],
    //        [b,v,g,y,b,g,r,g,p,g,p,r,y,y],
    //        [r,p,y,y,y,p,y,g,r,g,y,v,y,p],
    //        [y,p,y,v,y,g,g,v,r,b,v,y,r,g],
    //        [r,b,v,g,b,r,y,p,b,p,y,r,y,y],
    //        [p,g,v,y,y,r,b,r,v,r,v,y,p,y],
    //        [b,y,v,g,r,v,r,g,b,y,b,y,p,g],
    //        [r,b,b,v,g,v,p,y,r,v,r,y,p,g],
    //        [v,b,g,v,v,r,g,y,b,b,b,b,r,y],
    //        [v,v,b,r,p,b,g,g,p,p,b,y,v,p],
    //        [r,p,g,y,v,y,r,b,v,r,b,y,r,v],
    //        [r,b,b,v,p,y,p,r,b,g,p,y,b,r],
    //        [v,g,p,b,v,v,g,g,g,b,v,g,g,g]],r, Grid)

    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");

    const aux = JSON.stringify(this.state.adyacentesC).replaceAll('"', "");
    const queryS = `flick(${gridS}, ${color} , ${aux} , Grid, FAdyacentesC)`;


    //const queryS = "flick(" + gridS + "," + color + ",[[0,0]],Grid)";
    console.log(queryS);

    this.setState({
      waiting: true
    });

    this.pengine.query(queryS, (success, response) => {
      if (success) {
        console.log("no fallo consulta");
        this.setState({
          grid: response['Grid'],
          adyacentesC: response['FAdyacentesC'],
          turns: this.state.turns + 1,
          waiting: false
        });
        (this.state.history).push(color);
      } else {
        console.log("fallo consulta");
        // Prolog query will fail when the clicked color coincides with that in the top left cell.
        this.setState({
          waiting: false
        });
      }
      this.setState({
        longitud: this.state.adyacentesC.length
      });
      
      const cantFilas= this.state.grid.length;
      const cantCol= this.state.grid[0].length;
      
      if (this.state.longitud === cantFilas * cantCol) {
        this.setState({
          complete: true
        })
      }
    });
  }

  render() {
    if (this.state.grid === null) {
      return null;
    }
    return (
      <div className="game">
        <div className="leftPanel">
          <div className="buttonsPanel">
            {colors.map(color =>
              <button
                className="colorBtn"
                style={{ backgroundColor: colorToCss(color) }}
                onClick={() => this.handleClick(color)}
                key={color}
              />)}
          </div>
          <div className="turnsPanel">
            <div className="turnsLab">Turnos</div>
            <div className="turnsNum">{this.state.turns}</div>
          </div>
          <div className="longPanel">
            <div className="longLab">Celdas capturadas</div>
            <div className="longNum">{this.state.longitud}</div>
          </div>
          {this.state.gridSelected === false &&
          <div className="menuPanel">
            <div className='menuGrilla'> Cambiar grilla </div>
            {grillas.map(grilla =>
              <button
                className="menu"
                onClick={() => this.handlePengineCreate(grilla)}
              > {grilla} </button>)}
            <button className='menu'
              onClick={() => 
                this.setState({
                  gridSelected: true
                })}
            >
              Seleccionar grilla
            </button>
          </div>
          }
        </div>
        <Board
          grid={this.state.grid}
          onOriginSelected={this.state.playing? undefined :
            origin => {
              this.setState({
                playing: true,
                adyacentesC: [origin],
                gridSelected: true
              })
            }
          }
          origin={this.state.adyacentesC ? this.state.adyacentesC[0] : undefined}
        />
        <div className="rightPanel">
          <div className="historialPanel">
            <div className="historialLab">Historial de jugadas</div>
          </div>
          <div className="cellsPanel">
            {(this.state.history).map(color =>
              <button
                className="cells"
                style={{ backgroundColor: colorToCss(color) }}
                key={color}
              />)}
          </div>
        </div>
        {this.state.complete &&
          <div className={"won"}>
            <span class="wonText">
              JUEGO COMPLETADO
            </span>
          </div>
        }
      </div>
    );
  }
}

export default Game;