import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

/**
 * Lista de colores.
 */

const colors = ["r", "v", "p", "g", "b", "y"];  // red, violet, pink, green, blue, yellow

/**
 * Lista de tipos de grillas.
 */

const grillas = [1, 2, 3];

/**
 * Devuelve la representación CSS del color recibido.
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
      turns: 0, // cantidad de turnos realizados 
      grid: null, // grilla del juego
      longitud: 1, // longitud de la lista de adyacentesC
      history: [], // representa el historial de jugadas
      complete: false,  // verdadero si se completo el juego, falso en caso contrario
      waiting: false, // verdadero se esta esperando una respuesta, falso en caso contrario
      playing: false, // verdadero si se esta jugando una partida, falso en caso contrario
      adyacentesC: null, // lista de las celdas adyacentesC
      numGrid: 1, // numero asociado a una cierta grilla (por defecto es la numero 1)
      gridSelected: false, // verdadero si se confirmo la seleccion de una grilla, falso en caso contrario
      profundidad: 0, // cantidad maxima de movimientos que se mostraran por ayuda
      solucionOptimal: [], // secuencia de movimientos que hacen a la mejor jugada con la ayuda Optimal
      adySolucionOp: 0, // cantidad de adyacentesC luego de completar una solucion de ayuda Optimal
      solucionGreedy: [], // secuencia de movimientos que hacen a la mejor jugada con la ayuda Greedy
      adySolucionG: 0, // cantidad de adyacentesC luego de completar una solucion de ayuda Greedy
      helpO_visible: false, // verdadero si se pidio ayuda Optimal, falso en caso contrario
      helpG_visible: false // verdadero si se pidio ayuda Greedy, falso en caso contrario

    };
    this.handleClick = this.handleClick.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);
  }

  /*
  * Metodo que inicializa una grilla en base a un numero, a traves de una consulta a Prolog
  * numGrid: numero asociado a una cierta grilla 
  * */
  handlePengineCreate(numGrid) {
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
        }, () => this.adyacentesInicial());
      }
    });
  }

  /*
  * Metodo que actualiza la grilla en base a un color seleccionado
  */
  handleClick(color) {
    //Si el juego esta completo o esta esperando alguna respuesta, no realiza nada.
    if (this.state.complete || this.state.waiting) {
      return;
    }

    if (this.state.playing === false) {
      this.setState({
        playing: true,
        gridSelected: true
      })
    }
    // Ejemplo de como realizar la consulta flick a Prolog:
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

    this.setState({
      waiting: true
    });

    this.pengine.query(queryS, (success, response) => {
      //si la consulta es exitosa (es decir, el color seleccionado no coincide con el color de adyacentesC)
      if (success) {
        this.setState({
          grid: response['Grid'],
          adyacentesC: response['FAdyacentesC'],
          turns: this.state.turns + 1,
          waiting: false
        });
        (this.state.history).push(color);
      } else { //si la consulta no es exitosa (caso contrario)
        this.setState({
          waiting: false
        });
      }
      this.setState({
        longitud: this.state.adyacentesC.length
      });
      //si la longitud de la lista de adyacentesC es igual a la cantidad de celdas de la grilla 
      if (this.state.longitud === (this.state.grid.length * this.state.grid[0].length)) {
        this.setState({
          complete: true
        })
      }
    });
  }

  /*
  * Método que calcula las celdas adyacentesC* a partir de la celda origen.
  */
  adyacentesInicial() {

    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const aux = JSON.stringify(this.state.adyacentesC).replaceAll('"', "");
    const queryS = `adyCStar(${aux}, ${gridS}, FAdyacentesC)`;

    this.setState({
      waiting: true
    });

    this.pengine.query(queryS, (success, response) => {
      // si la consulta tiene éxito
      if (success) {
        this.setState({
          adyacentesC: response['FAdyacentesC'],
          longitud: response['FAdyacentesC'].length,
          waiting: false
        });
      } else { //si la consulta no es exitosa (caso contrario)
        this.setState({
          waiting: false
        });
      }
    });
  }

  // Metodo que reinicia todas las propiedades del juego e inicializa la grilla por defecto
  reiniciarJuego() {
    this.setState({
      turns: 0,
      longitud: 1,
      history: [],
      complete: false,
      waiting: false,
      playing: false,
      numGrid: 1,
      gridSelected: false,
      profundidad: 0
    })
    this.handlePengineCreate(this.state.numGrid);
  }

  helpOptimal() {
    // si no se completo el juego o se esta esperando una respuesta del servidor Prolog
    if (this.state.complete || this.state.waiting) {
      return;
    }

    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const aux = JSON.stringify(this.state.adyacentesC).replaceAll('"', "");
    const queryS = `help(${gridS},${aux},${this.state.profundidad}, Solucion, CantAdy)`;

    this.setState({
      waiting: true
    });

    this.pengine.query(queryS, (success, response) => {
      // si la consulta tiene éxito
      if (success) {
        // si no se comenzo a jugar la partida
        if (this.state.playing === false) {
          this.setState({
            playing: true,
            gridSelected: true,
          })
        }
        this.setState({
          solucionOptimal: response['Solucion'],
          adySolucionOp: response['CantAdy'],
          waiting: false,
          helpO_visible: true
        });
        console.log(this.state.solucionOptimal);
        console.log(this.state.adySolucionOp);
      } else { //si la consulta no es exitosa (caso contrario)
        console.log("fallo consulta");
        this.setState({
          waiting: false
        });
      }
      this.setState({
        longitud: this.state.adyacentesC.length
      });
    });
  }

  helpGreedy() {
    // si no se completo el juego o se esta esperando una respuesta del servidor Prolog
    if (this.state.complete || this.state.waiting) {
      return;
    }

    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const aux = JSON.stringify(this.state.adyacentesC).replaceAll('"', "");
    const queryS = `helpGreedy(${gridS},${aux},${this.state.profundidad}, Solucion, CantAdy)`;

    this.setState({
      waiting: true
    });

    this.pengine.query(queryS, (success, response) => {
      if (success) {
        // si no se comenzo a jugar la partida
        if (this.state.playing === false) {
          this.setState({
            playing: true,
            gridSelected: true,
          })
        }
        this.setState({
          solucionGreedy: response['Solucion'],
          adySolucionG: response['CantAdy'],
          waiting: false,
          helpG_visible: true
        });
        console.log(this.state.solucionGreedy);
        console.log(this.state.adySolucionG);
      } else { //si la consulta no es exitosa (caso contrario)
        console.log("fallo consulta");
        this.setState({
          waiting: false
        });
      }
      this.setState({
        longitud: this.state.adyacentesC.length
      });
    });
  }

  /*
  * Método que actualiza el valor de profundidad
  */
  handleChange(event) {
    this.setState({ profundidad: event.target.value });
  }

  render() {
    // si la grilla no esta inicializada
    if (this.state.grid === null) {
      return null;
    }
    // si la cantidad de celdas capturadas actualmente es mayor o igual a la cantidad de celdas capturadas por la ayuda Optimal
    if (this.state.longitud >= this.state.adySolucionOp) {
      this.state.helpO_visible = false;
    }
    // si la cantidad de celdas capturadas actualmente es mayor o igual a la cantidad de celdas capturadas por la ayuda Greedy
    if (this.state.longitud >= this.state.adySolucionG) {
      this.state.helpG_visible = false;
    }
    return (
      <div className="game">
        <div className="leftPanel">
          <div className="buttonsPanel" >
            {colors.map(color =>
              <button
                className="colorBtn" key={color}
                style={{ backgroundColor: colorToCss(color) }}
                onClick={() => this.handleClick(color)}
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
          <div className="helpPanel">
            <div className="strategyLab">Seleccionar Profundidad</div>
            <input className="strategyNum"
              type='number' min='1' max='20' value={this.state.profundidad}
              onChange={(e) => this.setState({ profundidad: e.target.value })} />
            <div className="btnsHelpPanel">
              <button className='help'
                onClick={() => this.helpOptimal()}
              >Ayuda optimal</button>
              <button className='help'
                onClick={() => this.helpGreedy()}
              >Ayuda greedy</button>
            </div>
          </div>
          {this.state.helpO_visible === true &&
            <div className="movPanel">
              <div className="historialLab">Movimientos de la ayuda optimal: </div>
              <div className="movCellsPanel">
                {this.state.solucionOptimal.map((color, idx) =>
                  <button key={idx}
                    className="cells"
                    style={{ backgroundColor: colorToCss(color) }}
                  />)}
              </div>
              <div className="movLab">Cantidad de celdas a capturar:</div>
              <div className="movLab">{this.state.adySolucionOp}</div>
            </div>}
          {this.state.helpG_visible === true &&
            <div className="movPanel">
              <div className="historialLab">Movimientos de la ayuda greedy: </div>
              <div className="movCellsPanel">
                {this.state.solucionGreedy.map((color, idx) =>
                  <button key={idx}
                    className="cells"
                    style={{ backgroundColor: colorToCss(color) }}
                  />)}
              </div>
              <div className="movLab">Cantidad de celdas a capturar:</div>
              <div className="movLab">{this.state.adySolucionG}</div>
            </div>}
        </div>
        <Board
          grid={this.state.grid}
          onOriginSelected={this.state.playing ? undefined :
            origin => {
              this.setState({
                playing: true,
                adyacentesC: [origin],
                gridSelected: true,
              }, () => this.adyacentesInicial())
            }
          }
          origin={this.state.adyacentesC ? this.state.adyacentesC[0] : undefined}
        />
        <div className="rightPanel">
          <div className="historialPanel">
            <div className="historialLab">Historial de jugadas</div>
            <div className="cellsPanel" >
              {(this.state.history).map((color, idx) =>
                <button key={idx}
                  className="cells"
                  style={{ backgroundColor: colorToCss(color) }}
                />)}
            </div>
          </div>
          {this.state.gridSelected === false &&
            <div className="menuPanel">
              <div className='menuGrilla'> Seleccionar grilla </div>
              {grillas.map(grilla =>
                <button
                  className="menu" key={grilla}
                  onClick={() => this.handlePengineCreate(grilla)}
                > {grilla} </button>)}
              <button className='menu'
                onClick={() =>
                  this.setState({
                    gridSelected: true
                  })}
              > Seleccionar </button>
            </div>
          }
        </div>

        {this.state.complete &&
          <div className={"won"}>
            <span className='wonText'>
              JUEGO COMPLETADO
            </span>
            <button className="reiniciarBtn"
              onClick={() => this.reiniciarJuego()}>
              Reiniciar
            </button>
          </div>
        }
      </div>
    );
  }
}

export default Game;