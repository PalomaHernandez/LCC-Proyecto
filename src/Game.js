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
      gridSelected: false, //verdadero si se confirmo la seleccion de una grilla, falso en caso contrario
      profundidad: 0,
      solucion: [],
      adySolucion: 0
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
        });
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

adyacentesInicial(){
  
  const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
  const aux = JSON.stringify(this.state.adyacentesC).replaceAll('"', "");
  const queryS = `buscarAdyacentesC(${gridS}, ${aux} , Color, FAdyacentesC)`;

  this.setState({
    waiting: true
  });

  this.pengine.query(queryS, (success, response) => {
    
    if (success) {
      this.setState({
        adyacentesC: response['FAdyacentesC'],
        waiting: false
      });
    } else { //si la consulta no es exitosa (caso contrario)
      this.setState({
        waiting: false
      });
    }
    this.setState({
      longitud: this.state.adyacentesC.length
    });
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
      gridSelected: false
    })
    this.handlePengineCreate(this.state.numGrid);
  }

  help(){
    if (this.state.complete || this.state.waiting) {
      return;
    }

    if (this.state.playing === false) {
      this.setState({
        playing: true,
        gridSelected: true
      })
      
    }
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const aux = JSON.stringify(this.state.adyacentesC).replaceAll('"', "");
    const queryS = `help(${gridS},${aux},${this.state.profundidad}, Solucion, CantAdy)`;
    
    this.setState({
      waiting: true
    });

    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          solucion: response['Solucion'],
          adySolucion: response['CantAdy'],
          waiting: false
        });
       console.log(this.state.solucion); 
      } else { //si la consulta no es exitosa (caso contrario)
        this.setState({
          waiting: false
        });
      }
      this.setState({
        longitud: this.state.adyacentesC.length
      });
  });
}

  handleChange(event){
    this.setState({profundidad: event.target.value});
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
          <div className= "helpPanel">
            <div className= "strategyLab">Seleccionar Profundidad</div>
            <input className="strategyNum" 
              type='number' min='1' max='20' value={this.state.profundidad} 
              onChange={(e) => this.setState({profundidad: e.target.value})}/>
            <button className= 'help'
              onClick={() => this.help()}
            >Ayuda</button>
          </div>
        </div>
        <Board
          grid={this.state.grid}
          onOriginSelected={this.state.playing ? undefined :
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
            <div className="cellsPanel">
            {(this.state.history).map(color =>
              <button
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
                  className="menu"
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