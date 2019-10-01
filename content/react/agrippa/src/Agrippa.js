import React from 'react';
import InfoPane from './InfoPane';

import './Agrippa.css';
import {SenatorRow} from './Senate';
import {PhaseView} from './PhaseView';

class FactionInfo extends React.Component {

    render() {
        return (
            <div className="col">
                <div className="faction-header container" style={{backgroundColor: this.props.faction.color}}>
                    <div className="row">
                        <div className="col align-self-start agrippa-faction-name">
                            {this.props.faction.name}
                        </div>
                        <div className="col-md-auto align-self-end">
                        <i className="fas fa-poll"></i> <span>{this.props.faction.votes}</span>
                        </div>
                        <div className="col-md-auto  align-self-end">
                        <i className="fas fa-coins"></i> <span>{this.props.faction.treasury}</span>
                        </div>
                        <div className="col-md-auto  align-self-end">
                        <i className="fas fa-project-diagram"></i><span>{this.props.faction.influence}</span>
                        </div>
                    </div>
                </div>

            <table className="table-sm">
                    <thead>
                        <tr>
                            <th>ID</th>
                            <th>Name</th>
                            <th>M</th>
                            <th>O</th>
                            <th>L</th>
                            <th>I</th>
                            <th>P</th>
                            <th>T</th>
                            <th></th>
                        </tr>
                    </thead>
                    <tbody>
                        {this.props.faction.senators.map(senator => (
                            <SenatorRow senator={senator}></SenatorRow>
                            )
                        )
                        }
                    </tbody>
                </table>
            </div>
        )
    }
}

class Board extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            isLoaded: false,
            factions: null,
        }
        this.handleContinue = this.handleContinue.bind(this);
    }

    componentDidMount() {
        fetch("http://localhost:8080/game/state?id=" + this.props.gameId)
        .then(response => response.json())
        .then(
            result => {
                this.setState({
                    isLoaded: true,
                    factions: result.factions,
                    republic: result.republic,
                })
            }
        )
    }

    handleContinue() {
        fetch("http://localhost:8080/game/continue?id=" + this.props.gameId, 
                { method: 'POST'
                })
        .then(response => response.json())
        .then(
            result => {
                this.setState({
                    isLoaded: true,
                    factions: result.factions,
                    republic: result.republic,
                })
            }
        )
    }

    render() {
        const { isLoaded, factions, republic } = this.state;
        if (!isLoaded) {
            return <div>Loading ...</div>;
        } else {
            return (
                <div>
                    <InfoPane republic={republic} onContinue={this.handleContinue} />
                    <div className="container-fluid">
                    <div className="row">
                        {factions.map(faction => (
                            <FactionInfo faction={faction} key={faction.name} />
                        ))}
                        </div>
                    <div className="row">
                        <div className="col">
                                <img src="./images/map2.png" className="img-fluid" />
                        </div>
                        <div className="col">
                            <PhaseView republic={republic} />
                        </div>
                    </div>   
                </div>
            </div>
            )
       }
    }
}

class Agrippa extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            haveGame : false,
            gameId: null,
        }
    }

    componentDidMount() {
        fetch("http://localhost:8080/new-game")
        .then(response => response.json())
        .then(
            (result) => {
                this.setState({
                    haveGame: true,
                    gameId: result.gameId,
                });
            },
            (error) => {
                this.setState({
                    haveGame: false,
                    error
                })
            }
        );
    }

    render() {
        const { haveGame, gameId, error } = this.state;
        if (error) {
            return <div>Error: {error.message}</div>;
        } else if (!haveGame) {
            return <div>No game</div>;
        } else {
            return (
                <Board 
                  gameId={gameId}
                >
                </Board>
        );
        }

    }
}

export default Agrippa;