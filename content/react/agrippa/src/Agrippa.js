import React from 'react';
import './Agrippa.css';

class FactionInfo extends React.Component {

    render() {
        return (
            <div>
                <span class="">{this.props.name}</span>
            </div>
        )
    }
}

class Phase extends React.Component {
    render() {
        return (
            <div class="info-phase-name">{this.props.phaseName}</div>
        );
    }
}

class Board extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            isLoaded: false,
            factions: null,
        }
    }

    componentDidMount() {
        fetch("http://localhost:8080/game/faction-names?id=" + this.props.gameId)
        .then(response => response.json())
        .then(
            result => {
                this.setState({
                    isLoaded: true,
                    factions: result,
                    currentPhase: {
                        localName: "Mortality Phase"
                    },
                })
            }
        )
    }

    render() {
        const { isLoaded, factions } = this.state;
        if (!isLoaded) {
            return <div>Loading ...</div>;
        } else {
            return (
             <div>
                <div class="left-pane">
                    <Phase phaseName={this.state.currentPhase.localName}>

                    </Phase>
                    <div class="info-treasury">
                        Treasury
                    </div>
                    <div class="info-unrest">
                        Unrest
                    </div>
                    <div class="info-legions">
                        Legions
                    </div>
                    <div class="info-fleets">
                        Fleets
                    </div>
                    <div class="info-wars">
                        Active Wars
                    </div>
                    <div class="info-wars">
                        Inactive Wars
                    </div>
                    <div class="info-deck">
                        Deck
                    </div>
                    <div class="info-hrao">
                        HRAO
                    </div>
                </div>
                <div class="agrippa-dashboard">
                    <div class="agrippa-header">
                        {this.state.factions.map(item => (
                            <FactionInfo name={item}>

                            </FactionInfo>
                        ))}
                    </div>
                    <div class="agrippa-main">

                    </div>
                    <div class="agrippa-footer">
<div class="agrippa-log"></div>
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
                <Board gameId={gameId}>
                </Board>
        );
        }

    }
}

export default Agrippa;