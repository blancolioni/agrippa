import React from 'react';

function MortalityPhase(props) {
    return (<div>Rolling dice ...</div>);
}

function PhaseChoice(props) {
    switch (props.republic.currentPhase.tag) {
        case 'mortality-phase':
            return (<MortalityPhase props={props}></MortalityPhase>);
        default:
            return (<div>{props.republic.currentPhase.tag}</div>);
    }
}

export class PhaseView extends React.Component {

    render() {
        return (
            <div className="agrippa-phase-view">
                <span className="agrippa-phase-name">{this.props.republic.currentPhase.localName}</span>
                <PhaseChoice republic={this.props.republic}></PhaseChoice>
            </div>
        )
    }
}