import React from 'react';

function Phase(props) {
    return <div class="info-phase-name">{props.phaseName}</div>;
}

function InfoPane(props) {
    return (
        <div className="col-md-auto agrippa-info-pane">
        <Phase phaseName={props.republic.currentPhase.localName}>
        </Phase>
        <div className="info-treasury">
            Treasury {props.republic.treasury}t
        </div>
        <div className="info-unrest">
            Unrest {props.republic.unrest}
        </div>
        <div className="info-legions">
            Legions {props.republic.legions.total}
        </div>
        <div className="info-fleets">
            Fleets {props.republic.fleets.total}
        </div>
        <div className="info-wars">
            Active Wars
        </div>
        <div className="info-wars">
            Inactive Wars
        </div>
        <div className="info-deck">
            Deck
        </div>
        <div className="info-hrao">
            {props.republic.hrao}
        </div>
    </div>

    )
}

export default InfoPane;
