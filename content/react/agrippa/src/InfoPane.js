import React from 'react';
import {SenatorCard} from './Senate';

function Phase(props) {
    return <div className="info-phase-name">{props.phaseName}</div>;
}

function NavItem(props) {
    return (
        <div className="navbar-nav">
            <span className="nav-item nav-link active" href="#">
                <i className={props.classes}></i>
                {props.value}
            </span>
        </div>
    );
}

function InfoPane(props) {
    return (
        <nav className="navbar navbar-expand-lg navbar-dark bg-dark">
            <img src="./images/senator.png" width="30" height="30" alt=""></img>
            <span>Agrippa</span>
            <div className="collapse navbar-collapse" id="navbarNavAltMarkup">
                <NavItem
                   classes="fas fa-coins gold"
                   value={props.republic.treasury}
                   >
                </NavItem>
                <NavItem
                   classes="fas fa-angry"
                   value={props.republic.unrest}
                   >
                </NavItem>
                <NavItem
                   classes="fas fa-shoe-prints"
                   value={props.republic.legions.total}
                   >
                </NavItem>
                <NavItem
                   classes="fas fa-ship"
                   value={props.republic.fleets.total}
                   >
                </NavItem>
            </div>
                <button
                    type="button"
                    className="btn btn-success"
                    onClick={() => props.onContinue()}
                >
                    Continue
                </button>
       </nav>
    )
}

export default InfoPane;
