import React from 'react';
import Popup from "reactjs-popup";

export class SenatorRow extends React.Component {

    render() {
        const senator = this.props.senator;
        const className = senator.factionLeader ? "senator-row faction-leader" : "senator-row";
        const states = (senator.priorConsul ? " pc" : "")
        + (senator.office ? " " + senator.office : "");

        return (
            <tr className={className}>
            <td>                                    
                {senator.id}
            </td>
            <Popup
                trigger={
                    <td>
                    {senator.family}
                    </td>
                }
                key={senator.id}
                closeOnDocumentClick
            >
               <SenatorCard senator={senator}></SenatorCard>
            </Popup>  
            <td>
                {senator.military}
            </td>
            <td>
                {senator.oratory}
            </td>
            <td>
                {senator.loyalty}
            </td>
            <td>
                {senator.influence}
            </td>
            <td>
                {senator.popularity}
            </td>
            <td>
                {senator.treasury}
            </td>
            <td>
                {states}
            </td>
        </tr>

        );
    }
}

function PriorConsul(props) {
    if (props.senator.priorConsul) {
        return (
            <span> Prior <br/> <i className="fas fa-crown"></i> <br/> Consul </span>
        );
    } else {
        return (<span/>);
    }
}

export function SenatorCard(props) {
    const senator = props.senator;
    return (
        <table className="agrippa-senator-card">
            <tbody>
                <tr>
                    <td rowSpan={3}>
                        <PriorConsul senator={senator}></PriorConsul>
                    </td>
                    <td rowSpan={4} colSpan={2}>
                        <img src="./images/senator.png" className="agrippa-bust"></img>
                    </td>
                    <td className="agrippa-card-large">
                        {senator.military}
                    </td>
                    <td>
                        #{senator.id}
                    </td>
                    <td className="agrippa-card-large">
                        {senator.oratory}
                    </td>
                </tr>
                <tr>
                    <td className="agrippa-card-attr">MILITARY</td>
                    <td/>
                    <td className="agrippa-card-attr">ORATORY</td>
                </tr>
                <tr>
                    <td colSpan={3} className="agrippa-card-family">
                        {senator.family}
                    </td>
                </tr>
                <tr>
                    <td/>
                    <td/>
                    <td><i className="fas fa-coins"></i></td>
                    <td/>
                    <td/>
                </tr>
                <tr>
                    <td className="agrippa-card-attr">POP</td>
                    <td/>
                    <td className="agrippa-card-attr">Knights</td>
                    <td className="agrippa-card-attr">LOYALTY</td>
                    <td/>
                    <td className="agrippa-card-attr">INFLUENCE</td>
                </tr>
                <tr>
                    <td className="agrippa-card-large">{senator.popularity}</td>
                    <td/>
                    <td className="agrippa-card-large">{senator.knights}</td>
                    <td className="agrippa-card-large">{senator.loyalty}</td>
                    <td/>
                    <td className="agrippa-card-large">{senator.influence}</td>
                </tr>
            </tbody>
        </table>
    );
}
