import React, { Component } from 'react';
import { observer } from 'mobx-react';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';

@observer
class StatusCouldDragOn extends Component {
  render() {
    const { statusId, swimlaneId } = this.props;
    const cantDragOn = ScrumBoardStore.getCanDragOn.get(statusId);
    const { draggingSwimlane, draggingStart } = ScrumBoardStore.getIsDragging;
    return (
      <div className={cantDragOn && draggingStart && (swimlaneId === draggingSwimlane) ? 'statusCantDragOn' : ''} />
    );
  }
}

export default StatusCouldDragOn;
