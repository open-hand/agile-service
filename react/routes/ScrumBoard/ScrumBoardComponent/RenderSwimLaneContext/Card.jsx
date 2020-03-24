import React, { Component } from 'react';
import { observer } from 'mobx-react';
import TypeTag from '@/components/TypeTag';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import {
  IssueNum, StayDay, StatusName, Priority, Assignee, Summary,
} from './CardComponent/index';
import './StatusIssue.less';

function getStyle({ draggableStyle, virtualStyle, isDragging }) {
  // If you don't want any spacing between your items
  // then you could just return this.
  // I do a little bit of magic to have some nice visual space
  // between the row items
  const combined = {
    ...virtualStyle,
    ...draggableStyle,
  };

  // Being lazy: this is defined in our css file
  const grid = 8;

  // when dragging we want to use the draggable style for placement, otherwise use the virtual style
  const result = {
    ...combined,
    height: isDragging ? combined.height : combined.height - grid,
    left: isDragging ? combined.left : combined.left + grid,
    width: isDragging
      ? draggableStyle.width
      : `calc(${combined.width} - ${grid * 2}px)`,
    marginBottom: grid,
  };

  return result;
}
// @observer
class Card extends Component {
  constructor(props) {
    super(props);
    this.ref = {};
  }

  
  handleClick = (e) => {
    e.stopPropagation();
    const { issue } = this.props;
    this.ref.style.backgroundColor = '#edeff6';
    ScrumBoardStore.setClickedIssue(issue, this.ref);
  };

  myOnMouseDown = () => {
    const { issue } = this.props;
    ScrumBoardStore.setWhichCanNotDragOn(issue.statusId, issue.issueTypeVO);
  };


  editRef = (e) => {
    this.ref = e;
  };

  render() {
    const {
      completed, issue, statusName, categoryCode, ...otherProps
    } = this.props;
    return (
      <div
        className="c7n-scrumboard-issue"
        role="none"
        onMouseDown={this.myOnMouseDown}
        onClick={e => this.handleClick(e)}
        ref={this.editRef}
        {...otherProps}
        key={issue.issueNum}
      >
        <div style={{ flexGrow: 1 }}>
          <div
            className="c7n-scrumboard-issueTop"
          >
            <div
              className="c7n-scrumboard-issueTop-left"
            >
              <div
                className="c7n-scrumboard-issueTop-left-type"
              >
                <TypeTag data={issue.issueTypeVO} />            
                <IssueNum issueNum={issue.issueNum} completed={completed} />
                <StayDay stayDay={issue.stayDay} completed={completed} />
              </div>
              <div
                className="c7n-scrumboard-issueTop-left-middle"
              >
                <StatusName
                  categoryCode={categoryCode}
                  statusName={statusName}
                />
                <Priority
                  priorityVO={issue.priorityVO}
                />
              </div>
            </div>
            <Assignee
              assigneeLoginName={issue.assigneeLoginName}
              assigneeRealName={issue.assigneeRealName}
              assigneeName={issue.assigneeName}
              assigneeId={issue.assigneeId}
              imageUrl={issue.imageUrl}
            />
          </div>
          <div className="c7n-scrumboard-issueBottom">
            <Summary summary={issue.summary} />
          </div>
        </div>
      </div>
    );
  }
}
function IssueItem({ 
  completed, issue, isDragging, index, statusName, categoryCode, onClick, clicked, style, provided, ...otherProps 
}) {
  return (
    <div
      key={issue.issueId}
      role="none"
      className="c7n-swimlaneContext-itemBodyCard"
      ref={provided.innerRef}
      {...provided.draggableProps}
      {...provided.dragHandleProps}
      style={getStyle({
        draggableStyle: provided.draggableProps.style,
        virtualStyle: style,
        isDragging,
      })}
    >
      <Card completed={completed} issue={issue} statusName={statusName} categoryCode={categoryCode} />
    </div>
  );
}

export default observer(IssueItem);
