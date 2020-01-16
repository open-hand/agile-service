import React, { Component } from 'react';
import { Draggable } from 'react-beautiful-dnd';
import {
  CardTypeTag, IssueNum, StayDay, StatusName, Priority, Assignee, Summary,
} from './CardComponent/index';
import TypeTag from '@/components/TypeTag';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import './StatusIssue.less';

export default class Card extends Component {
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

  myOnMouseDown = (issue) => {
    ScrumBoardStore.setWhichCanNotDragOn(issue.statusId, issue.issueTypeVO);
  };


  editRef = (e) => {
    this.ref = e;
  };

  render() {
    const {
      completed, issue, index, draggableId, statusName, categoryCode, onClick, clicked, ...otherProps
    } = this.props;
    return (
      <Draggable draggableId={draggableId} index={index} key={draggableId}>
        {(provided, snapshot) => {
          const onMouseDown = () => {
            if (!provided.dragHandleProps) {
              return onMouseDown;
            }
            // creating a new onMouseDown function that calls myOnMouseDown as well as the drag handle one.
            return (event) => {
              provided.dragHandleProps.onMouseDown(event);
              this.myOnMouseDown(issue);
            };
          };
          return (
            <div
              key={issue.issueId}
              role="none"
              className="c7n-swimlaneContext-itemBodyCard"
              ref={provided.innerRef}
              {...provided.draggableProps}
              {...provided.dragHandleProps}
              onMouseDown={onMouseDown}
              onClick={e => this.handleClick(e)}
            >
              <div
                className="c7n-scrumboard-issue"
                role="none"
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
                        {/* <CardTypeTag issueTypeVO={issue.issueTypeVO} /> */}
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
              {provided.placeholder}
            </div>
          );
        } }
      </Draggable>
    );
  }
}
