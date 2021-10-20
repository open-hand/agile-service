import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import moment from 'moment';
import TypeTag from '@/components/TypeTag';
import Star from '@/components/tag/star';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import { calcDays } from '@/utils/Date';
import {
  IssueNum, StayDay, StatusName, Priority, Assignee, Summary, Delay,
} from './CardComponent/index';
import './Card.less';

function getStyle({ draggableStyle, virtualStyle, isDragging }) {
  const combined = {
    ...virtualStyle,
    ...draggableStyle,
  };

  const grid = 8;
  const height = isDragging ? combined.height : combined.height - grid;
  const result = {
    ...combined,
    left: isDragging ? combined.left : combined.left + grid,
    width: isDragging
      ? draggableStyle.width
      : `calc(${combined.width} - ${grid * 2}px)`,
    marginBottom: grid,
  };
  // eslint-disable-next-line no-restricted-globals
  if (!isNaN(height)) {
    result.height = height;
  }
  return result;
}
// @observer
class Card extends Component {
  constructor() {
    super();
    this.ref = React.createRef();
  }

  componentDidMount() {
    this.scrollIntoView();
  }

  componentDidUpdate() {
    this.scrollIntoView();
  }

  handleClick = (e) => {
    e.stopPropagation();
    const { issue } = this.props;
    ScrumBoardStore.setClickedIssue(issue.issueId);
  };

  myOnMouseDown = () => {
    const { issue } = this.props;
    // 存储issue, 用于后续拖拽onBeforeCapture方法使用
    ScrumBoardStore.setClickIssueItem(issue);
    ScrumBoardStore.setWhichCanNotDragOn(issue.statusId, issue.issueTypeVO);
  };

  scrollIntoView() {
    const { selected } = this.props;
    if (selected && this.ref && this.ref.current.scrollIntoViewIfNeeded && ScrumBoardStore.getSwimLaneCode !== 'participant') {
      this.ref.current.scrollIntoViewIfNeeded();
    }
  }

  render() {
    const {
      completed, issue, statusName, categoryCode, selected, keyId, ...otherProps
    } = this.props;
    let delayDays = 0;
    const { estimatedEndTime } = issue;
    if (estimatedEndTime) {
      delayDays = moment().diff(moment(estimatedEndTime), 'days', true);
    }
    return (
      <div
        className={classNames('c7n-scrumboard-issue', {
          'c7n-scrumboard-issue-delay': !completed && delayDays > 0,
          'c7n-scrumboard-issue-soonDelay': !completed && (delayDays < 0 && delayDays >= -1),
        })}
        role="none"
        onMouseDown={this.myOnMouseDown}
        onClick={(e) => this.handleClick(e)}
        ref={this.ref}
        {...otherProps}
        key={`${issue.issueNum}-${keyId}`}
        style={{
          background: selected ? '#edeff6' : 'white',
        }}
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
                {issue.starBeacon && <Star active={issue.starBeacon} style={{ margin: '0 5px' }} />}
                {
                  !completed && (
                    delayDays > 0 || (delayDays >= -1 && delayDays < 0)
                  ) && (
                  <Delay day={Math.ceil(delayDays)} />
                  )
                }
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
  completed,
  issue,
  isDragging,
  index,
  statusName,
  categoryCode,
  onClick,
  clicked,
  style,
  provided,
  keyId,
  ...otherProps
}) {
  const selected = ScrumBoardStore.clickIssueMap.get(issue.issueId);
  return (
    <div
      key={`${issue.issueId}-${keyId}`}
      role="none"
      ref={provided.innerRef}
      {...provided.draggableProps}
      {...provided.dragHandleProps}
      style={{
        ...getStyle({
          draggableStyle: provided.draggableProps.style,
          virtualStyle: style,
          isDragging,
        }),
        cursor: 'all-scroll',
      }}
    >
      <Card
        completed={completed}
        issue={issue}
        statusName={statusName}
        categoryCode={categoryCode}
        selected={selected}
        keyId={keyId}
        key={`${issue.issueId}-${keyId}`}
      />
    </div>
  );
}

export default observer(IssueItem);
