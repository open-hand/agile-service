import React, { memo, Fragment } from 'react';
import { observer } from 'mobx-react';
import { Tooltip } from 'choerodon-ui';
import classnames from 'classnames';
import TypeTag from '@/components/TypeTag';
import UserHead from '@/components/UserHead';
import StatusTag from '@/components/StatusTag';
import PriorityTag from '@/components/PriorityTag';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import IsInProgramStore from '@/stores/common/program/IsInProgramStore';

import './IssueItem.less';

const prefix = 'c7n-backlog-issue';
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
  const grid = 0;

  // when dragging we want to use the draggable style for placement, otherwise use the virtual style
  // const result = {
  //   ...combined,
  //   height: isDragging ? combined.height : combined.height - grid,
  //   left: isDragging ? combined.left : combined.left + grid,
  //   width: isDragging
  //     ? draggableStyle.width
  //     : `calc(${combined.width} - ${grid * 2}px)`,
  //   marginBottom: grid,
  // };

  return combined;
}
<<<<<<< HEAD
const Item = memo(({ issue, draggingNum }) => {
=======
const Item = memo(({ issue }) => {
>>>>>>> ecc78d3f838596bc18e2ffc2d149ff25e6f47240
  const { isShowFeature } = IsInProgramStore; // 由后端判断是否显示特性
  return (
    <Fragment>
      <div
        className={`${prefix}-left`}
      >
        <TypeTag
          data={issue.issueTypeVO}
        />
    
        <div className={`${prefix}-issueNum`} style={{ textDecoration: issue.statusVO && issue.statusVO.completed ? 'line-through' : 'none' }}>
          {`${issue.issueNum}`}
        </div>
        <Tooltip title={issue.summary} placement="topLeft">
          <div className={`${prefix}-summary`}>{issue.summary}</div>
        </Tooltip>
      </div>    
      <div
        className={`${prefix}-right`}
      >
    
        {issue.versionNames.length > 0 ? (
          <Tooltip title={`版本: ${issue.versionNames.join(', ')}`}>
            <span className={`${prefix}-version`}>
              {issue.versionNames.join(', ')}
            </span>
          </Tooltip>
        ) : ''}
        {!isShowFeature && issue.epicName ? (
          <Tooltip title={`史诗: ${issue.epicName}`}>
            <span 
              className={`${prefix}-epic`}
              style={{
                color: issue.color || issue.epicColor,
                border: `1px solid ${issue.color || issue.epicColor}`,
              }}
            >
              {issue.epicName}
            </span>
          </Tooltip>
        ) : ''}
        {isShowFeature && issue.featureName ? (
          <Tooltip title={`特性: ${issue.featureName}`}>
            <span     
              className={`${prefix}-feature`}   
              style={{
                color: issue.featureColor || issue.color,
                border: `1px solid ${issue.featureColor || issue.color || BacklogStore.randomFeatureColor[issue.featureName]}`,
              }}          
            >
              {issue.featureName}
            </span>
          </Tooltip>
        ) : ''}
        {issue.assigneeId && (
        <UserHead
          user={{
            id: issue.assigneeId,
            loginName: issue.assigneeLoginName,
            realName: issue.assigneeRealName,
            name: issue.assigneeName,
            avatar: issue.imageUrl,
          }}
        />
        )}
        <Tooltip title={`状态: ${issue.statusVO ? issue.statusVO.name : ''}`}>
          <div className={`${prefix}-status`}>
            <StatusTag
              data={issue.statusVO}
            />
          </div>
        </Tooltip>
        <Tooltip title={`优先级: ${issue.priorityVO ? issue.priorityVO.name : ''}`}>
          <PriorityTag priority={issue.priorityVO} />
        </Tooltip>
        <Tooltip title={`故事点: ${issue.storyPoints}`}>
          <div     
            className={classnames(`${prefix}-storyPoint`, {
              visible: issue.storyPoints && issue.issueTypeVO && issue.issueTypeVO.typeCode === 'story',
            })}
          >
            {issue.storyPoints}
          </div>
        </Tooltip>
      </div>
    </Fragment>
  );
});
  

function IssueItem({
  provided, style, issue, isDragging, sprintId,
}) {
  const selected = BacklogStore.getMultiSelected.get(issue.issueId);
  return (
    <div
      role="none"
      ref={provided.innerRef}
      {...provided.draggableProps}
      {...provided.dragHandleProps}
      style={getStyle({
        draggableStyle: provided.draggableProps.style,
        virtualStyle: style,
        isDragging,
      })}
      className={`${prefix} ${selected ? `${prefix}-selected` : ''}`}
      onClick={(e) => { BacklogStore.handleIssueClick(e, issue, String(sprintId)); }}
    >
      <Item issue={issue} />
    </div>
  );
}

export default observer(IssueItem);
