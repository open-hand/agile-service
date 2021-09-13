/* eslint-disable react/jsx-curly-brace-presence */
import React, { useContext, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Progress,
} from 'choerodon-ui';
import {
  Button, Icon, Tooltip,
} from 'choerodon-ui/pro';
import QuickCreateSubIssue from '@/components/QuickCreateSubIssue';
import IssueList from '../../Component/IssueList';
import EditIssueContext from '../../stores';
import './SubTask.less';
import Divider from './Divider';

const SubTask = observer(({
  onDeleteSubIssue, reloadIssue, onUpdate, parentSummary, onCreateSubIssue, onOpenCreateSubTask,
}) => {
  const { store, disabled } = useContext(EditIssueContext);
  const {
    issueId: parentIssueId, subIssueVOList = [], priorityId, sprintId, typeCode, relateIssueId, activeSprint,
  } = store.getIssue;
  const disableCreate = disabled || (typeCode === 'bug' && relateIssueId);
  const renderIssueList = (issue, i) => (
    <IssueList
      showAssignee
      showDelete={!disabled}
      showPriority
      key={issue.issueId}
      issue={{
        ...issue,
        typeCode: issue.typeCode || 'sub_task',
      }}
      i={i}
      onOpen={() => {
        if (reloadIssue) {
          reloadIssue(issue.issueId);
        }
      }}
      onRefresh={(subIssueId) => {
        if (reloadIssue) {
          reloadIssue(parentIssueId);
        }
        if (onUpdate) {
          onUpdate();
        }
        if (onDeleteSubIssue) {
          onDeleteSubIssue(issue, subIssueId);
        }
      }}
    />
  );

  const renderSubIssues = () => (
    <div className="c7n-tasks">
      {
        subIssueVOList.map((subIssue, i) => renderIssueList(subIssue, i))
      }
    </div>
  );

  const resetDefault = useCallback(() => {
    store.setDefaultSummary(undefined);
    store.setDefaultTypeId(undefined);
    store.setDefaultSprint(undefined);
    store.setDefaultAssignee(undefined);
  }, [store]);

  const handleCreateSubIssue = (issue) => {
    resetDefault();
    if (onCreateSubIssue) {
      onCreateSubIssue(issue, parentIssueId);
    }
    if (reloadIssue) {
      reloadIssue(issue.issueId);
    }
  };

  const getPercent = () => {
    const completeList = subIssueVOList.filter((issue) => issue.completed);
    const allLength = (subIssueVOList && subIssueVOList.length) || 0;
    const completeLength = completeList.length;
    if (allLength === 0) {
      return 100;
    }
    return parseInt(completeLength / allLength * 100, 10);
  };

  return (
    disableCreate && subIssueVOList.length === 0 ? null : (
      <div id="sub_task">
        <Divider />
        <div className="c7n-title-wrapper">
          <div className="c7n-title-left">
            <span>子任务</span>
          </div>
          {!disableCreate && (
            <div className="c7n-title-right" style={{ marginLeft: '14px' }}>
              <Tooltip placement="topRight" title="创建子任务" >
                <Button onClick={onOpenCreateSubTask}>
                  <Icon type="playlist_add icon" />
                </Button>
              </Tooltip>
            </div>
          )}
        </div>
        {subIssueVOList && subIssueVOList.length
          ? (
            <div className="c7n-subTask-progress">
              <Progress percent={getPercent()} style={{ marginRight: 5 }} />
              已完成
            </div>
          ) : ''}
        {renderSubIssues()}
        {!disableCreate && (
          <QuickCreateSubIssue
            priorityId={priorityId}
            sprintId={sprintId}
            parentIssueId={parentIssueId}
            onCreate={handleCreateSubIssue}
            cantCreateEvent={() => { onOpenCreateSubTask(); }}
            typeIdChange={(id) => {
              store.setDefaultTypeId(id);
            }}
            summaryChange={(issueSummary) => {
              store.setDefaultSummary(issueSummary);
            }}
            assigneeChange={(assigneeId, assignee) => {
              store.setDefaultAssignee(assignee);
            }}
            setDefaultSprint={(value) => {
              store.setDefaultSprint(value);
            }}
          />
        )}
      </div>
    )
  );
});

export default SubTask;
