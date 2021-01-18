/* eslint-disable react/jsx-curly-brace-presence */
import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Button, Icon, Progress, Tooltip,
} from 'choerodon-ui';
import QuickCreateSubIssue from '@/components/QuickCreateSubIssue';
import CreateSubTask from '../../../CreateIssue/CreateSubTask';
import IssueList from '../../Component/IssueList';
import EditIssueContext from '../../stores';
import './SubTask.less';
import Divider from './Divider';

const SubTask = observer(({
  onDeleteSubIssue, reloadIssue, onUpdate, parentSummary,
}) => {
  const { store, disabled } = useContext(EditIssueContext);
  const {
    issueId: parentIssueId, subIssueVOList = [], priorityId, sprintId, typeCode, relateIssueId,
  } = store.getIssue;
  const { getCreateSubTaskShow: createSubTaskShow } = store;
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
      onRefresh={() => {
        if (reloadIssue) {
          reloadIssue(parentIssueId);
        }
        if (onUpdate) {
          onUpdate();
        }
        if (onDeleteSubIssue) {
          onDeleteSubIssue();
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

  const handleCreateSubIssue = () => {
    store.setCreateSubTaskShow(false);
    if (onUpdate) {
      onUpdate();
    }
    if (reloadIssue) {
      reloadIssue();
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
              <Tooltip placement="topRight" title="创建子任务" getPopupContainer={(triggerNode) => triggerNode.parentNode}>
                <Button style={{ padding: '0 6px' }} className="leftBtn" funcType="flat" onClick={() => store.setCreateSubTaskShow(true)}>
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
          />
        )}
        {
          createSubTaskShow ? (
            <CreateSubTask
              parentIssueId={parentIssueId}
              parentSummary={parentSummary}
              visible={createSubTaskShow}
              onCancel={() => store.setCreateSubTaskShow(false)}
              onOk={handleCreateSubIssue}
              store={store}
            />
          ) : null
        }
      </div>
    )
  );
});

export default SubTask;
