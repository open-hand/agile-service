import React, { useContext, useCallback } from 'react';
import { Button, Icon, Tooltip } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import QuickCreateIssue from '@/components/QuickCreateIssue/QuickCreateIssue';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import { usePersistFn } from 'ahooks';
import openCreateSubTask from '@/components/create-sub-task';
import IssueList from '../../Component/IssueList';
import EditIssueContext from '../../stores';
import Divider from './Divider';

const SubBug = observer(({
  reloadIssue, onDeleteSubIssue, onUpdate, applyType, onCreateSubIssue,
}) => {
  const { store, disabled } = useContext(EditIssueContext);
  const {
    issueId: relateIssueId, priorityId,
  } = store.getIssue;
  const disableCreate = disabled;
  const { data: issueTypeData } = useProjectIssueTypes({ onlyEnabled: true, typeCode: 'bug', applyType }, { enabled: !disabled });

  const {
    issueId, summary, subBugVOList = [], activeSprint,
  } = store.getIssue;
  const renderIssueList = (issue, i) => (
    <IssueList
      showAssignee
      showDelete={!disabled}
      showPriority
      key={issue.issueId}
      issue={{
        ...issue,
        typeCode: issue.typeCode || 'sub_bug',
      }}
      i={i}
      onOpen={() => {
        if (reloadIssue) {
          reloadIssue(issue.issueId);
        }
      }}
      onRefresh={(subIssueId) => {
        if (reloadIssue) {
          reloadIssue(issueId);
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
        subBugVOList.map((subIssue, i) => renderIssueList(subIssue, i))
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
    store.setCreateSubBugShow(false);
    resetDefault();
    if (onCreateSubIssue) {
      onCreateSubIssue(issue, relateIssueId);
    }
    if (reloadIssue) {
      reloadIssue(issue.issueId);
    }
  };
  const handleOpenCreateIssue = usePersistFn(() => {
    openCreateSubTask({
      typeCode: 'bug',
      onCreate: handleCreateSubIssue,
      parentIssue: {
        summary,
        issueId,
      },
      defaultValues: {
        summary: store.defaultSummary,
        sprint: activeSprint ? activeSprint.sprintId : undefined,
      },
      defaultAssignee: store.defaultAssignee,
      defaultTypeId: store.defaultTypeId,
    });
  });
  return (
    <div id="bug">
      <Divider />
      <div className="c7n-title-wrapper">
        <div className="c7n-title-left">
          <span>缺陷</span>
        </div>
        {!disabled && (
          <div className="c7n-title-right" style={{ marginLeft: '14px' }}>
            <Tooltip placement="topRight" title="创建缺陷" getPopupContainer={(triggerNode) => triggerNode.parentNode}>
              <Button onClick={() => store.setCreateSubBugShow(true)} disabled={(issueTypeData || []).length === 0}>
                <Icon type="playlist_add icon" />
              </Button>
            </Tooltip>
            <Tooltip placement="topRight" title="新创建缺陷" getPopupContainer={(triggerNode) => triggerNode.parentNode}>
              <Button onClick={handleOpenCreateIssue} disabled={(issueTypeData || []).length === 0}>
                <Icon type="playlist_add icon" />
              </Button>
            </Tooltip>
          </div>
        )}
      </div>
      {renderSubIssues()}
      {!disableCreate && (
        <div style={{ marginTop: 15 }}>
          <QuickCreateIssue
            sprintId={activeSprint?.sprintId}
            defaultPriority={{ id: priorityId }}
            issueTypes={issueTypeData || []}
            relateIssueId={relateIssueId}
            buttonShow={(issueTypeData || []).length > 1}
            buttonShowText="快速创建缺陷"
            onCreate={handleCreateSubIssue}
            cantCreateEvent={() => { store.setCreateSubBugShow(true); }}
            typeIdChange={(id) => {
              store.setDefaultTypeId(id);
            }}
            summaryChange={(issueSummary) => {
              store.setDefaultSummary(issueSummary);
            }}
            assigneeChange={(assigneeId) => {
              store.setDefaultAssignee(assigneeId);
            }}
            setDefaultSprint={(value) => {
              store.setDefaultSprint(value);
            }}
          />
        </div>
      )}
    </div>
  );
});

export default SubBug;
