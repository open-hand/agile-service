import React, { useContext, useCallback } from 'react';
import { Button, Icon, Tooltip } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import QuickCreateIssue from '@/components/QuickCreateIssue/QuickCreateIssue';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import IssueList from '../../Component/IssueList';
import EditIssueContext from '../../stores';
import Divider from './Divider';

const SubBug = observer(({
  reloadIssue, onDeleteSubIssue, onUpdate, applyType, onCreateSubIssue, onOpenCreateSubBug,
}) => {
  const { store, disabled, projectId } = useContext(EditIssueContext);
  const {
    issueId: relateIssueId, priorityId,
  } = store.getIssue;
  const disableCreate = disabled;
  const { data: issueTypeData } = useProjectIssueTypes({
    onlyEnabled: true, typeCode: 'bug', applyType, projectId,
  }, { enabled: !disabled });

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
    resetDefault();
    if (onCreateSubIssue) {
      onCreateSubIssue(issue, relateIssueId);
    }
    if (reloadIssue) {
      reloadIssue(issue.issueId);
    }
  };

  return (
    <div id="bug">
      <Divider />
      <div className="c7n-title-wrapper">
        <div className="c7n-title-left">
          <span>缺陷</span>
        </div>
        {!disabled && (
          <div className="c7n-title-right" style={{ marginLeft: '14px' }}>
            <Tooltip placement="topRight" title="新创建缺陷">
              <Button onClick={onOpenCreateSubBug} disabled={(issueTypeData || []).length === 0}>
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
            projectId={projectId}
            defaultPriority={{ id: priorityId }}
            issueTypes={issueTypeData || []}
            relateIssueId={relateIssueId}
            buttonShow={(issueTypeData || []).length > 1}
            buttonShowText="快速创建缺陷"
            onCreate={handleCreateSubIssue}
            cantCreateEvent={() => { onOpenCreateSubBug(); }}
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
        </div>
      )}
    </div>
  );
});

export default SubBug;
