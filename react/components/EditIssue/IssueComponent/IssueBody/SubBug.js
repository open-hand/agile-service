import React, { useContext, useState } from 'react';
import { Button, Icon, Tooltip } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import QuickCreateIssue from '@/components/QuickCreateIssue/QuickCreateIssue';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import CreateSubBug from '../../../CreateIssue/CreateSubBug';
import IssueList from '../../Component/IssueList';
import EditIssueContext from '../../stores';
import Divider from './Divider';

const SubBug = observer(({
  reloadIssue, onDeleteSubIssue, onUpdate,
}) => {
  const { store, disabled } = useContext(EditIssueContext);
  const {
    issueId: relateIssueId, priorityId,
  } = store.getIssue;
  const disableCreate = disabled;
  const { data: issueTypeData } = useProjectIssueTypes({ onlyEnabled: true, typeCode: 'bug' }, { enabled: !disabled });

  const { issueId, summary, subBugVOList = [] } = store.getIssue;
  const { getCreateSubBugShow: createSubBugShow } = store;
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
      onRefresh={() => {
        if (reloadIssue) {
          reloadIssue(issueId);
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
        subBugVOList.map((subIssue, i) => renderIssueList(subIssue, i))
      }
    </div>
  );

  const handleCreateSubIssue = () => {
    store.setCreateSubBugShow(false);
    if (onUpdate) {
      onUpdate();
    }
    if (reloadIssue) {
      reloadIssue();
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
            <Tooltip placement="topRight" title="创建缺陷" getPopupContainer={(triggerNode) => triggerNode.parentNode}>
              <Button onClick={() => store.setCreateSubBugShow(true)}>
                <Icon type="playlist_add icon" />
              </Button>
            </Tooltip>
          </div>
        )}
      </div>
      {renderSubIssues()}
      {!disableCreate && (
      <QuickCreateIssue
        defaultPriority={{ id: priorityId }}
        issueTypes={issueTypeData || []}
        relateIssueId={relateIssueId}
        buttonShowText="快速创建缺陷"
        onCreate={handleCreateSubIssue}
      />
      )}
      {
        createSubBugShow ? (
          <CreateSubBug
            relateIssueId={issueId}
            parentSummary={summary}
            visible={createSubBugShow}
            onCancel={() => store.setCreateSubBugShow(false)}
            onOk={handleCreateSubIssue}
          />
        ) : null
      }
    </div>
  );
});

export default SubBug;
