/* eslint-disable no-restricted-globals */
import React, { Fragment } from 'react';
import { observer } from 'mobx-react-lite';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import QuickCreateIssue from '@/components/QuickCreateIssue';
import IssueList from './IssueList';
import SprintHeader from './SprintHeader';

function Sprint({ data }) {
  const { sprintId, expand } = data;
  const issueList = BacklogStore.getIssueListBySprintId(sprintId);
  return (
    <div style={{ width: '100%' }}>
      <SprintHeader data={data} />
      {expand && (
        <Fragment>
          <IssueList data={issueList} sprintId={sprintId} />
          <div style={{ padding: '10px 0px 10px 33px', borderBottom: '0.01rem solid rgba(0, 0, 0, 0.12)' }}>
            <QuickCreateIssue
              epicId={!isNaN(BacklogStore.getChosenEpic) ? BacklogStore.getChosenEpic : undefined}
              versionIssueRelVOList={!isNaN(BacklogStore.getChosenVersion) ? [
                {
                  versionId: BacklogStore.getChosenVersion,
                },
              ] : undefined}
              sprintId={sprintId}
              onCreate={(res) => { BacklogStore.handleCreateIssue(res, String(sprintId)); }}
            />
          </div>          
        </Fragment>
      )}
    </div>
  );
}

export default observer(Sprint);
