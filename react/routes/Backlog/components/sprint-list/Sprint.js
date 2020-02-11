import React, { Fragment } from 'react';
import { observer } from 'mobx-react-lite';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
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
        </Fragment>
      )}
    </div>
  );
}

export default observer(Sprint);
