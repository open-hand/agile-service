import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { ISubIssue } from '@/common/types';
import Section from '../section';
import { useDetailContext } from '../../context';
import IssueList from '../issue-list';

const SubBug = () => {
  const { store } = useDetailContext();
  const { subBugVOList } = store.issue;
  const handleIssueClick = useCallback((issue:ISubIssue) => {
    store.select(issue.issueId);
    store.refresh();
  }, [store]);
  return (
    subBugVOList.length === 0 ? null : (
      <Section
        title="缺陷"
        border
      >
        <IssueList data={subBugVOList} onClick={handleIssueClick} />
      </Section>
    )
  );
};

export default observer(SubBug);
