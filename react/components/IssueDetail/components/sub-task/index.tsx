import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { Progress } from 'choerodon-ui';
import { ISubIssue } from '@/common/types';
import Section from '../section';
import { useDetailContext } from '../../context';
import IssueList from '../issue-list';
import './index.less';

const SubTask = () => {
  const { store } = useDetailContext();
  const { subIssueVOList } = store.issue;
  const getPercent = () => {
    const completeList = subIssueVOList.filter((issue) => issue.completed);
    const allLength = (subIssueVOList && subIssueVOList.length) || 0;
    const completeLength = completeList.length;
    if (allLength === 0) {
      return 100;
    }
    // @ts-ignore
    return parseInt(completeLength / allLength * 100, 10);
  };
  const handleIssueClick = useCallback((issue:ISubIssue) => {
    store.select(issue.issueId);
    store.refresh();
  }, [store]);
  return (
    subIssueVOList.length === 0 ? null : (
      <Section
        title="子任务"
        border
      >
        {subIssueVOList && subIssueVOList.length
          ? (
            <div className="c7n-subTask-progress">
              <Progress percent={getPercent()} style={{ marginRight: 5 }} />
              已完成
            </div>
          ) : ''}
        <IssueList data={subIssueVOList} onClick={handleIssueClick} />
      </Section>
    )
  );
};

export default observer(SubTask);
