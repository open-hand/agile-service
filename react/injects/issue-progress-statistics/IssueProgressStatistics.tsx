import React, { useEffect } from 'react';
import {
  OverviewWrap, SprintEmptyPage, useProjectOverviewStore,
} from '@choerodon/master';
import IssueProgressStatisticsCards from './components/progress-cards';
import IssueProgressStatisticsTable from './components/issue-table';
import styles from './IssueProgressStatistics.less';

const IssueProgressStatistics:React.FC = () => {
  const a = React.useMemo(() => [], []);

  return (
    <OverviewWrap style={{ height: '100%', width: '100%' }}>
      <OverviewWrap.Header
        title="工作项进度统计"
      />
      <OverviewWrap.Content className={styles.content}>
        <IssueProgressStatisticsCards />
        <IssueProgressStatisticsTable />
      </OverviewWrap.Content>
    </OverviewWrap>
  );
};
export default IssueProgressStatistics;
