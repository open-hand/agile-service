import React, { useState } from 'react';
import { withRouter } from 'react-router-dom';
import {
  PageWrap, PageTab,
} from '@choerodon/boot';
import WorkBenchWorkingHoursProvider from './stores';
import styles from './index.less';
import './fullScreen.less';

const WorkingHoursLog = withRouter(React.lazy(() => (import('@/routes/working-hours/working-hours-log'))));
const WorkingHoursIssue = withRouter(React.lazy(() => (import('@/routes/working-hours/working-hours-issue'))));
const WorkingHoursCalendar = withRouter(React.lazy(() => (import('@/routes/working-hours/working-hours-calendar'))));
const WorkingHours = () => {
  const [, setActiveKey] = useState('calendar');
  return (
    <WorkBenchWorkingHoursProvider>
      <PageWrap
        noHeader={['calendar']}
        cache
        onChange={setActiveKey}
        className={styles.workbench_workingHours_pageWrap}
      >
        <PageTab title="工时日历" tabKey="calendar" component={WorkingHoursCalendar} alwaysShow />
        <PageTab title="工作项工时" tabKey="issue" component={WorkingHoursIssue} alwaysShow />
        <PageTab title="工时日志" tabKey="log" component={WorkingHoursLog} alwaysShow />
      </PageWrap>
    </WorkBenchWorkingHoursProvider>
  );
};
export default WorkingHours;
