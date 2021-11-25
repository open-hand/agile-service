import React from 'react';
import { withRouter } from 'react-router-dom';
import {
  PageWrap, PageTab,
} from '@choerodon/boot';
import { useTabActiveKey } from '@choerodon/components';
import styles from './index.less';
import useFormatMessage from '@/hooks/useFormatMessage';

const WorkingHoursLog = withRouter(React.lazy(() => (import('./working-hours-log'))));
const WorkingHoursIssue = withRouter(React.lazy(() => (import('./working-hours-issue'))));
const WorkingHoursCalendar = withRouter(React.lazy(() => (import('./working-hours-calendar'))));
const WorkingHours = () => {
  const formatMessage = useFormatMessage();
  const [, setActiveKey] = useTabActiveKey('calendar');
  return (
    <PageWrap
      noHeader={[]}
      onChange={setActiveKey}
      className={styles.workingHours_pageWrap}
    >
      <PageTab title="工时日历" route="/agile/working-hours/calendar" tabKey="calendar" component={WorkingHoursCalendar} />
      <PageTab title="工作项工时" route="/agile/working-hours/issue" tabKey="issue" component={WorkingHoursIssue} />
      <PageTab title={formatMessage({ id: 'agile.workHours.work.log' })} route="/agile/working-hours/log" tabKey="log" component={WorkingHoursLog} />
    </PageWrap>
  );
};
export default WorkingHours;
