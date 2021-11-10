import React from 'react';
import { withRouter } from 'react-router-dom';
import {
  PageWrap, PageTab,
} from '@choerodon/boot';
import { useTabActiveKey } from '@choerodon/components';
import styles from './index.less';

const WorkingHoursLog = withRouter(React.lazy(() => (import('./working-hours-log'))));
const WorkingHoursCalendar = withRouter(React.lazy(() => (import('./working-hours-calendar'))));
const WorkingHours = () => {
  const [, setActiveKey] = useTabActiveKey('calendar');
  return (
    <PageWrap
      noHeader={[]}
      onChange={setActiveKey}
      className={styles.workingHoursTab}
    >
      <PageTab title="工时日历" route="/agile/working-hours/calendar" tabKey="calendar" component={WorkingHoursCalendar} />
      <PageTab title="工时日志" route="/agile/working-hours/log" tabKey="log" component={WorkingHoursLog} />
    </PageWrap>
  );
};
export default WorkingHours;
