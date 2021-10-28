import React from 'react';
import { useLocation, withRouter } from 'react-router-dom';
import {
  PageWrap, PageTab,
} from '@choerodon/boot';
import { useTabActiveKey } from '@choerodon/components';

const WorkingHoursLog = withRouter(React.lazy(() => (import('./working-hours-log'))));
const WorkingHoursCalendar = withRouter(React.lazy(() => (import('./working-hours-calendar'))));
const WorkList = () => {
  const location = useLocation();
  const [, setActiveKey] = useTabActiveKey('calendar');
  return (
    <PageWrap
      noHeader={[]}
      onChange={setActiveKey}
    >
      <PageTab title="工时日历" route="/agile/working-hours/calendar" tabKey="calendar" component={WorkingHoursCalendar} />
      <PageTab title="工时日志" route="/agile/working-hours/log" tabKey="log" component={WorkingHoursLog} />
    </PageWrap>
  );
};
export default WorkList;
