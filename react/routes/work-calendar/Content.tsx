import React, { useCallback, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import { map } from 'lodash';
import {
  Page, Breadcrumb, Content, Header, HeaderButtons,
} from '@choerodon/boot';
import { Select } from 'choerodon-ui/pro';
import { CustomTabs } from '@choerodon/components';
import openCreateIssue from '@/components/create-issue';
import { Issue } from '@/common/types';
import { formatIssueTime } from '@/routes/work-calendar/utils';
import { useWorkCalendarStore } from '@/routes/work-calendar/stores';
import CalendarContent from '@/routes/work-calendar/components/calendar';
import SelectProject from '@/components/select/select-project';

const { Option } = Select;

const WorkCalendar = observer(() => {
  const {
    prefixCls,
    mainStore,
    AppState,
    USER_OPTION,
    DEFAULT_USER,
  } = useWorkCalendarStore();

  const refresh = useCallback(() => {
    const calendarRef = mainStore.getCalendarRef;
    if (calendarRef.current) {
      const calendarApi = calendarRef.current.getApi();
      calendarApi?.refetchEvents();
    }
  }, [mainStore]);

  const handleUsersChange = useCallback((value) => {
    mainStore.setUsers(value);
    refresh();
  }, [mainStore, refresh]);

  const handleProjectChange = useCallback((value) => {
    mainStore.setCurrentProjectIds(value);
    refresh();
  }, [mainStore, refresh]);

  const handleChangeTab = useCallback((e, tabName, tabKey: string) => {
    const calendarRef = mainStore.getCalendarRef;
    if (calendarRef.current) {
      const calendarApi = calendarRef.current.getApi();
      calendarApi?.changeView(tabKey);
    }
  }, [mainStore]);

  const openCreate = useCallback(() => {
    const calendarRef = mainStore.getCalendarRef;
    openCreateIssue({
      // projectId: '223894445333270528',
      showSelectProject: true,
      defaultAssignee: AppState.userInfo,
      onCreate: (issue: Issue) => {
        const calenderApi = calendarRef.current?.getApi();
        if (calenderApi && issue.estimatedStartTime && issue.estimatedEndTime) {
          calenderApi?.addEvent({
            ...issue,
            id: issue.issueId,
            title: issue.summary,
            start: formatIssueTime(issue.estimatedStartTime),
            end: formatIssueTime(issue.estimatedEndTime),
          });
        }
      },
    });
  }, [mainStore]);

  return (
    <Page className={prefixCls}>
      <Header>
        <HeaderButtons
          items={[{
            display: true,
            name: '创建工作项',
            icon: 'playlist_add',
            handler: openCreate,
          }, {
            display: true,
            name: '订阅到本地',
            icon: 'API_subscription',
          }, {
            display: true,
            element: (
              <Select
                multiple
                defaultValue={DEFAULT_USER}
                onChange={handleUsersChange}
              >
                {map(USER_OPTION, ({ name, value }) => (
                  <Option value={value} key={value}>{name}</Option>
                ))}
              </Select>
            ),
          }, {
            display: true,
            element: (
              <SelectProject
                placeholder="所属项目"
                onChange={handleProjectChange}
                multiple
              />
            ),
          }, {
            display: true,
            element: (
              <CustomTabs
                onChange={handleChangeTab}
                data={[{
                  name: '周视图',
                  value: 'timeGridWeek',
                }, {
                  name: '月视图',
                  value: 'dayGridMonth',
                }]}
                customType="default"
              />
            ),
          }]}
        />
      </Header>
      <Breadcrumb title="工作日历" />
      <Content className={`${prefixCls}-content`}>
        <div className={`${prefixCls}-content-task`}>
          左侧任务内容
        </div>
        <div className={`${prefixCls}-content-main`}>
          <CalendarContent />
        </div>
      </Content>
    </Page>
  );
});

export default WorkCalendar;
