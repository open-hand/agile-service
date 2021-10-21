import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { map } from 'lodash';
import {
  Page, Breadcrumb, Content, Header, HeaderButtons,
} from '@choerodon/boot';
import { Select } from 'choerodon-ui/pro';
import { CustomTabs } from '@choerodon/components';
import openCreateIssue from '@/components/create-issue';
import { Issue } from '@/common/types';
import { useWorkCalendarStore } from '@/routes/work-calendar/stores';
import CalendarContent from '@/routes/work-calendar/components/calendar';
import IssueList from '@/routes/work-calendar/components/issue-list';
import openSubscribeModal from '@/routes/work-calendar/components/subscribe';
import SelectProject from '@/components/select/select-project';
import DetailContainer, { useDetail } from '@/components/detail-container';
import { CreateProps, ViewTypeCode } from '@/routes/work-calendar/types';

const { Option } = Select;

const WorkCalendar = observer(() => {
  const {
    prefixCls,
    mainStore,
    AppState,
    USER_OPTION,
    DEFAULT_USER,
  } = useWorkCalendarStore();

  const [issueDetailProps] = useDetail();

  const refreshCalendar = useCallback(() => {
    const calendarRef = mainStore.getCalendarRef;
    if (calendarRef.current) {
      const calendarApi = calendarRef.current.getApi();
      calendarApi?.refetchEvents();
    }
  }, [mainStore]);

  const refresh = useCallback(() => {
    refreshCalendar();
    mainStore.loadIssueList();
  }, [mainStore, refreshCalendar]);

  const handleUsersChange = useCallback((value) => {
    mainStore.setUsers(value);
    refresh();
  }, [mainStore, refresh]);

  const handleProjectChange = useCallback((value) => {
    mainStore.setCurrentProjectIds(value);
    refresh();
  }, [mainStore, refresh]);

  const handleChangeTab = useCallback((e, tabName, tabKey: ViewTypeCode) => {
    const calendarRef = mainStore.getCalendarRef;
    if (calendarRef.current) {
      mainStore.setCurrentViewType(tabKey);
      const calendarApi = calendarRef.current.getApi();
      calendarApi?.changeView(tabKey);
    }
  }, [mainStore]);

  const handleCreateIssue = useCallback((data?: CreateProps) => {
    const calendarRef = mainStore.getCalendarRef;
    const { defaultValues, clearSelect = false } = data || {};
    openCreateIssue({
      showSelectProject: true,
      defaultAssignee: AppState.userInfo,
      extendRequiredCodes: ['estimatedStartTime', 'estimatedEndTime', 'assignee'],
      defaultValues,
      onCreate: (issue: Issue) => {
        const calenderApi = calendarRef.current?.getApi();
        if (calenderApi && clearSelect) {
          calenderApi.unselect();
        }
        if (issue.estimatedStartTime && issue.estimatedEndTime) {
          refresh();
        }
      },
    });
  }, [mainStore]);

  const openEditIssue = useCallback(({ event }) => {
    issueDetailProps?.open({
      path: 'issue',
      props: {
        issueId: event?.id,
        projectId: event?.extendedProps?.projectId,
        showProjectInfo: true,
      },
    });
  }, [issueDetailProps]);

  return (
    <Page className={prefixCls}>
      <Header>
        <HeaderButtons
          items={[{
            display: true,
            name: '创建工作项',
            icon: 'playlist_add',
            handler: () => handleCreateIssue(),
          }, {
            display: true,
            name: '订阅到本地',
            icon: 'API_subscription',
            handler: openSubscribeModal,
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
                style={{ maxWidth: 600 }}
                category="N_AGILE"
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
          <IssueList refresh={refreshCalendar} openEditIssue={openEditIssue} />
        </div>
        <div className={`${prefixCls}-content-main`}>
          <CalendarContent openEditIssue={openEditIssue} handleCreateIssue={handleCreateIssue} />
        </div>
        <DetailContainer {...issueDetailProps} />
      </Content>
    </Page>
  );
});

export default WorkCalendar;
