/* eslint-disable no-param-reassign */
import React, {
  useState, useEffect, useCallback, useMemo,
} from 'react';
// eslint-disable-next-line camelcase
import { unstable_batchedUpdates } from 'react-dom';
import { Button, Tooltip, Icon } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { find } from 'lodash';
import dayjs from 'dayjs';
import weekday from 'dayjs/plugin/weekday';
import classNames from 'classnames';
import {
  Page, Header, Content, Breadcrumb, HeaderButtons,
} from '@choerodon/boot';
import GanttComponent, { GanttProps, Gantt, GanttRef } from 'react-gantt-component';
import 'react-gantt-component/dist/react-gantt-component.cjs.production.min.css';
import { ganttApi, issueApi, workCalendarApi } from '@/api';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import TypeTag from '@/components/TypeTag';
import Loading from '@/components/Loading';
import SelectSprint from '@/components/select/select-sprint';
import { FlatSelect } from '@choerodon/components';
import useFullScreen from '@/common/useFullScreen';
import { ILocalField } from '@/components/issue-search/store';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { useIssueSearchStore } from '@/components/issue-search';
import FilterManage from '@/components/FilterManage';
import HeaderLine from '@/components/HeaderLine';
import { Issue, User } from '@/common/types';
import { transformFilter } from '@/routes/Issue/stores/utils';
import Search from './components/search';
import GanttBar from './components/gantt-bar';
import GanttGroupBar from './components/gantt-group-bar';
import IssueDetail from './components/issue-detail';
import CreateIssue from './components/create-issue';
import Context from './context';
import GanttStore from './store';
import GanttOperation from './components/gantt-operation';
import './index.less';

dayjs.extend(weekday);
const typeOptions = [{
  value: 'task',
  label: '按任务查看',
}, {
  value: 'assignee',
  label: '按经办人查看',
}] as const;
const typeValues = typeOptions.map((t) => t.value);
type TypeValue = (typeof typeValues)[number];
const renderTooltip = (user: User) => {
  const {
    loginName, realName, email, ldap,
  } = user || {};
  return ldap ? `${realName}(${loginName})` : `${realName}(${email})`;
};
const { Option } = FlatSelect;
const tableColumns: GanttProps<Issue>['columns'] = [{
  flex: 2,
  minWidth: 200,
  name: 'summary',
  label: '名称',
  render: (record) => (
    <Tooltip title={record.summary}>
      {!record.group ? (
        <span style={{ cursor: 'pointer', color: 'rgba(0,0,0,0.87)' }}>
          <TypeTag iconSize={22} data={record.issueTypeVO} style={{ marginRight: 5 }} />
          <span style={{ verticalAlign: 'middle' }}>{record.summary}</span>
        </span>
      ) : <span style={{ color: 'rgba(0,0,0,0.87)' }}>{record.summary}</span>}
    </Tooltip>
  ),
},
{
  width: 80,
  minWidth: 80,
  name: 'assignee',
  label: '经办人',
  render: (record) => (
    <Tooltip title={renderTooltip(record.assignee)}>
      <span>{record.assignee?.realName}</span>
    </Tooltip>
  ),
},
{
  flex: 1,
  minWidth: 100,
  name: 'estimatedStartTime',
  label: '预计开始',
  render: (record) => record.estimatedStartTime && <Tooltip title={record.estimatedStartTime}><span>{dayjs(record.estimatedStartTime).format('YYYY-MM-DD')}</span></Tooltip>,
},
{
  flex: 1,
  minWidth: 100,
  name: 'estimatedEndTime',
  label: '预计结束',
  render: (record) => record.estimatedEndTime && <Tooltip title={record.estimatedEndTime}><span>{dayjs(record.estimatedEndTime).format('YYYY-MM-DD')}</span></Tooltip>,
}];
const GanttPage: React.FC = () => {
  const [data, setData] = useState<any[]>([]);
  const [type, setType] = useState<TypeValue>(localPageCacheStore.getItem('gantt.search.type') ?? typeValues[0]);
  const [columns, setColumns] = useState<Gantt.Column[]>([]);
  const [workCalendar, setWorkCalendar] = useState<any>();
  const [projectWorkCalendar, setProjectWorkCalendar] = useState<any>();
  const [filterManageVisible, setFilterManageVisible] = useState<boolean>();
  const [loading, setLoading] = useState(false);
  const issueSearchStore = useIssueSearchStore({
    getSystemFields: () => getSystemFields().filter((item) => item.code !== 'sprint') as ILocalField[],
    transformFilter,
  });
  const store = useMemo(() => new GanttStore(), []);
  const { sprintId } = store;
  const [isFullScreen, toggleFullScreen] = useFullScreen(() => document.body, () => { }, 'c7n-gantt-fullScreen');
  const loadData = useCallback(() => {
    (async () => {
      const year = dayjs().year();
      const filter = issueSearchStore.getCustomFieldFilters();
      if (sprintId === null) {
        return;
      }
      filter.otherArgs.sprint = [sprintId];
      setLoading(true);
      const [workCalendarRes, projectWorkCalendarRes, res] = await Promise.all([
        workCalendarApi.getWorkSetting(year),
        workCalendarApi.getYearCalendar(year),
        type === 'task' ? ganttApi.loadByTask(filter) : ganttApi.loadByUser(filter),
      ]);
      // setColumns(headers.map((h: any) => ({
      //   width: 100,
      //   name: h.fieldCode,
      //   label: h.name,
      // })));
      unstable_batchedUpdates(() => {
        setWorkCalendar(workCalendarRes);
        setProjectWorkCalendar(projectWorkCalendarRes);
        setColumns(tableColumns);
        setData(res);
        setLoading(false);
      });
    })();
  }, [issueSearchStore, sprintId, type]);
  useEffect(() => {
    loadData();
  }, [issueSearchStore, loadData]);
  const handleUpdate = useCallback<GanttProps<Issue>['onUpdate']>(async (issue, startDate, endDate) => {
    try {
      await issueApi.update({
        issueId: issue.issueId,
        objectVersionNumber: issue.objectVersionNumber,
        estimatedStartTime: startDate,
        estimatedEndTime: endDate,
      });
      issue.objectVersionNumber += 1;
      issue.estimatedStartTime = startDate;
      issue.estimatedEndTime = endDate;
      return true;
    } catch (error) {
      return false;
    }
  }, []);
  const handleSprintChange = useCallback((value: string) => {
    store.setSprintId(value);
  }, [store]);
  const afterSprintLoad = useCallback((sprints) => {
    if (!sprintId) {
      const cachedSprintId = localPageCacheStore.getItem('gantt.search.sprint');
      if (cachedSprintId) {
        store.setSprintId(cachedSprintId);
      } else {
        const currentSprint = find(sprints, { statusCode: 'started' });
        if (currentSprint) {
          store.setSprintId(currentSprint.sprintId);
        } else {
          store.setSprintId(sprints[0]?.sprintId || '0');
        }
      }
    }
  }, [sprintId, store]);
  const handleTypeChange = useCallback((newType) => {
    setType(newType);
    localPageCacheStore.setItem('gantt.search.type', newType);
  }, []);
  const isRestDay = useCallback((date: string) => {
    if (!workCalendar) {
      return false;
    }
    const weekDay = dayjs(date).weekday();
    const day = dayjs(date).format('YYYY-MM-DD');
    const { saturdayWork, sundayWork, timeZoneWorkCalendarDTOS } = workCalendar;
    const unWorkDays = timeZoneWorkCalendarDTOS.map((w: any) => w.workDay);
    const projectSetting = find(projectWorkCalendar, { workDay: day });
    if (projectSetting) {
      return projectSetting.status === 0;
    }
    if (!saturdayWork && weekDay === 6) {
      return true;
    }
    if (!sundayWork && weekDay === 0) {
      return true;
    }
    if (unWorkDays.includes(dayjs(date).format('YYYY-MM-DD'))) {
      return true;
    }
    return false;
  }, [projectWorkCalendar, workCalendar]);
  const handleClickFilterManage = () => {
    setFilterManageVisible(true);
  };
  const { unit } = store;
  const onRow: GanttProps<Issue>['onRow'] = useMemo(() => ({
    onClick: (issue) => {
      store.setIssueId(issue.issueId);
    },
  }), [store]);
  const getExpandIcon = useCallback(({ level, collapsed, onClick }) => (
    <div
      role="none"
      onClick={onClick}
      className={classNames('c7n-gantt-expand-icon', {
        'c7n-gantt-expand-icon-expanded': !collapsed,
      })}
    >
      <Icon type="navigate_next" />
    </div>
  ), []);
  const renderBar: GanttProps['renderBar'] = useCallback((bar, { width, height }) => (
    <GanttBar
      type={type}
      bar={bar}
      width={width}
      height={height}
      onClick={onRow.onClick}
    />
  ), [onRow.onClick, type]);
  const renderGroupBar: GanttProps['renderBar'] = useCallback((bar, { width, height }) => (
    <GanttGroupBar
      bar={bar}
      width={width}
      height={height}
    />
  ), []);
  const renderInvalidBar: GanttProps['renderInvalidBar'] = useCallback((element, barInfo) => (
    <Tooltip
      hidden={barInfo.stepGesture === 'moving'}
      placement="top"
      title="点击并拖动以设置预计开始、结束时间。"
    >
      {element}
    </Tooltip>
  ), []);

  const renderBarThumb: GanttProps['renderBarThumb'] = useCallback((record, t) => (
    <div
      role="none"
      className="c7n-gantt-thumb-icon"
    >
      {t === 'left' ? <Icon type="navigate_before" /> : <Icon type="navigate_next" />}
    </div>
  ), []);
  return (
    <Page>
      <Header>
        <SelectSprint
          flat
          placeholder="冲刺"
          value={sprintId}
          onChange={handleSprintChange}
          clearButton={false}
          afterLoad={afterSprintLoad}
          hasUnassign
          style={{ marginRight: 40 }}
          searchable={false}
        />
        <FlatSelect value={type} onChange={handleTypeChange} clearButton={false}>
          {typeOptions.map((o) => (
            <Option value={o.value}>
              {o.label}
            </Option>
          ))}
        </FlatSelect>
        <HeaderLine />
        <HeaderButtons
          showClassName={false}
          items={[
            {
              name: '创建问题',
              icon: 'playlist_add',
              display: true,
              handler: () => {
                store.setCreateIssueVisible(true);
              },
            },
            {
              name: '个人筛选',
              icon: 'settings',
              display: true,
              handler: handleClickFilterManage,
            },
            {
              icon: isFullScreen ? 'fullscreen_exit' : 'zoom_out_map',
              iconOnly: true,
              display: true,
              handler: () => {
                // @ts-ignore
                toggleFullScreen();
              },
              tooltipsConfig: {
                title: isFullScreen ? '退出全屏' : '全屏',
              },
            },
          ]}
        />
      </Header>
      <Breadcrumb />
      <Content
        className="c7n-gantt-content"
        style={{
          borderTop: '1px solid rgb(216, 216, 216)',
          display: 'flex',
          paddingTop: 7,
          flexDirection: 'column',
        }}
      >
        <Context.Provider value={{ store }}>
          <div style={{ display: 'flex' }}>
            <Search issueSearchStore={issueSearchStore} loadData={loadData} />
            <GanttOperation />
          </div>
          <Loading loading={loading} />
          {columns.length > 0 && workCalendar && (
            <GanttComponent
              innerRef={store.ganttRef as React.MutableRefObject<GanttRef>}
              data={data}
              columns={columns}
              onUpdate={handleUpdate}
              startDateKey="estimatedStartTime"
              endDateKey="estimatedEndTime"
              isRestDay={isRestDay}
              showBackToday={false}
              showUnitSwitch={false}
              unit={unit}
              onRow={onRow}
              onBarClick={onRow.onClick}
              tableIndent={20}
              expandIcon={getExpandIcon}
              renderBar={renderBar}
              renderInvalidBar={renderInvalidBar}
              renderGroupBar={renderGroupBar}
              renderBarThumb={renderBarThumb}
              tableCollapseAble={false}
              scrollTop={{
                right: -4,
                bottom: 8,
              }}
              rowHeight={34}
            />
          )}
          <IssueDetail refresh={loadData} />
          <CreateIssue refresh={loadData} />
          <FilterManage
            visible={filterManageVisible!}
            setVisible={setFilterManageVisible}
            issueSearchStore={issueSearchStore}
          />
        </Context.Provider>
      </Content>
    </Page>
  );
};
export default observer(GanttPage);
