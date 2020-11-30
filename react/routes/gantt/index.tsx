import React, { useState, useEffect, useCallback } from 'react';
import { Button, Tooltip } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { find } from 'lodash';
import dayjs from 'dayjs';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import GanttComponent from '@/components/gantt';
import { ganttApi, issueApi, workCalendarApi } from '@/api';
import UserHead from '@/components/UserHead';
import { Gantt } from '@/components/gantt/types';
import TypeTag from '@/components/TypeTag';
import Loading from '@/components/Loading';
import SelectSprint from '@/components/select/select-sprint';
import FlatSelect from '@/components/flat-select';
import useFullScreen from '@/common/useFullScreen';
import STATUS from '@/constants/STATUS';
import { Issue } from '@/common/types';
import Search from './components/search';
import './index.less';

const { Option } = FlatSelect;
const tableColumns = [{
  width: 214,
  name: 'summary',
  label: '名称',
  // @ts-ignore
  render: (record) => (
    !record.group ? (
      <span>
        <TypeTag data={record.issueTypeVO} />
        {record.summary}
      </span>
    ) : record.summary
  ),
},
{
  width: 100,
  name: 'assignee',
  label: '经办人',
  // @ts-ignore
  render: (record) => <UserHead user={record.assignee} />,
},
{
  width: 100,
  name: 'estimatedStartTime',
  label: '预计开始时间',
  // @ts-ignore
  render: (record) => record.estimatedStartTime && <Tooltip title={record.estimatedStartTime}><span>{dayjs(record.estimatedStartTime).format('YYYY-MM-DD')}</span></Tooltip>,
},
{
  width: 100,
  name: 'estimatedEndTime',
  label: '预计结束时间',
  // @ts-ignore
  render: (record) => record.estimatedEndTime && <Tooltip title={record.estimatedEndTime}><span>{dayjs(record.estimatedEndTime).format('YYYY-MM-DD')}</span></Tooltip>,
}];
const GanttPage: React.FC = () => {
  const [data, setData] = useState<any[]>([]);
  const [type, setType] = useState<string>('task');
  const [sprintId, setSprintId] = useState<string|null>(null);
  const [columns, setColumns] = useState<Gantt.Column[]>([]);
  const [workCalendar, setWorkCalendar] = useState<any>();
  const [loading, setLoading] = useState(false);
  const [isFullScreen, toggleFullScreen] = useFullScreen(() => document.body, () => { }, 'c7n-gantt-fullScreen');
  useEffect(() => {
    (async () => {
      setLoading(true);
      const year = dayjs().year();
      const [workCalendarRes, res] = await Promise.all([
        workCalendarApi.getWorkSetting(year),
        type === 'task' ? ganttApi.loadByTask() : ganttApi.loadByUser(),
      ]);
      // setColumns(headers.map((h: any) => ({
      //   width: 100,
      //   name: h.fieldCode,
      //   label: h.name,
      // })));
      setWorkCalendar(workCalendarRes);
      setColumns(tableColumns);
      setData(res);
      setLoading(false);
    })();
  }, [type]);
  const handleUpdate = useCallback(async (issue: Gantt.Item, startDate: string, endDate: string) => {
    try {
      await issueApi.update({
        issueId: issue.issueId as number,
        objectVersionNumber: issue.objectVersionNumber as number,
        estimatedStartTime: startDate,
        estimatedEndTime: endDate,
      });
      // eslint-disable-next-line no-param-reassign
      issue.objectVersionNumber += 1;
      return true;
    } catch (error) {
      return false;
    }
  }, []);
  const handleSprintChange = useCallback(() => {

  }, []);
  const afterSprintLoad = useCallback((sprints) => {
    if (!sprintId) {
      const currentSprint = find(sprints, { statusCode: 'started' });
      if (currentSprint) {
        setSprintId(currentSprint.sprintId);
      } else {
        setSprintId(sprints[0]?.sprintId);
      }
    }
  }, [sprintId]);
  const isRestDay = useCallback((date:string) => {
    if (!workCalendar) {
      return false;
    }
    const day = dayjs(date).weekday();
    const { saturdayWork, sundayWork, timeZoneWorkCalendarDTOS } = workCalendar;
    const unWorkDays = timeZoneWorkCalendarDTOS.map((w:any) => w.workDay);
    if (!saturdayWork && day === 6) {
      return true;
    }
    if (!sundayWork && day === 0) {
      return true;
    }
    if (unWorkDays.includes(dayjs(date).format('YYYY-MM-DD'))) {
      return true;
    }
    return false;
  }, [workCalendar]);
  const getBarColor = useCallback((issue:Issue) => {
    const statusType = issue.statusVO.type;
    const color = STATUS[statusType];
    if (color) {
      return {
        borderColor: color,
        backgroundColor: color,
      };
    }
    return {
      borderColor: '',
      backgroundColor: '',
    };
  }, []);
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
        />
        <FlatSelect value={type} onChange={setType} clearButton={false}>
          <Option value="task">
            按任务查看
          </Option>
          <Option value="assignee">
            按经办人查看
          </Option>
        </FlatSelect>
        <Button
          icon="playlist_add"
        >
          创建问题
        </Button>
        <Button
          // @ts-ignore
          onClick={() => { toggleFullScreen(); }}
          icon={isFullScreen ? 'fullscreen_exit' : 'zoom_out_map'}
        >
          {isFullScreen ? '退出全屏' : '全屏'}
        </Button>
      </Header>
      <Breadcrumb />
      <Content style={{
        borderTop: '1px solid rgb(216, 216, 216)',
        display: 'flex',
        flexDirection: 'column',
      }}
      >
        <Search />
        <Loading loading={loading} />
        {columns.length > 0 && workCalendar && (
          <GanttComponent
            data={data}
            columns={columns}
            onUpdate={handleUpdate}
            startDateKey="estimatedStartTime"
            endDateKey="estimatedEndTime"
            isRestDay={isRestDay}
            // @ts-ignore
            getBarColor={getBarColor}
          />
        )}
      </Content>
    </Page>
  );
};
export default observer(GanttPage);
