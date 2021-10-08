/* eslint-disable no-param-reassign */
import React, {
  useState, useEffect, useCallback, useMemo, useRef,
} from 'react';
// eslint-disable-next-line camelcase
import { unstable_batchedUpdates } from 'react-dom';
import { Tooltip, Icon, Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import {
  find, findIndex, merge, omit, pick, remove, set, some,
} from 'lodash';
import produce from 'immer';
import dayjs from 'dayjs';
import weekday from 'dayjs/plugin/weekday';
import classNames from 'classnames';
import {
  usePersistFn, useDebounceFn, useUpdateEffect,
} from 'ahooks';
import moment from 'moment';
import {
  Page, Header, Content, Breadcrumb, HeaderButtons,
} from '@choerodon/boot';
import GanttComponent, { GanttProps, Gantt, GanttRef } from '@choerodon/gantt';
import '@choerodon/gantt/dist/gantt.cjs.production.min.css';
import { FlatSelect } from '@choerodon/components';
import { ganttApi, issueApi, workCalendarApi } from '@/api';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import TypeTag from '@/components/TypeTag';
import Loading from '@/components/Loading';
import SelectSprint from '@/components/select/select-sprint';
import useFullScreen from '@/common/useFullScreen';
import { ILocalField } from '@/components/issue-search/store';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { useIssueSearchStore } from '@/components/issue-search';
import FilterManage from '@/components/FilterManage';
import { IIssueType, Issue, User } from '@/common/types';
import isHoliday from '@/utils/holiday';
import { list2tree } from '@/utils/tree';
import { transformFilter } from '@/routes/Issue/stores/utils';
import openCreateIssue from '@/components/create-issue';
import Search from './components/search';
import GanttBar from './components/gantt-bar';
import GanttGroupBar from './components/gantt-group-bar';
import GanttGroupBarSprint from './components/gantt-group-bar-sprint';
import IssueDetail from './components/issue-detail';
import Context from './context';
import GanttStore from './store';
import GanttOperation from './components/gantt-operation';
import GanttSortLabel, { useGanttSortLabel } from './components/gantt-sort-label';
import './index.less';
import StatusLinkageWSHandle from '@/components/StatusLinkageWSHandle';
import { openCustomColumnManageModal } from '@/components/table-cache/column-manage/Modal';
import QuickCreateIssue from '@/components/QuickCreateIssue';
import useIsInProgram from '@/hooks/useIsInProgram';
import QuickCreateSubIssue from '@/components/QuickCreateSubIssue';
import { useUpdateColumnMutation } from '@/hooks/data/useTableColumns';
import TableCache, { TableCacheRenderProps } from '@/components/table-cache';
import { getTableColumns as getIssueTableColumns } from '@/components/issue-table/columns';
import getListLayoutColumns from '../Issue/components/issue-table/utils/getListLayoutColumns';
import useIssueTableFields from '@/hooks/data/useIssueTableFields';

dayjs.extend(weekday);
const typeOptions = [{
  value: 'task',
  label: '按任务查看',
}, {
  value: 'assignee',
  label: '按经办人查看',
}, {
  value: 'sprint',
  label: '按冲刺查看',
}, {
  value: 'epic',
  label: '按史诗查看',
}] as const;
const typeValues = typeOptions.map((t) => t.value);
type TypeValue = (typeof typeValues)[number];
const isCanQuickCreateIssue = (record: Gantt.Record<any>) => (!record.group && !record.parentId) && ['story', 'bug', 'task'].includes(record.issueTypeVO?.typeCode);
const ganttList2Tree = (data: any[]) => list2tree(data, { valueField: 'issueId', parentField: 'parentId' });
const groupByTask = (data: any[]) => ganttList2Tree(data);
const groupByUser = (data: any[]) => {
  const map = new Map<string, any[]>();
  const noAssigneeData: any[] = [];
  data.forEach((issue) => {
    if (issue.assignee) {
      if (map.has(issue.assignee.name)) {
        map.get(issue.assignee.name)?.push(issue);
      } else {
        map.set(issue.assignee.name, [issue]);
      }
    } else {
      noAssigneeData.push(issue);
    }
  });
  if (noAssigneeData.length > 0) {
    map.set('未分配', noAssigneeData);
  }
  return [...map.entries()].map(([name, children]) => ({
    summary: name,
    group: true,
    groupType: 'assignee',
    children: ganttList2Tree(children),
  }));
};
const groupBySprint = (data: any[]) => {
  const map = new Map<string, { sprint: any, children: any[] }>();
  const noSprintData: any[] = [];
  data.forEach((issue) => {
    if (issue.sprint) {
      if (map.has(issue.sprint.sprintName)) {
        map.get(issue.sprint.sprintName)?.children.push(issue);
      } else {
        map.set(issue.sprint.sprintName, { sprint: issue.sprint, children: [issue] });
      }
    } else {
      noSprintData.push(issue);
    }
  });
  if (noSprintData.length > 0) {
    map.set('无冲刺', { sprint: {}, children: noSprintData });
  }
  return [...map.entries()].map(([name, { sprint, children }]) => ({
    summary: name,
    group: true,
    groupType: 'sprint',
    groupWidthSelf: true,
    estimatedStartTime: sprint.startDate,
    estimatedEndTime: sprint.endDate,
    children: ganttList2Tree(children),
  }));
};

const groupByFeature = (epicChildrenData: any) => {
  const map = new Map<string, { feature: any, children: any[] }>();
  const noFeatureData: any[] = [];
  epicChildrenData.forEach((issue: any) => {
    if (issue.feature) {
      if (map.has(issue.feature.featureName)) {
        map.get(issue.feature.featureName)?.children.push(issue);
      } else {
        map.set(issue.feature.featureName, {
          feature: issue.feature,
          children: [issue],
        });
      }
    } else {
      noFeatureData.push(issue);
    }
  });
  if (noFeatureData.length > 0) {
    map.set('未分配', { feature: {}, children: noFeatureData });
  }

  return [...map.entries()].map(([name, { feature, children }]) => ({
    group: name === '未分配',
    groupType: 'feature',
    summary: name,
    ...feature,
    children: ganttList2Tree(children),
  }));
};

const groupByEpic = (data: any, isInProgram: boolean) => {
  const map = new Map<string, { epic: any, children: any[] }>();
  const noEpicData: any[] = [];
  data.forEach((issue: any) => {
    if (issue.epic) {
      if (map.has(issue.epic.epicName)) {
        map.get(issue.epic.epicName)?.children.push(issue);
      } else {
        map.set(issue.epic.epicName, {
          epic: issue.epic,
          children: [issue],
        });
      }
    } else {
      noEpicData.push(issue);
    }
  });
  if (noEpicData.length > 0) {
    map.set('未分配', { epic: {}, children: noEpicData });
  }

  return [...map.entries()].map(([name, { epic, children }]) => ({
    group: name === '未分配',
    groupType: 'epic',
    summary: name,
    ...epic,
    children: isInProgram ? groupByFeature(children) : ganttList2Tree(children),
  }));
};
const renderTooltip = (user: User) => {
  const {
    loginName, realName, email, ldap,
  } = user || {};
  return ldap ? `${realName}(${loginName})` : `${realName}(${email})`;
};
const { Option } = FlatSelect;

const getTableColumns = ({ onSortChange }: any, openCreateSubIssue: (parentIssue: Issue) => void, onCreateAfter: (createId: number, createSuccessData?: { subIssue: Issue, parentIssueId: string }, flagFailed?: boolean) => void) => {
  const tableColumns: GanttProps<Issue>['columns'] = [{
    flex: 2,
    minWidth: 300,
    // width: 300,
    lock: 'left',
    name: 'summary',
    label: '名称',
    render: (record) => {
      if (record.create) {
        const parentIssue: Issue = record.parent;
        const onCreate = (issue: Issue) => onCreateAfter(record.createId, { subIssue: issue, parentIssueId: parentIssue.issueId });
        return (
          <span role="none" onClick={(e) => e.stopPropagation()} className="c7n-gantt-content-body-create">
            <QuickCreateSubIssue
              mountCreate
              typeCode={['sub_task', 'bug']}
              priorityId={parentIssue.priorityVO?.id}
              parentIssueId={parentIssue.issueId}
              sprintId={(parentIssue as any).sprint?.sprintId!}
              cantCreateEvent={() => {
                onCreateAfter(record.createId, undefined, true);
                // 这里延迟打开
                setTimeout(() => {
                  openCreateIssue({
                    onCreate,
                  });
                }, 110);
              }}
              onCreate={onCreate}
              defaultAssignee={undefined}
              onAwayClick={(createFn) => {
                createFn().then((res: boolean) => {
                  !res && onCreateAfter(record.createId, undefined, true);
                });
              }}
            />
          </span>
        );
      }
      const isCanCreateIssue = isCanQuickCreateIssue(record);
      return !record.group ? (
        // eslint-disable-next-line no-underscore-dangle
        <span style={{ cursor: 'pointer', color: 'var(--table-click-color)' }} className={classNames('c7n-gantt-content-body-summary')}>
          <TypeTag iconSize={22} data={record.issueTypeVO} style={{ marginRight: 5 }} />
          <Tooltip title={record.summary}>
            <span style={{ verticalAlign: 'middle', flex: 1 }} className="c7n-gantt-content-body-summary-text">{record.summary}</span>
          </Tooltip>
          {isCanCreateIssue && (
            <Icon
              type="add"
              className="c7n-gantt-content-body-parent_create"
              onClick={(e) => {
                e.stopPropagation();
                openCreateSubIssue(record as any);
              }}
            />
          )}
        </span>
      ) : (
        <Tooltip title={record.summary}>
          <span style={{ color: 'var(--table-click-color)' }}>{record.summary}</span>
        </Tooltip>
      );
    },
  },
  {
    width: 85,
    minWidth: 85,
    name: 'assignee',
    label: (
      <GanttSortLabel dataKey="assigneeId" onChange={onSortChange}>
        经办人
      </GanttSortLabel>) as any,
    render: (record) => (
      <Tooltip title={renderTooltip(record.assignee)}>
        <span>{record.assignee?.realName}</span>
      </Tooltip>
    ),
  },
  {
    width: 100,
    minWidth: 100,
    name: 'estimatedStartTime',
    label: (
      <GanttSortLabel dataKey="estimatedStartTime" onChange={onSortChange}>
        预计开始
      </GanttSortLabel>) as any,
    render: (record) => record.estimatedStartTime && <Tooltip title={record.estimatedStartTime}><span>{dayjs(record.estimatedStartTime).format('YYYY-MM-DD')}</span></Tooltip>,
  },
  {
    width: 100,
    minWidth: 100,
    name: 'estimatedEndTime',
    label: '预计结束',
    render: (record) => record.estimatedEndTime && <Tooltip title={record.estimatedEndTime}><span>{dayjs(record.estimatedEndTime).format('YYYY-MM-DD')}</span></Tooltip>,
  },
    // {
    //   // flex: 1,
    //   width: 100,
    //   minWidth: 100,
    //   name: 'estimatedEndTime1',
    //   label: '预计结束',
    // },
    // {
    //   // flex: 1,
    //   width: 100,
    //   minWidth: 100,
    //   name: 'estimatedEndTime2',
    //   label: '预计结束',
    // },
    // {
    //   // flex: 1,
    //   width: 100,
    //   minWidth: 100,
    //   name: 'estimatedEndTime3',
    //   label: '预计结束',
    // },
  ];

  return tableColumns;
};
const GanttPage: React.FC<TableCacheRenderProps> = ({ cached }) => {
  const { isInProgram } = useIsInProgram();
  const [data, setData] = useState<any[]>([]);
  const [isCreate, setIsCreate] = useState(false);
  const typeChangeRefreshFlag = useRef<boolean>(false);
  const [type, setType] = useState<TypeValue>(localPageCacheStore.getItem('gantt.search.type') ?? typeValues[0]);
  const [columns, setColumns] = useState<Gantt.Column[]>([]);
  const mutation = useUpdateColumnMutation('gantt');
  const { data: tableFields } = useIssueTableFields({ hiddenFieldCodes: ['epicSelfName'] });
  const listLayoutColumns = useMemo(() => getListLayoutColumns(cached?.listLayoutColumns || [], tableFields || []), [cached?.listLayoutColumns, tableFields]);

  const [{ data: sortedList }, sortLabelProps] = useGanttSortLabel();

  const [workCalendar, setWorkCalendar] = useState<any>();
  const [projectWorkCalendar, setProjectWorkCalendar] = useState<any>();
  const [filterManageVisible, setFilterManageVisible] = useState<boolean>();
  const [loading, setLoading] = useState(false);
  const quickCreateDataRef = useRef<any>({});
  const issueSearchStore = useIssueSearchStore({
    getSystemFields: () => getSystemFields().map((item) => (item.code === 'feature' || item.code === 'epic' ? { ...item, defaultShow: false } : item)).filter((item) => item.code !== 'sprint') as ILocalField[],
    transformFilter,
  });
  const store = useMemo(() => new GanttStore(), []);
  const { sprintIds } = store;
  const [isFullScreen, toggleFullScreen] = useFullScreen(() => document.body, () => { }, 'c7n-gantt-fullScreen');

  const handleQuickCreateSubIssue = usePersistFn((parentIssue: Issue) => {
    setData(produce(data, (draft) => {
      const targetIndex = findIndex(draft, (issue) => issue.parentId === parentIssue.issueId || issue.issueId === parentIssue.issueId);
      targetIndex !== -1 && draft.splice(targetIndex, 0, {
        parentId: parentIssue.issueId, parent: parentIssue, create: true, createId: targetIndex,
      });
    }));
  });

  const { run, flush } = useDebounceFn(() => {
    (async () => {
      const year = dayjs().year();
      const filter = issueSearchStore.getCustomFieldFilters();
      if (sprintIds === null) {
        return;
      }
      filter.otherArgs.sprint = sprintIds;
      setLoading(true);
      const [workCalendarRes, projectWorkCalendarRes, res] = await Promise.all([
        workCalendarApi.getWorkSetting(year),
        workCalendarApi.getYearCalendar(year),
        ganttApi.loadByTask(merge(filter, {
          searchArgs: { tree: type !== 'assignee' },
        }), type, sortedList),
      ]);
      // setColumns(headers.map((h: any) => ({
      //   width: 100,
      //   name: h.fieldCode,
      //   label: h.name,
      // })));
      typeChangeRefreshFlag.current = false;
      unstable_batchedUpdates(() => {
        setWorkCalendar(workCalendarRes);
        setProjectWorkCalendar(projectWorkCalendarRes);
        setColumns(tableWithSortedColumns);
        setData(res);
        setLoading(false);
      });
    })();
  });
  useEffect(() => {
    run();
  }, [issueSearchStore, sprintIds, run]);
  useUpdateEffect(() => {
    run();
    flush();
  }, [sortedList]);
  useUpdateEffect(() => {
    if (typeChangeRefreshFlag.current) {
      run();
      flush();
    }
  }, [type]);
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
  const handleSprintChange = useCallback((value: string[]) => {
    store.setSprintIds(value);
  }, [store]);
  const afterSprintLoad = useCallback((sprints) => {
    if (!sprintIds) {
      const cachedSprintId = localPageCacheStore.getItem('gantt.search.sprints');
      if (cachedSprintId) {
        store.setSprintIds(cachedSprintId);
      } else {
        const currentSprint = find(sprints, { statusCode: 'started' });
        if (currentSprint) {
          store.setSprintIds([currentSprint.sprintId]);
        } else {
          store.setSprintIds([sprints[0]?.sprintId || '0']);
        }
      }
    }
  }, [sprintIds, store]);
  const handleTypeChange = useCallback((newType) => {
    setType((oldType) => {
      typeChangeRefreshFlag.current = [newType, oldType].includes('assignee') || [newType, oldType].includes('epic');
      return newType;
    });
    localPageCacheStore.setItem('gantt.search.type', newType);
  }, []);

  const isRestDay = useCallback((date: string) => isHoliday({
    sprintSetting: projectWorkCalendar,
    orgWorkCalendar: workCalendar,
  }, moment(date)), [projectWorkCalendar, workCalendar]);
  const handleClickFilterManage = () => {
    setFilterManageVisible(true);
  };
  const { unit } = store;
  const onRow: GanttProps<Issue>['onRow'] = useMemo(() => ({
    onClick: (issue) => {
      store.setIssueId(issue.issueId);
      store.setProgramId(issue.programId);
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
  const renderGroupBar: GanttProps['renderBar'] = useCallback((bar, { width, height }) => {
    const { record } = bar;
    if (record.groupType === 'sprint') {
      return (
        <GanttGroupBarSprint
          bar={bar}
          width={width}
          height={height}
        />
      );
    }
    return (
      <GanttGroupBar
        bar={bar}
        width={width}
        height={height}
      />
    );
  }, []);
  const renderInvalidBar: GanttProps['renderInvalidBar'] = useCallback((element, barInfo) => (
    <Tooltip
      // @ts-ignore
      getPopupContainer={(t) => document.getElementsByClassName('gantt-chart')[0] as HTMLElement}
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
  const normalizeIssue = (issue: Issue, source: any = {}) => Object.assign(source, {
    parentId: issue.relateIssueId || issue.parentIssueId,
    estimatedEndTime: issue.estimatedEndTime,
    estimatedStartTime: issue.estimatedStartTime,
    issueTypeVO: issue.issueTypeVO,
    objectVersionNumber: issue.objectVersionNumber,
    statusVO: issue.statusVO,
    summary: issue.summary,
    actualCompletedDate: issue.actualCompletedDate,
    completed: issue.completed,
    issueId: issue.issueId,
    assignee: issue.assigneeId ? {
      name: issue.assigneeName,
      realName: issue.assigneeRealName,
    } : null,
    sprint: issue.activeSprint,
  });

  const handleCreateIssue = usePersistFn((issue: Issue, issueId?: string, parentId?: string, dontCopyEpic = false) => {
    setData(produce(data, (draft) => {
      const normalizeIssueWidthParentId = Object.assign(normalizeIssue(issue), { parentId });
      if (!issueId) {
        draft.unshift(normalizeIssueWidthParentId);
      } else {
        const target = find(draft, { issueId });
        if (target && !dontCopyEpic) {
          draft.unshift(Object.assign(normalizeIssueWidthParentId, pick(target, ['epic', 'feature'])));
        } else {
          draft.unshift(normalizeIssueWidthParentId);
        }
      }
    }));

    updateInfluenceIssues(issue);
  });

  const handleCreateSubIssue = usePersistFn((subIssue: Issue, parentIssueId: string) => {
    handleCreateIssue(subIssue, parentIssueId, parentIssueId);
  });

  const handleQuickCreateSubIssueAfter = usePersistFn((createId: number, createSuccessData?: { subIssue: Issue, parentIssueId: string }, flagFailed = false) => {
    setData(produce(data, (draft) => {
      const delCreateIndex = findIndex(draft, { createId });
      draft.splice(delCreateIndex, 1);
    }));
    if (!flagFailed && createSuccessData) {
      const { subIssue, parentIssueId } = createSuccessData;
      handleCreateSubIssue(subIssue, parentIssueId);
    }
  });
  const tableWithSortedColumns = useMemo(() => getTableColumns(sortLabelProps, handleQuickCreateSubIssue, handleQuickCreateSubIssueAfter), [handleQuickCreateSubIssue, handleQuickCreateSubIssueAfter, sortLabelProps]);

  const handleCopyIssue = usePersistFn((issue: Issue, issueId: string, isSubTask?: boolean, dontCopyEpic?: boolean) => {
    handleCreateIssue(issue, issueId, isSubTask ? issueId : undefined, dontCopyEpic);
    const subIssues = [...(issue.subIssueVOList ?? []), ...(issue.subBugVOList ?? [])];
    if (subIssues.length > 0) {
      subIssues.forEach((child) => {
        handleCreateIssue(child, issueId, issue.issueId, dontCopyEpic);
      });
    }
  });

  const handleIssueUpdate = usePersistFn((issue: Issue | null) => {
    if (issue) {
      setData(produce(data, (draft) => {
        const target = find(draft, { issueId: issue.issueId });
        if (target) {
          // 更新属性
          normalizeIssue(issue, target);
        }
      }));
      updateInfluenceIssues(issue);
    }
  });
  const updateInfluenceIssues = usePersistFn((res: { influenceIssueIds?: string[], [key: string]: any }) => {
    // @ts-ignore
    const { influenceIssueIds } = res;
    if (influenceIssueIds && influenceIssueIds.length > 0) {
      ganttApi.loadInfluenceIssues(influenceIssueIds).then((issues: any[]) => {
        updateIssues(issues);
      });
    }
  });
  const updateIssues = usePersistFn((issues: Issue[]) => {
    issues.forEach((issue) => {
      setData(produce(data, (draft) => {
        const target = find(draft, { issueId: issue.issueId });
        if (target) {
          // 更新属性
          Object.assign(target, omit(issue, 'children'));
        }
      }));
    });
  });
  const handleTransformType = usePersistFn((newIssue: Issue, oldIssue: Issue) => {
    const parentTypes = ['story', 'task'];
    const oldType = oldIssue.issueTypeVO.typeCode;
    const newType = newIssue.issueTypeVO.typeCode;
    // 缺陷转子缺陷
    if (oldType === 'bug' && newType === 'bug' && newIssue.relateIssueId && !oldIssue.relateIssueId) {
      handleIssueDelete(oldIssue);
      handleCreateIssue(newIssue);
    } else if (oldType === newType || (parentTypes.includes(oldType) && parentTypes.includes(newType))) {
      // 同类型或者父任务类型之间相互转换，当做更新
      handleIssueUpdate(newIssue);
    } else {
      // 其他的，当做删除再创建
      handleIssueDelete(oldIssue);
      handleCreateIssue(newIssue);
    }
  });
  const handleChangeParent = usePersistFn((newIssue: Issue, oldIssue: Issue) => {
    handleIssueDelete(oldIssue);
    handleCreateIssue(newIssue);
  });
  const handleLinkIssue = usePersistFn((res) => {
    updateInfluenceIssues(res);
  });
  const handleIssueDelete = usePersistFn((issue: Issue | null) => {
    if (issue) {
      setData(produce(data, (draft) => {
        remove(draft, (item) => item.issueId === issue.issueId || some(issue.subIssueVOList || [], { issueId: item.issueId }));
      }));
    }
  });

  const handleDeleteSubIssue = usePersistFn((issue: Issue, subIssueId: string) => {
    handleIssueDelete(issue);
  });

  const ganttData = useMemo(() => {
    // 需要刷新时先不进行排序，等待数据请求完再进行
    if (typeChangeRefreshFlag.current) {
      return data;
    }
    if (type === 'assignee') {
      return groupByUser(data);
    } if (type === 'sprint') {
      return groupBySprint(data);
    } if (type === 'task') {
      return groupByTask(data);
    } if (type === 'epic') {
      return groupByEpic(data, isInProgram);
    }
    return data;
  }, [data, isInProgram, type]);
  const renderEmpty = usePersistFn(() => {
    if (!sprintIds || sprintIds?.length === 0) {
      return <span>暂无数据，请选择冲刺</span>;
    }
    return <span>暂无数据</span>;
  });
  const handleChangeQuickCreateData = usePersistFn((key: string, value: any) => {
    let defaultKey = key;
    if (['summary', 'sprint'].includes(key)) {
      defaultKey = `defaultValues.${key}`;
    }
    set(quickCreateDataRef.current, defaultKey, value);
  });
  const visibleColumnCodes = useMemo(() => (listLayoutColumns.filter((c) => c.display).map((c) => c.columnCode)), [listLayoutColumns]);

  // const showColumns = useMemo(() => getTableColumns({
  //   cached?.listLayoutColumns, tableFields, onSummaryClick: () => { }, handleColumnResize: () => { },
  // }), [listLayoutColumns]);

  return (
    <Page>
      <Header>
        <SelectSprint
          flat
          placeholder="冲刺"
          value={sprintIds}
          multiple
          onChange={handleSprintChange}
          clearButton={false}
          afterLoad={afterSprintLoad}
          hasUnassign
          style={{ marginRight: 16 }}
          maxTagCount={3}
          searchable={false}
          selectAllButton={false}
        />
        <FlatSelect value={type} onChange={handleTypeChange} clearButton={false} style={{ marginRight: 8 }}>
          {typeOptions.map((o) => (
            <Option value={o.value}>
              {o.label}
            </Option>
          ))}
        </FlatSelect>
        <HeaderButtons
          showClassName={false}
          items={[
            {
              name: '创建问题',
              icon: 'playlist_add',
              display: true,
              handler: () => {
                openCreateIssue({
                  onCreate: handleCreateIssue,
                });
              },
            },
            {
              name: '个人筛选',
              icon: 'settings-o',
              display: true,

              handler: handleClickFilterManage,
            },
            {
              display: true,
              name: '列配置',
              // icon: 'view_column-o',
              handler: () => {
                // openColumnManageModal
                openCustomColumnManageModal({
                  modelProps: {
                    title: '设置列显示字段',
                  },

                  options: tableFields || [],
                  type: 'gannt',
                });
              },
              element: (
                <Button>
                  <Icon
                    type="view_column-o"
                    style={{ fontSize: 20, marginBottom: -1 }}
                  />
                  <span>列配置</span>
                </Button>),
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

            {
              display: true,
              icon: 'refresh',
              // funcType: 'flat',
              handler: run,
            },
          ]}
        />
      </Header>
      <Breadcrumb />
      <Content
        className="c7n-gantt-content"
        style={{
          borderTop: '1px solid var(--divider)',
          display: 'flex',
          paddingTop: 7,
          flexDirection: 'column',
          paddingBottom: 0,
        }}
      >
        <Context.Provider value={{ store }}>
          <div style={{ display: 'flex', flexWrap: 'wrap' }}>
            <Search issueSearchStore={issueSearchStore} loadData={run} />
            <GanttOperation />
          </div>
          <Loading loading={loading} />
          {columns.length > 0 && workCalendar && (
            <div className="c7n-gantt-content-body">
              <GanttComponent
                innerRef={store.ganttRef as React.MutableRefObject<GanttRef>}
                data={ganttData}
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
                // @ts-ignore
                renderEmpty={renderEmpty}
              />
              <div className={classNames('c7n-gantt-content-body-quick-create', { 'c7n-gantt-content-body-quick-create-open': isCreate })}>
                <QuickCreateIssue
                  onCreateChange={setIsCreate}
                  cantCreateEvent={() => {
                    openCreateIssue({
                      ...quickCreateDataRef.current,
                      onCreate: handleCreateIssue,
                    });
                  }}
                  onCreate={(res: any) => {
                    handleCreateIssue(res);
                  }}
                  typeIdChange={(id: any) => {
                    handleChangeQuickCreateData('defaultTypeId', id);
                  }}
                  summaryChange={(issueSummary: any) => {
                    handleChangeQuickCreateData('summary', issueSummary);
                  }}
                  assigneeChange={(assigneeId: string, assignee: any) => {
                    handleChangeQuickCreateData('defaultAssignee', assignee);
                  }}
                  setDefaultSprint={(value: any) => {
                    handleChangeQuickCreateData('sprint', value);
                  }}

                />
              </div>
            </div>
          )}
          <IssueDetail
            refresh={run}
            onUpdate={handleIssueUpdate}
            onDelete={handleIssueDelete}
            onDeleteSubIssue={handleDeleteSubIssue}
            onCreateSubIssue={handleCreateSubIssue}
            onCopyIssue={handleCopyIssue}
            onTransformType={handleTransformType}
            onChangeParent={handleChangeParent}
            onLinkIssue={handleLinkIssue}
          />
          <FilterManage
            visible={filterManageVisible!}
            setVisible={setFilterManageVisible}
            issueSearchStore={issueSearchStore}
          />
        </Context.Provider>
      </Content>
      <StatusLinkageWSHandle />
    </Page>
  );
};
const ObserverGanttPage = observer(GanttPage) as React.FC<TableCacheRenderProps>;
const GanntPageHOC = () => (
  <TableCache type="gantt">
    {(cacheProps) => <ObserverGanttPage {...cacheProps} />}
  </TableCache>
);
export default GanntPageHOC;
