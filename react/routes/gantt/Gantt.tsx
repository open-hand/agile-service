/* eslint-disable no-param-reassign */
import React, {
  useState, useEffect, useCallback, useMemo, useRef,
} from 'react';
// eslint-disable-next-line camelcase
import { unstable_batchedUpdates } from 'react-dom';
import {
  Choerodon,
  Page, Header, Content, Breadcrumb, HeaderButtons,
} from '@choerodon/boot';
import {
  Tooltip, Icon, Button, CheckBox,
} from 'choerodon-ui/pro';
import { observer, useComputed } from 'mobx-react-lite';
import {
  find, findIndex, flow, merge, noop, omit, pick, remove, set, some,
} from 'lodash';
import produce from 'immer';
import dayjs from 'dayjs';
import weekday from 'dayjs/plugin/weekday';
import classNames from 'classnames';
import {
  usePersistFn, useDebounceFn, useUpdateEffect,
} from 'ahooks';
import moment from 'moment';

import GanttComponent, { Gantt, GanttProps, GanttRef } from '@choerodon/gantt';
import '@choerodon/gantt/dist/gantt.cjs.production.min.css';
import { FlatSelect } from '@choerodon/components';
import { set as mobxSet } from 'mobx';
import {
  ganttApi, IGanttConflictAssignee, issueApi, workCalendarApi,
} from '@/api';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import Loading from '@/components/Loading';
import SelectSprint from '@/components/select/select-sprint';
import useFullScreen from '@/common/useFullScreen';
import { ILocalField } from '@/components/issue-search/store';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { useIssueSearchStore } from '@/components/issue-search';
import FilterManage from '@/components/FilterManage';
import {
  Issue, User,
} from '@/common/types';
import isHoliday from '@/utils/holiday';
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
import { useGanttSortLabel } from './components/gantt-sort-label';
import './index.less';
import StatusLinkageWSHandle from '@/components/StatusLinkageWSHandle';
import { openCustomColumnManageModal } from '@/components/table-cache/column-manage/Modal';
import QuickCreateIssue from '@/components/QuickCreateIssue';
import TableCache, { TableCacheRenderProps } from '@/components/table-cache';
import useGanttColumns, { useGanttOrgColumns } from './hooks/useGanttColumns';
import {
  ganttLocalMove, getGanttMoveDataOrigin, getGanttMoveSubmitData, ganttDataGroupByType, getGanttCreatingSubIssue, ganttNormalizeIssue, ganttSaveCollapsedStatus, ganttRestoreCollapsedStatus, ganttIsCanQuickCreateIssue,
} from './utils';
import GanttDragWrapper from './components/gantt-drag-wrapper';
import useQuickCreateIssue from './hooks/useQuickCreateIssue';
import { GanttIssue, IGanttCollapsedHistory } from './types';
import { getProjectId } from '@/utils/common';
import SelectProject from '@/components/select/select-project';
import localCacheStore from '@/stores/common/LocalCacheStore';
import { IPersonalFilter } from '@/components/quick-search';

const middleDateKeys = [{ key: 'actualStartTime', maxDateKey: 'actualEndTime', ignoreCheckDateKeys: ['actualEndTime'] }, { key: 'actualEndTime', minDateKey: 'actualStartTime' }];
const { Option } = FlatSelect;
export interface IGanttPageProps extends TableCacheRenderProps {
  isInProgram: boolean
  /** 组织层禁止编辑 */
  menuType: 'project' | 'org'
  myDefaultFilter: IPersonalFilter | undefined
  projectId?: string
  projects?: any[]
  setCurrentProject?: any
}
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
const progressOptions = [{ value: 'task', label: '工作项个数' }, { value: 'workTime', label: '工时计数' }];
const typeValues = typeOptions.map((t) => t.value);
export type IGanttDimensionTypeValue = (typeof typeValues)[number];

const GanttPage: React.FC<IGanttPageProps> = (props) => {
  const {
    isInProgram, menuType, projectId, setCurrentProject, projects, myDefaultFilter,
  } = props;
  const [conflictAssignees, setConflictAssignees] = useState<Array<IGanttConflictAssignee>>([]);
  const [data, setData] = useState<any[]>([]);
  const [processType, setProcessType] = useState<'task' | 'workTime'>('task');
  const [type, setType] = useState<IGanttDimensionTypeValue>(localPageCacheStore.project(projectId).getItem('gantt.search.type') ?? typeValues[0]);
  const [rankList, setRankList] = useState<string[] | undefined>(undefined);
  const [workCalendar, setWorkCalendar] = useState<any>();
  const collapsedHistoryRef = useRef<{ [key: string]: IGanttCollapsedHistory }>({});
  const [{ origin: sortedList, data: sorted, loading: sortLoading }, onSortChange] = useGanttSortLabel({ projectId });
  const [projectWorkCalendar, setProjectWorkCalendar] = useState<any>();
  const [filterManageVisible, setFilterManageVisible] = useState<boolean>();
  const [loading, setLoading] = useState(false);
  const issueSearchStore = useIssueSearchStore({
    projectId,
    fieldConfigs: { issueTypeId: { excludeTypeCodes: ['issue_epic'] } },
    getSystemFields: () => getSystemFields().map((item) => (item.code === 'feature' || item.code === 'epic' ? { ...item, defaultShow: false } : item)).filter((item) => item.code !== 'sprint') as ILocalField[],
    transformFilter,
    defaultSearchVO: localPageCacheStore.project(projectId).getItem('agile.gantt.search') ?? (myDefaultFilter && myDefaultFilter.filterJson ? JSON.parse(myDefaultFilter.filterJson) : undefined) ?? undefined,
  });

  const store = useMemo(() => new GanttStore({ projectId }), [projectId]);
  const { sprintIds, unit } = store;
  const [isFullScreen, toggleFullScreen] = useFullScreen(() => document.body, () => { }, 'c7n-gantt-fullScreen');
  const handleQuickCreateSubIssue = usePersistFn((parentIssue: GanttIssue & { groupType?: string, firstIssue?: GanttIssue }) => {
    setData(produce(data, (draft) => {
      let targetIndex = -1;
      if (parentIssue.groupType === 'assignee') {
        targetIndex = parentIssue.firstIssue ? findIndex(draft, (issue) => issue.issueId === parentIssue.firstIssue?.issueId) : -1;
      } else if (parentIssue.groupType === 'sprint') {
        targetIndex = parentIssue.firstIssue ? findIndex(draft, (issue) => issue.issueId === parentIssue.firstIssue?.issueId) : -1;
      } else if (parentIssue.groupType === 'epic') {
        targetIndex = findIndex(draft, (issue) => issue.epicId === parentIssue.issueId || issue.issueId === parentIssue.issueId);
        // targetIndex = targetIndex !== -1 && draft.length !== (targetIndex + 1) ? targetIndex + 1 : targetIndex;
      } else if (parentIssue.groupType === 'feature') {
        targetIndex = findIndex(draft, (issue) => issue.featureId === parentIssue.issueId || issue.issueId === parentIssue.issueId);
      } else {
        targetIndex = findIndex(draft, (issue) => issue.parentId === parentIssue.issueId || issue.issueId === parentIssue.issueId);
      }
      const newIssue = getGanttCreatingSubIssue(parentIssue, targetIndex);
      if (!parentIssue.sprint) {
        set(newIssue, 'createSprintIds', store.sprintIds || []);
      }

      targetIndex !== -1 && draft.splice(targetIndex, 0, newIssue);
    }));
  });
  const handleCreateSubIssue = usePersistFn((subIssue: Issue, parentIssueId: string) => {
    handleCreateIssue(subIssue, undefined, parentIssueId);
  });

  const handleQuickCreateSubIssueAfter = usePersistFn((createId?: number, createSuccessData?: { subIssue: Issue, parentIssueId: string }, flagFailed = false) => {
    Number.isSafeInteger(createId) && setData(produce(data, (draft) => {
      const delCreateIndex = findIndex(draft, { createId });
      draft.splice(delCreateIndex, 1);
    }));
    if (!flagFailed && createSuccessData) {
      const { subIssue, parentIssueId } = createSuccessData;
      handleCreateSubIssue(subIssue, parentIssueId);
      run();
    }
  });
  const handleClickSummary = useCallback((issue: any) => {
    store.setIssue(issue);
    store.setProgramId(issue.programId && (String(issue.programId) !== String(getProjectId())) ? String(issue.programId) : null);
  }, [store]);
  const {
    columns, setColumns, visibleColumnCodes, tableWithSortedColumns, listLayoutColumns,
  } = useGanttColumns({
    ...props, onSortChange, sortedList, projectId, isInProgram, onClickSummary: handleClickSummary, onCreateSubIssue: handleQuickCreateSubIssue, onAfterCreateSubIssue: handleQuickCreateSubIssueAfter,
  });

  const searchFilter = useComputed(() => {
    const filter = issueSearchStore.getCustomFieldFilters();
    filter.otherArgs.sprint = sprintIds;
    filter.searchArgs.dimension = type;
    if (menuType === 'org') {
      filter.searchArgs.teamProjectIds = [projectId];
    }
    return merge(filter, {
      displayFields: visibleColumnCodes.map((code) => ({ code, projectId })),
      searchArgs: { tree: type !== 'assignee', dimension: type },
    });
  }, [sprintIds, visibleColumnCodes, menuType, type, projectId]);

  const { run, flush } = useDebounceFn(() => {
    (async () => {
      const year = dayjs().year();
      if (sprintIds === null || !projectId || !tableWithSortedColumns.length || sortLoading) {
        return;
      }
      setLoading(true);
      const requestArr = menuType === 'project' || true ? [
        workCalendarApi.getWorkSetting(year),
        workCalendarApi.project(projectId).getYearCalendar(year),
        ['sprint', 'assignee'].includes(type) ? ganttApi.project(projectId).loadDimensionRank(searchFilter) : { ids: [] },
        ganttApi.project(projectId).loadByTask(searchFilter, sorted),
      ] : [workCalendarApi.getWorkSetting(year), null, { ids: [] }, ganttApi.loadOrgByTask(searchFilter, 1)];
      const [workCalendarRes, projectWorkCalendarRes, rankListRes, res] = await Promise.all(requestArr);
      const conflictUsers = menuType === 'org' && type === 'assignee' ? await ganttApi.loadTimeConflict(searchFilter) : [];
      // setColumns(headers.map((h: any) => ({
      //   width: 100,
      //   name: h.fieldCode,
      //   label: h.name,
      // })));
      unstable_batchedUpdates(() => {
        setProjectWorkCalendar(projectWorkCalendarRes);
        setWorkCalendar(workCalendarRes);
        setColumns(tableWithSortedColumns);
        setRankList(rankListRes?.ids);
        setConflictAssignees(conflictUsers);
        setData(res.map((item: any) => ({ ...item, onlyShow: menuType === 'org' })));
        setLoading(false);
      });
    })();
  });

  const [{ isCreate }, quickCreateProps] = useQuickCreateIssue({
    onCreate: run,
    isCanQuickCreate: () => ganttIsCanQuickCreateIssue(store.sprintIds),
  });

  useEffect(() => {
    run();
  }, [issueSearchStore, sprintIds, run, visibleColumnCodes, searchFilter, sortLoading]);
  useUpdateEffect(() => {
    run();
    flush();
  }, [sorted, type]);

  const handleUpdate = useCallback<GanttProps<Issue>['onUpdate']>(async (issue, startDate, endDate, middleDates) => {
    try {
      const changeDateObj = middleDates.filter((item) => item.value && item.value !== '').reduce((pre, current) => ({ ...pre, [current.key]: current.value }), {});
      const res = await issueApi.update({
        ...changeDateObj,
        issueId: issue.issueId,
        objectVersionNumber: issue.objectVersionNumber,
      });
      issue.objectVersionNumber = res.objectVersionNumber;
      mobxSet(issue, changeDateObj);
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
      const cachedSprintId = localPageCacheStore.project(projectId).getItem('gantt.search.sprints');
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
    setRankList(undefined);
    setType(newType);
    localPageCacheStore.project(projectId).setItem('gantt.search.type', newType);
  }, []);

  const isRestDay = useCallback((date: string) => isHoliday({
    sprintSetting: projectWorkCalendar,
    orgWorkCalendar: workCalendar,
  }, moment(date)), [projectWorkCalendar, workCalendar]);
  const isShowBar: GanttProps['isShowBar'] = useCallback((({ record }, barType): boolean => {
    if (barType === 'task') {
      //  无的成对的时间区间则不展示bar
      return (record.actualStartTime && record.actualEndTime) || (record.estimatedStartTime && record.estimatedEndTime);
    }
    if (barType === 'create') {
      return !(record.actualStartTime || record.actualEndTime || record.estimatedStartTime || record.estimatedEndTime);
    }
    return true;
  }), []);
  const handleClickFilterManage = () => {
    setFilterManageVisible(true);
  };

  const getExpandIcon = useCallback(({
    level, index, collapsed, onClick,
  }) => (
    <div
      role="none"
      onClick={(event) => {
        onClick(event);
        collapsedHistoryRef.current[index] = { path: store.ganttRef.current?.flattenData[index].flatPath!, collapsed: !collapsed };
      }}
      className={classNames('c7n-gantt-expand-icon', {
        'c7n-gantt-expand-icon-expanded': !collapsed,
      })}
    >
      <Icon type="navigate_next" />
    </div>
  ), [store.ganttRef]);
  const renderBar: GanttProps['renderBar'] = useCallback((bar, { width, height }, dateKeyRange) => (
    <GanttBar
      type={type}
      bar={bar}
      width={width}
      height={height}
      dateKeyRange={dateKeyRange}
      onClick={noop}
    // onClick={onRow.onClick}
    />
  ), [type]);
  const renderGroupBar: GanttProps['renderGroupBar'] = useCallback((bar, { width, height }) => {
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
  const handleTooltipMouseEnter = useCallback(
    (e) => Tooltip.show(e.target, {
      title: '点击并拖动以设置预计开始、结束时间。',
      placement: 'topLeft',
    }),
    [],
  );
  const handleTooltipMouseLeave = useCallback(() => Tooltip.hide(), []);
  const renderInvalidBar: GanttProps['renderInvalidBar'] = useCallback((element, barInfo) => (
    <span onMouseEnter={handleTooltipMouseEnter} onMouseLeave={handleTooltipMouseLeave}>
      {element}
    </span>
  ), [handleTooltipMouseEnter, handleTooltipMouseLeave]);

  const renderBarThumb: GanttProps['renderBarThumb'] = useCallback((record, t) => (
    <div
      role="none"
      className="c7n-gantt-thumb-icon"
    >
      {t === 'left' ? <Icon type="navigate_before" /> : <Icon type="navigate_next" />}
    </div>
  ), []);

  const handleCreateIssue = usePersistFn((issue: Issue, issueId?: string, parentId?: string, dontCopyEpic = false) => {
    setData(produce(data, (draft) => {
      const normalizeIssueWidthParentId = Object.assign(ganttNormalizeIssue(issue), { parentId });
      if (!issueId) {
        draft.unshift(normalizeIssueWidthParentId);
      } else {
        const target = find(draft, { issueId });
        if (target && !dontCopyEpic) {
          draft.unshift(Object.assign(normalizeIssueWidthParentId, pick(target, ['epicId', 'featureId'])));
        } else {
          draft.unshift(normalizeIssueWidthParentId);
        }
      }
    }));
    updateInfluenceIssues(issue);
  });

  const handleCopyIssue = usePersistFn((issue: Issue, issueId: string, isSubTask?: boolean, dontCopyEpic?: boolean) => {
    handleCreateIssue(issue, issueId, isSubTask ? issueId : undefined, dontCopyEpic);
    const subIssues = [...(issue.subIssueVOList ?? []), ...(issue.subBugVOList ?? [])];
    if (subIssues.length > 0) {
      subIssues.forEach((child) => {
        handleCreateIssue(child, issueId, issue.issueId, dontCopyEpic);
      });
    }
  });

  const handleIssueUpdate = usePersistFn((issue: GanttIssue | Issue | null) => {
    if (type === 'epic') {
      run();
    } else if (issue) {
      setData(produce(data, (draft) => {
        const target = find(draft, { issueId: issue.issueId });
        if (target) {
          // 更新属性
          ganttNormalizeIssue(issue as Issue, target);
        }
      }));
      updateInfluenceIssues(issue);
    }
  });
  const updateIssues = usePersistFn((issues: GanttIssue[]) => {
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
  const updateInfluenceIssues = useCallback((res: { influenceIssueIds?: string[], [key: string]: any }) => {
    // @ts-ignore
    const { influenceIssueIds } = res;
    // 更新自身 及影响的issue
    const updateIssueIds = [res.issueId, ...(influenceIssueIds || [])];
    ganttApi.project(projectId).loadInfluenceIssues(type, updateIssueIds, searchFilter.displayFields).then((issues: any[]) => {
      updateIssues(issues);
    });
  }, [projectId, searchFilter.displayFields, type, updateIssues]);

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
  const handleRecordTime = usePersistFn((oldIssue: Issue) => {
    const influenceIssueIds = [...((oldIssue as any).influenceIssueIds || []), oldIssue.parentIssueId].filter(Boolean);
    updateInfluenceIssues({ ...oldIssue, influenceIssueIds });
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

  const ganttData = useMemo(() => ganttRestoreCollapsedStatus(ganttDataGroupByType({
    data, type, isInProgram, rankList, conflictAssignees, menuType,
  }), Object.values(collapsedHistoryRef.current).filter((i) => i.collapsed)), [data, type, isInProgram, rankList, conflictAssignees, menuType]);
  const renderEmpty = usePersistFn(() => {
    if (!sprintIds || sprintIds?.length === 0) {
      return <span>暂无数据，请选择冲刺</span>;
    }
    return <span>暂无数据</span>;
  });

  const renderClone = usePersistFn((record: Gantt.Record) => tableWithSortedColumns[0].render!(record) as React.ReactElement);
  const handleDragEnd = useCallback((sourceBar: Gantt.Bar, destinationBar: Gantt.Bar) => {
    const requestData = getGanttMoveSubmitData({
      sourceBar, destinationBar, flattenData: store.ganttRef.current?.flattenData || [], type,
    });
    if (!requestData) {
      return;
    }
    setLoading(true);

    // 先本地移动 再请求
    const dataOrigin = getGanttMoveDataOrigin({ type, sourceBar });
    const moveConfig = dataOrigin === 'rankList' ? {
      data: rankList || [],
      setData: setRankList,
      request: () => ganttApi.project(projectId).moveTopDimension(requestData, searchFilter),
    } : {
      data,
      setData,
      request: () => ganttApi.project(projectId).move(requestData, searchFilter),
    };
    async function request(success: boolean) {
      success && await moveConfig.request();
      setLoading(false);
    }
    const moveFn = flow(ganttLocalMove, ({ newData, success }) => {
      if (success) {
        // collapsedHistoryRef.current = ganttSaveCollapsedStatus({ flattenData: store.ganttRef.current?.flattenData || [] });
        moveConfig.setData(newData);
      }
      return success;
    }, request);
    moveFn({
      sourceBar, destinationBar, type, data: moveConfig.data,
    });
  }, [data, projectId, rankList, searchFilter, store.ganttRef, type]);
  const handleResizeWidth: GanttProps['onResizeWidth'] = usePersistFn((tableWidth) => {
    localCacheStore.unPrefix().setItem('agile.gantt.table.width', tableWidth);
  });

  useEffect(() => () => { localPageCacheStore.project(projectId).setItem('agile.gantt.search', issueSearchStore.getCustomFieldFilters()); }, []);
  return (
    <Page>
      <Header>
        {menuType === 'org' && (
          <SelectProject
            value={projectId}
            flat
            clearButton={false}
            style={{ marginRight: 16 }}
            maxTagTextLength={12}
            optionData={projects}
            onChange={(val) => {
              setCurrentProject && setCurrentProject((oldValue: string) => {
                if (oldValue === val || !val) {
                  return String(oldValue);
                }
                setLoading(true);
                store.setSprintIds(null);
                localPageCacheStore.setItem('org.gantt.projectId', val);
                return val;
              });
            }}
          />
        )}
        <SelectSprint
          key={`SelectSprint-${projectId}`}
          flat
          statusList={[]}
          projectId={projectId}
          placeholder="冲刺"
          value={sprintIds}
          multiple
          onChange={handleSprintChange}
          clearButton={false}
          afterLoad={afterSprintLoad}
          hasUnassign
          maxTagTextLength={12}
          style={{ marginRight: 16 }}
          maxTagCount={2}
          searchable={false}
          selectAllButton={false}
        />

        <FlatSelect value={type} onChange={handleTypeChange} clearButton={false} style={{ marginRight: 16, width: 120 }}>
          {typeOptions.map((o) => (
            <Option value={o.value}>
              {o.label}
            </Option>
          ))}
        </FlatSelect>
        <FlatSelect value={processType} onChange={setProcessType} clearButton={false} style={{ marginRight: 8, width: 100 }}>
          {progressOptions.map((o) => (
            <Option value={o.value}>
              {o.label}
            </Option>
          ))}
        </FlatSelect>
        <HeaderButtons
          showClassName={false}
          items={menuType === 'project' ? [
            {
              name: '创建工作项',
              icon: 'playlist_add',
              display: true,
              handler: () => {
                openCreateIssue({
                  defaultValues: { sprint: sprintIds?.length === 1 ? sprintIds.filter((item) => item !== '0')[0] : undefined },
                  onCreate: run,
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
                openCustomColumnManageModal({
                  modelProps: {
                    title: '设置列显示字段',
                  },
                  projectId,
                  value: visibleColumnCodes,
                  options: listLayoutColumns.map((item) => ({ code: item.columnCode, title: item.label })),
                  type: 'gantt',
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
              icon: 'refresh',
              display: menuType === 'project',
              // funcType: 'flat',
              handler: run,
            },
          ] : [{
            display: true,
            name: '列配置',
            // icon: 'view_column-o',
            handler: () => {
              openCustomColumnManageModal({
                modelProps: {
                  title: '设置列显示字段',
                },
                projectId,
                value: visibleColumnCodes,
                options: listLayoutColumns.map((item) => ({ code: item.columnCode, title: item.label })),
                type: 'gantt',
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
          }, {
            name: '个人筛选',
            icon: 'settings-o',
            display: true,

            handler: handleClickFilterManage,
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
        <Context.Provider value={{
          store, searchFilter, dimensionType: type, menuType, disable: menuType === 'org', projectId, processType, sortedList,
        }}
        >
          <div style={{ display: 'flex', flexWrap: 'wrap' }}>
            <Search issueSearchStore={issueSearchStore} loadData={run} />
            <GanttOperation />
          </div>
          <Loading loading={sortLoading || loading} />
          {columns.length > 0 && workCalendar && (
            <div className="c7n-gantt-content-body">
              <GanttDragWrapper renderClone={renderClone} onDragEnd={handleDragEnd}>
                <GanttComponent
                  innerRef={store.ganttRef as React.MutableRefObject<GanttRef>}
                  data={ganttData}
                  columns={columns}
                  onUpdate={handleUpdate}
                  onResizeWidth={handleResizeWidth}
                  defaultTableWidth={Number(localCacheStore.unPrefix().getItem('agile.gantt.table.width')) || undefined}
                  startDateKey="estimatedStartTime"
                  endDateKey="estimatedEndTime"
                  isRestDay={isRestDay}
                  isShowBar={isShowBar}
                  showBackToday={false}
                  showUnitSwitch={false}
                  unit={unit}
                  middleDateKeys={middleDateKeys}
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
                  barHeight={13}
                  // @ts-ignore
                  renderEmpty={renderEmpty}
                />
              </GanttDragWrapper>
              {menuType === 'project' && (
                <div className={classNames('c7n-gantt-content-body-quick-create', { 'c7n-gantt-content-body-quick-create-open': isCreate })}>
                  <QuickCreateIssue
                    {...quickCreateProps}
                    projectId={projectId}
                    sprintId={sprintIds?.length === 1 ? sprintIds.filter((i) => i !== '0')[0] : undefined}
                  />
                </div>

              )}
            </div>
          )}
          <IssueDetail
            refresh={run}
            onRecordTime={handleRecordTime}
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
            projectId={projectId}
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

export default observer(GanttPage);
