/* eslint-disable no-param-reassign */
import React, {
  useState, useEffect, useCallback, useMemo, useRef,
} from 'react';
// eslint-disable-next-line camelcase
import { unstable_batchedUpdates } from 'react-dom';
import { observer, useComputed } from 'mobx-react-lite';
import {
  find, findIndex, flow, merge, noop, omit, pick, remove, set, some,
} from 'lodash';
import produce from 'immer';
import dayjs from 'dayjs';
import weekday from 'dayjs/plugin/weekday';
import classNames from 'classnames';
import {
  usePersistFn, useDebounceFn, useUpdateEffect, useMount,
} from 'ahooks';
import moment from 'moment';

import GanttComponent, { Gantt, GanttProps, GanttRef } from '@choerodon/gantt';
import '@choerodon/gantt/dist/gantt.cjs.production.min.css';
import { set as mobxSet } from 'mobx';
import {
  ganttApi, IGanttConflictAssignee, issueApi, workCalendarApi,
} from '@/api';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import Loading from '@/components/Loading';
import FilterManage from '@/components/FilterManage';
import {
  Issue,
} from '@/common/types';
import isHoliday from '@/utils/holiday';
import Search from './components/search';
import GanttBar from './components/gantt-bar';
import GanttGroupBar from './components/gantt-group-bar';
import GanttGroupBarSprint from './components/gantt-group-bar-sprint';
import IssueDetail from './components/issue-detail';
import Context from './stores/bodyContext';
import { useGanttContext } from './stores/context';
import GanttOperation from './components/gantt-operation';
import { useGanttSortLabel } from './components/gantt-sort-label';
import './index.less';
import QuickCreateIssue from '@/components/QuickCreateIssue';
import useGanttColumns from './hooks/useGanttColumns';
import {
  ganttLocalMove, getGanttMoveDataOrigin, getGanttMoveSubmitData, ganttDataGroupByType, getGanttCreatingSubIssue, ganttNormalizeIssue, ganttIsCanQuickCreateIssue,
} from './utils';
import GanttDragWrapper from './components/gantt-drag-wrapper';
import useQuickCreateIssue from './hooks/useQuickCreateIssue';
import type { GanttIssue } from './types';
import { getProjectId } from '@/utils/common';
import { IGanttHeaderHookData } from './hooks/useGanttHeader';
import useGanttComponentProps from './hooks/useGanttComponentProps';

export interface IGanttGanttBodyProps extends IGanttHeaderHookData {

}
dayjs.extend(weekday);

const GanttBody: React.FC<IGanttGanttBodyProps> = (props) => {
  const { type, processType, setConfig } = props;
  const context = useGanttContext();
  const {
    isInProgram, menuType, projectId, cached, updateCache, store, issueSearchStore,
  } = context;
  const [conflictAssignees, setConflictAssignees] = useState<Array<IGanttConflictAssignee>>([]);
  const [data, setData] = useState<any[]>([]);
  const [rankList, setRankList] = useState<string[] | undefined>(undefined);
  const [workCalendar, setWorkCalendar] = useState<any>();
  const isHasConflict = useRef<boolean>(false);
  const [{ origin: sortedList, data: sorted, loading: sortLoading }, onSortChange] = useGanttSortLabel({ projectId });
  const [projectWorkCalendar, setProjectWorkCalendar] = useState<any>();
  const [filterManageVisible, setFilterManageVisible] = useState<boolean>();
  const [loading, setLoading] = useState(false);

  const { sprintIds, unit } = store;
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
  const {
    columns, setColumns, visibleColumnCodes, tableWithSortedColumns, listLayoutColumns,
  } = useGanttColumns({
    ...props,
    cached,
    updateCache,
    onSortChange,
    sortedList,
    menuType,
    projectId,
    isInProgram,
    onUpdate: handleIssueUpdate,
    onClickSummary: handleClickSummary,
    onCreateSubIssue: handleQuickCreateSubIssue,
    onAfterCreateSubIssue: handleQuickCreateSubIssueAfter,
  });
  // 装载header 所需方法
  useMount(() => {
    setConfig('onTypeChange', () => setRankList(undefined));
    setConfig('onRefresh', () => run());
    setConfig('onClickPersonalFilter', () => setFilterManageVisible(true));
  });
  useEffect(() => {
    setConfig('visibleColumnCodes', visibleColumnCodes);
    setConfig('columnOptions', listLayoutColumns.map((item) => ({ code: item.columnCode, title: item.label })));
  }, [visibleColumnCodes, listLayoutColumns, setConfig]);
  const searchFilter = useComputed(() => {
    const filter = issueSearchStore.getCustomFieldFilters();
    filter.otherArgs.sprint = sprintIds;
    filter.searchArgs.dimension = type;
    if (menuType === 'org') {
      filter.searchArgs.teamProjectIds = [projectId];
    }
    return merge(filter, {
      displayFields: visibleColumnCodes.map((code) => ({ code, projectId })),
      searchArgs: { tree: !['assignee', 'sprint'].includes(type), dimension: type },
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
        workCalendarApi.menu('project').project(projectId).getWorkSetting(year),
        workCalendarApi.project(projectId).getYearCalendar(year),
        ['sprint', 'assignee'].includes(type) ? ganttApi.project(projectId).loadDimensionRank(searchFilter) : { ids: [] },
        ganttApi.project(projectId).loadByTask(searchFilter, sorted),
      ] : [workCalendarApi.getWorkSetting(year), null, { ids: [] }, ganttApi.loadOrgByTask(searchFilter, 1)];
      const [workCalendarRes, projectWorkCalendarRes, rankListRes, res] = await Promise.all(requestArr);
      const conflictUsers = menuType === 'org' && type === 'assignee' ? await ganttApi.loadTimeConflict(searchFilter) : [];
      isHasConflict.current = menuType === 'org' && type !== 'assignee' && await ganttApi.loadTimeConflictExcludeUser(searchFilter);
      // setColumns(headers.map((h: any) => ({
      //   width: 100,
      //   name: h.fieldCode,
      //   label: h.name,
      // })));
      localPageCacheStore.project(projectId).setItem('agile.gantt.search', searchFilter);
      setConfig('isHasConflict', isHasConflict.current);
      unstable_batchedUpdates(() => {
        setProjectWorkCalendar(projectWorkCalendarRes);
        setWorkCalendar(workCalendarRes);
        setColumns(tableWithSortedColumns);
        setRankList(rankListRes?.ids);
        setConflictAssignees(conflictUsers);
        setData(res.map((item: any) => ({ ...item, onlyShow: menuType === 'org' })));
        setLoading(false);
      });
    })().catch(() => {
      setLoading(false);
    });
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

  const renderBar: GanttProps['renderBar'] = useCallback((bar, { width, height }, dateKeyRange) => (
    <GanttBar
      type={type}
      bar={bar}
      width={width}
      height={height}
      dateKeyRange={dateKeyRange}
      ganttRef={store.ganttRef}
      processType={processType}
      onClick={noop}
    />
  ), [processType, store.ganttRef, type]);
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
        ganttRef={store.ganttRef}
      />
    );
  }, [store.ganttRef]);

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
        issue.subBugVOList.forEach((item) => {
          const bugIssue = find(draft, (i) => i.issueId === item.issueId);
          if (bugIssue) {
            bugIssue.parentId = null;
          }
        });
      }));
    }
  });

  const handleDeleteSubIssue = usePersistFn((issue: Issue, subIssueId: string) => {
    handleIssueDelete(issue);
  });

  const ganttData = useMemo(() => ganttDataGroupByType({
    data, type, isInProgram, rankList, conflictAssignees, menuType,
  }), [data, type, isInProgram, rankList, conflictAssignees, menuType]);
  const renderEmpty = usePersistFn(() => {
    if (!sprintIds || sprintIds?.length === 0) {
      return <span>暂无数据，请选择冲刺</span>;
    }
    return <span>暂无数据</span>;
  });

  const [ganttComponentProps] = useGanttComponentProps({
    onUpdate: handleUpdate,
    innerRef: store.ganttRef as React.MutableRefObject<GanttRef>,
    data: ganttData,
    columns,
    isRestDay,
    isShowBar,
    unit,
    renderBar,
    renderGroupBar,
    renderEmpty,
  });
  // @ts-ignore
  const renderClone = usePersistFn((record: Gantt.Record) => tableWithSortedColumns[0].render!(record, {} as any) as React.ReactElement);

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
        moveConfig.setData(newData);
      }
      return success;
    }, request);
    moveFn({
      sourceBar, destinationBar, type, data: moveConfig.data,
    });
  }, [data, projectId, rankList, searchFilter, store.ganttRef, type]);

  const handleChangeUnit = useCallback(({ key }) => {
    store.switchUnit(key);
  }, [store]);
  return (
    <Context.Provider value={{
      ...context,
      store,
      searchFilter,
      dimensionType: type,
      menuType,
      disable: menuType === 'org',
      projectId,
      processType,
      sortedList,
    }}
    >
      <div className="c7n-gantt-content-header">
        <Search issueSearchStore={issueSearchStore} loadData={run} />
        <GanttOperation
          value={store.unit}
          onChangeUnit={handleChangeUnit}
          onClickToday={() => {
            store.ganttRef.current && store.ganttRef.current.backToday();
          }}
        />
      </div>
      <Loading loading={sortLoading || loading} />
      {columns.length > 0 && workCalendar && (
        <div className="c7n-gantt-content-body">
          <GanttDragWrapper renderClone={renderClone} onDragEnd={handleDragEnd}>
            <GanttComponent
              {...ganttComponentProps}
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
        // 复制改为异步，无法最小局部更新
        onCopyIssue={run}
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
  );
};

export default observer(GanttBody);
