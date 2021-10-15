/* eslint-disable no-param-reassign */
import React, {
  useState, useEffect, useCallback, useMemo, useRef,
} from 'react';
// eslint-disable-next-line camelcase
import { unstable_batchedUpdates } from 'react-dom';
import { Tooltip, Icon, Button } from 'choerodon-ui/pro';
import { observer, useComputed } from 'mobx-react-lite';
import {
  find, findIndex, flow, merge, omit, pick, remove, set, some,
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
import GanttComponent, { Gantt, GanttProps, GanttRef } from '@choerodon/gantt';
import '@choerodon/gantt/dist/gantt.cjs.production.min.css';
import { FlatSelect } from '@choerodon/components';
import {
  ganttApi, issueApi, workCalendarApi,
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
import useIsInProgram from '@/hooks/useIsInProgram';
import TableCache, { TableCacheRenderProps } from '@/components/table-cache';
import useGanttColumns from './hooks/useGanttColumns';
import {
  ganttLocalMove, getGanttMoveDataOrigin, getGanttMoveSubmitData, ganttDataGroupByType, ganttNormalizeIssue,
} from './utils';
import GanttDragWrapper from './components/gantt-drag-wrapper';
import useQuickCreateIssue from './hooks/useQuickCreateIssue';

const { Option } = FlatSelect;

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
export type IGanttDimensionTypeValue = (typeof typeValues)[number];

const GanttPage: React.FC<TableCacheRenderProps> = (props) => {
  const { isInProgram } = useIsInProgram();
  const [data, setData] = useState<any[]>([]);
  const [type, setType] = useState<IGanttDimensionTypeValue>(localPageCacheStore.getItem('gantt.search.type') ?? typeValues[0]);
  const [rankList, setRankList] = useState<string[] | undefined>(undefined);
  const [workCalendar, setWorkCalendar] = useState<any>();
  const [{ data: sortedList }, onSortChange] = useGanttSortLabel();

  const [projectWorkCalendar, setProjectWorkCalendar] = useState<any>();
  const [filterManageVisible, setFilterManageVisible] = useState<boolean>();
  const [loading, setLoading] = useState(false);
  const issueSearchStore = useIssueSearchStore({
    getSystemFields: () => getSystemFields().map((item) => (item.code === 'feature' || item.code === 'epic' ? { ...item, defaultShow: false } : item)).filter((item) => item.code !== 'sprint') as ILocalField[],
    transformFilter,
  });

  const store = useMemo(() => new GanttStore(), []);
  const { sprintIds, unit } = store;
  const [isFullScreen, toggleFullScreen] = useFullScreen(() => document.body, () => { }, 'c7n-gantt-fullScreen');
  const handleQuickCreateSubIssue = usePersistFn((parentIssue: Issue) => {
    setData(produce(data, (draft) => {
      const targetIndex = findIndex(draft, (issue) => issue.parentId === parentIssue.issueId || issue.issueId === parentIssue.issueId);
      targetIndex !== -1 && draft.splice(targetIndex, 0, {
        parentId: parentIssue.issueId, parent: parentIssue, create: true, createId: targetIndex,
      });
    }));
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
      run();
    }
  });
  const {
    columns, setColumns, visibleColumnCodes, tableWithSortedColumns, listLayoutColumns,
  } = useGanttColumns({
    ...props, onSortChange, onCreateSubIssue: handleQuickCreateSubIssue, onAfterCreateSubIssue: handleQuickCreateSubIssueAfter,
  });
  const searchFilter = useComputed(() => {
    const filter = issueSearchStore.getCustomFieldFilters();
    filter.otherArgs.sprint = sprintIds;
    filter.searchArgs.dimension = type;
    return merge(filter, {
      displayFieldCodes: visibleColumnCodes,
      searchArgs: { tree: type !== 'assignee', dimension: type },
    });
  }, [sprintIds, type]);

  const { run, flush } = useDebounceFn(() => {
    (async () => {
      const year = dayjs().year();
      if (sprintIds === null) {
        return;
      }
      setLoading(true);
      const [workCalendarRes, projectWorkCalendarRes, rankListRes, res] = await Promise.all([
        workCalendarApi.getWorkSetting(year),
        workCalendarApi.getYearCalendar(year),
        ['sprint', 'assignee'].includes(type) ? ganttApi.loadDimensionRank(searchFilter) : { ids: [] },
        ganttApi.loadByTask(searchFilter, sortedList),
      ]);
      // setColumns(headers.map((h: any) => ({
      //   width: 100,
      //   name: h.fieldCode,
      //   label: h.name,
      // })));
      unstable_batchedUpdates(() => {
        setWorkCalendar(workCalendarRes);
        setProjectWorkCalendar(projectWorkCalendarRes);
        setColumns(tableWithSortedColumns);
        setRankList(rankListRes?.ids);
        setData(res);
        setLoading(false);
      });
    })();
  });

  const [{ isCreate }, quickCreateProps] = useQuickCreateIssue({ onCreate: run });

  useEffect(() => {
    run();
  }, [issueSearchStore, sprintIds, run, visibleColumnCodes]);
  useUpdateEffect(() => {
    run();
    flush();
  }, [sortedList, type]);

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
    setRankList(undefined);
    setType(newType);
    localPageCacheStore.setItem('gantt.search.type', newType);
  }, []);

  const isRestDay = useCallback((date: string) => isHoliday({
    sprintSetting: projectWorkCalendar,
    orgWorkCalendar: workCalendar,
  }, moment(date)), [projectWorkCalendar, workCalendar]);
  const handleClickFilterManage = () => {
    setFilterManageVisible(true);
  };
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

  const handleIssueUpdate = usePersistFn((issue: Issue | null) => {
    if (type === 'epic') {
      run();
    } else if (issue) {
      setData(produce(data, (draft) => {
        const target = find(draft, { issueId: issue.issueId });
        if (target) {
          // 更新属性
          ganttNormalizeIssue(issue, target);
        }
      }));
      updateInfluenceIssues(issue);
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
  const updateInfluenceIssues = useCallback((res: { influenceIssueIds?: string[], [key: string]: any }) => {
    // @ts-ignore
    const { influenceIssueIds } = res;
    // 更新自身 及影响的issue
    const updateIssueIds = [res.issueId, ...(influenceIssueIds || [])];
    ganttApi.loadInfluenceIssues(type, updateIssueIds, visibleColumnCodes).then((issues: any[]) => {
      updateIssues(issues);
    });
  }, [type, updateIssues, visibleColumnCodes]);

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

  const ganttData = useMemo(() => ganttDataGroupByType({
    data, type, isInProgram, rankList,
  }), [data, isInProgram, rankList, type]);
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
      request: () => ganttApi.moveTopDimension(requestData, searchFilter),
    } : {
      data,
      setData,
      request: () => ganttApi.move(requestData, searchFilter),
    };
    async function request(success: boolean) {
      success && await moveConfig.request();
      setLoading(false);
    }
    const moveFn = flow(ganttLocalMove, ({ newData, success }) => {
      success && moveConfig.setData(newData);
      return success;
    }, request);
    moveFn({
      sourceBar, destinationBar, type, data: moveConfig.data,
    });
  }, [data, rankList, searchFilter, store.ganttRef, type]);
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
        <Context.Provider value={{
          store, searchFilter, dimensionType: type,
        }}
        >
          <div style={{ display: 'flex', flexWrap: 'wrap' }}>
            <Search issueSearchStore={issueSearchStore} loadData={run} />
            <GanttOperation />
          </div>
          <Loading loading={loading} />
          {columns.length > 0 && workCalendar && (
            <div className="c7n-gantt-content-body">
              <GanttDragWrapper renderClone={renderClone} onDragEnd={handleDragEnd}>
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
              </GanttDragWrapper>
              <div className={classNames('c7n-gantt-content-body-quick-create', { 'c7n-gantt-content-body-quick-create-open': isCreate })}>
                <QuickCreateIssue
                  {...quickCreateProps}
                  sprintId={sprintIds?.length === 1 ? sprintIds.filter((i) => i !== '0')[0] : undefined}
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
const GanttPageHOC = () => (
  <TableCache type="gantt">
    {(cacheProps) => <ObserverGanttPage {...cacheProps} />}
  </TableCache>
);
export default GanttPageHOC;
