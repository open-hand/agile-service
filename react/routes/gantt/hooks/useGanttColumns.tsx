import React, {
  useState, useMemo, useEffect, useRef,
} from 'react';
import { Tooltip, Icon } from 'choerodon-ui/pro';
import {
  find, findIndex, get, intersection, merge, noop,
} from 'lodash';
import dayjs from 'dayjs';
import classNames from 'classnames';
import { GanttProps, Gantt } from '@choerodon/gantt';
import '@choerodon/gantt/dist/gantt.cjs.production.min.css';
import { AnyMap } from 'immer/dist/internal';
import { useUpdateEffect } from 'ahooks';
import {
  ganttApi,
  ListLayoutColumnVO,
} from '@/api';
import TypeTag from '@/components/TypeTag';
import {
  IFoundationHeader, Issue, User,
} from '@/common/types';
import openCreateIssue from '@/components/create-issue';
import GanttSortLabel, { IGanttSortLabelProps, IGanttSortLabelSortItem } from '../components/gantt-sort-label';
import QuickCreateSubIssue from '@/components/QuickCreateSubIssue';
import { TableCacheRenderProps } from '@/components/table-cache';
import useIssueTableFields from '@/hooks/data/useIssueTableFields';
import { getCustomColumn, systemColumnsMap, BaseSystemColumnRender } from '@/components/issue-table/baseColumns';
import type { GanttIssue } from '../types';
import UserTag from '@/components/tag/user-tag';
import { IGanttPageProps } from '../Gantt';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import openGanttConflictModal from '../components/gannt-conflict-modal';
import { ganttIsCanQuickCreateIssue } from '../utils';

interface IGanttColumnsHookProps extends TableCacheRenderProps {
  menuType: IGanttPageProps['menuType']
  projectId?: string
  isInProgram: boolean
  sortedList: IGanttSortLabelSortItem[]
  onClickSummary?: (issue: GanttIssue) => void
  onSortChange: IGanttSortLabelProps['onChange']
  onCreateSubIssue?: (parentIssue: GanttIssue) => void
  onAfterCreateSubIssue?: (createId?: number, createSuccessData?: { subIssue: Issue, parentIssueId: string }, flagFailed?: boolean) => void
}
interface IGanttOrgColumnsHookProps extends TableCacheRenderProps {
  onClickSummary?: (issue: GanttIssue) => void
  projectId?: string
}
const renderTooltip = (user: User) => {
  const {
    loginName, realName, email, ldap,
  } = user || {};
  return ldap ? `${realName}(${loginName})` : `${realName}(${email})`;
};

const isCanQuickCreateIssue = (record: Gantt.Record<any>, { disableFeature }: { disableFeature: boolean }) => {
  const { typeCode } = record.issueTypeVO || {};
  if (record.disabledCreate) {
    return false;
  }
  if (record.groupType === 'feature') {
    return !!disableFeature;
  }
  if (['sprint', 'assignee'].includes(record.groupType)) {
    return true;
  }
  if (record.groupType === 'epic' && !record.isInProgram) {
    return true;
  }
  return typeCode && (typeCode === 'story' || (!record.parentId && ['bug', 'task'].includes(typeCode)));
};
const ganttColumnCodeMapProps: Record<string, { title?: string, width?: number }> = {
  issueNum: {
    title: '编号',
  },
  estimatedStartTime: {
    title: '预计开始',
  },
  estimatedEndTime: {
    title: '预计结束',
  },
  actualStartTime: {
    title: '实际开始',
  },
  actualEndTime: {
    title: '实际结束',
  },
};
const ganttColumnMap = new Map<string, any>([['assignee', (onSortChange: any) => ({
  width: 85,
  minWidth: 85,
  name: 'assignee',
  label: (
    <GanttSortLabel dataKey="assigneeId" onChange={onSortChange}>
      经办人
    </GanttSortLabel>) as any,
  render: (record: any) => (
    <Tooltip title={renderTooltip(record.assignee)}>
      <span>{record.assignee?.realName}</span>
    </Tooltip>
  ),
}),
], ['estimatedStartTime', (onSortChange: any) => ({
  width: 100,
  minWidth: 100,
  name: 'estimatedStartTime',
  label: (
    <GanttSortLabel dataKey="estimatedStartTime" onChange={onSortChange}>
      预计开始
    </GanttSortLabel>) as any,
  render: (record: any) => record.estimatedStartTime && <Tooltip title={record.estimatedStartTime}><span>{dayjs(record.estimatedStartTime).format('YYYY-MM-DD')}</span></Tooltip>,
})], ['estimatedEndTime', (onSortChange: any) => ({
  width: 100,
  minWidth: 100,
  name: 'estimatedEndTime',
  label: (
    <GanttSortLabel dataKey="estimatedEndTime" onChange={onSortChange}>
      预计结束
    </GanttSortLabel>
  ),
  render: (record: any) => record.estimatedEndTime && <Tooltip title={record.estimatedEndTime}><span>{dayjs(record.estimatedEndTime).format('YYYY-MM-DD')}</span></Tooltip>,
}),
],
['actualStartTime', (onSortChange: any) => ({
  width: 100,
  minWidth: 100,
  name: 'actualStartTime',
  label: (
    <GanttSortLabel dataKey="actualStartTime" onChange={onSortChange}>
      实际开始
    </GanttSortLabel>
  ),
  render: (record: any) => record.actualStartTime && <Tooltip title={record.actualStartTime}><span>{dayjs(record.actualStartTime).format('YYYY-MM-DD')}</span></Tooltip>,
}),
],
['actualEndTime', (onSortChange: any) => ({
  width: 100,
  minWidth: 100,
  name: 'actualEndTime',
  label: (
    <GanttSortLabel dataKey="actualEndTime" onChange={onSortChange}>
      实际结束
    </GanttSortLabel>
  ),
  render: (record: any) => record.actualEndTime && <Tooltip title={record.actualEndTime}><span>{dayjs(record.actualEndTime).format('YYYY-MM-DD')}</span></Tooltip>,
}),
],
]);

function getListLayoutColumns(listLayoutColumns: ListLayoutColumnVO[] | null, fields: IFoundationHeader[]): Array<ListLayoutColumnVO & IFoundationHeader & { label: string }> {
  let res: any[] = [];
  if (listLayoutColumns) {
    // TODO 过滤已被删除的字段
    res = [...listLayoutColumns];
  }
  fields.forEach(((f) => {
    const field = { ...f, ...ganttColumnCodeMapProps[f.code] };
    const resIndex = findIndex(res, { columnCode: field.code });
    if (resIndex === -1) {
      res.push({
        width: 0,
        sort: 0,
        label: field.title,
        columnCode: field.code,
        display: false,
        ...field,
        // fieldId: field.id,
      });
    } else {
      res[resIndex] = {
        ...res[resIndex],
        label: field.title,
        ...field,
      };
    }
  }));
  return res;
}
interface TableColumnEvent {
  onClickSummary?: IGanttColumnsHookProps['onClickSummary']
  onSortChange?: IGanttColumnsHookProps['onSortChange']
  openCreateSubIssue?: IGanttColumnsHookProps['onCreateSubIssue']
  onAfterCreateSubIssue?: IGanttColumnsHookProps['onAfterCreateSubIssue']
}
const getTableColumns = (visibleColumns: Array<ListLayoutColumnVO & { disable?: boolean }>,
  tableFields: IFoundationHeader[], events: TableColumnEvent = {}, disable: { disableOperate: boolean, disableFeatureCreateIssue: boolean } = { disableFeatureCreateIssue: false, disableOperate: false }) => {
  const {
    onSortChange = noop, onClickSummary = noop,
    openCreateSubIssue = noop, onAfterCreateSubIssue = noop,
  } = events;
  function renderSummary(record: Gantt.Record<{ createSprintIds?: string[] } & any>) {
    if (record.create) {
      const parentIssue: Gantt.Record<GanttIssue> & { groupType?: string, isInProgram?: boolean } = record.parent;

      const onCreate = (issue: Issue) => onAfterCreateSubIssue(record.createId, { subIssue: issue, parentIssueId: parentIssue.issueId });
      let typeCodes = ['sub_task', 'bug'];
      const defaultValues = {} as any;
      let priorityId: string | undefined = parentIssue.priorityVO?.id;
      let parentIssueId: string | undefined = parentIssue.issueId;
      const sprintId = (parentIssue as any).sprint?.sprintId! || record.sprint?.sprintId || (record.createSprintIds || [])[0];
      if (record.groupType) {
        parentIssueId = undefined;
        priorityId = undefined;
        typeCodes = ['story', 'bug', 'task'];
      }
      if (record.groupType === 'epic') {
        defaultValues.epicId = parentIssue.issueId;
      } else if (record.groupType === 'feature') {
        defaultValues.featureId = parentIssue.issueId;
        typeCodes = ['story'];
      }
      return (
        <span role="none" onClick={(e) => e.stopPropagation()} className="c7n-gantt-content-body-create">
          <QuickCreateSubIssue
            mountCreate
            typeCode={typeCodes}
            priorityId={priorityId}
            parentIssueId={parentIssueId}
            defaultValues={defaultValues}
            isCanQuickCreate={() => ganttIsCanQuickCreateIssue(record.createSprintIds)}
            sprintId={sprintId}
            cantCreateEvent={(res) => {
              onAfterCreateSubIssue(record.createId, undefined, true);
              // 这里延迟打开
              setTimeout(() => {
                openCreateIssue({
                  ...merge(res, {
                    parentIssue: !parentIssue.groupType ? parentIssue : undefined,
                    defaultFeature: parentIssue.groupType === 'feature' ? parentIssue : undefined,
                    defaultValues: {
                      feature: defaultValues.featureId,
                      epic: defaultValues.epicId,
                    },
                  }),
                  onCreate: (issue) => onAfterCreateSubIssue(undefined, { subIssue: issue, parentIssueId: parentIssue.issueId }),
                });
              }, 110);
            }}
            onCreate={onCreate}
            defaultAssignee={parentIssue.assignee ?? undefined}
            onAwayClick={(createFn) => {
              createFn().then((res: boolean) => {
                !res && onAfterCreateSubIssue(record.createId, undefined, true);
              });
            }}
          />
        </span>
      );
    }
    const isCanCreateIssue = !disable.disableOperate && isCanQuickCreateIssue(record, { disableFeature: disable.disableFeatureCreateIssue });
    return !record.group ? (
      // eslint-disable-next-line no-underscore-dangle
      <span className={classNames('c7n-gantt-content-body-summary')}>
        <TypeTag iconSize={22} data={record.issueTypeVO} featureType={record.featureType} style={{ marginRight: 5 }} />
        <Tooltip placement="topLeft" title={record.summary}>
          <span
            role="none"
            style={{ verticalAlign: 'middle', flex: 1 }}
            className="c7n-gantt-content-body-summary-text c7n-agile-table-cell-click"
            onClick={() => {
              onClickSummary(record);
            }}
          >
            {record.summary}

          </span>
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
        <span style={{ color: 'var(--table-click-color)' }} className="c7n-gantt-content-body-summary">
          <span style={{ verticalAlign: 'middle', flex: 1 }} className="c7n-gantt-content-body-summary-text">
            {record.timeConflict && (
              <Icon
                type="info"
                className="c7n-gantt-content-body-summary-conflict"
                onClick={() => {
                  openGanttConflictModal({ assigneeId: record.assigneeId, assigneeName: record.assignee?.realName });
                }}
              />
            )}
            {record.summary}
          </span>
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

      </Tooltip>
    );
  }
  const tableColumns: GanttProps<Issue>['columns'] = [{
    flex: 2,
    minWidth: 300,
    // width: 300,
    // @ts-ignore
    lock: 'left',
    name: 'summary',
    label: (<GanttSortLabel dataKey="summary" onChange={onSortChange}>名称</GanttSortLabel>) as any,
    render: renderSummary,
  },
  ];
  const fieldMapRender = {
    label: BaseSystemColumnRender.renderTag('labels', 'labelName'),
    component: BaseSystemColumnRender.renderTag('components', 'name'),
    fixVersion: BaseSystemColumnRender.renderTag('fixVersion', 'name'),
    influenceVersion: BaseSystemColumnRender.renderTag('influenceVersion', 'name'),
    // assignee: (rowData: AnyMap) => <UserTag data={get(rowData, 'assignee')} />,
  };
  tableColumns.push(...visibleColumns.map(({ columnCode }) => {
    const baseColumn = { width: 100 } as any;
    if (ganttColumnMap.has(columnCode)) {
      const field = ganttColumnMap.get(columnCode);
      merge(baseColumn, typeof field === 'function' ? field(onSortChange) as Gantt.Column : field);
    } else if (systemColumnsMap.has(columnCode)) {
      const column = systemColumnsMap.get(columnCode)!;
      merge(baseColumn, {
        ...column,
        label: column.sortable ? (<GanttSortLabel dataKey={column.dataIndex} onChange={onSortChange}>{column?.title}</GanttSortLabel>) : column?.title,
        name: column?.dataIndex,
        render: fieldMapRender[columnCode as keyof typeof fieldMapRender] ?? column?.render,
      });
    } else {
      const field = find(tableFields, { code: columnCode });
      merge(baseColumn, field ? {
        ...getCustomColumn(field),
        label: field.title,
      } : {});
    }
    const { render, name } = baseColumn;
    return merge(baseColumn, {
      render: (record: any) => {
        if (record?.create) {
          return undefined;
        }
        return render ? render(record) : record[name];
      },
    });
  }));
  return tableColumns;
};
const defaultVisibleColumns = ['assignee', 'estimatedStartTime', 'estimatedEndTime', 'actualStartTime', 'actualEndTime'];
const defaultListLayoutColumns = defaultVisibleColumns.map((code) => ({
  columnCode: code,
  display: true,
}));
function useGanttProjectColumns({
  cached, onAfterCreateSubIssue, onCreateSubIssue, onClickSummary, onSortChange, projectId, menuType, isInProgram, sortedList,
}: IGanttColumnsHookProps) {
  // 恒为 项目层级
  const { data: tableFields } = useIssueTableFields({ hiddenFieldCodes: ['epicSelfName', 'summary'], projectId, menuType: 'project' });
  const { data: issueTypes, isLoading } = useProjectIssueTypes({ projectId, isInProgram });
  const disableFeatureCreateIssue = !!issueTypes?.some((issueType) => issueType.typeCode === 'story');
  const [columns, setColumns] = useState<Gantt.Column[]>([]);
  const listLayoutColumns = useMemo(() => getListLayoutColumns(cached?.listLayoutColumns || defaultListLayoutColumns as any, tableFields || []), [cached?.listLayoutColumns, tableFields]);
  const visibleColumnCodes = useMemo(() => (listLayoutColumns.filter((c) => c.display).map((c) => c.columnCode)), [listLayoutColumns]);
  const tableWithSortedColumns = useMemo(() => getTableColumns(listLayoutColumns.filter((item) => item.display)
    .map((item) => ({ ...item, disable: true })), tableFields || [], {
    onClickSummary, onSortChange, openCreateSubIssue: onCreateSubIssue, onAfterCreateSubIssue,
  }, { disableOperate: menuType !== 'project', disableFeatureCreateIssue }), [disableFeatureCreateIssue, listLayoutColumns, menuType, onAfterCreateSubIssue, onClickSummary, onCreateSubIssue, onSortChange, tableFields]);
  const sortedListRef = useRef<IGanttSortLabelSortItem[]>(sortedList);
  sortedListRef.current = sortedList;

  useUpdateEffect(() => {
    // 检查排序是否有效
    if (sortedListRef.current.length > 0) {
      const sortKeyMapSystemKey = [...systemColumnsMap.entries()].filter(([_, value]) => sortedListRef.current.some((sorted) => sorted.dataKey === value.dataIndex)).map(([key]) => key);
      const sortedCodes = intersection(sortKeyMapSystemKey, visibleColumnCodes).map((key) => systemColumnsMap.get(key)?.dataIndex!);
      const newSortedList = sortedListRef.current
        .map((item) => (sortedCodes.includes(item.dataKey) ? item : { ...item, sorted: undefined }));
      sortedCodes.length !== sortedListRef.current.length && onSortChange && onSortChange(newSortedList);
    }
  }, [onSortChange, visibleColumnCodes]);
  return {
    columns,
    setColumns,
    visibleColumnCodes,
    tableWithSortedColumns: isLoading ? [] : tableWithSortedColumns,
    listLayoutColumns,
  };
}
function useGanttOrgColumns({
  cached,
}: IGanttOrgColumnsHookProps) {
  const tableFields: any[] = useMemo(() => [], []);
  const [columns, setColumns] = useState<Gantt.Column[]>([]);
  const listLayoutColumns = useMemo(() => getListLayoutColumns(cached?.listLayoutColumns || defaultListLayoutColumns as any, tableFields || []), [cached?.listLayoutColumns, tableFields]);
  const visibleColumnCodes = useMemo(() => (listLayoutColumns.filter((c) => c.display).map((c) => c.columnCode)), [listLayoutColumns]);
  const tableWithSortedColumns = useMemo(() => getTableColumns(listLayoutColumns.filter((item) => item.display), tableFields || [], {}, { disableFeatureCreateIssue: true, disableOperate: true }), [listLayoutColumns, tableFields]);
  return {
    columns,
    setColumns,
    visibleColumnCodes,
    tableWithSortedColumns,
    listLayoutColumns,
  };
}
/**
 * 甘特图列配置获取hook
 * @param param0
 * @returns
 */
function useGanttColumns(props: IGanttColumnsHookProps) {
  if (props.menuType === 'project') {
    // eslint-disable-next-line react-hooks/rules-of-hooks
    return useGanttProjectColumns(props);
  }
  // eslint-disable-next-line react-hooks/rules-of-hooks
  return useGanttOrgColumns({ cached: props.cached, updateCache: props.updateCache, onClickSummary: props.onClickSummary });
}

export { useGanttOrgColumns };
export default useGanttProjectColumns;
