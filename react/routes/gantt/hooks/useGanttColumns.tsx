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
import { useMount, useUpdateEffect } from 'ahooks';
import {
  ganttApi,
  IGanttPredecessorType,
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
import type { IGanttProps } from '../stores/context';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import openGanttConflictModal from '../components/gannt-conflict-modal';
import { ganttIsCanQuickCreateIssue } from '../utils';
import openGanttDependencyModal from '../components/gantt-dependency-modal';
import TableDropMenu from '@/components/table-drop-menu';
import useProjectPredecessorTypes from '@/hooks/data/useProjectPredecessorTypes';
import useFormatMessage from '@/hooks/useFormatMessage';

interface IGanttColumnsHookProps extends TableCacheRenderProps {
  menuType: IGanttProps['menuType']
  projectId?: string
  isInProgram: boolean
  sortedList: IGanttSortLabelSortItem[]
  onUpdate: (issue: GanttIssue) => void
  onClickSummary?: (issue: GanttIssue) => void
  onSortChange: IGanttSortLabelProps['onChange']
  onCreateSubIssue?: (parentIssue: GanttIssue) => void
  onAfterCreateSubIssue?: (createId?: number, createSuccessData?: { subIssue: Issue, parentIssueId: string }, flagFailed?: boolean) => void
}
interface IGanttOrgColumnsHookProps extends TableCacheRenderProps {
  onClickSummary?: (issue: GanttIssue) => void
  projectId?: string
}
const IntlField: React.FC<{ column: any }> = ({ children, column }) => {
  const formatMessage = useFormatMessage();
  const name = column.titleKey ? formatMessage({ id: column.titleKey }) : column.label ?? column.title; // , defaultMessage: column.title
  return React.isValidElement(name) ? name : <Tooltip title={name}>{name}</Tooltip>;
};
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

const ganttColumnMap = new Map<string, any>([['assignee', {
  width: 85,
  minWidth: 85,
  name: 'assignee',
  render: (record: any) => (
    <Tooltip title={renderTooltip(record.assignee)}>
      <span>{record.assignee?.realName}</span>
    </Tooltip>
  ),
}],
['predecessor', {
  width: 120,
  minWidth: 120,
  name: 'predecessor',
  titleKey: 'agile.gantt.column.predecessor',
  label: '前置依赖',
  render: (record: any) => record.predecessors,
}], ['estimatedStartTime', {
  width: 100,
  minWidth: 100,
  name: 'estimatedStartTime',
  titleKey: 'agile.gantt.column.estimatedStartTime',
  render: (record: any) => record.estimatedStartTime && <Tooltip title={record.estimatedStartTime}><span>{dayjs(record.estimatedStartTime).format('YYYY-MM-DD')}</span></Tooltip>,
}], ['estimatedEndTime', {
  width: 100,
  minWidth: 100,
  name: 'estimatedEndTime',
  titleKey: 'agile.gantt.column.estimatedEndTime',
  render: (record: any) => record.estimatedEndTime && <Tooltip title={record.estimatedEndTime}><span>{dayjs(record.estimatedEndTime).format('YYYY-MM-DD')}</span></Tooltip>,
},
],
['actualStartTime', {
  width: 100,
  minWidth: 100,
  name: 'actualStartTime',
  titleKey: 'agile.gantt.column.actualStartTime',
  render: (record: any) => record.actualStartTime && <Tooltip title={record.actualStartTime}><span>{dayjs(record.actualStartTime).format('YYYY-MM-DD')}</span></Tooltip>,
},
],
['actualEndTime', {
  width: 100,
  minWidth: 100,
  name: 'actualEndTime',
  titleKey: 'agile.gantt.column.actualEndTime',
  render: (record: any) => record.actualEndTime && <Tooltip title={record.actualEndTime}><span>{dayjs(record.actualEndTime).format('YYYY-MM-DD')}</span></Tooltip>,
},
],
]);
function findIntlTitleKey(code: string): string | undefined {
  return ganttColumnMap.get(code)?.titleKey ?? systemColumnsMap.get(code)?.titleKey;
}
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
        // 寻找多语言 key
        label: <IntlField column={{ ...field, titleKey: findIntlTitleKey(field.code) }} />,
        columnCode: field.code,
        display: false,
        ...field,
        // fieldId: field.id,
      });
    } else {
      res[resIndex] = {
        ...res[resIndex],
        // 寻找多语言 key
        label: <IntlField column={{ ...field, titleKey: findIntlTitleKey(field.code) }} />,
        ...field,
      };
    }
  }));
  return res;
}
interface TableColumnEvent {
  onUpdate?: IGanttColumnsHookProps['onUpdate']
  onClickSummary?: IGanttColumnsHookProps['onClickSummary']
  onSortChange?: IGanttColumnsHookProps['onSortChange']
  openCreateSubIssue?: IGanttColumnsHookProps['onCreateSubIssue']
  onAfterCreateSubIssue?: IGanttColumnsHookProps['onAfterCreateSubIssue']
}
const getTableColumns = (visibleColumns: Array<ListLayoutColumnVO & { disable?: boolean }>,
  tableFields: IFoundationHeader[], events: TableColumnEvent = {}, disable: { disableOperate: boolean, disableFeatureCreateIssue: boolean } = { disableFeatureCreateIssue: false, disableOperate: false }, predecessorTypes: IGanttPredecessorType[] = []) => {
  const {
    onSortChange = noop, onClickSummary = noop, onUpdate = noop,
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
      } else if (parentIssue.issueTypeVO?.typeCode === 'bug') {
        typeCodes = ['sub_task'];
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
                  isProgram: false,
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
    const isShowDependency = !record.group && record.issueTypeVO?.typeCode && !['issue_epic', 'feature'].includes(record.issueTypeVO?.typeCode);
    const hasDependency = record.predecessors?.length;
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
        {!disable.disableOperate && (
          <TableDropMenu
            showText={false}
            menuData={[
              { text: '添加子工作项', action: () => openCreateSubIssue(record as any), display: isShowDependency && isCanCreateIssue },
              {
                text: `${hasDependency ? '编辑' : '添加'}前置依赖`,
                action: () => openGanttDependencyModal({
                  issueId: record.issueId,
                  data: record.predecessors || [],
                  onOk: () => onUpdate(record),
                }),
                display: isShowDependency,
              },
            ]}
          />
        )}
        {!isShowDependency && isCanCreateIssue && (
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
                type="error"
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
  const predecessorMaps = new Map(predecessorTypes.map((item) => [item.valueCode, item]));
  function renderPredecessor(record: Gantt.Record<any>) {
    const predecessors: any[] = record.predecessors?.map((item: any) => ({ ...item, predecessorName: predecessorMaps.get(item.predecessorType)!.name })) || [];
    return (
      <Tooltip placement="topLeft" title={predecessors.map((predecessor: any) => <div>{`${predecessor.predecessorName}：${predecessor.issueNum} ${predecessor.summary}`}</div>)}>
        <span>
          {predecessors.map<string>((predecessor: any) => `${predecessor.issueNum?.split('-').slice(-1)}${predecessor.predecessorName}`).join('、')}
        </span>
      </Tooltip>
    );
  }
  const tableColumns: Array<GanttProps<Issue>['columns'][0] & { titleKey: string, }> = [{
    flex: 2,
    minWidth: 300,
    // width: 300,
    lock: 'left',
    name: 'summary',
    // label: '名称',
    label: <GanttSortLabel dataKey="summary" onChange={onSortChange}><IntlField column={{ titleKey: 'agile.gantt.column.summary' }} /></GanttSortLabel> as any,
    titleKey: 'agile.gantt.column.summary',
    render: renderSummary,
  },
  ];
  const fieldMapRender = {
    label: BaseSystemColumnRender.renderTag('labels', 'labelName'),
    component: BaseSystemColumnRender.renderTag('components', 'name'),
    fixVersion: BaseSystemColumnRender.renderTag('fixVersion', 'name'),
    influenceVersion: BaseSystemColumnRender.renderTag('influenceVersion', 'name'),
    sprint: BaseSystemColumnRender.renderTag('sprints', 'sprintName'),
    reporter: (rowData: any) => <UserTag data={get(rowData, 'reporter')} />,
    environmentName: (rowData: any) => get(rowData, 'environment'),
  };
  tableColumns.push(...visibleColumns.map(({ columnCode }) => {
    const baseColumn = { width: 100 } as any;
    if (systemColumnsMap.has(columnCode)) {
      const column = systemColumnsMap.get(columnCode)!;
      merge(baseColumn, {
        ...column,
        name: column?.dataIndex,
        render: fieldMapRender[columnCode as keyof typeof fieldMapRender] ?? column?.render,
      });
    } else {
      const field = find(tableFields, { code: columnCode });
      const column = field ? getCustomColumn(field) : {} as any;
      merge(baseColumn, column);
    }

    if (ganttColumnMap.has(columnCode)) {
      const field = ganttColumnMap.get(columnCode);
      merge(baseColumn, { ...field, sortable: true });
      columnCode === 'predecessor' && merge(baseColumn, { render: renderPredecessor });
    }
    const { render, name } = baseColumn;
    return merge(baseColumn, {
      label: baseColumn.sortable ? (
        <GanttSortLabel
          dataKey={baseColumn.dataIndex}
          onChange={onSortChange}
        >
          <IntlField column={baseColumn} />
        </GanttSortLabel>
      ) : <IntlField column={baseColumn} />,
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
const ganttSystemFields = [{
  title: '前置依赖', titleKey: 'agile.gantt.column.predecessor', code: 'predecessor', fieldType: 'multiple',
}];
const defaultVisibleColumns = ['assignee', 'predecessor', 'estimatedStartTime', 'estimatedEndTime', 'actualStartTime', 'actualEndTime'];
const defaultListLayoutColumns = defaultVisibleColumns.map((code) => ({
  columnCode: code,
  display: true,
}));
const hiddenFieldCodes = ['epicSelfName', 'summary'];
function useGanttProjectColumns({
  cached, onAfterCreateSubIssue, onCreateSubIssue, onClickSummary, onSortChange, onUpdate, projectId, menuType, isInProgram, sortedList,
}: IGanttColumnsHookProps) {
  // 恒为 项目层级
  const { data: tableFields } = useIssueTableFields({
    hiddenFieldCodes, extraFields: ganttSystemFields, projectId, menuType: 'project', issueTypeList: 'agileIssueType',
  });
  const { data: issueTypes, isLoading: issueTypeIsLoading } = useProjectIssueTypes({ projectId, isInProgram, applyType: 'agile' });
  const { data: predecessorTypes, isLoading: predecessorTypesLoading } = useProjectPredecessorTypes({ projectId });
  const isLoading = issueTypeIsLoading && predecessorTypesLoading;
  const disableFeatureCreateIssue = !!issueTypes?.some((issueType) => issueType.typeCode === 'story');
  const [columns, setColumns] = useState<Gantt.Column[]>([]);
  const listLayoutColumns = useMemo(() => getListLayoutColumns(cached?.listLayoutColumns || defaultListLayoutColumns as any, tableFields || []), [cached?.listLayoutColumns, tableFields]);
  const visibleColumnCodes = useMemo(() => (listLayoutColumns.filter((c) => c.display).map((c) => c.columnCode)), [listLayoutColumns]);
  const tableWithSortedColumns = useMemo(() => getTableColumns(listLayoutColumns.filter((item) => item.display)
    .map((item) => ({ ...item, disable: true })), tableFields || [], {
    onClickSummary, onSortChange, openCreateSubIssue: onCreateSubIssue, onAfterCreateSubIssue, onUpdate,
  }, { disableOperate: menuType !== 'project', disableFeatureCreateIssue }, predecessorTypes), [disableFeatureCreateIssue, listLayoutColumns, menuType, onAfterCreateSubIssue, onClickSummary, onCreateSubIssue, onSortChange, onUpdate, predecessorTypes, tableFields]);
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
