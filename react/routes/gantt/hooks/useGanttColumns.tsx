import React, {
  useState, useMemo,
} from 'react';
import { Tooltip, Icon } from 'choerodon-ui/pro';
import {
  find, findIndex, merge, noop,
} from 'lodash';
import dayjs from 'dayjs';
import classNames from 'classnames';
import { GanttProps, Gantt } from '@choerodon/gantt';
import '@choerodon/gantt/dist/gantt.cjs.production.min.css';
import {
  ListLayoutColumnVO,
} from '@/api';
import TypeTag from '@/components/TypeTag';
import {
  IFoundationHeader, Issue, User,
} from '@/common/types';
import openCreateIssue from '@/components/create-issue';
import GanttSortLabel, { IGanttSortLabelProps } from '../components/gantt-sort-label';
import QuickCreateSubIssue from '@/components/QuickCreateSubIssue';
import { TableCacheRenderProps } from '@/components/table-cache';
import useIssueTableFields from '@/hooks/data/useIssueTableFields';
import { getCustomColumn, systemColumnsMap } from '@/components/issue-table/baseColumns';

interface IGanttColumnsHookProps extends TableCacheRenderProps {
  onSortChange: IGanttSortLabelProps['onChange']
  onCreateSubIssue?: (parentIssue: Issue) => void
  onAfterCreateSubIssue?: (createId: number, createSuccessData?: { subIssue: Issue, parentIssueId: string }, flagFailed?: boolean) => void
}
const renderTooltip = (user: User) => {
  const {
    loginName, realName, email, ldap,
  } = user || {};
  return ldap ? `${realName}(${loginName})` : `${realName}(${email})`;
};

const isCanQuickCreateIssue = (record: Gantt.Record<any>) => {
  const { typeCode } = record.issueTypeVO || {};
  if (record.group || !typeCode) {
    return false;
  }
  return typeCode === 'story' || (!record.parentId && ['bug', 'task'].includes(typeCode));
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
  label: '预计结束',
  render: (record: any) => record.estimatedEndTime && <Tooltip title={record.estimatedEndTime}><span>{dayjs(record.estimatedEndTime).format('YYYY-MM-DD')}</span></Tooltip>,
}),
]]);

function getListLayoutColumns(listLayoutColumns: ListLayoutColumnVO[] | null, fields: IFoundationHeader[]): Array<ListLayoutColumnVO & IFoundationHeader & { label: string }> {
  let res: any[] = [];
  if (listLayoutColumns) {
    // TODO 过滤已被删除的字段
    res = [...listLayoutColumns];
  }
  fields.forEach(((field) => {
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
const getTableColumns = (visibleColumns: ListLayoutColumnVO[], tableFields: IFoundationHeader[], onSortChange: IGanttColumnsHookProps['onSortChange'], openCreateSubIssue: IGanttColumnsHookProps['onCreateSubIssue'] = noop, onAfterCreateSubIssue: IGanttColumnsHookProps['onAfterCreateSubIssue'] = noop) => {
  const tableColumns: GanttProps<Issue>['columns'] = [{
    flex: 2,
    minWidth: 300,
    // width: 300,
    // @ts-ignore
    lock: 'left',
    name: 'summary',
    label: '名称',
    render: (record) => {
      if (record.create) {
        const parentIssue: Issue = record.parent;
        const onCreate = (issue: Issue) => onAfterCreateSubIssue(record.createId, { subIssue: issue, parentIssueId: parentIssue.issueId });
        return (
          <span role="none" onClick={(e) => e.stopPropagation()} className="c7n-gantt-content-body-create">
            <QuickCreateSubIssue
              mountCreate
              typeCode={['sub_task', 'bug']}
              priorityId={parentIssue.priorityVO?.id}
              parentIssueId={parentIssue.issueId}
              sprintId={(parentIssue as any).sprint?.sprintId!}
              cantCreateEvent={() => {
                onAfterCreateSubIssue(record.createId, undefined, true);
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
                  !res && onAfterCreateSubIssue(record.createId, undefined, true);
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
          <TypeTag iconSize={22} data={record.issueTypeVO} featureType={record.featureType} style={{ marginRight: 5 }} />
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
  ];
  tableColumns.push(...visibleColumns.map(({ columnCode }) => {
    const baseColumn = { width: 100 };
    if (ganttColumnMap.has(columnCode)) {
      const field = ganttColumnMap.get(columnCode);
      return merge(baseColumn, typeof field === 'function' ? field(onSortChange) as Gantt.Column : field);
    }
    if (systemColumnsMap.has(columnCode)) {
      const column = systemColumnsMap.get(columnCode);
      return merge(baseColumn, {
        ...column, label: column?.title, name: column?.dataIndex,
      });
    }
    const field = find(tableFields, { code: columnCode });
    return merge(baseColumn, field ? { ...getCustomColumn(field), label: field.title } : {});
  }));
  return tableColumns;
};
const defaultVisibleColumns = ['assignee', 'estimatedStartTime', 'estimatedEndTime'];
const defaultListLayoutColumns = defaultVisibleColumns.map((code) => ({
  columnCode: code,
  display: true,
}));
/**
 * 甘特图列配置获取hook
 * @param param0
 * @returns
 */
function useGanttColumns({
  cached, onAfterCreateSubIssue, onCreateSubIssue, onSortChange,
}: IGanttColumnsHookProps) {
  const { data: tableFields } = useIssueTableFields({ hiddenFieldCodes: ['epicSelfName', 'summary'] });
  const [columns, setColumns] = useState<Gantt.Column[]>([]);
  const listLayoutColumns = useMemo(() => getListLayoutColumns(cached?.listLayoutColumns || defaultListLayoutColumns as any, tableFields || []), [cached?.listLayoutColumns, tableFields]);
  const visibleColumnCodes = useMemo(() => (listLayoutColumns.filter((c) => c.display).map((c) => c.columnCode)), [listLayoutColumns]);
  const tableWithSortedColumns = useMemo(() => getTableColumns(listLayoutColumns.filter((item) => item.display), tableFields || [], onSortChange, onCreateSubIssue, onAfterCreateSubIssue), [listLayoutColumns, onAfterCreateSubIssue, onCreateSubIssue, onSortChange, tableFields]);
  return {
    columns,
    setColumns,
    visibleColumnCodes,
    tableWithSortedColumns,
    listLayoutColumns,
  };
}
export default useGanttColumns;
