/* eslint-disable no-underscore-dangle */
/* eslint-disable no-param-reassign */
import React, {
  useState, useEffect, useCallback, useMemo, useRef,
} from 'react';
// eslint-disable-next-line camelcase
import { unstable_batchedUpdates } from 'react-dom';
import { Tag } from 'choerodon-ui';
import { Tooltip, Icon, Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { runInAction } from 'mobx';
import {
  find, findIndex, isNumber, merge, omit, pick, remove, set, some, get, includes, map as lodashMap,
} from 'lodash';
import produce from 'immer';
import dayjs from 'dayjs';
import weekday from 'dayjs/plugin/weekday';
import classNames from 'classnames';
import {
  usePersistFn, useDebounceFn, useUpdateEffect, useCreation,
} from 'ahooks';
import moment from 'moment';
import {
  Page, Header, Content, Breadcrumb, HeaderButtons,
} from '@choerodon/boot';
import GanttComponent, { GanttProps, Gantt, GanttRef } from '@choerodon/gantt';
import '@choerodon/gantt/dist/gantt.cjs.production.min.css';
import { FlatSelect } from '@choerodon/components';
import {
  DragDropContext, Draggable, DraggableStateSnapshot, DragStart, DragUpdate, Droppable, DropResult, ResponderProvided,
} from 'react-beautiful-dnd';
import {
  ganttApi, IGanttMoveRequestData, IGanttMoveRequestDataPreviousWithNext, issueApi, ListLayoutColumnVO, workCalendarApi,
} from '@/api';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import TypeTag from '@/components/TypeTag';
import Loading from '@/components/Loading';
import SelectSprint from '@/components/select/select-sprint';
import useFullScreen from '@/common/useFullScreen';
import { ILocalField } from '@/components/issue-search/store';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { useIssueSearchStore } from '@/components/issue-search';
import FilterManage from '@/components/FilterManage';
import {
  IFoundationHeader, IIssueType, Issue, User,
} from '@/common/types';
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
import TableCache, { TableCacheRenderProps } from '@/components/table-cache';
import useIssueTableFields from '@/hooks/data/useIssueTableFields';
import { PriorityTag, StatusTag } from '@/components';
import UserTag from '@/components/tag/user-tag';

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
const isDisableDrag = (bar: Gantt.Bar) => {
  const { record } = bar;
  if (record.disabledDrag || record.create) {
    return true;
  }
  return false;
};
/**
 * 拖拽的元素是否能放置到目标位置
 */
function isDragRowDrop(bar: Gantt.Bar, destinationBar?: Gantt.Bar) {
  if (bar._depth === destinationBar?._depth && bar._parent?.key === destinationBar?._parent?.key) {
    return true;
  }
  return false;
}

function getDestinationBar(sourceDepth: number, bar?: Gantt.Bar): Gantt.Bar | undefined {
  //
  if (!bar || bar._depth < sourceDepth) {
    return undefined;
  }
  if (bar._depth > sourceDepth && bar._parent?._bar) {
    return getDestinationBar(sourceDepth, bar._parent?._bar!);
  }
  return bar._depth === sourceDepth ? bar : undefined;
}
const isCanQuickCreateIssue = (record: Gantt.Record<any>) => {
  const { typeCode } = record.issueTypeVO || {};
  if (record.group || !typeCode) {
    return false;
  }
  return typeCode === 'story' || (!record.parentId && ['bug', 'task'].includes(typeCode));
};
const ganttList2Tree = (data: any[]) => list2tree(data, { valueField: 'issueId', parentField: 'parentId' });

const formatData = (data: any[]) => data.map((item, i, arr) => {
  let newItem = Object.assign(item, {});
  if (item.parentId && item.parentId !== '0' && !arr.find((issue) => issue.issueId === item.parentId)) {
    Object.assign(newItem, { parentId: '0' });
  }
  if (item.epicId && item.epicId !== '0' && !arr.find((issue) => issue.issueId === item.epicId)) {
    Object.assign(newItem, { epicId: '0' });
  }
  if (item.featureId && item.featureId !== '0' && !arr.find((issue) => issue.issueId === item.featureId)) {
    Object.assign(newItem, { featureId: null });
  }
  if ((item.create || item.issueTypeVO.typeCode === 'sub_task' || item.issueTypeVO.typeCode === 'bug') && item.parentId) {
    const parent = arr.find((issue) => issue.issueId === item.parentId);
    const newParent = Object.assign(parent, {});
    if (parent.epicId && parent.epicId !== '0' && !arr.find((issue) => issue.issueId === parent.epicId)) {
      Object.assign(newParent, { epicId: '0' });
    }
    if (parent.featureId && parent.featureId !== '0' && !arr.find((issue) => issue.issueId === parent.featureId)) {
      Object.assign(newParent, { featureId: null });
    }
    newItem = {
      ...newItem,
      ...{
        epicId: newParent?.epicId,
        featureId: newParent?.featureId,
      },
    };
  }
  return newItem;
});

const groupByTask = (data: any[]) => ganttList2Tree(data);
const groupByUser = (data: any[]) => {
  const map = new Map<string, { user: User, children: any[] }>();
  const noAssigneeData: any[] = [];
  data.forEach((issue) => {
    if (issue.assignee?.id) {
      if (map.has(issue.assignee.id)) {
        map.get(issue.assignee.id)?.children.push(issue);
      } else {
        map.set(issue.assignee.id, { user: issue.assignee, children: [issue] });
      }
    } else {
      noAssigneeData.push(issue);
    }
  });
  if (noAssigneeData.length > 0) {
    map.set('0', { user: { id: '0', name: '未分配' } as User, children: noAssigneeData });
  }
  return [...map.entries()].map(([assigneeId, { user, children }]) => ({
    summary: user.name,
    group: true,
    assigneeId,
    disabledDrag: assigneeId === '0',
    groupType: 'assignee',
    children: ganttList2Tree(children),
  }));
};
const groupBySprint = (data: any[]) => {
  const map = new Map<string, { sprint: any, children: any[], disabledDrag?: boolean }>();
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
    map.set('无冲刺', { sprint: { sprintId: 0 }, disabledDrag: true, children: noSprintData });
  }
  return [...map.entries()].map(([name, { sprint, disabledDrag, children }]) => ({
    summary: name,
    group: true,
    disabledDrag: !!disabledDrag,
    sprintId: sprint.sprintId,
    groupType: 'sprint',
    groupWidthSelf: true,
    estimatedStartTime: sprint.startDate,
    estimatedEndTime: sprint.endDate,
    children: ganttList2Tree(children),
  }));
};

const groupByFeature = (epicChildrenData: any, data: any) => {
  const map = new Map<string, { feature: any, disabledDrag?: boolean, children: any[] }>();
  const noFeatureData: any[] = [];
  epicChildrenData.forEach((issue: any) => {
    if (issue.featureId && issue.featureId !== '0') {
      const feature = data.find((item: any) => !item.create && item.issueId.toString() === issue.featureId.toString());
      if (map.has(feature?.featureName)) {
        map.get(feature?.featureName)?.children.push(issue);
      } else {
        map.set(feature?.featureName, {
          feature,
          children: [issue],
        });
      }
    } else {
      noFeatureData.push(issue);
    }
  });
  if (noFeatureData.length > 0) {
    map.set('未分配特性', { feature: { issueId: 0 }, disabledDrag: true, children: noFeatureData });
  }

  return [...map.entries()].map(([name, { feature, disabledDrag, children }]) => ({
    group: name === '未分配特性',
    disabledDrag: !!disabledDrag,
    groupType: 'feature',
    summary: name,
    ...feature,
    children: ganttList2Tree(children),
  }));
};

const groupByEpic = (data: any, isInProgram: boolean) => {
  const map = new Map<string, { epic: any, disabledDrag?: boolean, children: any[] }>();
  const noEpicData: any[] = [];
  data.filter((item: any) => item.issueTypeVO?.typeCode !== 'issue_epic' && item.issueTypeVO?.typeCode !== 'feature').forEach((issue: any) => {
    if (issue.epicId && issue.epicId !== '0') {
      const epic = data.find((item: any) => item.issueId === issue.epicId);
      if (map.has(epic?.epicName)) {
        map.get(epic?.epicName)?.children.push(issue);
      } else {
        map.set(epic?.epicName, {
          epic,
          children: [issue],
        });
      }
    } else {
      noEpicData.push(issue);
    }
  });
  if (noEpicData.length > 0) {
    map.set('未分配史诗', { epic: { issueId: '0' }, disabledDrag: true, children: noEpicData });
  }

  return [...map.entries()].map(([name, { epic, children }]) => ({
    group: name === '未分配史诗',
    disabledDrag: true,
    groupType: 'epic',
    summary: name,
    ...epic,
    children: isInProgram ? groupByFeature(children, data) : ganttList2Tree(children),
  }));
};

const renderTooltip = (user: User) => {
  const {
    loginName, realName, email, ldap,
  } = user || {};
  return ldap ? `${realName}(${loginName})` : `${realName}(${email})`;
};
const { Option } = FlatSelect;
function renderEpicOrFeature(rowData: any, fieldName: any) {
  const color = fieldName === 'epicId' ? get(rowData, 'epicColor') : get(rowData, 'featureColor');
  const name = fieldName === 'epicId' ? get(rowData, 'epicName') : get(rowData, 'featureName');
  return name ? (
    <Tooltip title={name}>
      <span style={{
        width: '100%',
        color,
        borderWidth: '1px',
        borderStyle: 'solid',
        borderColor: color,
        borderRadius: '2px',
        fontSize: '13px',
        lineHeight: '20px',
        verticalAlign: 'middle',
        padding: '0 4px',
        display: 'inline-block',
        overflow: 'hidden',
        textOverflow: 'ellipsis',
        whiteSpace: 'nowrap',
      }}
      >
        {name}
      </span>
    </Tooltip>
  ) : null;
}
const renderTag = (listField: any, nameField: any) => (rowData: any) => {
  const list = get(rowData, listField);
  if (list) {
    if (list.length > 0) {
      // return list;
      return (
        <Tooltip title={<div>{lodashMap(list, (item) => item[nameField]).map((name) => <div>{name}</div>)}</div>}>
          <div style={{ display: 'inline-flex', maxWidth: '100%' }}>
            <Tag
              color="blue"
              style={{
                maxWidth: 160,
                overflow: 'hidden',
                textOverflow: 'ellipsis',
                whiteSpace: 'nowrap',
                cursor: 'auto',
              }}
            >
              {list[0][nameField]}
            </Tag>
            {list.length > 1 ? <Tag color="blue">...</Tag> : null}
          </div>
        </Tooltip>
      );
    }
  }
  return null;
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
], ['issueNum', {
  label: <Tooltip title="编号">编号</Tooltip>,
  name: 'issueNum',
  width: 120,
  className: 'c7n-agile-table-cell',
  sortable: true,
}],
['priority', {
  label: <Tooltip title="优先级">优先级</Tooltip>,
  name: 'priorityId',
  className: 'c7n-agile-table-cell',
  sortable: true,
  render: (rowData: any) => (
    <Tooltip mouseEnterDelay={0.5} title={`优先级： ${get(rowData, 'priorityDTO') ? get(rowData, 'priorityDTO').name : ''}`}>
      <PriorityTag
        priority={get(rowData, 'priorityVO')}
        style={{ display: 'inline-flex' }}
      />
    </Tooltip>
  ),
}], ['createUser', {
  label: <Tooltip title="创建人">创建人</Tooltip>,
  name: 'createdBy',
  render: (rowData: any) => (
    <div style={{ display: 'inline-flex', height: 40 }}>
      {
        get(rowData, 'createUser') && (
          <UserTag
            data={get(rowData, 'createUser')}
          />
        )
      }
    </div>
  ),
  sortable: true,
}],
['updateUser', {
  label: <Tooltip title="更新人">更新人</Tooltip>,
  name: 'lastUpdatedBy',
  render: (rowData: any) => (
    <div style={{ display: 'inline-flex', height: 40 }}>
      {
        get(rowData, 'updateUser') && (
          <UserTag
            data={get(rowData, 'updateUser')}
          />
        )
      }
    </div>
  ),
  sortable: true,
}],
['status', {
  label: <Tooltip title="状态">状态</Tooltip>,
  name: 'statusId',
  sortable: true,
  render: (rowData: any) => (
    <Tooltip title={get(rowData, 'statusVO')?.name}>
      <div style={{
        display: 'inline-flex',
        overflow: 'hidden',
      }}
      >
        <StatusTag
          data={get(rowData, 'statusVO')}
          style={{ display: 'inline-flex' }}
        />
      </div>
    </Tooltip>
  ),
}],
['reporter', {
  label: <Tooltip title="报告人">报告人</Tooltip>,
  name: 'reporterId',
  sortable: true,
  render: (rowData: any) => (
    <div style={{ display: 'inline-flex', height: 40 }}>
      {get(rowData, 'reporterId') && get(rowData, 'reporterId') !== '0' && (
        <UserTag
          data={{
            id: get(rowData, 'reporterId'),
            tooltip: get(rowData, 'reporterName'),
            loginName: get(rowData, 'reporterLoginName'),
            realName: get(rowData, 'reporterRealName'),
            imageUrl: get(rowData, 'reporterImageUrl'),
          }}
        />
      )}
    </div>
  ),
}],
['lastUpdateDate', {
  label: <Tooltip title="最近更新时间">最近更新时间</Tooltip>,
  width: 170,
  name: 'lastUpdateDate',
  sortable: true,
}],
['creationDate', {
  label: <Tooltip title="创建时间">创建时间</Tooltip>,
  width: 170,
  name: 'creationDate',
  sortable: true,
}],
['estimatedStartTime', {
  label: <Tooltip title="预计开始时间">预计开始时间</Tooltip>,
  width: 170,
  name: 'estimatedStartTime',
  sortable: true,
}],
['estimatedEndTime', {
  label: <Tooltip title="预计结束时间">预计结束时间</Tooltip>,
  width: 170,
  name: 'estimatedEndTime',
  sortable: true,
}],
['actualStartTime', {
  label: <Tooltip title="实际开始时间">实际开始时间</Tooltip>,
  width: 170,
  name: 'actualStartTime',
  sortable: true,
}],
['actualEndTime', {
  label: <Tooltip title="实际结束时间">实际结束时间</Tooltip>,
  width: 170,
  name: 'actualEndTime',
  sortable: true,
}],
['remainingTime', {
  label: <Tooltip title="剩余预估时间">剩余预估时间</Tooltip>,
  width: 170,
  name: 'remainingTime',
  sortable: true,
}],
['spentWorkTime', {
  label: <Tooltip title="已耗费时间">已耗费时间</Tooltip>,
  width: 170,
  name: 'spentWorkTime',
}],
['allEstimateTime', {
  label: <Tooltip title="总预估时间">总预估时间</Tooltip>,
  width: 170,
  name: 'allEstimateTime',
}],
['label', {
  label: <Tooltip title="标签">标签</Tooltip>,
  name: 'label',
  render: renderTag('labels', 'labelName'),
}],
['component', {
  label: <Tooltip title="模块">模块</Tooltip>,
  name: 'component',
  render: renderTag('components', 'name'),
}],
['fixVersion', {
  label: <Tooltip title="修复的版本">修复的版本</Tooltip>,
  name: 'fixVersion',
  render: renderTag('fixVersion', 'name'),
}],
['influenceVersion', {
  label: <Tooltip title="影响的版本">影响的版本</Tooltip>,
  name: 'influenceVersion',
  render: renderTag('influenceVersion', 'name'),
}],
['sprint', {
  label: <Tooltip title="冲刺">冲刺</Tooltip>,
  name: 'sprint',
  render: renderTag('sprints', 'sprintName'),
}],
['storyPoints', {
  label: <Tooltip title="故事点">故事点</Tooltip>,
  name: 'storyPoints',
  render: (rowData: any) => rowData.storyPoints ?? '-',
  sortable: true,
}],
['feature', {
  label: <Tooltip title="特性">特性</Tooltip>,
  name: 'featureId',
  render: (row: any) => renderEpicOrFeature(row, 'featureId'),
  sortable: true,
}],
['epic', {
  label: <Tooltip title="史诗">史诗</Tooltip>,
  name: 'epicId',
  render: (row: any) => renderEpicOrFeature(row, 'epicId'),
  sortable: true,
}],
['mainResponsibleUser', {
  label: <Tooltip title="主要负责人">主要负责人</Tooltip>,
  name: 'mainResponsibleId',
  render: (rowData: any) => rowData.mainResponsibleUser && <UserTag data={rowData.mainResponsibleUser} />,
  sortable: true,
}],
['environmentName', {
  label: <Tooltip title="环境">环境</Tooltip>,
  name: 'environment',
  render: (rowData: any) => rowData.environmentName,
  sortable: true,
}],
['tags', {
  label: <Tooltip title="Tag">Tag</Tooltip>,
  name: 'tags',
  render: (rowData: any) => {
    const tagShowText = rowData.tags && rowData.tags.map((tag: any) => `${tag.appServiceCode}:${tag.tagName}`).join('、');
    return tagShowText ? <Tooltip title={tagShowText}>{tagShowText}</Tooltip> : '';
  },
}]]);
const getCustomColumn = (field?: IFoundationHeader) => (field && {
  label: <Tooltip title={field.title}>{field.title}</Tooltip>,
  name: `foundation.${field.code}`,
  sortable: !(field.fieldType === 'multiple' || field.fieldType === 'checkbox' || field.fieldType === 'multiMember'),
  render: (rowData: any) => {
    const { fieldType, code } = field;
    const value = get(get(rowData, 'foundationFieldValue') || {}, code);
    if (['member', 'multiMember'].includes(fieldType)) {
      return value && (
        <div style={{ display: 'inline-flex', verticalAlign: 'middle', height: 40 }}>
          <UserTag
            data={value}
          />
        </div>
      );
    }
    return (
      <Tooltip title={value || ''}>
        <span>{value || ''}</span>
      </Tooltip>
    );
  },
});
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
const getTableColumns = (visibleColumns: ListLayoutColumnVO[], tableFields: IFoundationHeader[], { onSortChange }: any, openCreateSubIssue: (parentIssue: Issue) => void, onCreateAfter: (createId: number, createSuccessData?: { subIssue: Issue, parentIssueId: string }, flagFailed?: boolean) => void) => {
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
    return merge(baseColumn, getCustomColumn(find(tableFields, { code: columnCode })));
  }));
  return tableColumns;
};
const defaultVisibleColumns = ['assignee', 'estimatedStartTime', 'estimatedEndTime'];
const defaultListLayoutColumns = defaultVisibleColumns.map((code) => ({
  columnCode: code,
  display: true,
}));
const GanttPage: React.FC<TableCacheRenderProps> = ({ cached }) => {
  const { isInProgram } = useIsInProgram();
  const [data, setData] = useState<any[]>([]);
  const [isCreate, setIsCreate] = useState(false);
  const [type, setType] = useState<TypeValue>(localPageCacheStore.getItem('gantt.search.type') ?? typeValues[0]);
  const [columns, setColumns] = useState<Gantt.Column[]>([]);
  // const mutation = useUpdateColumnMutation('gantt');
  const { data: tableFields } = useIssueTableFields({ hiddenFieldCodes: ['epicSelfName', 'summary'] });
  const listLayoutColumns = useMemo(() => getListLayoutColumns(cached?.listLayoutColumns || defaultListLayoutColumns as any, tableFields || []), [cached?.listLayoutColumns, tableFields]);
  const visibleColumnCodes = useMemo(() => (listLayoutColumns.filter((c) => c.display).map((c) => c.columnCode)), [listLayoutColumns]);
  const [{ data: sortedList }, sortLabelProps] = useGanttSortLabel();

  const [workCalendar, setWorkCalendar] = useState<any>();
  const [{ source: draggingBar, destination: draggingBarDestinationBar }, setDraggingBar] = useState<{ source?: Gantt.Bar, destination?: Gantt.Bar }>({} as any);
  const draggingStyle = useRef<React.CSSProperties>();
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
  const getFilter = useCallback(() => {
    const filter = issueSearchStore.getCustomFieldFilters();
    filter.otherArgs.sprint = sprintIds;
    filter.searchArgs.dimension = type;
    return merge(filter, {
      displayFieldCodes: visibleColumnCodes,
      searchArgs: { tree: type !== 'assignee', dimension: type },
    });
  }, [issueSearchStore, sprintIds, type, visibleColumnCodes]);
  const { run, flush } = useDebounceFn(() => {
    (async () => {
      const year = dayjs().year();
      if (sprintIds === null) {
        return;
      }
      setLoading(true);
      const [workCalendarRes, projectWorkCalendarRes, res] = await Promise.all([
        workCalendarApi.getWorkSetting(year),
        workCalendarApi.getYearCalendar(year),
        ganttApi.loadByTask(getFilter(), sortedList),
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
        setData(res);
        setLoading(false);
      });
    })();
  });
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
    featureId: issue.featureId,
    epicId: issue.epicId,
  });

  const handleCreateIssue = usePersistFn((issue: Issue, issueId?: string, parentId?: string, dontCopyEpic = false) => {
    setData(produce(data, (draft) => {
      const normalizeIssueWidthParentId = Object.assign(normalizeIssue(issue), { parentId });
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

  const tableWithSortedColumns = useMemo(() => getTableColumns(listLayoutColumns.filter((item) => item.display), tableFields || [], sortLabelProps, handleQuickCreateSubIssue, handleQuickCreateSubIssueAfter), [handleQuickCreateSubIssue, handleQuickCreateSubIssueAfter, listLayoutColumns, sortLabelProps, tableFields]);

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
    if (type === 'assignee') {
      return groupByUser(data);
    } if (type === 'sprint') {
      return groupBySprint(data);
    } if (type === 'task') {
      return groupByTask(data);
    } if (type === 'epic') {
      const formattedData = formatData(data);
      return groupByEpic(formattedData, isInProgram);
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

  const renderTableBody = useCallback((Component: React.ReactElement) => (
    <Droppable
      droppableId="table"
      direction="vertical"
      mode="virtual"
      renderClone={(provider, snapshot, rubric) => {
        const record = draggingBar?.record;
        return (
          <div
            ref={provider.innerRef}
            {...provider.draggableProps}
            {...provider.draggableProps}
          >
            {record && tableWithSortedColumns[0]?.render!(record)}
          </div>
        );
      }}
    >
      {(provided, dropSnapshot) => {
        const { children } = Component.props;
        provided.innerRef((Component as any).ref?.current);
        return React.cloneElement(Component, {
          ...Component.props,
          // ref: provided.innerRef,
          ...provided.droppableProps,
          style: { ...Component.props.style, background: dropSnapshot.isDraggingOver ? '#F1F3F6' : undefined } as React.CSSProperties,
        });
      }}
    </Droppable>
  ), [draggingBar?.record, tableWithSortedColumns]);

  /**
   * 获取拖拽行样式
   */
  const getDragRowStyle = useCallback((style: React.CSSProperties, bar: Gantt.Bar, snapshot: DraggableStateSnapshot, dragStyle?: React.CSSProperties): React.CSSProperties => {
    const baseStyle: React.CSSProperties = { ...style };
    function isNeedDraggingStyle(nextBar?: Gantt.Bar): boolean {
      if (nextBar?._parent?.key === draggingBarDestinationBar?.key) {
        return true;
      }
      if (nextBar && nextBar._depth > draggingBarDestinationBar!._depth && nextBar?._parent) {
        return isNeedDraggingStyle(nextBar?._parent._bar!);
      }
      return false;
    }
    if (draggingBar && draggingBarDestinationBar) {
      baseStyle.cursor = 'not-allowed';
      if (bar.key === draggingBarDestinationBar.key) {
        merge(baseStyle, draggingStyle.current);
        return baseStyle;
      }
      if (isDragRowDrop(draggingBar, bar)) {
        merge(baseStyle, dragStyle);
        baseStyle.cursor = undefined;
      } else if (isNeedDraggingStyle(bar)) {
        console.log('need DraggingStyle', bar.record.summary);
        merge(baseStyle, draggingStyle.current);
      }
      // 如果
    }
    return baseStyle;
  }, [draggingBar, draggingBarDestinationBar]);
  const renderTableRow = useCallback((row: React.ReactElement, bar: Gantt.Bar) => (
    <Draggable
      key={`drag-${bar.absoluteIndex}`}
      draggableId={String(bar.absoluteIndex)}
      isDragDisabled={isDisableDrag(bar)}
      index={bar.absoluteIndex}
    >
      {(provided, snapshot) => React.cloneElement(row, {
        ...provided.dragHandleProps,
        ...provided.draggableProps,
        ref: provided.innerRef,
        style: getDragRowStyle(row.props?.style, bar, snapshot, provided.draggableProps?.style),
      })}
    </Draggable>
  ), [getDragRowStyle]);
  const handleDragStart = useCallback((initial: DragStart, provided: ResponderProvided) => {
    const dragBar = store.ganttRef.current?.flattenData[initial.source.index];
    runInAction(() => {
      if (dragBar) {
        dragBar.task.collapsed = true;
        setDraggingBar({ source: dragBar });
      }
    });
  }, [store.ganttRef]);

  const handleDragEnd = useCallback((result: DropResult, provider: ResponderProvided) => {
    setDraggingBar({} as any);
    if (!result.destination || result.destination.index === result.source.index) {
      return;
    }
    const flattenData = store.ganttRef.current?.flattenData || [];
    const destinationData = flattenData[result.destination.index];
    const sourceData = flattenData[result.source.index];
    if (!destinationData || !sourceData || !isDragRowDrop(sourceData, destinationData)) {
      return;
    }
    console.log(sourceData.record.summary, 'move--->', destinationData.record.summary);
    function getInstanceObject() {
      const instanceId = sourceData._depth > 0 ? sourceData._parent?.record.issueId : 0;
      switch (type) {
        case 'task':
          return {
            instanceType: 'task',
            instanceId,
          };
        case 'assignee': {
          return sourceData._depth === 1 ? {
            instanceType: 'assignee',
            instanceId: sourceData._parent?.record.assigneeId,
          } : {
            instanceType: 'task',
            instanceId,
          };
        }
        case 'sprint': {
          return sourceData._depth === 1 ? {
            instanceType: 'sprint',
            instanceId: sourceData._parent?.record.sprintId,
          } : {
            instanceType: 'task',
            instanceId,
          };
        }
        case 'epic': {
          if (sourceData._depth === 1) {
            return {
              instanceType: 'epic',
              instanceId: sourceData._parent?.record.issueId,
            };
          }
          return sourceData._parent?._depth === 1 && sourceData._parent?.record.groupType === 'feature' ? {
            instanceType: 'feature',
            instanceId: sourceData._parent?.record.issueId,
          } : {
            instanceType: 'task',
            instanceId,
          };
        }
        default:
          break;
      }
      return {
        instanceType: 'task',
        instanceId: '0',
      };
    }
    //  是否有上层级
    const instanceObject = getInstanceObject();
    function getSameDepthBar(depth: number, bar?: Gantt.Bar): Gantt.Bar | undefined {
      console.log('getSameDepthBar', bar?._depth, bar?._depth === depth);
      return bar?._depth === depth ? bar : undefined;
    }
    // 上一个 下一个
    const previousAndNextIdObject = {} as IGanttMoveRequestDataPreviousWithNext;
    if (sourceData.absoluteIndex > destinationData.absoluteIndex) {
      const previousRecord = getSameDepthBar(sourceData._depth, flattenData[result.destination.index - 1])?.record;
      previousAndNextIdObject.previousId = previousRecord?.issueId;
      previousAndNextIdObject.nextId = destinationData.record.issueId;
    } else {
      previousAndNextIdObject.previousId = destinationData.record.issueId;
      previousAndNextIdObject.nextId = getSameDepthBar(sourceData._depth, flattenData[result.destination.index + 1])?.record.issueId;
    }

    const requestData: IGanttMoveRequestData = {
      dimension: type,
      currentId: sourceData.record.issueId,
      ...instanceObject,
      ...previousAndNextIdObject,
    };
    setLoading(true);
    // 这里对 data 原数据进行移动，避免创建删除等操作导致排序混乱
    setData(produce(data, (draft) => {
      // 问题移动
      const sourceDataIndex = findIndex(draft, { issueId: sourceData.record.issueId });
      const destinationDataIndx = findIndex(draft, { issueId: destinationData.record.issueId });
      if ((sourceDataIndex + destinationDataIndx) >= 0) {
        const startIndex = destinationDataIndx < 0 ? draft.length + destinationDataIndx : destinationDataIndx;
        const delItem = draft.splice(sourceDataIndex, 1)[0];
        draft.splice(startIndex, 0, delItem);
      }
      // 冲刺移动

      // 经办人移动
    }));
    ganttApi.move(requestData, getFilter()).then(() => {
      setLoading(false);
    });
  }, [data, getFilter, store.ganttRef, type]);
  const handleDragUpdate = useCallback((initial: DragUpdate, provided: ResponderProvided) => {
    setDraggingBar((oldValue) => {
      if (!oldValue.source || oldValue.destination?.absoluteIndex === initial.destination?.index) {
        return oldValue;
      }
      if (!initial.destination) {
        return { source: oldValue.source, destination: undefined };
      }
      const destinationBar = store.ganttRef.current?.flattenData[initial.destination.index];
      // 移动是同层级进行移动，因此这里获取的目标节点应该是同层级的,当目标节点为高层级别的，则无效
      const newDestination = getDestinationBar(oldValue.source._depth, destinationBar);
      console.log(oldValue.source.record.summary, oldValue.source._depth, 'drag===>', newDestination, newDestination?.record.summary);
      // runInAction(() => {
      //   if (newDestination?.task) {
      //     newDestination.task.collapsed = true;
      //   }
      // });
      draggingStyle.current = newDestination && { transform: `translate(0px, ${oldValue.source.absoluteIndex > newDestination.absoluteIndex ? 34 : -34}px)` };

      return { source: oldValue.source, destination: newDestination };
    });
  }, [store.ganttRef]);

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
        <Context.Provider value={{ store }}>
          <div style={{ display: 'flex', flexWrap: 'wrap' }}>
            <Search issueSearchStore={issueSearchStore} loadData={run} />
            <GanttOperation />
          </div>
          <Loading loading={loading} />
          {columns.length > 0 && workCalendar && (
            <div className="c7n-gantt-content-body">
              <DragDropContext
                onBeforeDragStart={handleDragStart}
                onDragEnd={handleDragEnd}
                onDragUpdate={handleDragUpdate}
              >
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
                  components={{ tableBody: renderTableBody, tableRow: renderTableRow }}
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
              </DragDropContext>
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
const GanttPageHOC = () => (
  <TableCache type="gantt">
    {(cacheProps) => <ObserverGanttPage {...cacheProps} />}
  </TableCache>
);
export default GanttPageHOC;
