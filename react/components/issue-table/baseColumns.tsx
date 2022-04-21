import React from 'react';
import {
  map, get, find, intersection,
} from 'lodash';
import { Tag } from 'choerodon-ui';
import { CheckBox, Tooltip } from 'choerodon-ui/pro';
import moment from 'moment';
import PriorityTag from '@/components/PriorityTag';
import TypeTag from '@/components/TypeTag';
import StatusTag from '@/components/StatusTag';
import UserTag from '../tag/user-tag';
import { IFoundationHeader } from '@/common/types';
import { MINUTE } from '@/constants/DATE_FORMAT';
import GanttPredecessor from '@/routes/gantt/components/gantt-predecessor';

type IIssueTableBaseColumnRenderGetData<T> = (data: T, nameKey: string) => any
export interface IIssueTableBaseColumn<D extends object = any> {
  /** 标题 */
  title: React.ReactNode
  /** 对应的多语言 key */
  titleKey?: string
  // originTitle: string
  dataIndex: string
  // name: string
  width?: number
  className?: string
  fixed?: boolean
  /** 能否排序 */
  sortable?: boolean
  render?: (data: D, getDataMethod?: IIssueTableBaseColumnRenderGetData<D>, ...args: any) => React.ReactElement | string
}
/**
 * 获取自定义列配置
 * @param field
 * @returns
 */
function getCustomColumn(field: IFoundationHeader): IIssueTableBaseColumn {
  return {
    title: <Tooltip title={field.title}>{field.title}</Tooltip>,
    dataIndex: `foundation.${field.code}`,
    sortable: !(field.fieldType === 'multiple' || field.fieldType === 'checkbox' || field.fieldType === 'multiMember'),
    render: (rowData, getDataMethod = get) => {
      const { fieldType, code } = field;
      const value = get(getDataMethod(rowData, 'foundationFieldValue') || {}, code);
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
  };
}

const renderFormatDate = (fieldName: string) => (rowData: any, getDataMethod = get): string => {
  const date = getDataMethod(rowData, fieldName);
  if (date && moment(date).isValid()) {
    return moment(date).format(MINUTE);
  }
  return date;
};

const renderTag = (listField: string, nameField: string) => (rowData: any, getDataMethod = get): React.ReactElement => {
  const list = getDataMethod(rowData, listField);
  if (list) {
    if (list.length > 0) {
      return (
        <Tooltip title={<div>{map(list, (item) => item[nameField]).map((name) => <div>{name}</div>)}</div>}>
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
  return <></>;
};
const renderEpicOrFeature = (fieldName: string) => (rowData: any, getDataMethod = get): React.ReactElement => {
  const color = fieldName === 'epicId' ? getDataMethod(rowData, 'epicColor') : getDataMethod(rowData, 'featureColor');
  const name = fieldName === 'epicId' ? getDataMethod(rowData, 'epicName') : getDataMethod(rowData, 'featureName');
  return name ? (
    <Tooltip title={name}>
      <span style={{
        maxWidth: '100%',
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
  ) : <></>;
};
const systemColumnsMap = new Map<string, IIssueTableBaseColumn>([
  ['summary', {
    title: <Tooltip title="概要">概要</Tooltip>,
    dataIndex: 'summary',
    titleKey: 'agile.systemField.summary',
    width: 400,
    fixed: true,
    sortable: true,
    render: (rowData, getDataMethod = get, summaryProps?: any, typeTagProps?: any) => (
      <>
        <TypeTag data={getDataMethod(rowData, 'issueTypeVO')} style={{ marginRight: 5, marginTop: -1 }} {...typeTagProps} />
        <Tooltip mouseEnterDelay={0.5} placement="topLeft" title={`工作项概要： ${getDataMethod(rowData, 'summary')}`}>
          <span role="none" className="c7n-agile-table-cell-click" {...summaryProps}>
            {getDataMethod(rowData, 'summary')}
          </span>
        </Tooltip>
      </>
    ),
  }],
  ['issueNum', {
    title: <Tooltip title="编号">编号</Tooltip>,
    titleKey: 'agile.common.key',
    dataIndex: 'issueNum',
    width: 120,
    className: 'c7n-agile-table-cell',
    sortable: true,
  }],
  ['priority', {
    title: <Tooltip title="优先级">优先级</Tooltip>,
    titleKey: 'agile.systemField.priority',
    dataIndex: 'priorityId',
    className: 'c7n-agile-table-cell',
    sortable: true,
    render: (rowData, getDataMethod = get) => (
      <Tooltip mouseEnterDelay={0.5} title={`优先级： ${getDataMethod(rowData, 'priorityDTO') ? getDataMethod(rowData, 'priorityDTO').name : ''}`}>
        <PriorityTag
          priority={getDataMethod(rowData, 'priorityVO')}
          style={{ display: 'inline-flex' }}
        />
      </Tooltip>
    ),
  }],
  ['assignee', {
    title: <Tooltip title="经办人">经办人</Tooltip>,
    titleKey: 'agile.systemField.assignee',
    dataIndex: 'assigneeId',
    sortable: true,
    render: (rowData, getDataMethod = get) => (
      <div style={{ display: 'inline-flex' }}>
        {getDataMethod(rowData, 'assigneeId') && getDataMethod(rowData, 'assigneeId') !== '0' && (
          <UserTag
            data={{
              id: getDataMethod(rowData, 'assigneeId'),
              tooltip: getDataMethod(rowData, 'assigneeName'),
              loginName: getDataMethod(rowData, 'assigneeLoginName'),
              realName: getDataMethod(rowData, 'assigneeRealName'),
              imageUrl: getDataMethod(rowData, 'assigneeImageUrl'),
            }}
          />
        )}
      </div>
    ),
  }],
  ['createUser', {
    title: <Tooltip title="创建人">创建人</Tooltip>,
    titleKey: 'agile.systemField.creator',
    dataIndex: 'createdBy',
    render: (rowData, getDataMethod = get) => (
      <div style={{ display: 'inline-flex' }}>
        {getDataMethod(rowData, 'createUser') && (
          <UserTag
            data={getDataMethod(rowData, 'createUser')}
          />
        )}
      </div>
    ),
    sortable: true,
  }],
  ['updateUser', {
    title: <Tooltip title="更新人">更新人</Tooltip>,
    titleKey: 'agile.systemField.updater',
    dataIndex: 'lastUpdatedBy',
    render: (rowData, getDataMethod = get) => (
      <div style={{ display: 'inline-flex' }}>
        {getDataMethod(rowData, 'updateUser') && (
          <UserTag
            data={getDataMethod(rowData, 'updateUser')}
          />
        )}
      </div>
    ),
    sortable: true,
  }],
  ['status', {
    title: <Tooltip title="状态">状态</Tooltip>,
    titleKey: 'agile.systemField.status',
    dataIndex: 'statusId',
    sortable: true,
    render: (rowData, getDataMethod = get) => (
      <Tooltip title={getDataMethod(rowData, 'statusVO')?.name}>
        <div style={{
          display: 'inline-flex',
          overflow: 'hidden',
          maxWidth: '100%',
        }}
        >
          <StatusTag
            data={getDataMethod(rowData, 'statusVO')}
            style={{ display: 'inline-flex' }}
          />
        </div>
      </Tooltip>
    ),
  }],
  ['reporter', {
    title: <Tooltip title="报告人">报告人</Tooltip>,
    titleKey: 'agile.systemField.reporter',
    dataIndex: 'reporterId',
    sortable: true,
    render: (rowData, getDataMethod = get) => (
      <div style={{ display: 'inline-flex' }}>
        {getDataMethod(rowData, 'reporterId') && getDataMethod(rowData, 'reporterId') !== '0' && (
          <UserTag
            data={{
              id: getDataMethod(rowData, 'reporterId'),
              tooltip: getDataMethod(rowData, 'reporterName'),
              loginName: getDataMethod(rowData, 'reporterLoginName'),
              realName: getDataMethod(rowData, 'reporterRealName'),
              imageUrl: getDataMethod(rowData, 'reporterImageUrl'),
            }}
          />
        )}
      </div>
    ),
  }],
  ['lastUpdateDate', {
    title: <Tooltip title="最近更新时间">最近更新时间</Tooltip>,
    titleKey: 'agile.columnField.lastUpdateDate',
    width: 170,
    dataIndex: 'lastUpdateDate',
    sortable: true,
  }],
  ['creationDate', {
    title: <Tooltip title="创建时间">创建时间</Tooltip>,
    titleKey: 'agile.systemField.createDate',
    width: 170,
    dataIndex: 'creationDate',
    sortable: true,
  }],
  ['estimatedStartTime', {
    title: <Tooltip title="预计开始时间">预计开始时间</Tooltip>,
    titleKey: 'agile.systemField.estimatedStartTime',
    width: 170,
    dataIndex: 'estimatedStartTime',
    sortable: true,
    render: renderFormatDate('estimatedStartTime'),
  }],
  ['estimatedEndTime', {
    title: <Tooltip title="预计结束时间">预计结束时间</Tooltip>,
    titleKey: 'agile.systemField.estimatedEndTime',
    width: 170,
    dataIndex: 'estimatedEndTime',
    sortable: true,
    render: renderFormatDate('estimatedEndTime'),
  }],
  ['actualStartTime', {
    title: <Tooltip title="实际开始时间">实际开始时间</Tooltip>,
    titleKey: 'agile.systemField.actualStartTime',
    width: 170,
    dataIndex: 'actualStartTime',
    sortable: true,
    render: renderFormatDate('actualStartTime'),
  }],
  ['actualEndTime', {
    title: <Tooltip title="实际结束时间">实际结束时间</Tooltip>,
    titleKey: 'agile.systemField.actualEndTime',
    width: 170,
    dataIndex: 'actualEndTime',
    sortable: true,
    render: renderFormatDate('actualEndTime'),
  }],
  ['remainingTime', {
    title: <Tooltip title="剩余预估时间">剩余预估时间</Tooltip>,
    titleKey: 'agile.systemField.remainingTime',
    width: 170,
    dataIndex: 'remainingTime',
    sortable: true,
  }],
  ['spentWorkTime', {
    title: <Tooltip title="已耗费时间">已耗费时间</Tooltip>,
    titleKey: 'agile.columnField.spentWorkTime',
    width: 170,
    dataIndex: 'spentWorkTime',
  }],
  ['allEstimateTime', {
    title: <Tooltip title="当前预估时间">当前预估时间</Tooltip>,
    titleKey: 'agile.columnField.allEstimateTime',
    width: 170,
    dataIndex: 'allEstimateTime',
  }],
  ['estimateTime', {
    title: <Tooltip title="原始预估时间">原始预估时间</Tooltip>,
    titleKey: 'agile.systemField.estimateTime',
    width: 140,
    dataIndex: 'estimateTime',
    sortable: true,
    render: (rowData, getDataMethod = get) => (
      <div style={{ display: 'inline-flex' }}>
        {getDataMethod(rowData, 'estimateTime') ? `${getDataMethod(rowData, 'estimateTime')}h` : ''}
      </div>
    ),
  }],
  ['label', {
    title: <Tooltip title="标签">标签</Tooltip>,
    titleKey: 'agile.systemField.label',
    dataIndex: 'label',
    render: renderTag('labelIssueRelVOS', 'labelName'),
  }],
  ['component', {
    title: <Tooltip title="模块">模块</Tooltip>,
    titleKey: 'agile.systemField.component',
    dataIndex: 'component',
    render: renderTag('issueComponentBriefVOS', 'name'),
  }],
  ['fixVersion', {
    title: <Tooltip title="修复的版本">修复的版本</Tooltip>,
    titleKey: 'agile.systemField.fixVersion',
    dataIndex: 'fixVersion',
    render: renderTag('fixVersionIssueRelVOS', 'name'),
  }],
  ['influenceVersion', {
    title: <Tooltip title="影响的版本">影响的版本</Tooltip>,
    titleKey: 'agile.systemField.influenceVersion',
    dataIndex: 'influenceVersion',
    render: renderTag('influenceVersionIssueRelVOS', 'name'),
  }],
  ['sprint', {
    title: <Tooltip title="冲刺">冲刺</Tooltip>,
    titleKey: 'agile.systemField.sprint',
    dataIndex: 'sprint',
    render: renderTag('issueSprintVOS', 'sprintName'),
  }],
  ['storyPoints', {
    title: <Tooltip title="故事点">故事点</Tooltip>,
    titleKey: 'agile.systemField.storyPoint',
    dataIndex: 'storyPoints',
    render: (rowData, getDataMethod = get) => getDataMethod(rowData, 'storyPoints') ?? '-',
    sortable: true,
  }],
  ['feature', {
    title: <Tooltip title="特性">特性</Tooltip>,
    titleKey: 'agile.common.feature',
    dataIndex: 'featureId',
    render: renderEpicOrFeature('featureId'),
    sortable: true,
  }],
  ['epic', {
    title: <Tooltip title="史诗">史诗</Tooltip>,
    titleKey: 'agile.common.epic',
    dataIndex: 'epicId',
    render: renderEpicOrFeature('epicId'),
    sortable: true,
  }],
  ['mainResponsibleUser', {
    title: <Tooltip title="主要负责人">主要负责人</Tooltip>,
    titleKey: 'agile.systemField.mainResponsible',
    dataIndex: 'mainResponsibleId',
    render: (rowData, getDataMethod = get) => getDataMethod(rowData, 'mainResponsibleUser') && <UserTag data={getDataMethod(rowData, 'mainResponsibleUser')} />,
    sortable: true,
  }],
  ['environmentName', {
    title: <Tooltip title="环境">环境</Tooltip>,
    titleKey: 'agile.systemField.environment',
    dataIndex: 'environment',
    render: (rowData) => rowData.environmentName,
    sortable: true,
  }],
  ['tags', {
    title: <Tooltip title="Tag">Tag</Tooltip>,
    titleKey: 'agile.systemField.tag',
    dataIndex: 'tags',
    render: (rowData, getDataMethod = get) => {
      const tagShowText = getDataMethod(rowData, 'tags')?.map((tag: any) => `${tag.appServiceCode}:${tag.tagName}`).join('、');
      return tagShowText ? <Tooltip title={tagShowText}>{tagShowText}</Tooltip> : <></>;
    },
  }],
  ['epicSelfName', {
    title: <Tooltip title="史诗名称">史诗名称</Tooltip>,
    titleKey: 'agile.columnField.epicSelfName',
    width: 150,
    dataIndex: 'epicSelfName',
    render: (rowData, getDataMethod = get) => (getDataMethod(rowData, 'epicSelfName') ? <Tooltip title={getDataMethod(rowData, 'epicSelfName')}>{getDataMethod(rowData, 'epicSelfName')}</Tooltip> : <></>),
  }],
  ['participants', {
    title: <Tooltip title="参与人">参与人</Tooltip>,
    titleKey: 'agile.systemField.participant',
    dataIndex: 'participants',
    render: (rowData, getDataMethod = get) => getDataMethod(rowData, 'participants') && <UserTag data={getDataMethod(rowData, 'participants')} />,
  }],
  ['workTime', {
    title: <Tooltip title="工时">工时</Tooltip>,
    titleKey: 'agile.columnField.workTime',
    width: 90,
    dataIndex: 'workTime',
    render: (rowData, getDataMethod = get) => (
      <div style={{ display: 'inline-flex' }}>
        {`${getDataMethod(rowData, 'workTime')}h`}
      </div>
    ),
  }],
  ['predecessor', {
    title: <Tooltip title="前置依赖">前置依赖</Tooltip>,
    titleKey: 'agile.gantt.column.predecessor',
    width: 120,
    sortable: false,
    dataIndex: 'predecessors',
    render: (rowData, getDataMethod = get) => {
      const predecessors = getDataMethod(rowData, 'predecessors');
      const projectId = getDataMethod(rowData, 'projectId');
      return predecessors?.length ? <GanttPredecessor data={predecessors} projectId={projectId} /> : <></>;
    },
  }],
  ['cumulativeWorkTime', {
    title: <Tooltip title="历史累计工时">历史累计工时</Tooltip>,
    titleKey: 'agile.columnField.cumulativeWorkTime',
    width: 120,
    dataIndex: 'cumulativeWorkTime',
    render: (rowData, getDataMethod = get) => (
      <div style={{ display: 'inline-flex' }}>
        {`${getDataMethod(rowData, 'cumulativeWorkTime')}h`}
      </div>
    ),
  }],
  ['deviationRate', {
    title: <Tooltip title="偏差率">偏差率</Tooltip>,
    titleKey: 'agile.columnField.deviationRate',
    width: 100,
    dataIndex: 'deviationRate',
    render: (rowData, getDataMethod = get) => {
      const numberValue = Number(getDataMethod(rowData, 'deviationRate'));
      const percentValue = numberValue * 100;
      const toFixedNumber = percentValue.toString().split('.')[1] && percentValue.toString().split('.')[1].length > 1 ? percentValue.toFixed(2) : percentValue;
      return (
        <div style={{
          display: 'inline-flex',
          // eslint-disable-next-line no-nested-ternary
          color: numberValue > 0 ? '#00BFA5' : (
            numberValue === 0 ? 'var(--text-color)' : '#F76776'
          ),
        }}
        >
          {`${numberValue !== 0 ? `${toFixedNumber}%` : 0}`}
        </div>
      );
    },
  }],
]);

const BaseSystemColumnRender = {
  renderTag,
  renderEpicOrFeature,
};
export { systemColumnsMap, getCustomColumn, BaseSystemColumnRender };
