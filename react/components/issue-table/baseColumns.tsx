import React from 'react';
import {
  map, get, find, intersection,
} from 'lodash';
import { Tag } from 'choerodon-ui';
import { CheckBox, Tooltip } from 'choerodon-ui/pro';
import PriorityTag from '@/components/PriorityTag';
import TypeTag from '@/components/TypeTag';
import StatusTag from '@/components/StatusTag';
import UserTag from '../tag/user-tag';
import { IFoundationHeader } from '@/common/types';

type IIssueTableBaseColumnRenderGetData<T> = (data: T, nameKey: string) => any
interface IIssueTableBaseColumn<D extends object = any> {
  /** 标题 */
  title: React.ReactElement
  // originTitle: string
  dataIndex: string
  // name: string
  width?: number
  className?: string
  fixed?: boolean
  /** 能否排序 */
  sortable?: boolean
  render?: (data: D, getDataMethod?: IIssueTableBaseColumnRenderGetData<D>, ...args: any) => React.ReactElement
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
  ) : <></>;
};
const systemColumnsMap = new Map<string, IIssueTableBaseColumn>([
  ['summary', {
    title: <Tooltip title="概要">概要</Tooltip>,
    dataIndex: 'summary',
    width: 400,
    fixed: true,
    sortable: true,
    render: (rowData, getDataMethod = get, summaryProps?: any) => (
      <>
        <TypeTag data={getDataMethod(rowData, 'issueTypeVO')} style={{ marginRight: 5, marginTop: -2 }} />
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
    dataIndex: 'issueNum',
    width: 120,
    className: 'c7n-agile-table-cell',
    sortable: true,
  }],
  ['priority', {
    title: <Tooltip title="优先级">优先级</Tooltip>,
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
    dataIndex: 'assigneeId',
    sortable: true,
    render: (rowData, getDataMethod = get) => (
      <div style={{ display: 'inline-flex', height: 40 }}>
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
    dataIndex: 'createdBy',
    render: (rowData, getDataMethod = get) => (
      <div style={{ display: 'inline-flex', height: 40 }}>
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
    dataIndex: 'lastUpdatedBy',
    render: (rowData, getDataMethod = get) => (
      <div style={{ display: 'inline-flex', height: 40 }}>
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
    dataIndex: 'statusId',
    sortable: true,
    render: (rowData, getDataMethod = get) => (
      <Tooltip title={getDataMethod(rowData, 'statusVO')?.name}>
        <div style={{
          display: 'inline-flex',
          overflow: 'hidden',
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
    dataIndex: 'reporterId',
    sortable: true,
    render: (rowData, getDataMethod = get) => (
      <div style={{ display: 'inline-flex', height: 40 }}>
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
    width: 170,
    dataIndex: 'lastUpdateDate',
    sortable: true,
  }],
  ['creationDate', {
    title: <Tooltip title="创建时间">创建时间</Tooltip>,
    width: 170,
    dataIndex: 'creationDate',
    sortable: true,
  }],
  ['estimatedStartTime', {
    title: <Tooltip title="预计开始时间">预计开始时间</Tooltip>,
    width: 170,
    dataIndex: 'estimatedStartTime',
    sortable: true,
  }],
  ['estimatedEndTime', {
    title: <Tooltip title="预计结束时间">预计结束时间</Tooltip>,
    width: 170,
    dataIndex: 'estimatedEndTime',
    sortable: true,
  }],
  ['actualStartTime', {
    title: <Tooltip title="实际开始时间">实际开始时间</Tooltip>,
    width: 170,
    dataIndex: 'actualStartTime',
    sortable: true,
  }],
  ['actualEndTime', {
    title: <Tooltip title="实际结束时间">实际结束时间</Tooltip>,
    width: 170,
    dataIndex: 'actualEndTime',
    sortable: true,
  }],
  ['remainingTime', {
    title: <Tooltip title="剩余预估时间">剩余预估时间</Tooltip>,
    width: 170,
    dataIndex: 'remainingTime',
    sortable: true,
  }],
  ['spentWorkTime', {
    title: <Tooltip title="已耗费时间">已耗费时间</Tooltip>,
    width: 170,
    dataIndex: 'spentWorkTime',
  }],
  ['allEstimateTime', {
    title: <Tooltip title="当前预估时间">当前预估时间</Tooltip>,
    width: 170,
    dataIndex: 'allEstimateTime',
  }],
  ['estimateTime', {
    title: <Tooltip title="原始预估时间">原始预估时间</Tooltip>,
    width: 170,
    dataIndex: 'estimateTime',
    sortable: true,
  }],
  ['label', {
    title: <Tooltip title="标签">标签</Tooltip>,
    dataIndex: 'label',
    render: renderTag('labelIssueRelVOS', 'labelName'),
  }],
  ['component', {
    title: <Tooltip title="模块">模块</Tooltip>,
    dataIndex: 'component',
    render: renderTag('issueComponentBriefVOS', 'name'),
  }],
  ['fixVersion', {
    title: <Tooltip title="修复的版本">修复的版本</Tooltip>,
    dataIndex: 'fixVersion',
    render: renderTag('fixVersionIssueRelVOS', 'name'),
  }],
  ['influenceVersion', {
    title: <Tooltip title="影响的版本">影响的版本</Tooltip>,
    dataIndex: 'influenceVersion',
    render: renderTag('influenceVersionIssueRelVOS', 'name'),
  }],
  ['sprint', {
    title: <Tooltip title="冲刺">冲刺</Tooltip>,
    dataIndex: 'sprint',
    render: renderTag('issueSprintVOS', 'sprintName'),
  }],
  ['storyPoints', {
    title: <Tooltip title="故事点">故事点</Tooltip>,
    dataIndex: 'storyPoints',
    render: (rowData, getDataMethod = get) => getDataMethod(rowData, 'storyPoints') ?? '-',
    sortable: true,
  }],
  ['feature', {
    title: <Tooltip title="特性">特性</Tooltip>,
    dataIndex: 'featureId',
    render: renderEpicOrFeature('featureId'),
    sortable: true,
  }],
  ['epic', {
    title: <Tooltip title="史诗">史诗</Tooltip>,
    dataIndex: 'epicId',
    render: renderEpicOrFeature('epicId'),
    sortable: true,
  }],
  ['mainResponsibleUser', {
    title: <Tooltip title="主要负责人">主要负责人</Tooltip>,
    dataIndex: 'mainResponsibleId',
    render: (rowData, getDataMethod = get) => getDataMethod(rowData, 'mainResponsibleUser') && <UserTag data={getDataMethod(rowData, 'mainResponsibleUser')} />,
    sortable: true,
  }],
  ['environmentName', {
    title: <Tooltip title="环境">环境</Tooltip>,
    dataIndex: 'environment',
    render: (rowData) => rowData.environmentName,
    sortable: true,
  }],
  ['tags', {
    title: <Tooltip title="Tag">Tag</Tooltip>,
    dataIndex: 'tags',
    render: (rowData, getDataMethod = get) => {
      const tagShowText = getDataMethod(rowData, 'tags')?.map((tag: any) => `${tag.appServiceCode}:${tag.tagName}`).join('、');
      return tagShowText ? <Tooltip title={tagShowText}>{tagShowText}</Tooltip> : <></>;
    },
  }],
  ['epicSelfName', {
    title: <Tooltip title="史诗名称">史诗名称</Tooltip>,
    width: 150,
    dataIndex: 'epicSelfName',
    render: (rowData, getDataMethod = get) => (getDataMethod(rowData, 'epicSelfName') ? <Tooltip title={getDataMethod(rowData, 'epicSelfName')}>{getDataMethod(rowData, 'epicSelfName')}</Tooltip> : <></>),
  }],
  ['participants', {
    title: <Tooltip title="参与人">参与人</Tooltip>,
    dataIndex: 'participants',
    render: (rowData, getDataMethod = get) => getDataMethod(rowData, 'participants') && <UserTag data={getDataMethod(rowData, 'participants')} />,
  }],
]);

const BaseSystemColumnRender = {
  renderTag,
  renderEpicOrFeature,
};
export { systemColumnsMap, getCustomColumn, BaseSystemColumnRender };
