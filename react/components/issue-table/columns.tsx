// @ts-nocheck
import React from 'react';
import { map, get } from 'lodash';
import { Tooltip, Tag } from 'choerodon-ui';
import { CheckBox } from 'choerodon-ui/pro';
import PriorityTag from '@/components/PriorityTag';
import TypeTag from '@/components/TypeTag';
import StatusTag from '@/components/StatusTag';
import './index.less';
import UserTag from '../tag/user-tag';

export const checkBoxColumn = ({
  checkValues, data, handleCheckChange, handleCheckAllChange,
}) => ({
  title: (
    <CheckBox
      indeterminate={checkValues.length > 0 && checkValues.length < data.length}
      checked={checkValues.length > 0 && checkValues.length === data.length}
      onChange={handleCheckAllChange}
    />
  ),
  dataIndex: 'issueId',
  key: 'issueId',
  width: 40,
  fixed: true,
  render: ({ rowData, dataIndex, rowIndex }) => (
    <CheckBox
      key={rowIndex}
      value={rowData.issueId}
      checked={checkValues.indexOf(rowData.issueId) !== -1}
      onChange={handleCheckChange}
    />
  ),
});
const normalColumn = (field) => (field && {
  title: field.title,
  dataIndex: field?.code,
  render: ({ rowData, dataIndex, rowIndex }) => {
    const { fieldType, code } = field;
    const value = get(rowData, 'foundationFieldValue')[code];
    if (['member', 'multiMember'].includes(fieldType)) {
      return value && (
        <div style={{ display: 'inline-flex' }}>
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
const renderTag = (listField, nameField) => ({ rowData }) => {
  const list = get(rowData, listField);
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
  return null;
};
function renderEpicOrFeature({ rowData, dataIndex: fieldName }) {
  const color = fieldName === 'epic' ? get(rowData, 'epicColor') : get(rowData, 'featureColor');
  const name = fieldName === 'epic' ? get(rowData, 'epicName') : get(rowData, 'featureName');
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
const columns = ({ onSummaryClick }) => new Map([
  ['summary', {
    title: '概要',
    dataIndex: 'summary',
    width: 400,
    fixed: true,
    treeCol: true,
    render: ({ rowData }) => (
      <>
        <TypeTag data={get(rowData, 'issueTypeVO')} style={{ marginRight: 5, marginTop: -2 }} />
        <Tooltip mouseEnterDelay={0.5} placement="topLeft" title={`问题概要： ${get(rowData, 'summary')}`}>
          <span role="none" className="c7n-agile-table-cell-click" onClick={() => onSummaryClick(rowData)}>
            {get(rowData, 'summary')}
          </span>
        </Tooltip>
      </>
    ),
  }],
  ['issueNum', {
    title: '编号',
    dataIndex: 'issueNum',
    width: 120,
    className: 'c7n-agile-table-cell',
    sortable: true,
  }],
  ['priority', {
    title: '优先级',
    dataIndex: 'priority',
    className: 'c7n-agile-table-cell',
    sortable: true,
    render: ({ rowData }) => (
      <Tooltip mouseEnterDelay={0.5} title={`优先级： ${get(rowData, 'priorityDTO') ? get(rowData, 'priorityDTO').name : ''}`}>
        <PriorityTag
          priority={get(rowData, 'priorityVO')}
          style={{ display: 'inline-block' }}
        />
      </Tooltip>
    ),
  }],
  ['assign', {
    title: '经办人',
    dataIndex: 'assigneeId',
    sortable: true,
    render: ({ rowData }) => (
      <div style={{ display: 'inline-flex' }}>
        {
          get(rowData, 'assigneeId') && get(rowData, 'assigneeId') !== '0' && (
            <UserTag
              data={{
                id: get(rowData, 'assigneeId'),
                tooltip: get(rowData, 'assigneeName'),
                loginName: get(rowData, 'assigneeLoginName'),
                realName: get(rowData, 'assigneeRealName'),
                imageUrl: get(rowData, 'assigneeImageUrl'),
              }}
            />
          )
        }
      </div>
    ),
  }],
  ['createUser', {
    title: '创建人',
    dataIndex: 'createUser',
    render: ({ rowData }) => (
      <div style={{ display: 'inline-flex' }}>
        {
          get(rowData, 'createUser') && (
            <UserTag
              data={get(rowData, 'createUser')}
            />
          )
        }
      </div>
    ),
  }],
  ['updateUser', {
    title: '更新人',
    dataIndex: 'updateUser',
    render: ({ rowData }) => (
      <div style={{ display: 'inline-flex' }}>
        {
          get(rowData, 'updateUser') && (
            <UserTag
              data={get(rowData, 'updateUser')}
            />
          )
        }
      </div>
    ),
  }],
  ['status', {
    title: '状态',
    dataIndex: 'statusId',
    sortable: true,
    render: ({ rowData }) => (
      <Tooltip title={get(rowData, 'statusVO')?.name}>
        <div style={{
          display: 'inline-flex',
          overflow: 'hidden',
        }}
        >
          <StatusTag
            data={get(rowData, 'statusVO')}
            style={{ display: 'inline-block' }}
          />
        </div>
      </Tooltip>
    ),
  }],
  ['reporter', {
    title: '报告人',
    dataIndex: 'reporterId',
    sortable: true,
    render: ({ rowData }) => (
      <div style={{ display: 'inline-flex' }}>
        {get('reporterId') && get(rowData, 'reporterId') !== '0' && (
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
    title: '最近更新时间',
    width: 170,
    dataIndex: 'lastUpdateDate',
    sortable: true,
  }],
  ['creationDate', {
    title: '创建时间',
    width: 170,
    dataIndex: 'creationDate',
  }],
  ['estimatedStartTime', {
    title: '预计开始时间',
    width: 170,
    dataIndex: 'estimatedStartTime',
  }],
  ['estimatedEndTime', {
    title: '预计结束时间',
    width: 170,
    dataIndex: 'estimatedEndTime',
  }],
  ['remainingTime', {
    title: '剩余预估时间',
    width: 170,
    dataIndex: 'remainingTime',
  }],
  ['spentWorkTime', {
    title: '已耗费时间',
    width: 170,
    dataIndex: 'spentWorkTime',
  }],
  ['allEstimateTime', {
    title: '总预估时间',
    width: 170,
    dataIndex: 'allEstimateTime',
  }],
  ['label', {
    title: '标签',
    dataIndex: 'label',
    render: renderTag('labelIssueRelVOS', 'labelName'),
  }],
  ['component', {
    title: '模块',
    dataIndex: 'component',
    render: renderTag('issueComponentBriefVOS', 'name'),
  }],
  ['fixVersion', {
    title: '修复的版本',
    dataIndex: 'fixVersion',
    render: renderTag('fixVersionIssueRelVOS', 'name'),
  }],
  ['influenceVersion', {
    title: '影响的版本',
    dataIndex: 'influenceVersion',
    render: renderTag('influenceVersionIssueRelVOS', 'name'),
  }],
  ['sprint', {
    title: '冲刺',
    dataIndex: 'issueSprintVOS',
    render: renderTag('issueSprintVOS', 'sprintName'),
  }],
  ['storyPoints', {
    title: '故事点',
    dataIndex: 'storyPoints',
    render: ({ rowData, dataIndex }) => rowData[dataIndex] ?? '-',
  }],
  ['feature', {
    title: '特性',
    dataIndex: 'feature',
    render: renderEpicOrFeature,
  }],
  ['epic', {
    title: '史诗',
    dataIndex: 'epic',
    render: renderEpicOrFeature,
  }],
  ['mainResponsibleUser', {
    title: '负责人',
    dataIndex: 'mainResponsibleUser',
    render: ({ rowData, dataIndex }) => rowData[dataIndex] && <UserTag data={rowData[dataIndex]} />,
  }],
  ['environmentName', {
    title: '环境',
    dataIndex: 'environmentName',
  }],
]);
export { normalColumn };
export default columns;
