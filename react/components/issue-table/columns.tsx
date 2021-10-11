// @ts-nocheck
import React from 'react';
import {
  map, get, find, intersection,
} from 'lodash';
import { Tag } from 'choerodon-ui';
import { CheckBox, Tooltip } from 'choerodon-ui/pro';
import PriorityTag from '@/components/PriorityTag';
import TypeTag from '@/components/TypeTag';
import StatusTag from '@/components/StatusTag';
import './index.less';
import UserTag from '../tag/user-tag';

export const checkBoxColumn = ({
  checkValues, data, handleCheckChange, handleCheckAllChange, checkValuesRef,
}) => {
  const keys = data.map((i) => i.issueId);
  const pageCheckedKeys = intersection(keys, checkValues);
  const checked = pageCheckedKeys.length > 0;
  const allChecked = pageCheckedKeys.length === keys.length;
  const indeterminate = checked && !allChecked;
  return ({
    title: (
      <CheckBox
        indeterminate={indeterminate}
        checked={checked}
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
        checked={checkValuesRef.current.includes(rowData.issueId)}
        onChange={(value) => handleCheckChange(value, rowData.issueId)}
      />
    ),
  });
};
export const expandColumn = {
  title: '',
  dataIndex: 'issueId',
  key: 'issueId',
  className: 'expand-column',
  width: 10,
  fixed: true,
  treeCol: true,
  render: () => null,
};
export const getCustomColumn = (field) => (field && {
  title: <Tooltip title={field.title}>{field.title}</Tooltip>,
  dataIndex: `foundation.${field.code}`,
  sortable: !(field.fieldType === 'multiple' || field.fieldType === 'checkbox' || field.fieldType === 'multiMember'),
  render: ({ rowData, dataIndex, rowIndex }) => {
    const { fieldType, code } = field;
    const value = get(rowData, 'foundationFieldValue')[code];
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
const getColumnsMap = ({ onSummaryClick }) => new Map([
  ['summary', {
    title: <Tooltip title="概要">概要</Tooltip>,
    dataIndex: 'summary',
    width: 400,
    fixed: true,
    sortable: true,
    render: ({ rowData }) => (
      <>
        <TypeTag data={get(rowData, 'issueTypeVO')} style={{ marginRight: 5, marginTop: -2 }} />
        <Tooltip mouseEnterDelay={0.5} placement="topLeft" title={`工作项概要： ${get(rowData, 'summary')}`}>
          <span role="none" className="c7n-agile-table-cell-click" onClick={() => onSummaryClick(rowData)}>
            {get(rowData, 'summary')}
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
    render: ({ rowData }) => (
      <Tooltip mouseEnterDelay={0.5} title={`优先级： ${get(rowData, 'priorityDTO') ? get(rowData, 'priorityDTO').name : ''}`}>
        <PriorityTag
          priority={get(rowData, 'priorityVO')}
          style={{ display: 'inline-flex' }}
        />
      </Tooltip>
    ),
  }],
  ['assignee', {
    title: <Tooltip title="经办人">经办人</Tooltip>,
    dataIndex: 'assigneeId',
    sortable: true,
    render: ({ rowData }) => (
      <div style={{ display: 'inline-flex', height: 40 }}>
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
    title: <Tooltip title="创建人">创建人</Tooltip>,
    dataIndex: 'createdBy',
    render: ({ rowData }) => (
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
    title: <Tooltip title="更新人">更新人</Tooltip>,
    dataIndex: 'lastUpdatedBy',
    render: ({ rowData }) => (
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
    title: <Tooltip title="状态">状态</Tooltip>,
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
    render: ({ rowData }) => (
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
    title: <Tooltip title="总预估时间">总预估时间</Tooltip>,
    width: 170,
    dataIndex: 'allEstimateTime',
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
    render: ({ rowData, dataIndex }) => rowData[dataIndex] ?? '-',
    sortable: true,
  }],
  ['feature', {
    title: <Tooltip title="特性">特性</Tooltip>,
    dataIndex: 'featureId',
    render: renderEpicOrFeature,
    sortable: true,
  }],
  ['epic', {
    title: <Tooltip title="史诗">史诗</Tooltip>,
    dataIndex: 'epicId',
    render: renderEpicOrFeature,
    sortable: true,
  }],
  ['mainResponsibleUser', {
    title: <Tooltip title="主要负责人">主要负责人</Tooltip>,
    dataIndex: 'mainResponsibleId',
    render: ({ rowData }) => rowData.mainResponsibleUser && <UserTag data={rowData.mainResponsibleUser} />,
    sortable: true,
  }],
  ['environmentName', {
    title: <Tooltip title="环境">环境</Tooltip>,
    dataIndex: 'environment',
    render: ({ rowData }) => rowData.environmentName,
    sortable: true,
  }],
  ['tags', {
    title: <Tooltip title="Tag">Tag</Tooltip>,
    dataIndex: 'tags',
    render: ({ rowData, dataIndex }) => {
      const tagShowText = rowData[dataIndex] && rowData[dataIndex].map((tag: any) => `${tag.appServiceCode}:${tag.tagName}`).join('、');
      return tagShowText ? <Tooltip title={tagShowText}>{tagShowText}</Tooltip> : '';
    },
  }],
  ['epicSelfName', {
    title: <Tooltip title="史诗名称">史诗名称</Tooltip>,
    width: 150,
    dataIndex: 'epicSelfName',
    render: ({ rowData, dataIndex }) => (rowData[dataIndex] ? <Tooltip title={rowData[dataIndex]}>{rowData[dataIndex]}</Tooltip> : ''),
  }],
]);

export function getTableColumns({
  listLayoutColumns, fields, onSummaryClick, handleColumnResize,
}) {
  const res = [];
  listLayoutColumns.forEach((layoutColumn) => {
    const { columnCode: code, width } = layoutColumn;
    const field = find(fields, { code });
    if (field) {
      const columnsMap = getColumnsMap({ onSummaryClick });
      // 系统字段和自定义字段处理
      const column = columnsMap.has(code) ? columnsMap.get(code) : getCustomColumn(field);
      res.push({
        ...column,
        code,
        display: layoutColumn.display,
        resizable: true,
        onResize: handleColumnResize,
        width: width && width > 0 ? width : column.width,
      });
    }
  });
  return res;
}
