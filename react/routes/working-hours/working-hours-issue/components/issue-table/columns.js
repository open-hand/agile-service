import React from 'react';

import { Table, Tooltip } from 'choerodon-ui/pro';
import { find } from 'lodash';
import UserTag from '@/components/tag/user-tag';
import TypeTag from '@/components/TypeTag';
import StatusTag from '@/components/StatusTag';
import ProjectTags from '@/components/tag/project-tags';
import TableProgress from '@/components/table-progress';
import renderTag from '@/components/column-renderer/tag';
import { Epic, Sprint } from './tags';
import featureTableStyles from './index.less';

const renderSubProjects = ({ value = [] }) => (
  <ProjectTags data={value} />
);
const renderVersions = ({ value }) => {
  let text = '';
  if (value && value.length > 0) {
    text = value.map((item, index, arr) => [` ${item.name}${index === arr.length - 1 ? '' : '，'}`, <br />]);
    return (
      <Tooltip title={text}>
        <span className={featureTableStyles.version}>{value.map((item) => item.name).join('，')}</span>
      </Tooltip>
    );
  }
  return text;
};
const { Column } = Table;
const tooltipHeader = (dataSet, name) => {
  const title = dataSet.getField(name).get('label');
  return (
    <Tooltip title={title}>
      {title}
    </Tooltip>
  );
};
const getColumnsMap = ({ onSummaryClick }) => new Map([
  ['summary', {
    name: 'summary',
    header: tooltipHeader,
    lock: 'left',
    width: 400,
    sortable: true,
    onCell: ({ record }) => ({
      onClick: () => {
        onSummaryClick(record);
      },
    }),
    renderer: ({ record }) => (
      <Tooltip mouseEnterDelay={0.5} placement="topLeft" title={`概要： ${record.get('summary')}`}>
        <span className="c7n-agile-table-cell-click">
          {record.get('summary')}
        </span>
      </Tooltip>
    ),
  }],
  ['issueNum', {
    width: 130,
    header: tooltipHeader,
    name: 'issueNum',
    sortable: true,
    className: 'c7n-agile-table-cell',
  }],
  ['issueTypeId', {
    name: 'issueTypeId',
    header: tooltipHeader,
    sortable: true,
    className: 'c7n-agile-table-cell',
    renderer: ({ record }) => (<TypeTag data={record.get('issueTypeVO')} featureType={record.get('featureType')} showName />),
  }],
  ['statusId', {
    name: 'statusId',
    header: tooltipHeader,
    sortable: true,
    renderer: ({ record }) => (
      <StatusTag
        data={record.get('statusVO')}
        style={{ display: 'inline-block' }}
      />
    ),
  }],
  ['progress', {
    name: 'progress',
    header: tooltipHeader,
    width: 150,
    renderer: ({ record }) => {
      const storyCount = record.get('storyCount') || 0;
      const completedStoryCount = record.get('completedStoryCount') || 0;
      return storyCount ? (
        <TableProgress
          percent={(completedStoryCount / (storyCount) * 100).toFixed(1)}
          tooltipTitles={[`已完成故事数：${completedStoryCount}`, `总故事数：${storyCount}`]}
        />
      ) : <Tooltip title="无故事">--</Tooltip>;
    },
  }],
  ['reporterId', {
    name: 'reporterId',
    header: tooltipHeader,
    sortable: true,
    renderer: ({ record }) => (
      record.get('reporterId') ? (
        <UserTag
          style={{ display: 'inline-flex' }}
          data={{
            id: record.get('reporterId'),
            tooltip: record.get('reporterName'),
            realName: record.get('reporterRealName'),
            loginName: record.get('reporterLoginName'),
            imageUrl: record.get('reporterImageUrl'),
          }}
        />
      ) : null
    ),
  }],
  ['createdBy', {
    name: 'createdBy',
    sortable: true,
    header: tooltipHeader,
    renderer: ({ record }) => (
      <div style={{ display: 'inline-flex' }}>
        {
          record.get('createUser') && (
            <UserTag
              data={record.get('createUser')}
            />
          )
        }
      </div>
    ),
  }],
  ['lastUpdatedBy', {
    name: 'lastUpdatedBy',
    sortable: true,
    header: tooltipHeader,
    renderer: ({ record }) => (
      <div style={{ display: 'inline-flex' }}>
        {
          record.get('updateUser') && (
            <UserTag
              data={record.get('updateUser')}
            />
          )
        }
      </div>
    ),
  }],
  ['epicId', {
    name: 'epicId',
    header: tooltipHeader,
    sortable: true,
    renderer: ({ record }) => (
      <Epic
        color={record.get('epicColor')}
        name={record.get('epicName')}
      />
    ),
  }],
  ['piNameVOList', {
    name: 'piNameVOList',
    header: tooltipHeader,
    // sortable
    renderer: ({ record }) => {
      const piNameVOList = record.get('piNameVOList');
      return (
        <Tooltip
          placement="top"
          title={piNameVOList.length ? (
            <div style={{ maxHeight: '3rem', overflow: 'auto' }}>
              {piNameVOList.map((o) => (
                <div>
                  PI:
                  {o.fullName || `${o.code}-${o.name}`}
                </div>
              ))}
            </div>
          ) : ''}
        >
          {Sprint({
            objArray: piNameVOList,
            name: piNameVOList.length > 0 && (piNameVOList[0].fullName || `${piNameVOList[0].code}-${piNameVOList[0].name}`),
          })}
        </Tooltip>
      );
    },
  }],
  ['sprints', {
    name: 'sprints',
    header: tooltipHeader,
    // sortable
    renderer: ({ record }) => {
      const sprints = record.get('sprints');
      if (!sprints) {
        return null;
      }
      return (
        <Tooltip
          placement="top"
          title={sprints.length ? sprints.map((o) => (
            <div>
              <div>
                PI:
                {o.piName ? o.fullName || `${o.piName.code}-${o.piName.name}` : '-'}
              </div>
              <div>
                冲刺:
                {o.sprintName}
              </div>
            </div>
          )) : ''}
        >
          {Sprint({ objArray: sprints, name: sprints.length > 0 && sprints[0].sprintName })}
        </Tooltip>
      );
    },
  }],
  ['programVersionFeatureRelVOS', {
    name: 'programVersionFeatureRelVOS',
    width: 150,
    className: 'c7n-agile-table-cell',
    renderer: renderVersions,
  }],
  ['wsjf', {
    name: 'wsjf',
    sortable: true,
    className: 'c7n-agile-table-cell',
  }],
  ['teamProjects', {
    width: 150,
    name: 'teamProjects',
    renderer: renderSubProjects,
  }],
  ['creationDate', {
    sortable: true,
    width: 150,
    name: 'creationDate',
    className: 'c7n-agile-table-cell',
  }],
  ['lastUpdateDate', {
    sortable: true,
    width: 150,
    name: 'lastUpdateDate',
    className: 'c7n-agile-table-cell',
  }],
  ['estimatedStartTime', {
    sortable: true,
    width: 150,
    name: 'estimatedStartTime',
    className: 'c7n-agile-table-cell',
  }],
  ['estimatedEndTime', {
    sortable: true,
    width: 150,
    name: 'estimatedEndTime',
    className: 'c7n-agile-table-cell',
  }],
  ['actualStartTime', {
    sortable: true,
    width: 150,
    name: 'actualStartTime',
    className: 'c7n-agile-table-cell',
  }],
  ['actualEndTime', {
    sortable: true,
    width: 150,
    name: 'actualEndTime',
    className: 'c7n-agile-table-cell',
  }],
  ['storyPoints', {
    name: 'storyPoints',
    className: 'c7n-agile-table-cell',
  }],
  ['benfitHypothesis', {
    sortable: true,
    name: 'benfitHypothesis',
    className: 'c7n-agile-table-cell',
  }],
  ['acceptanceCritera', {
    sortable: true,
    name: 'acceptanceCritera',
    className: 'c7n-agile-table-cell',
  }],
  ['tags', {
    name: 'tags',
    className: 'c7n-agile-table-cell',
    renderer: renderTag,
  }],
  ['epicSelfName', {
    name: 'epicSelfName',
    width: 150,
    renderer: ({ value }) => value && <Tooltip title={value}>{value}</Tooltip>,
    className: 'c7n-agile-table-cell',
  }],
]);
export const getCustomColumn = (field) => (field
  && ({
    name: `foundation.${field.code}`,
    sortable: !(field.fieldType === 'multiple' || field.fieldType === 'checkbox' || field.fieldType === 'multiMember'),
    className: 'c7n-agile-table-cell',
    renderer: ({ record }) => {
      const { fieldType, code } = field;
      const value = record?.get('foundationFieldValue')[code];
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
  }));
export function getTableColumns({
  listLayoutColumns, fields, onSummaryClick,
}) {
  const res = [];
  const columnsMap = getColumnsMap({ onSummaryClick });
  listLayoutColumns.forEach((layoutColumn) => {
    const { columnCode: code, width } = layoutColumn || {};
    const field = find(fields, { code });
    if (field) {
      // 系统字段和自定义字段处理
      const column = columnsMap.has(code) ? columnsMap.get(code) : getCustomColumn(field);
      res.push({
        header: tooltipHeader,
        ...column,
        hidden: !layoutColumn.display,
        width: width && width > 0 ? width : column.width,
      });
    }
  });
  return res;
}
