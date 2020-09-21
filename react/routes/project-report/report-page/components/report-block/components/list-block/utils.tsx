import React from 'react';
import TypeTag from '@/components/TypeTag';
import StatusTag from '@/components/StatusTag';
import PriorityTag from '@/components/PriorityTag';
import { Issue, IIssueColumnName } from '@/common/types';
import type { Column } from './table';

type TreeShapeData<T> = T & {
  children: TreeShapeData<T>[]
}
interface Config {
  parentKey?: string
  idKey?: string
}
export function flat2tree<T extends { [key: string]: any }>(flattered: T[], {
  parentKey = 'parentId',
  idKey = 'id',
}: Config = {} as Config): TreeShapeData<T>[] {
  const map = new Map<any, TreeShapeData<T>>();
  const result: TreeShapeData<T>[] = [];
  for (let i = 0; i < flattered.length; i += 1) {
    map.set(flattered[i][idKey], { ...flattered[i], children: [] });
  }
  for (let i = 0; i < flattered.length; i += 1) {
    const item = flattered[i];
    if (!item[parentKey]) {
      result.push(map.get(item[idKey]) as TreeShapeData<T>);
    } else {
      const parent = map.get(item[parentKey]);
      if (parent) {
        parent.children.push(map.get(item[idKey]) as TreeShapeData<T>);
      }
    }
  }
  return result;
}
const renderTag = (listField: string, nameField: string) => (record: { [key: string]: any }) => {
  const list = record.listField;
  if (list) {
    if (list.length > 0) {
      return list.map((item: any) => item.nameField).join(',');
    }
  }
  return null;
};
export function getColumnByName(name: IIssueColumnName): Column<Issue> | undefined {
  const map = new Map<IIssueColumnName, Column<Issue>>([
    ['summary', {
      title: '概要',
      dataIndex: 'summary',
      render: (item) => (
        <div style={{ display: 'inline-flex', alignItems: 'center' }}>
          <TypeTag data={item.issueTypeVO} />
          <span style={{ marginLeft: 5 }}>{item.summary}</span>
        </div>
      ),
    }],
    ['issueNum', {
      title: '编号',
      dataIndex: 'issueNum',
    }],
    ['priority', {
      title: '优先级',
      dataIndex: 'priorityVO',
      render: (issue) => <PriorityTag priority={issue.priorityVO} />,
    }],
    ['assign', {
      title: '经办人',
      dataIndex: 'assign',
      render: (item) => item.assigneeRealName,
    }],
    ['reporter', {
      title: '报告人',
      dataIndex: 'assign',
      render: (item) => item.reporterRealName,
    }],
    ['status', {
      title: '状态',
      dataIndex: 'status',
      render: (item) => (
        <StatusTag
          data={item.statusVO}
          style={{
            display: 'inline-block',
          }}
        />
      ),
    }],
    ['creationDate', {
      title: '创建时间',
      dataIndex: 'creationDate',
    }],
    ['lastUpdateDate', {
      title: '最后更新时间',
      dataIndex: 'lastUpdateDate',
    }],
    ['estimatedStartTime', {
      title: '预计开始时间',
      dataIndex: 'estimatedStartTime',
    }],
    ['estimatedEndTime', {
      title: '预计结束时间',
      dataIndex: 'estimatedEndTime',
    }],
    ['feature', {
      title: '特性',
      dataIndex: 'feature',
      render: ({ featureColor, featureName }) => (
        featureName && (
          <span style={{
            width: '100%',
            color: featureColor,
            borderWidth: '1px',
            borderStyle: 'solid',
            borderColor: featureColor,
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
            {featureName}
          </span>
        )
      ),
    }],
    ['epic', {
      title: '史诗',
      dataIndex: 'epic',
      render: ({ epicColor, epicName }) => (
        epicName && (
          <span style={{
            width: '100%',
            color: epicColor,
            borderWidth: '1px',
            borderStyle: 'solid',
            borderColor: epicColor,
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
            {epicName}
          </span>
        )
      ),
    }],
    ['label', {
      title: '史诗',
      dataIndex: 'epic',
      render: renderTag('labelIssueRelVOS', 'labelName'),
    }],
    ['component', {
      title: '模块',
      dataIndex: 'component',
      render: renderTag('issueComponentBriefVOS', 'name'),
    }],
    ['version', {
      title: '版本',
      dataIndex: 'version',
      render: renderTag('versionIssueRelVOS', 'name'),
    }],
    ['sprint', {
      title: '冲刺',
      dataIndex: 'sprint',
      render: renderTag('issueSprintVOS', 'sprintName'),
    }],
    ['storyPoints', {
      title: '故事点',
      dataIndex: 'storyPoints',
    }],
  ]);
  return map.get(name);
}
