// @ts-nocheck
import React from 'react';
import { observer } from 'mobx-react-lite';
import { map } from 'lodash';
import { Tooltip, Tag } from 'choerodon-ui';
import { Table, DataSet } from 'choerodon-ui/pro';
import QuickCreateIssue from '@/components/QuickCreateIssue';
import PriorityTag from '@/components/PriorityTag';
import TypeTag from '@/components/TypeTag';
import StatusTag from '@/components/StatusTag';
import UserHead from '@/components/UserHead';
import useIsInProgram from '@/hooks/useIsInProgram';
import { IField, IIssueColumnName } from '@/common/types';
import { TableMode, ColumnAlign, ColumnLock } from 'choerodon-ui/pro/lib/table/enum';
import { TableProps } from 'choerodon-ui/pro/lib/table/Table';
import './index.less';
import UserTag from '../tag/user-tag';

const { Column } = Table;
interface Props extends Partial<TableProps> {
  tableRef?: React.RefObject<any>
  onCreateIssue?: () => void
  dataSet: DataSet
  fields: IField[]
  onRowClick?: (record: any) => void
  selectedIssue?: string
  createIssue?: boolean
  visibleColumns?: IIssueColumnName[]
}
const mapper = (key: IIssueColumnName): string => ({
  summary: 'issueId',
  issueNum: 'issueNum',
  priority: 'priorityId',
  sprint: 'issueSprintVOS',
  reporter: 'reporterId',
  creationDate: 'creationDate',
  assign: 'assigneeId',
  status: 'statusId',
  lastUpdateDate: 'lastUpdateDate',
  estimatedStartTime: 'estimatedStartTime',
  estimatedEndTime: 'estimatedEndTime',
  label: 'label',
  component: 'component',
  storyPoints: 'storyPoints',
  fixVersion: 'fixVersion',
  influenceVersion: 'influenceVersion',
  epic: 'epic',
  feature: 'feature',
}[key] || key);
const defaultVisibleColumns: IIssueColumnName[] = [
  'summary',
  'issueNum',
  'priority',
  'assign',
  'status',
  'sprint',
  'reporter',
  'lastUpdateDate',
];
const IssueTable: React.FC<Props> = ({
  tableRef,
  onCreateIssue,
  dataSet,
  fields,
  onRowClick,
  selectedIssue,
  createIssue = true,
  visibleColumns = defaultVisibleColumns,
  ...otherProps
}) => {
  const { isInProgram } = useIsInProgram();
  const columnHidden = (name: string) => !visibleColumns.map((n) => mapper(n)).includes(name);
  const renderTag = (listField, nameField) => ({ record }) => {
    const list = record.get(listField);
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
  function renderEpicOrFeature({ record, name: fieldName }) {
    const color = fieldName === 'epic' ? record.get('epicColor') : record.get('featureColor');
    const name = fieldName === 'epic' ? record.get('epicName') : record.get('featureName');
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

  return (
    <div className="c7nagile-issue-table">
      <Table
        mode={'tree' as TableMode}
        ref={tableRef}
        dataSet={dataSet}
        footer={createIssue && <div style={{ paddingTop: 5 }}><QuickCreateIssue onCreate={onCreateIssue} /></div>}
        onRow={({ record }) => ({
          className: selectedIssue && record.get('issueId') === selectedIssue ? 'c7nagile-row-selected' : null,
        })}
        {...otherProps}
      >
        <Column
          align={'left' as ColumnAlign}
          lock={'left' as ColumnLock}
          name="issueId"
          hidden={columnHidden('issueId')}
          width={400}
          header={() => (
            <div className="c7nagile-issue-table-summary">
              概要
            </div>
          )}
          onCell={({ record }) => ({
            onClick: () => {
              if (onRowClick) {
                onRowClick(record);
              }
            },
          })}
          renderer={({ record }) => (
            <>
              <TypeTag data={record.get('issueTypeVO')} style={{ marginRight: 5, marginTop: -2 }} />
              <Tooltip mouseEnterDelay={0.5} placement="topLeft" title={`问题概要： ${record.get('summary')}`}>
                <span className="c7n-agile-table-cell-click">
                  {record.get('summary')}
                </span>
              </Tooltip>
            </>
          )}
        />
        <Column
          sortable
          name="issueNum"
          hidden={columnHidden('issueNum')}
          width={120}
          className="c7n-agile-table-cell"
        />
        <Column
          sortable
          name="priorityId"
          hidden={columnHidden('priorityId')}
          className="c7n-agile-table-cell"
          renderer={({ record }) => (
            <Tooltip mouseEnterDelay={0.5} title={`优先级： ${record.get('priorityDTO') ? record.get('priorityDTO').name : ''}`}>
              <PriorityTag
                priority={record.get('priorityVO')}
                style={{ display: 'inline-block' }}
              />
            </Tooltip>
          )}
        />
        <Column
          sortable
          name="assigneeId"
          hidden={columnHidden('assigneeId')}
          renderer={({ record }) => (
            <div style={{ display: 'inline-flex' }}>
              {
                record.get('assigneeId') && record.get('assigneeId') !== '0' && (
                  <UserHead
                    user={{
                      id: record.get('assigneeId'),
                      name: record.get('assigneeName'),
                      loginName: record.get('assigneeLoginName'),
                      realName: record.get('assigneeRealName'),
                      avatar: record.get('assigneeImageUrl'),
                    }}
                  />
                )
              }
            </div>
          )}
        />
        <Column
          name="createUser"
          hidden={columnHidden('createUser')}
          renderer={({ record }) => (
            <div style={{ display: 'inline-flex' }}>
              {
                record.get('createUser') && (
                  <UserHead
                    user={record.get('createUser')}
                  />
                )
              }
            </div>
          )}
        />
        <Column
          name="updateUser"
          hidden={columnHidden('updateUser')}
          renderer={({ record }) => (
            <div style={{ display: 'inline-flex' }}>
              {
                record.get('updateUser') && (
                  <UserHead
                    user={record.get('updateUser')}
                  />
                )
              }
            </div>
          )}
        />
        <Column
          sortable
          name="statusId"
          hidden={columnHidden('statusId')}
          renderer={({ record }) => (
            <Tooltip title={record?.get('statusVO')?.name}>
              <div style={{
                display: 'inline-flex',
                overflow: 'hidden',
              }}
              >
                <StatusTag
                  data={record?.get('statusVO')}
                  style={{ display: 'inline-block' }}
                />
              </div>
            </Tooltip>
          )}
        />
        <Column
          name="reporterId"
          hidden={columnHidden('reporterId')}
          sortable
          className="c7n-agile-table-cell"
          renderer={({ record }) => (
            <div style={{ display: 'inline-flex' }}>
              {record?.get('reporterId') && record?.get('reporterId') !== '0' && (
                <UserHead
                  user={{
                    id: record?.get('reporterId'),
                    name: record?.get('reporterName'),
                    loginName: record?.get('reporterLoginName'),
                    realName: record?.get('reporterRealName'),
                    avatar: record?.get('reporterImageUrl'),
                  }}
                />
              )}
            </div>
          )}
        />
        <Column
          sortable
          width={170}
          name="lastUpdateDate"
          hidden={columnHidden('lastUpdateDate')}
          className="c7n-agile-table-cell"
        />
        <Column
          width={170}
          hidden={columnHidden('creationDate')}
          name="creationDate"
          className="c7n-agile-table-cell"
        />
        <Column
          width={170}
          hidden={columnHidden('estimatedStartTime')}
          name="estimatedStartTime"
          className="c7n-agile-table-cell"
        />
        <Column
          width={170}
          hidden={columnHidden('estimatedEndTime')}
          name="estimatedEndTime"
          className="c7n-agile-table-cell"
        />
        <Column
          width={170}
          hidden={columnHidden('remainingTime')}
          name="remainingTime"
          className="c7n-agile-table-cell"
          renderer={({ text }) => (text ? `${text}小时` : '-')}
        />
        <Column hidden={columnHidden('label')} name="label" className="c7n-agile-table-cell" renderer={renderTag('labelIssueRelVOS', 'labelName')} />
        <Column hidden={columnHidden('component')} name="component" className="c7n-agile-table-cell" renderer={renderTag('issueComponentBriefVOS', 'name')} />
        <Column hidden={columnHidden('storyPoints')} name="storyPoints" className="c7n-agile-table-cell" renderer={({ text }) => text || '-'} />
        <Column hidden={columnHidden('fixVersion')} name="fixVersion" className="c7n-agile-table-cell" renderer={renderTag('fixVersionIssueRelVOS', 'name')} />
        <Column hidden={columnHidden('influenceVersion')} name="influenceVersion" className="c7n-agile-table-cell" renderer={renderTag('influenceVersionIssueRelVOS', 'name')} />
        <Column hidden={columnHidden('epic')} name="epic" className="c7n-agile-table-cell" renderer={renderEpicOrFeature} />
        {isInProgram && <Column hidden={columnHidden('feature')} name="feature" className="c7n-agile-table-cell" renderer={renderEpicOrFeature} />}
        <Column hidden={columnHidden('issueSprintVOS')} name="issueSprintVOS" renderer={renderTag('issueSprintVOS', 'sprintName')} />
        {fields.map((field) => (
          <Column
            hidden={columnHidden(field.code)}
            name={field.code}
            header={field.title || field.name}
            className="c7n-agile-table-cell"
            renderer={({ record }) => {
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
            }}
          />
        ))}
      </Table>
    </div>
  );
};
export default observer(IssueTable);
