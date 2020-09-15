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
import { IField } from '@/common/types';
import { TableMode, ColumnAlign, ColumnLock } from 'choerodon-ui/pro/lib/table/enum';
import { TableProps } from 'choerodon-ui/pro/lib/table/Table';
import './index.less';

const { Column } = Table;
interface Props extends Partial<TableProps> {
  tableRef?: React.RefObject<any>
  onCreateIssue?: () => void
  dataSet: DataSet
  fields: IField[]
  onRowClick?: (record: any) => void
  selectedIssue?: string
  createIssue?: boolean
}
const IssueTable: React.FC<Props> = ({
  tableRef,
  onCreateIssue,
  dataSet,
  fields,
  onRowClick,
  selectedIssue,
  createIssue = true,
}) => {
  const { isInProgram } = useIsInProgram();

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
      >
        <Column
          align={'left' as ColumnAlign}
          lock={'left' as ColumnLock}
          name="issueId"
          width={320}
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
          width={120}
          className="c7n-agile-table-cell"
        />
        <Column
          sortable
          name="priorityId"
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
          sortable
          name="statusId"
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
          className="c7n-agile-table-cell"
        />
        <Column
          width={170}
          hidden
          name="creationDate"
          className="c7n-agile-table-cell"
        />
        <Column
          width={170}
          hidden
          name="estimatedStartTime"
          className="c7n-agile-table-cell"
        />
        <Column
          width={170}
          hidden
          name="estimatedEndTime"
          className="c7n-agile-table-cell"
        />
        <Column hidden name="label" className="c7n-agile-table-cell" renderer={renderTag('labelIssueRelVOS', 'labelName')} />
        <Column hidden name="component" className="c7n-agile-table-cell" renderer={renderTag('issueComponentBriefVOS', 'name')} />
        <Column hidden name="storyPoints" className="c7n-agile-table-cell" renderer={({ text }) => text || '-'} />
        <Column hidden name="version" className="c7n-agile-table-cell" renderer={renderTag('versionIssueRelVOS', 'name')} />
        <Column hidden name="epic" className="c7n-agile-table-cell" renderer={renderEpicOrFeature} />
        {isInProgram && <Column hidden name="feature" className="c7n-agile-table-cell" renderer={renderEpicOrFeature} />}
        <Column name="issueSprintVOS" renderer={renderTag('issueSprintVOS', 'sprintName')} />
        {fields.map((field) => (
          <Column
            hidden
            name={field.code}
            header={field.title}
            className="c7n-agile-table-cell"
            renderer={({ record }) => {
              const { fieldType, code } = field;
              const value = record?.get('foundationFieldValue')[code];
              if (fieldType === 'member') {
                return value && (
                  <div style={{ display: 'inline-flex' }}>
                    <UserHead
                      user={value}
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
