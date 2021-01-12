import React, {
  useContext, Fragment,
} from 'react';
import { observer } from 'mobx-react-lite';
import _, { map } from 'lodash';
import { Tooltip, Tag } from 'choerodon-ui';
import { Table } from 'choerodon-ui/pro';

import QuickCreateIssue from '@/components/QuickCreateIssue';
import PriorityTag from '@/components/PriorityTag';
import TypeTag from '@/components/TypeTag';
import StatusTag from '@/components/StatusTag';
import UserHead from '@/components/UserHead';
import IssueStore from '@/stores/project/issue/IssueStore';
import IsInProgramStore from '@/stores/common/program/IsInProgramStore';
import CollapseAll from './CollapseAll';
import Store from '../../stores';
import './index.less';

const { Column } = Table;
function IssueTable({ tableRef, onCreateIssue }) {
  const {
    dataSet, fields,
  } = useContext(Store);
  const { isInProgram } = IsInProgramStore;
  const handleRowClick = (record) => {
    // dataSet.select(record);
    const editFilterInfo = IssueStore.getEditFilterInfo;
    IssueStore.setClickedRow({
      selectedIssue: {
        issueId: record.get('issueId'),
      },
      expand: true,
    });
    IssueStore.setFilterListVisible(false);
    IssueStore.setEditFilterInfo(map(editFilterInfo, (item) => Object.assign(item, { isEditing: false })));
  };
  const renderTag = (listField, nameField) => ({ record }) => {
    const list = record.get(listField);
    if (list) {
      if (list.length > 0) {
        return (
          <Tooltip title={<div>{_.map(list, (item) => item[nameField]).map((name) => <div>{name}</div>)}</div>}>
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
    const style = {
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
    };
    return name ? <Tooltip title={name}><span style={style}>{name}</span></Tooltip> : null;
  }

  return (
    <div className="c7nagile-issue-table">
      <Table
        mode="tree"
        ref={tableRef}
        dataSet={dataSet}
        footer={<div style={{ paddingTop: 5 }}><QuickCreateIssue onCreate={onCreateIssue} /></div>}
        onRow={({ record }) => ({
          className: IssueStore.selectedIssue.issueId && record.get('issueId') === IssueStore.selectedIssue.issueId ? 'c7nagile-row-selected' : null,
        })}
      >
        <Column
          align="left"
          lock="left"
          name="issueId"
          width={400}
          header={() => (
            <div style={{ display: 'inline-block' }}>
              <CollapseAll tableRef={tableRef} />
              概要
            </div>
          )}
          onCell={({ record }) => ({
            onClick: () => {
              handleRowClick(record);
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
            <Tooltip title={record.get('statusVO').name}>
              <div style={{
                display: 'inline-flex',
                overflow: 'hidden',
              }}
              >
                <StatusTag
                  data={record.get('statusVO')}
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
              {record?.get('reporterId') && record.get('reporterId') !== '0' && (
                <UserHead
                  user={{
                    id: record.get('reporterId'),
                    name: record.get('reporterName'),
                    loginName: record.get('reporterLoginName'),
                    realName: record.get('reporterRealName'),
                    avatar: record.get('reporterImageUrl'),
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
              const value = record.get('foundationFieldValue')[code];
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
}
export default observer(IssueTable);
