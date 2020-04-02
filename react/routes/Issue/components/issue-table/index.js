import React, {
  useContext, Fragment,
} from 'react';
import { observer } from 'mobx-react-lite';
import { Tooltip, Tag } from 'choerodon-ui';
import { Table, Pagination } from 'choerodon-ui/pro';
import { map } from 'lodash';
import QuickCreateIssue from '@/components/QuickCreateIssue';
import PriorityTag from '@/components/PriorityTag';
import TypeTag from '@/components/TypeTag';
import StatusTag from '@/components/StatusTag';
import UserHead from '@/components/UserHead';
import IssueStore from '@/stores/project/sprint/IssueStore/IssueStore';
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
    IssueStore.setEditFilterInfo(map(editFilterInfo, item => Object.assign(item, { isEditing: false })));
  };
  const renderTag = (listField, nameField) => ({ record }) => {
    const list = record.get(listField);
    if (list) {
      if (list.length > 0) {
        return (
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
        );
      }
    }
    return null;
  };
  function renderEpicOrFeature({ record, name: fieldName }) {
    const color = fieldName === 'epic' ? record.get('epicColor') : record.get('featureColor');
    const name = fieldName === 'epic' ? record.get('epicName') : record.get('featureName');
    const style = {
      color,
      borderWidth: '1px',
      borderStyle: 'solid',
      borderColor: color,
      borderRadius: '2px',
      fontSize: '13px',
      lineHeight: '20px',
      padding: '0 8px',
      display: 'inline-block',
      overflow: 'hidden',
      textOverflow: 'ellipsis',
      whiteSpace: 'nowrap',
    };
    return name ? <span style={style}>{name}</span> : null;
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
          sortable          
          align="left"
          name="issueId"
          width={240}
          onCell={({ record }) => ({
            onClick: () => {
              handleRowClick(record);
            },
          })}
          renderer={({ record }) => (
            <Tooltip mouseEnterDelay={0.5} placement="topLeft" title={`问题概要： ${record.get('summary')}`}>
              <span className="c7n-agile-table-cell-click">
                {record.get('summary')}
              </span>
            </Tooltip>
          )}
        />
        <Column
          sortable
          name="issueTypeId"
          className="c7n-agile-table-cell"
          renderer={({ record }) => (<TypeTag data={record.get('issueTypeVO')} showName />)}
        />
        <Column sortable name="issueNum" className="c7n-agile-table-cell" />
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
          name="reporterId"
          className="c7n-agile-table-cell"
          renderer={({ record }) => (
            <div style={{ display: 'inline-flex' }}>
              {record.get('reporterId') && record.get('reporterId') !== '0' && (
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
          name="statusId"
          renderer={({ record }) => (
            <StatusTag
              data={record.get('statusVO')}
              style={{ display: 'inline-block' }}
            />
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
        <Column sortable name="lastUpdateDate" className="c7n-agile-table-cell" />
        <Column hidden name="label" className="c7n-agile-table-cell" renderer={renderTag('labelIssueRelVOS', 'labelName')} />
        <Column hidden name="component" className="c7n-agile-table-cell" renderer={renderTag('issueComponentBriefVOS', 'name')} />
        <Column hidden name="storyPoints" className="c7n-agile-table-cell" renderer={({ text }) => text || '-'} />
        <Column hidden name="version" className="c7n-agile-table-cell" renderer={renderTag('versionIssueRelVOS', 'name')} />
        <Column hidden name="epic" className="c7n-agile-table-cell" renderer={renderEpicOrFeature} />
        {isInProgram && <Column hidden name="feature" className="c7n-agile-table-cell" renderer={renderEpicOrFeature} />}
        <Column name="issueSprintVOS" renderer={renderTag('issueSprintVOS', 'sprintName')} />
        {fields.map(field => (
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
              return <span>{value || ''}</span>;
            }}
          />
        ))}
      </Table>
      <Pagination          
        pageSize={dataSet.issuePageSize}
        page={dataSet.issueCurrentPage}
        total={dataSet.issueTotal}
        style={{ float: 'right', marginTop: 10 }}
        onChange={(page, pageSize) => {
          if (dataSet.issuePageSize !== pageSize) {
            dataSet.issuePageSize = pageSize;
            dataSet.issueCurrentPage = 1;
            dataSet.query();
          } else {
            dataSet.issueCurrentPage = page;
            dataSet.query();
          }
        }}
      />       
      <CollapseAll tableRef={tableRef} />
    </div>    
  );
}
export default observer(IssueTable);
