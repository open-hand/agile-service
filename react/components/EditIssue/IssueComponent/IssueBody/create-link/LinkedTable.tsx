import React, {
  useRef, useMemo, useImperativeHandle, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Form, DataSet, Table, TextField,
  Icon,
} from 'choerodon-ui/pro';

import renderSummary from '@/components/column-renderer/summary';
import renderStatus from '@/components/column-renderer/status';
import renderPriority from '@/components/column-renderer/priority';
import renderSprint from '@/components/column-renderer/sprint';
import SelectStatus from '@/components/select/select-status';
import SelectPriority from '@/components/select/select-priority';
import SelectUser from '@/components/select/select-user';
import { issueApiConfig, statusApi } from '@/api';
import SelectIssueType from '@/components/select/select-issue-type';
import styles from './LinkedTable.less';

const { Column } = Table;
interface Props {
  issueId: string
  linkedTableRef: React.MutableRefObject<{
    linkedIssues: string[] | undefined,
  } | undefined>
  projectId?: string
}

const LinkedTable: React.FC<Props> = ({ issueId, linkedTableRef, projectId }) => {
  const [, setUpdateCount] = useState<number>(0);
  const dataSetRef = useRef<DataSet>();
  const queryDataSet = useMemo(() => new DataSet({
    fields: [{
      name: 'content',
    }, {
      name: 'status',
      multiple: true,
    }, {
      name: 'priority',
      multiple: true,
    }, {
      name: 'assignee',
      multiple: true,
    }, {
      name: 'issueType',
      multiple: true,
    }],
    events: {
      update() {
        dataSetRef.current?.query();
      },
    },
  }), []);
  const dataSet = useMemo(() => new DataSet({
    autoQuery: true,
    cacheSelection: true,
    primaryKey: 'issueId',
    fields: [{
      name: 'summary',
      label: '概要',
    }, {
      name: 'issueNum',
      label: '任务编号',
    }, {
      name: 'statusId',
      label: '状态',
    }, {
      name: 'priorityId',
      label: '优先级',
    }, {
      name: 'sprintId',
      label: '冲刺',
    }],
    transport: {
      read: ({ params, data }) => issueApiConfig.project(projectId).getUnLinkedIssues(issueId, {
        contents: data.content ? [data.content] : undefined,
        advancedSearchArgs: {
          statusId: data.status,
          priorityId: data.priority,
          issueTypeId: data.issueType,
        },
        otherArgs: {
          assigneeId: data.assignee,
          sprint: data.sprint,
        },
        searchArgs: {
          tree: false,
        },
      }),
    },
    queryDataSet,
    events: {
      select: () => {
        setUpdateCount((count) => count + 1);
      },
      selectAll: () => {
        setUpdateCount((count) => count + 1);
      },
      unSelect: () => {
        setUpdateCount((count) => count + 1);
      },
      unSelectAll: () => {
        setUpdateCount((count) => count + 1);
      },
    },
  }), [issueId, projectId, queryDataSet]);
  dataSetRef.current = dataSet;

  useImperativeHandle(linkedTableRef, () => ({
    linkedIssues: dataSet.selected?.map((record) => record.get('issueId')),
  }));

  return (
    <Table
      className={styles.linkedTable}
      dataSet={dataSet}
      queryBar={() => (
        <Form dataSet={queryDataSet} style={{ marginTop: -10, marginBottom: -6 }} labelLayout={'none' as any}>
          <div style={{ display: 'flex', alignItems: 'center', flexWrap: 'wrap' }}>
            <TextField
              placeholder="请输入搜索内容"
              name="content"
              style={{ marginRight: 10, marginBottom: 10 }}
              prefix={<Icon type="search" />}
              valueChangeAction={'input' as any}
            />
            <SelectIssueType placeholder="工作项类型" flat name="issueType" dataSet={queryDataSet} style={{ marginRight: 10, marginBottom: 10 }} filterList={['issue_epic', 'sub_task', 'feature']} dropdownMatchSelectWidth={false} projectId={projectId} />
            <SelectStatus placeholder="状态" flat name="status" dataSet={queryDataSet} style={{ marginRight: 10, marginBottom: 10 }} request={() => statusApi.project(projectId).loadByProject('agile')} dropdownMatchSelectWidth={false} />
            <SelectPriority placeholder="优先级" flat name="priority" dataSet={queryDataSet} style={{ marginRight: 10, marginBottom: 10 }} clearButton dropdownMatchSelectWidth={false} projectId={projectId} />
            <SelectUser placeholder="经办人" flat name="assignee" dataSet={queryDataSet} style={{ marginRight: 10, marginBottom: 10 }} clearButton dropdownMatchSelectWidth={false} projectId={projectId} />
          </div>
        </Form>
      )}
    >
      <Column name="summary" renderer={({ record }) => renderSummary({ record, clickable: false })} />
      <Column name="issueNum" sortable width={135} />
      <Column name="statusId" sortable width={135} renderer={renderStatus} />
      <Column name="priorityId" sortable width={95} renderer={renderPriority} />
      <Column name="sprintId" sortable width={135} renderer={renderSprint} />
    </Table>
  );
};

export default observer(LinkedTable);
