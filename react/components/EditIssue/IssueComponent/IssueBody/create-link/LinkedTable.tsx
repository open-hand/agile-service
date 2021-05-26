import React, {
  useRef, useMemo, useImperativeHandle, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Form, DataSet, Table, TextField,
} from 'choerodon-ui/pro';
import { Icon } from 'choerodon-ui';
import renderSummary from '@/components/column-renderer/summary';
import renderStatus from '@/components/column-renderer/status';
import renderPriority from '@/components/column-renderer/priority';
import renderSprint from '@/components/column-renderer/sprint';
import SelectStatus from '@/components/select/select-status';
import SelectPriority from '@/components/select/select-priority';
import SelectUser from '@/components/select/select-user';
import { issueApiConfig, statusApi } from '@/api';
import SelectIssueType from '@/components/select/select-issue-type';

const { Column } = Table;
interface Props {
  issueId: string
  linkedTableRef: React.MutableRefObject<{
    linkedIssues: string[] | undefined,
  } | undefined>
}

const LinkedTable: React.FC<Props> = ({ issueId, linkedTableRef }) => {
  const [, setUpdateCount] = useState<number>(0);
  const dataSetRef = useRef<DataSet>();
  const queryDataSet = useMemo(() => new DataSet({
    fields: [{
      name: 'content',
      label: '请输入搜索内容',
    }, {
      name: 'status',
      label: '状态',
      multiple: true,
    }, {
      name: 'priority',
      label: '优先级',
      multiple: true,
    }, {
      name: 'assignee',
      label: '经办人',
      multiple: true,
    }, {
      name: 'issueType',
      label: '问题类型',
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
      read: ({ params, data }) => issueApiConfig.getUnLinkedIssues(issueId, {
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
  }), [issueId, queryDataSet]);
  dataSetRef.current = dataSet;

  useImperativeHandle(linkedTableRef, () => ({
    linkedIssues: dataSet.selected?.map((record) => record.get('issueId')),
  }));

  return (
    <Table
      dataSet={dataSet}
      queryBar={() => (
        <Form dataSet={queryDataSet} columns={5}>
          <TextField name="content" prefix={<Icon type="search" />} />
          <SelectIssueType name="issueType" />
          <SelectStatus name="status" request={() => statusApi.loadByProject('agile')} />
          <SelectPriority name="priority" clearButton />
          <SelectUser name="assignee" clearButton />
        </Form>
      )}
    >
      <Column name="summary" renderer={({ record }) => renderSummary({ record, clickable: false })} />
      <Column name="issueNum" sortable width={135} />
      <Column name="statusId" sortable width={135} renderer={renderStatus} />
      <Column name="priorityId" sortable width={80} renderer={renderPriority} />
      <Column name="sprintId" sortable width={135} renderer={renderSprint} />
    </Table>
  );
};

export default observer(LinkedTable);
