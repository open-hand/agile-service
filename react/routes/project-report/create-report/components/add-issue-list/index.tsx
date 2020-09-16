import React, { useMemo, useCallback, useEffect } from 'react';
import {
  Form, DataSet, TextField, Select,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import IssueTable from '@/components/issue-table';
import IssueTableDataSet from '@/components/issue-table/dataSet';
import IssueSearch, { useIssueSearchStore } from '@/components/issue-search';
import { getProjectId, getOrganizationId } from '@/utils/common';
import {
  transformFilter,
} from '@/routes/Issue/stores/utils';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { TableQueryBarType } from 'choerodon-ui/pro/lib/table/enum';

const { Option } = Select;
const defaultVisibleColumns = [
  'issueId',
  'issueNum',
  'priorityId',
  'assigneeId',
  'statusId',
  'reporterId',
];
const AddIssueList: React.FC = () => {
  const issueSearchStore = useIssueSearchStore({
    // @ts-ignore
    getSystemFields,
    transformFilter,
  });
  const formDataSet = useMemo(() => new DataSet({
    data: [{ fields: defaultVisibleColumns }],
    fields: [{
      name: 'title',
      label: '列表标题',
      required: true,
    }, {
      name: 'fields',
      required: true,
      label: '列表显示字段',
    }],
  }), []);
  // @ts-ignore
  const dataSet = useMemo(() => new DataSet(IssueTableDataSet({
    projectId: getProjectId(),
    organizationId: getOrganizationId(),
    issueSearchStore,
  })), [issueSearchStore]);
  const refresh = useCallback(() => {
    dataSet.query();
  }, [dataSet]);
  useEffect(() => {
    refresh();
  }, [refresh]);
  return (
    <div className="agile-portal">
      <Form dataSet={formDataSet} style={{ width: 512 }}>
        <TextField name="title" />
        <Select name="fields" multiple help="为了保证最佳的预览效果，请将字段控制在6个以内">
          <Option value="issueId">概要</Option>
          <Option value="issueNum">编号</Option>
          <Option value="priorityId">优先级</Option>
          <Option value="assigneeId">经办人</Option>
          <Option value="statusId">状态</Option>
          <Option value="issueSprintVOS">冲刺</Option>
          <Option value="reporterId">报告人</Option>
          <Option value="creationDate">创建时间</Option>
          <Option value="lastUpdateDate">最后更新时间</Option>
          <Option value="estimatedStartTime">预计开始时间</Option>
          <Option value="estimatedEndTime">预计结束时间</Option>
          <Option value="label">标签</Option>
          <Option value="component">模块</Option>
          <Option value="storyPoints">故事点</Option>
          <Option value="version">版本</Option>
          <Option value="epic">史诗</Option>
          <Option value="feature">特性</Option>
          {issueSearchStore.fields.map((field) => (
            <Option value={field.code}>
              {field.name}
            </Option>
          ))}
        </Select>
      </Form>
      <IssueSearch
        store={issueSearchStore}
        onClear={refresh}
        onChange={refresh}
        onClickSaveFilter={() => { }}
      />
      <IssueTable
        queryBar={'none' as TableQueryBarType}
        dataSet={dataSet}
        fields={issueSearchStore.fields}
        createIssue={false}
        visibleColumns={formDataSet.current?.get('fields') || []}
      />
    </div>
  );
};
export default observer(AddIssueList);
