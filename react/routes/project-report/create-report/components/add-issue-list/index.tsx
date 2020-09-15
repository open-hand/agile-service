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
const AddIssueList: React.FC = () => {
  const issueSearchStore = useIssueSearchStore({
    // @ts-ignore
    getSystemFields,
    transformFilter,
  });
  const formDataSet = useMemo(() => new DataSet({
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
    <>
      <Form dataSet={formDataSet} style={{ width: 512 }}>
        <TextField name="title" />
        <Select name="fields" multiple help="为了保证最佳的预览效果，请将字段控制在6个以内">
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
        onClickSaveFilter={() => {}}
      />
      <IssueTable
        queryBar={'none' as TableQueryBarType}
        dataSet={dataSet}
        fields={issueSearchStore.fields}
        createIssue={false}
      />
    </>
  );
};
export default observer(AddIssueList);
