import React, {
  useMemo, useCallback, useEffect, useImperativeHandle, useRef,
} from 'react';
import {
  Form, DataSet, TextField, Select,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { Choerodon } from '@choerodon/boot';
import { TableQueryBarType } from 'choerodon-ui/pro/lib/table/enum';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import {
  set, isEmpty, map, some,
} from 'lodash';
import IssueTable from '@/components/issue-table';
import IssueSearch, { useIssueSearchStore } from '@/components/issue-search';
import {
  transformFilter,
} from '@/routes/Issue/stores/utils';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { IIssueColumnName } from '@/common/types';
import useIsInProgram from '@/hooks/useIsInProgram';
import { issueApi } from '@/api';
import useTable from '@/hooks/useTable';
import useIssueTableFields from '@/hooks/data/useIssueTableFields';
import { IReportListBlock } from '../../store';
import { RefProps } from '../add-modal';

const { Option } = Select;
const defaultVisibleColumns: IIssueColumnName[] = [
  'summary',
  'issueNum',
  'priority',
  'assignee',
  'status',
  'reporter',
];

const systemColumns = [
  { label: '概要', code: 'summary' },
  { label: '编号', code: 'issueNum' },
  { label: '优先级', code: 'priority' },
  { label: '经办人', code: 'assignee' },
  { label: '状态', code: 'status' },
  { label: '报告人', code: 'reporter' },
  { label: '冲刺', code: 'sprint' },
  { label: '创建人', code: 'createUser' },
  { label: '更新人', code: 'updateUser' },
  { label: '创建时间', code: 'creationDate' },
  { label: '剩余预估时间', code: 'remainingTime' },
  { label: '最后更新时间', code: 'lastUpdateDate' },
  { label: '预计开始时间', code: 'estimatedStartTime' },
  { label: '预计结束时间', code: 'estimatedEndTime' },
  { label: '标签', code: 'label' },
  { label: '模块', code: 'component' },
  { label: '故事点', code: 'storyPoints' },
  { label: '修复的版本', code: 'fixVersion' },
  { label: '影响的版本', code: 'influenceVersion' },
  { label: '史诗', code: 'epic' },
  { label: '主要负责人', code: 'mainResponsibleUser' },
  { label: '环境', code: 'environmentName' },
  { label: 'Tag', code: 'tags' },
];

const featureColumn = { label: '特性', code: 'feature' };

interface Props {
  innerRef: React.MutableRefObject<RefProps>
  data?: IReportListBlock
}
const autoSelectChildren = (dataSet: DataSet, record: Record) => {
  if (record.children && record.children.length > 0) {
    record.children.forEach((child) => {
      if (!child.isSelected) {
        child.set('source', 'auto');
        dataSet.select(child);
      }
    });
  }
};
const autoSelectParent = (dataSet: DataSet, record: Record) => {
  if (record.parent) {
    if (!record.parent.isSelected) {
      record.parent.set('source', 'auto');
      dataSet.select(record.parent);
    }
  }
};
const autoUnSelectChildren = (dataSet: DataSet, record: Record) => {
  if (record.children && record.children.length > 0) {
    record.children.forEach((child) => {
      if (child.isSelected) {
        child.set('source', 'auto');
        dataSet.unSelect(child);
      }
    });
  }
};
const autoUnSelectParent = (dataSet: DataSet, record: Record) => {

};
const AddIssueList: React.FC<Props> = ({ innerRef, data: editData }) => {
  const isEdit = Boolean(editData);
  const selectedRef = useRef<Set<string>>(new Set([...(editData?.searchVO?.otherArgs?.issueIds || [])]));
  const issueSearchStore = useIssueSearchStore({
    // @ts-ignore
    getSystemFields,
    transformFilter,
  });
  const formDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    data: editData ? [{ title: editData.title, visibleColumns: editData.colList }] : [{ visibleColumns: defaultVisibleColumns }],
    fields: [{
      name: 'title',
      label: '列表标题',
      maxLength: 44,
      required: true,
    }, {
      name: 'visibleColumns',
      required: true,
      label: '列表显示字段',
      validator: async (value) => {
        if (value && value.length > 6) {
          return '最多可选6个字段';
        }
        return true;
      },
    }],
  }), [editData]);

  const getTableData = useCallback(({ page, sort, size }) => {
    const search = issueSearchStore.getCustomFieldFilters();
    set(search, 'searchArgs.tree', false);
    return issueApi.loadIssues(page, size, sort, search);
  }, [issueSearchStore]);
  const tableProps = useTable(getTableData, {
    isTree: false,
    rowKey: 'issueId',
    defaultChecked: [...selectedRef.current],
  });
  const refresh = useCallback(() => {
    tableProps.query();
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [tableProps.query]);
  useEffect(() => {
    refresh();
  }, [refresh]);
  const handleSubmit = useCallback(async () => {
    if (await formDataSet.current?.validate(true)) {
      const data = formDataSet.current?.toData();
      const issueIds = tableProps.checkValues;
      if (issueIds.length === 0) {
        Choerodon.prompt('请至少勾选一个工作项');
        return false;
      }
      const block: IReportListBlock = {
        key: String(Math.random()),
        title: data.title,
        type: 'static_list',
        collapse: false,
        colList: data.visibleColumns,
        searchVO: {
          otherArgs: {
            issueIds,
          },
        },
      };
      return block;
    }
    return false;
  }, [formDataSet, tableProps.checkValues]);
  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  const { isInProgram } = useIsInProgram();

  useEffect(() => {
    if (isInProgram && !some(systemColumns, { code: 'feature' })) {
      systemColumns.splice(-3, 0, featureColumn);
    }
  }, [isInProgram]);

  const { data } = useIssueTableFields();
  if (!data) {
    return null;
  }
  return (
    <div className="agile-portal">
      <Form dataSet={formDataSet} style={{ width: 512 }}>
        <TextField name="title" />
        <Select
          name="visibleColumns"
          multiple // @ts-ignore
          help={(
            <div style={{ fontSize: '12px', color: 'var(--text-color3)', marginTop: 8 }}>
              为了保证最佳的预览效果，请将字段控制在6个以内
            </div>
          )}
        >
          {map(systemColumns, ({ label, code }) => (
            <Option value={code}>{label}</Option>
          ))}
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
      />
      {!isEmpty(formDataSet.current?.get('visibleColumns')) && (
        // @ts-ignore
        <IssueTable
          tableProps={tableProps}
          style={{ marginTop: 10 }}
          queryBar={false}
          fields={data}
          createIssue={false}
          listLayoutColumns={formDataSet.current?.get('visibleColumns')?.map((code: string) => ({
            columnCode: code,
            display: true,
          })) || []}
        />
      )}
    </div>
  );
};
export default observer(AddIssueList);
