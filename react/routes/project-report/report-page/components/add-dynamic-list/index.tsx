import React, {
  useCallback, useEffect, useImperativeHandle, useState, useRef, useMemo,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Form, TextField, DataSet, Select,
} from 'choerodon-ui/pro';
import { find } from 'lodash';
import { IField } from '@/common/types';
import { fieldApi } from '@/api';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { flattenObject } from '@/components/issue-search/utils';
import useIsInProgram from '@/hooks/useIsInProgram';
import { IReportListBlock } from '../../store';
import { RefProps } from '../add-modal';
import ExportIssueContextProvider from './stores';
import FormArea from './FormArea';

const { Option } = Select;
interface Props {
  innerRef: React.MutableRefObject<RefProps>
  data?: IReportListBlock
}
export interface ListRefProps {
  submit: () => Promise<boolean | Pick<IReportListBlock, 'searchVO'>>
}
const AddDynamicIssueList: React.FC<Props> = ({ innerRef, data: editData }) => {
  const listRef = useRef<ListRefProps>({} as ListRefProps);
  const [loading, setLoading] = useState(true);
  const [fields, setFields] = useState<IField[]>([]);
  const allFields = useMemo(() => [...getSystemFields(), ...fields].map((item) => ({ ...item, immutableCheck: item.code === 'sprint' ? true : undefined, otherComponentProps: { range: item.fieldType && ['time', 'datetime', 'date'].includes(item.fieldType) ? true : undefined } })), [fields]);
  const getCustomFieldById = useCallback((id: string) => {
    const field = find(allFields, { id });
    return field || undefined;
  }, [allFields]);
  const getSystemFieldByCode = useCallback((code: string) => {
    const field = find(allFields, { code });
    return field;
  }, [allFields]);
  const chosenFields = useMemo(() => {
    if (!editData) {
      return [];
    }
    const filterObject = flattenObject(editData.searchVO);
    const result = [];
    for (const [key, value] of Object.entries(filterObject)) {
      if (value) {
        // 自定义字段保存的时候只保存了id，这里要找到code
        if (value.isCustom) {
          const field = getCustomFieldById(key);
          if (field) {
            result.push({ ...field, code: field.code, value: value.value });
          }
        } else if (key === 'createEndDate' || key === 'createStartDate') {
          result.push({ ...getSystemFieldByCode('createDate'), code: 'createDate', value: [filterObject.createStartDate, filterObject.createEndDate] });
        } else if (key === 'updateEndDate' || key === 'updateStartDate') {
          result.push({ ...getSystemFieldByCode('updateDate'), code: 'updateDate', value: [filterObject.updateStartDate, filterObject.updateEndDate] });
        } else {
          result.push({
            ...getSystemFieldByCode(key), code: key, value,
          });
        }
      }
    }
    return result;
  }, [editData, getCustomFieldById, getSystemFieldByCode]);
  const formDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    data: editData ? [{ title: editData.title, visibleColumns: editData.colList }] : undefined,
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
  const refresh = useCallback(async () => {
    setLoading(true);
    const res = await fieldApi.getCustomFields();
    setFields(res);
    setLoading(false);
  }, []);
  useEffect(() => {
    refresh();
  }, [refresh]);
  const handleSubmit = useCallback(async () => {
    if (await formDataSet.current?.validate(true)) {
      const data = formDataSet.current?.toData();
      const search = await listRef.current.submit();
      if (typeof search === 'object') {
        const { searchVO } = search;
        const block: IReportListBlock = {
          key: String(Math.random()),
          title: data.title,
          type: 'dynamic_list',
          collapse: false,
          colList: data.visibleColumns,
          searchVO,
        };
        console.log('search', block, search);
        return block;
      }
    }
    return false;
  }, [formDataSet]);
  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  const { isInProgram } = useIsInProgram();
  return (
    <>
      <Form dataSet={formDataSet}>
        <TextField name="title" />
        <Select
          name="visibleColumns"
          multiple
          // @ts-ignore
          help={(
            <div style={{ fontSize: '12px', color: 'var(--text-color3)', marginTop: 8 }}>
              为了保证最佳的预览效果，请将字段控制在6个以内
            </div>
          )}
        >
          <Option value="summary">概要</Option>
          <Option value="issueNum">编号</Option>
          <Option value="priority">优先级</Option>
          <Option value="assign">经办人</Option>
          <Option value="status">状态</Option>
          <Option value="sprint">冲刺</Option>
          <Option value="reporter">报告人</Option>
          <Option value="createUser">创建人</Option>
          <Option value="updateUser">更新人</Option>
          <Option value="creationDate">创建时间</Option>
          <Option value="lastUpdateDate">最后更新时间</Option>
          <Option value="estimatedStartTime">预计开始时间</Option>
          <Option value="estimatedEndTime">预计结束时间</Option>
          <Option value="label">标签</Option>
          <Option value="component">模块</Option>
          <Option value="storyPoints">故事点</Option>
          <Option value="fixVersion">修复的版本</Option>
          <Option value="influenceVersion">影响的版本</Option>
          <Option value="epic">史诗</Option>
          {isInProgram && <Option value="feature">特性</Option>}
          <Option value="mainResponsibleUser">主要负责人</Option>
          <Option value="environmentName">环境</Option>
          {fields.map((field) => (
            <Option value={field.code}>
              {field.name}
            </Option>
          ))}
        </Select>
      </Form>
      {!loading ? (
        <ExportIssueContextProvider
          colList={editData?.colList || []}
          fields={allFields}
          // @ts-ignore
          chosenFields={chosenFields}
        >
          <FormArea
            innerRef={listRef}
          />
        </ExportIssueContextProvider>
      ) : null}
    </>
  );
};
export default observer(AddDynamicIssueList);
