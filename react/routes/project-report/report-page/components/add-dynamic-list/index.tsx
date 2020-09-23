import React, {
  useCallback, useEffect, useImperativeHandle, useState, useRef, useMemo,
} from 'react';
import { observer } from 'mobx-react-lite';
import { Form, TextField, DataSet } from 'choerodon-ui/pro';
import { IField } from '@/common/types';
import { find } from 'lodash';
import { fieldApi } from '@/api';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { flattenObject } from '@/components/issue-search/utils';
import { IReportListBlock } from '../../store';
import { RefProps } from '../add-modal';
import ExportIssueContextProvider from './stores';
import FormArea from './FormArea';

const systemColumns = [{
  value: 'summary',
  label: '概要',
}, {
  value: 'issueNum',
  label: '编号',
}, {
  value: 'priority',
  label: '优先级',
}, {
  value: 'assign',
  label: '经办人',
}, {
  value: 'status',
  label: '状态',
}, {
  value: 'sprint',
  label: '冲刺',
}, {
  value: 'reporter',
  label: '报告人',
}, {
  value: 'creationDate',
  label: '创建时间',
}, {
  value: 'lastUpdateDate',
  label: '最后更新时间',
}, {
  value: 'estimatedStartTime',
  label: '预计开始时间',
}, {
  value: 'estimatedEndTime',
  label: '预计结束时间',
}, {
  value: 'label',
  label: '标签',
}, {
  value: 'component',
  label: '模块',
}, {
  value: 'storyPoints',
  label: '故事点',
}, {
  value: 'version',
  label: '版本',
}, {
  value: 'epic',
  label: '史诗',
}, {
  value: 'feature',
  label: '特性',
}];
interface Props {
  innerRef: React.MutableRefObject<RefProps>
  data?: IReportListBlock
}
export interface ListRefProps {
  submit: () => Promise<boolean | Pick<IReportListBlock, 'colList' | 'searchVO'>>
}
const AddDynamicIssueList: React.FC<Props> = ({ innerRef, data: editData }) => {
  const listRef = useRef<ListRefProps>({} as ListRefProps);
  const [loading, setLoading] = useState(true);
  const [fields, setFields] = useState<IField[]>([]);
  const getFieldCodeById = useCallback((id: string) => {
    const field = find(fields, { id });
    return field ? field.code : undefined;
  }, [fields]);

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
          const code = getFieldCodeById(key);
          if (code) {
            result.push([{ code, value: value.value }]);
          }
        } else if (key === 'createEndDate' || key === 'createStartDate') {
          result.push({ code: 'createDate', value: [filterObject.createStartDate, filterObject.createEndDate] });
        } else if (key === 'updateEndDate' || key === 'updateStartDate') {
          result.push({ code: 'updateDate', value: [filterObject.updateStartDate, filterObject.updateEndDate] });
        } else {
          result.push({ code: key, value });
        }
      }
    }
    return result;
  }, [editData, getFieldCodeById]);
  const formDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    data: editData ? [{ title: editData.title }] : undefined,
    fields: [{
      name: 'title',
      label: '列表标题',
      maxLength: 44,
      required: true,
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
        const { searchVO, colList } = search;
        const block: IReportListBlock = {
          key: String(Math.random()),
          title: data.title,
          type: 'dynamic_list',
          colList,
          searchVO,
        };
        return block;
      }
    }
    return false;
  }, [formDataSet]);
  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <>
      <Form dataSet={formDataSet}>
        <TextField name="title" />
      </Form>
      {!loading ? (
        <ExportIssueContextProvider
          colList={editData?.colList || []}
          fields={[...getSystemFields(), ...fields]}
          // @ts-ignore
          chosenFields={chosenFields}
          checkOptions={[...systemColumns, ...fields.map((field) => ({ value: field.code, label: field.name }))]}
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
