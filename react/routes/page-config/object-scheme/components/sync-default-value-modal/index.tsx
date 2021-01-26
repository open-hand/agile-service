import { pageConfigApi, pageConfigApiConfig } from '@/api';
import { IIssueType, IModalProps } from '@/common/types';
import TextEditToggle from '@/components/TextEditTogglePro';
import { toJS } from 'mobx';
import beforeSubmitProcessData from '@/routes/page-config/components/create-field/util';
import renderEditor from '@/routes/page-config/components/renderEditor';
import { getMenuType } from '@/utils/common';
import {
  Form, DataSet, Modal, Select,
} from 'choerodon-ui/pro/lib';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import moment from 'moment';
import React, {
  useCallback, useEffect, useMemo, useState,
} from 'react';

interface Props {
  prefixCls: string
  record: Record
  defaultTypes: Array<string>
  text: string
  options: IIssueType[]
  modal?: IModalProps
}
const dateList = ['datetime', 'time', 'date'];
const SyncDefaultValueEditForm: React.FC<Props> = ({
  text, record, options, modal, prefixCls, defaultTypes,
}) => {
  const initValue = useMemo(() => {
    let defaultValue = record.get('defaultValue');
    if (defaultValue === '') {
      defaultValue = undefined;
    }
    if (defaultValue && ['checkbox', 'multiple', 'radio', 'single', 'multiMember'].includes(record.get('fieldType'))) {
      defaultValue = String(defaultValue).split(',');
    }
    return defaultValue;
  }, [record]);
  const ds = useMemo(() => {
    const defaultValueFieldProps = dateList.includes(record.get('fieldType')); // valueField

    return new DataSet({
      autoCreate: true,
      autoQuery: false,
      fields: [
        {
          name: 'defaultValue',
          label: '默认值',
          defaultValue: initValue,
        },
        {
          name: 'syncIssueType',
          label: '将默认值同步到',
          multiple: true,
          required: true,
          defaultValue: defaultTypes,
          // defaultValidationMessages: '请选择同步的问题类型',
        },
      ],
    });
  }, [defaultTypes, initValue]);

  const handleOk = useCallback(async () => {
    if (await ds.validate()) {
      const syncIssueType = toJS(ds.current!.get('syncIssueType'));
      console.log(syncIssueType);
      const defaultValue = ds.current!.get('defaultValue');
      // const originFieldOptions: any[] | undefined = record.get('fieldOptions');
      // const extraConfig = record.get('extraConfig');
      record.set('defaultValue', defaultValue);
      const newData = beforeSubmitProcessData(record);
      // if (!record.get('system') && ['checkbox', 'multiple', 'radio', 'single'].includes(record.get('fieldType'))) {
      //   fieldOptions = !defaultValue || defaultValue.length === 0 ? originFieldOptions?.map((item) => ({ ...item, isDefault: false }))
      //     : originFieldOptions?.map((item) => ({ ...item, isDefault: defaultValue.includes(item.id) }));
      // }
      // if()
      if (!(record.get('system') || (getMenuType() === 'project' && record.get('projectId') === null))) {
        await pageConfigApi.updateField(record.get('id'), newData);
      }
      await pageConfigApi.syncDefaultValue(record.get('id'), {
        issueTypeIds: syncIssueType,
        extraConfig: newData.extraConfig,
        fieldType: newData.fieldType,
        fieldOptions: newData.fieldOptions,
        defaultValue: newData.defaultValue,
        custom: false,
      });
      return true;
    }
    return false;
  }, [record]);
  useEffect(() => {
    modal?.handleOk(handleOk);
  }, [handleOk, modal]);
  const handleChangeDate = useCallback((value) => {
    if (value === 'current') {
      record.set('defaultValue', moment().format('YYYY-MM-DD HH:mm:ss'));
      record.set('check', true);
    } else {
      record.set('check', false);
    }
  }, [record]);
  return (
    <Form dataSet={ds}>
      {renderEditor({
        data: {
          ...record.toData(),
          defaultValue: ['datetime', 'time', 'date'].includes(record.get('fieldType')) && record.get('extraConfig') ? 'current' : initValue,
        },
        onChange: ['datetime', 'time', 'date'].includes(record.get('fieldType')) ? handleChangeDate : undefined,
        name: 'defaultValue',
      })}
      <Select name="syncIssueType" required style={{ minWidth: 280 }} multiple>
        {options.map((option) => <Select.Option value={option.id}>{option.name}</Select.Option>)}
      </Select>
    </Form>

  );
};

const openSyncDefaultValueEditForm = async (record: Record, prefixCls: string) => {
  const issueTypes: IIssueType[] = record?.get('issueTypeVOList');
  const issueTypesArr: string[] = issueTypes.map((t) => t.id);
  const contextName: string = record?.get('contextName');
  const contextNameArr = contextName.split(',');
  record.set('fieldCode', record.get('code'));
  let newRecord = record;
  if (!record.get('system')) { // && ['checkbox', 'multiple', 'radio', 'single'].includes(record.get('fieldType'))
    const data = await pageConfigApi.loadById(record.get('id'));
    // record.set('fieldOptions', fieldOptions);
    newRecord = new Record({ ...data, check: data.extraConfig });
  }

  Modal.open({
    key: Modal.key(),
    title: '默认值同步',
    children: <SyncDefaultValueEditForm
      prefixCls={prefixCls}
      record={newRecord}
      text={contextName}
      defaultTypes={issueTypesArr}
      options={issueTypes}
    />,
    className: `${prefixCls}-detail-sync`,
    okText: '确定',
  });
};
export { openSyncDefaultValueEditForm };
export default SyncDefaultValueEditForm;
