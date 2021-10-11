import { toJS } from 'mobx';
import {
  Form, DataSet, Modal, Select,
} from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import moment from 'moment';
import React, {
  useCallback, useEffect, useMemo,
} from 'react';
import { observer } from 'mobx-react-lite';
import { getMenuType } from '@/utils/common';
import renderEditor from '@/routes/page-config/components/renderEditor';
import beforeSubmitProcessData from '@/routes/page-config/components/create-field/util';
import { IIssueType, IModalProps } from '@/common/types';
import { pageConfigApi } from '@/api';

interface Props {
  prefixCls: string
  record: Record
  defaultTypes: Array<string>
  options: IIssueType[]
  modal?: IModalProps
}

const SyncDefaultValueEditForm: React.FC<Props> = ({
  record, options, modal, prefixCls, defaultTypes,
}) => {
  const initValue = useMemo(() => {
    let defaultValue = record.get('defaultValue');
    if (defaultValue === '') {
      defaultValue = undefined;
    }
    if (defaultValue && ['checkbox', 'multiple', 'radio', 'single', 'multiMember', 'member'].includes(record.get('fieldType'))) {
      defaultValue = String(defaultValue).split(',');
    }
    if (['datetime', 'time', 'date'].includes(record.get('fieldType')) && record.get('extraConfig')) {
      defaultValue = 'current';
    }

    return defaultValue;
  }, [record]);
  const ds = useMemo(() => {
    let defaultValue = initValue;
    if (['radio', 'single', 'member'].includes(record.get('fieldType')) && initValue) {
      defaultValue = Array.isArray(defaultValue) ? initValue[0] : initValue;
    }

    return new DataSet({
      autoCreate: true,
      autoQuery: false,
      fields: [
        {
          name: 'defaultValue',
          label: '默认值',
          defaultValue,
        },
        {
          name: 'syncIssueType',
          label: '将默认值同步到',
          multiple: true,
          required: true,
          defaultValue: defaultTypes,
          // defaultValidationMessages: '请选择同步的工作项类型',
        },
      ],
    });
  }, [defaultTypes, initValue, record]);

  const handleOk = useCallback(async () => {
    if (await ds.validate()) {
      const syncIssueType = toJS(ds.current!.get('syncIssueType'));
      let defaultValue = toJS(ds.current!.get('defaultValue'));
      if (['date', 'datetime', 'time'].includes(record.get('fieldType')) && defaultValue && typeof (defaultValue) === 'object') {
        if (defaultValue.value === 'current') {
          record.set('check', true);
          defaultValue = moment();
        } else {
          defaultValue = defaultValue.meaning;
        }
      }
      record.set('defaultValue', defaultValue);
      const newData = beforeSubmitProcessData(record);
      let updateFiledFlag = false;
      if (!(record.get('system') || (getMenuType() === 'project' && record.get('projectId') === null))) {
        await pageConfigApi.updateField(record.get('id'), newData);
        updateFiledFlag = true;
      }
      await pageConfigApi.syncDefaultValue(record.get('id'), {
        issueTypeIds: syncIssueType,
        extraConfig: newData.extraConfig,
        fieldType: newData.fieldType,
        fieldOptions: newData.fieldOptions,
        defaultValue: newData.defaultValue,
        custom: updateFiledFlag,
      });
      return true;
    }
    return false;
  }, [ds, record]);
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
          defaultValue: initValue,
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
  const issueTypes: IIssueType[] = record?.get('issueTypeVOList')?.filter((t: IIssueType) => t.enabled);
  const issueTypesArr: string[] = issueTypes.map((t) => t.id);
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
      defaultTypes={issueTypesArr}
      options={issueTypes}
    />,
    className: `${prefixCls}-detail-sync`,
    okText: '确定',
  });
};
export { openSyncDefaultValueEditForm };
export default SyncDefaultValueEditForm;
