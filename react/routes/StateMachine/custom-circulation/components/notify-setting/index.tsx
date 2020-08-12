import React, { useMemo, useEffect } from 'react';
import { DataSet, Form, Select } from 'choerodon-ui/pro';
import SelectUser from '@/components/select/select-user';
import { observer } from 'mobx-react-lite';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { statusTransformApi } from '@/api';
import styles from './index.less';

// @ts-ignore
const NotifySetting = ({ modal, record, selectedType }) => {
  const memberOptionsDataSet = useMemo(() => new DataSet({
    data: [
      { code: 'owner', label: '项目所有者' },
      { code: 'agent', label: '经办人' },
      { code: 'reporter', label: '报告人' },
      { code: 'assigners', label: '指定人' },
    ],
    fields: [
      { name: 'code', type: 'string' as FieldType },
      { name: 'label', type: 'string' as FieldType },
    ],
  }), []);
  const notifyMethodDataSet = useMemo(() => new DataSet({
    data: [
      { label: '邮件', code: 'email' },
      { label: '站内信', code: 'mail' },
      { label: 'webhook', code: 'webhook' },
    ],
    fields: [
      { name: 'code', type: 'string' as FieldType },
      { name: 'label', type: 'string' as FieldType },
    ],
  }), []);
  const notifySettingDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'member',
        label: '选择人员',
        type: 'string' as FieldType,
        textField: 'label',
        valueField: 'code',
        options: memberOptionsDataSet,
        multiple: true,
        required: true,
      },
      {
        name: 'assigners',
        label: '指定人',
        type: 'array' as FieldType,
        required: true,
        multiple: true,
        textField: 'realName',
        valueField: 'id',
      },
      {
        name: 'notifyMethod',
        label: '通知方式',
        type: 'string' as FieldType,
        textField: 'label',
        valueField: 'code',
        options: notifyMethodDataSet,
        required: true,
        multiple: true,
      },
    ],
  }), [memberOptionsDataSet, notifyMethodDataSet]);

  useEffect(() => {
    const { current } = notifySettingDataSet;
    // @ts-ignore
    statusTransformApi.getNotifySetting(selectedType, record.get('id')).then((res) => {
      current?.set('member', res.member);
      current?.set('assigners', res.assigners);
      current?.set('notifyMethod', res.notifyMethod);
    });
    const handleOk = async () => {
      const validate = await notifySettingDataSet.validate();
      const data = notifySettingDataSet.toData();
      // @ts-ignore
      const { member, assigners, notifyMethod } = data && data[0];
      // @ts-ignore
      if (validate || (member.length && member.findIndex((item: string) => item === 'assigners') === -1 && notifyMethod.length)) {
        console.log('validate：');
        console.log(validate);
        console.log(data[0]);
        return true;
      }
      return false;
    };
    if (modal) {
      modal.handleOk(handleOk);
    }
  }, [modal, notifySettingDataSet]);

  const data = notifySettingDataSet.toData();
  // @ts-ignore
  const { member } = data && data[0];
  return (
    <div className={styles.notify_setting}>
      <Form dataSet={notifySettingDataSet}>
        <Select name="member" />
        {
          member.find((item: string) => item === 'assigners') && (
            <SelectUser name="assigners" />
          )
        }
        <Select name="notifyMethod" />
      </Form>
    </div>
  );
};

export default observer(NotifySetting);
