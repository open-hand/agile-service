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
      { code: 'projectOwner', label: '项目所有者' },
      { code: 'assignee', label: '经办人' },
      { code: 'reporter', label: '报告人' },
      { code: 'specifier', label: '指定人' },
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
        name: 'userTypeList',
        label: '选择人员',
        type: 'string' as FieldType,
        textField: 'label',
        valueField: 'code',
        options: memberOptionsDataSet,
        multiple: true,
        required: true,
      },
      {
        name: 'userIdList',
        label: '指定人',
        type: 'array' as FieldType,
        required: true,
        multiple: true,
        textField: 'realName',
        valueField: 'id',
      },
      {
        name: 'notifyTypeList',
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
      current?.set('userTypeList', res.userTypeList);
      current?.set('userIdList', res.userIdList);
      current?.set('notifyTypeList', res.notifyTypeList);
    });
    const handleOk = async () => {
      const validate = await notifySettingDataSet.validate();
      const data = notifySettingDataSet.toData();
      // @ts-ignore
      const { userTypeList, userIdList, notifyTypeList } = data && data[0];
      // @ts-ignore
      if (validate || (userTypeList.length && userTypeList.findIndex((item: string) => item === 'assignee') === -1 && notifyTypeList.length)) {
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
  const { userTypeList } = data && data[0];
  return (
    <div className={styles.notify_setting}>
      <Form dataSet={notifySettingDataSet}>
        <Select name="userTypeList" />
        {
          userTypeList.find((item: string) => item === 'assignee') && (
            <SelectUser name="userIdList" />
          )
        }
        <Select name="notifyTypeList" />
      </Form>
    </div>
  );
};

export default observer(NotifySetting);
