import React, { useMemo, useEffect } from 'react';
import { DataSet, Form, Select } from 'choerodon-ui/pro';
import SelectUser from '@/components/select/select-user';
import { find } from 'lodash';
import { observer } from 'mobx-react-lite';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { statusTransformApi, IUpdateNotifySetting, statusTransformApiConfig } from '@/api';
import { getProjectId } from '@/utils/common';
import styles from './index.less';

const NotifySetting = ({
// @ts-ignore
  modal, record, selectedType, customCirculationDataSet,
}) => {
  const memberOptionsDataSet = useMemo(() => new DataSet({
    autoQuery: true,
    paging: false,
    transport: {
      read: () => statusTransformApiConfig.getCustomMember(selectedType),
    },
    fields: [
      { name: 'code', type: 'string' as FieldType },
      { name: 'name', type: 'string' as FieldType },
    ],
  }), [selectedType]);

  const notifyMethodDataSet = useMemo(() => new DataSet({
    data: [
      { label: '邮件', code: 'EMAIL' },
      { label: '站内信', code: 'WEB' },
      { label: 'webhook', code: 'WEB_HOOK' },
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
        textField: 'name',
        valueField: 'code',
        options: memberOptionsDataSet,
        multiple: true,
        dynamicProps: {
          // eslint-disable-next-line no-shadow
          required: ({ record }) => record.get('noticeTypeList').length,
        },
      },
      {
        name: 'userIdList',
        label: '指定人',
        type: 'array' as FieldType,
        multiple: true,
        textField: 'realName',
        valueField: 'id',
        dynamicProps: {
          // eslint-disable-next-line no-shadow
          required: ({ record }) => find(record.get('userTypeList') || [], (item: string) => item === 'specifier'),
        },
      },
      {
        name: 'noticeTypeList',
        label: '通知方式',
        type: 'array' as FieldType,
        textField: 'label',
        valueField: 'code',
        options: notifyMethodDataSet,
        multiple: true,
        dynamicProps: {
          // eslint-disable-next-line no-shadow
          required: ({ record }) => record.get('userTypeList').length,
        },
      },
    ],
  }), [memberOptionsDataSet, notifyMethodDataSet]);

  useEffect(() => {
    const { current } = notifySettingDataSet;
    // @ts-ignore
    statusTransformApi.getNotifySetting(selectedType, record.get('id')).then((res) => {
      current?.set('userTypeList', [...(res.userList && res.userList.length ? ['specifier'] : []), ...res.userTypeList]);
      current?.set('userIdList', (res.userList || []).map((item: { id: string}) => item.id));
      current?.set('noticeTypeList', res.noticeTypeList);
    });
  }, [selectedType, record, notifySettingDataSet]);

  useEffect(() => {
    const handleOk = async () => {
      const validate = await notifySettingDataSet.validate();
      const data = notifySettingDataSet.toData();
      // @ts-ignore
      const { userTypeList, userIdList, noticeTypeList } = data && data[0];
      if (validate) {
        const updateData: IUpdateNotifySetting = {
          issueTypeId: selectedType,
          projectId: getProjectId(),
          statusId: record.get('id'),
          userTypeList,
          noticeTypeList,
          userIdList: find(userTypeList, (item) => item === 'specifier') && userIdList,
          objectVersionNumber: record.get('objectVersionNumber'),
        };
        try {
          await statusTransformApi.updateNotifySetting(updateData);
          customCirculationDataSet.query();
          return true;
        } catch (e) {
          return false;
        }
      }
      return false;
    };
    if (modal) {
      modal.handleOk(handleOk);
    }
  }, [customCirculationDataSet, modal, notifySettingDataSet, record, selectedType]);

  const data = notifySettingDataSet.toData();
  // @ts-ignore
  const { userTypeList } = data && data[0];
  return (
    <div className={styles.notify_setting}>
      <Form dataSet={notifySettingDataSet}>
        <Select name="userTypeList" />
        {
          userTypeList.find((item: string) => item === 'specifier') && (
            <SelectUser name="userIdList" />
          )
        }
        <Select name="noticeTypeList" />
      </Form>
    </div>
  );
};

export default observer(NotifySetting);
