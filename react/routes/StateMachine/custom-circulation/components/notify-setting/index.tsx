import React, {
  useMemo, useEffect, useRef, useCallback, useState,
} from 'react';
import {
  DataSet, Form, Select, Dropdown, CheckBox,
} from 'choerodon-ui/pro';
import { Icon } from 'choerodon-ui';
import SelectUser from '@/components/select/select-user';
import { find } from 'lodash';
import { observer } from 'mobx-react-lite';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { statusTransformApi, IUpdateNotifySetting, statusTransformApiConfig } from '@/api';
import { getProjectId } from '@/utils/common';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import { User } from '@/common/types';
import Loading from '@/components/Loading';
import styles from './index.less';

interface NotifySelectProps {
  notifySettingDataSet: DataSet,
  memberOptionsDataSet: DataSet,
}

const NotifySelect: React.FC<NotifySelectProps> = (
  { memberOptionsDataSet, notifySettingDataSet },
) => {
  const data = notifySettingDataSet.toData()[0];
  return (
    <div
      className={styles.notifyMember_select}
    >
      {
          // @ts-ignore
          memberOptionsDataSet.toData().map((item) => (
            // @ts-ignore
            <CheckBox name={item.code}>{item.name}</CheckBox>
          ))
        }
      {
        // @ts-ignore
        data.specifier && (
          <Select
            name="userIdList"
            maxTagCount={2}
            className={styles.notify_assigners}
            // @ts-ignore
            getPopupContainer={(trigger) => trigger.parentNode}
          />
        )
      }
    </div>
  );
};

// @ts-ignore
function useClickOut(onClickOut) {
  const ref = useRef();
  const handleClick = useCallback((e) => {
    const popupContainerEles = document.getElementsByClassName('c7n-pro-popup-container');
    const triggerBtn = document.getElementsByClassName(styles.dropDown_trigger)[0];
    let allIsNotContain = true;
    for (let i = 0; i < popupContainerEles.length; i += 1) {
      if (popupContainerEles[i].contains(e.target)) {
        allIsNotContain = false;
        break;
      }
    }
    // @ts-ignore
    if (ref.current && (!ref.current.contains(e.target) && allIsNotContain && e.target.tagName !== 'BODY' && !triggerBtn.contains(e.target))) {
      onClickOut(e);
    }
  }, [onClickOut]);
  useEffect(() => {
    document.addEventListener('click', handleClick, true);
    return () => {
      document.removeEventListener('click', handleClick, true);
    };
  }, [handleClick]);
  return ref;
}

const NotifySetting = ({
// @ts-ignore
  modal, record, selectedType, customCirculationDataSet,
}) => {
  const [loading, setLoading] = useState(false);
  const [hidden, setHidden] = useState(true);
  const handleClickOut = useCallback(() => {
    setHidden(true);
  }, []);
  const ref = useClickOut(handleClickOut);

  const userDs = useMemo(() => new DataSet({
    autoQuery: true,
    selection: false,
    paging: false,
    transport: {
      read: {
        url: `/iam/choerodon/v1/projects/${getProjectId()}/users/search_by_name`,
        method: 'get',
      },
    },
  }), []);

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
        name: 'userIdList',
        label: '指定人',
        type: 'array' as FieldType,
        multiple: true,
        textField: 'realName',
        valueField: 'id',
        options: userDs,
        dynamicProps: {
          // eslint-disable-next-line no-shadow
          required: ({ record }) => record.get('specifier'),
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
          required: ({ record }) => {
            const members = memberOptionsDataSet.toData() || [];
            for (let i = 0; i < members.length; i += 1) {
              // @ts-ignore
              if (record.get(members[i].code)) {
                return true;
              }
            }
            return false;
          },
        },
      },
    ],
  }), [memberOptionsDataSet, notifyMethodDataSet, userDs]);

  useEffect(() => {
    const { current } = notifySettingDataSet;
    setLoading(true);
    // @ts-ignore
    statusTransformApi.getNotifySetting(selectedType, record.get('id')).then((res) => {
      setLoading(false);
      if (res.userList && res.userList.length) {
        current?.set('specifier', true);
      }
      (res.userTypeList || []).forEach((usertype: string) => {
        current?.set(usertype, true);
      });
      current?.set('userIdList', (res.userList || []).map((item: { id: string}) => item.id));
      current?.set('noticeTypeList', res.noticeTypeList);
    });
  }, [selectedType, record, notifySettingDataSet]);

  useEffect(() => {
    const handleOk = async () => {
      const validate = await notifySettingDataSet.validate();
      const data = notifySettingDataSet.toData();
      // @ts-ignore
      const { specifier, userIdList, noticeTypeList } = data && data[0];
      const userTypeList = [];
      if (validate) {
        for (const [key, value] of Object.entries(data[0])) {
          if (key !== 'userIdList' && key !== 'noticeTypeList') {
            if (typeof value === 'boolean' && value) {
              userTypeList.push(key);
            }
          }
        }
        const updateData: IUpdateNotifySetting = {
          issueTypeId: selectedType,
          projectId: getProjectId(),
          statusId: record.get('id'),
          userTypeList,
          noticeTypeList,
          userIdList: specifier ? userIdList : undefined,
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
      if (specifier && (!userIdList || userIdList.length === 0)) {
        setHidden(false);
      }
      return false;
    };
    if (modal) {
      modal.handleOk(handleOk);
    }
  }, [customCirculationDataSet, modal, notifySettingDataSet, record, selectedType]);

  const data = notifySettingDataSet.toData();
  const selected = [];
  if (data[0]) {
    for (const [key, value] of Object.entries(data[0])) {
      if (key !== 'userIdList' && key !== 'noticeTypeList' && key !== 'specifier') {
        if (typeof value === 'boolean' && value) {
          const memberItem = memberOptionsDataSet.toData().find((item: {
            code: string, name: string}) => item.code === key);
          if (memberItem) {
            // @ts-ignore
            selected.push(memberItem.name);
          }
        }
      }
    }
    // @ts-ignore
    if (data[0].specifier) {
      // @ts-ignore
      (data[0].userIdList || []).forEach((userId: string) => {
        const userItem = userDs.toData().find((item: User) => item.id === userId) as User;
        if (userItem) {
          selected.push(userItem.realName);
        }
      });
    }
  }

  // @ts-ignore
  return (
    <div className={styles.notify_setting}>
      <Loading loading={loading} />
      <Form dataSet={notifySettingDataSet}>
        <Dropdown
          // @ts-ignore
          getPopupContainer={(trigger) => trigger.parentNode}
          visible={!hidden}
          overlay={(
            <div
            // @ts-ignore
              ref={ref}
              role="none"
              onClick={(e) => {
                e.stopPropagation();
              }}
            >
              <NotifySelect
                memberOptionsDataSet={memberOptionsDataSet}
                notifySettingDataSet={notifySettingDataSet}
              />
            </div>
          )}
          trigger={['click'] as Action[]}
        >
          <div
            className={styles.dropDown_trigger}
            role="none"
            onClick={(e) => {
              e.nativeEvent.stopImmediatePropagation();
              setHidden(!hidden);
            }}
            tabIndex={0}
          >
            {/* <span className={styles.trigger_label}>通知人员</span> */}
            <span className={styles.selected}>
              {selected.join(',')}
            </span>
            <Icon type="arrow_drop_down" />
          </div>
        </Dropdown>
        <Select name="noticeTypeList" />
      </Form>
    </div>
  );
};

export default observer(NotifySetting);
