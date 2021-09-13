import React, {
  useMemo, useEffect, useRef, useCallback, useState,
} from 'react';
import {
  DataSet, Form, Select, Dropdown, CheckBox,
  Icon,
} from 'choerodon-ui/pro';

import { stores } from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import { statusTransformApi, IUpdateNotifySetting, statusTransformApiConfig } from '@/api';
import { getProjectId, getIsOrganization } from '@/utils/common';
import { User } from '@/common/types';
import { OldLoading } from '@/components/Loading';
import useIsProgram from '@/hooks/useIsProgram';
import SelectUser from '@/components/select/select-user';
import styles from './index.less';

const { AppState } = stores;
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
          <SelectUser
            name="userList"
            maxTagCount={2}
            selectedUser={notifySettingDataSet.current?.getState('defaultSelectUsers')}
            className={styles.notify_assigners}
            // @ts-ignore
            
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
  modal, record, selectedType, customCirculationDataSet, memberOptions = [] as Array<{ code: string, name: string }>,
}) => {
  const [loading, setLoading] = useState(false);
  const [hidden, setHidden] = useState(true);
  const handleClickOut = useCallback(() => {
    setHidden(true);
  }, []);
  const ref = useClickOut(handleClickOut);
  const { isProgram } = useIsProgram();
  const isOrganization = getIsOrganization();

  const memberOptionsDataSet = useMemo(() => new DataSet({
    autoQuery: true,
    paging: false,
    transport: {
      read: () => statusTransformApiConfig[isOrganization ? 'orgGetCustomMember' : 'getCustomMember'](selectedType, memberOptions),

    },
    fields: [
      { name: 'code', type: 'string' as FieldType },
      { name: 'name', type: 'string' as FieldType },
    ],
  }), [isOrganization, memberOptions, selectedType]);

  const notifyMethodDataSet = useMemo(() => new DataSet({
    data: [
      { label: '邮件', code: 'EMAIL' },
      { label: '站内信', code: 'WEB' },
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
        name: 'userList',
        label: '指定人',
        type: 'object' as any,
        // type: 'array' as FieldType,
        multiple: true,
        textField: 'realName',
        valueField: 'id',
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
      {
        name: 'webhook',
        label: '启用Webhook通知',
        type: 'boolean' as FieldType,
      },
    ],
  }), [memberOptionsDataSet, notifyMethodDataSet]);

  useEffect(() => {
    const { current } = notifySettingDataSet;
    setLoading(true);
    // @ts-ignore
    statusTransformApi[isOrganization ? 'orgGetNotifySetting' : 'getNotifySetting'](selectedType, record.get('id')).then((res) => {
      setLoading(false);
      if (res.userList && res.userList.length) {
        current?.set('specifier', true);
        current?.set('userList', res.userList);
        current?.setState('defaultSelectUsers', res.userList);
      }
      (res.userTypeList || []).forEach((usertype: string) => {
        current?.set(usertype, true);
      });
      (res.memberList || []).forEach((item: { code: string, name: string }) => {
        current?.set(item.code, true);
      });

      // current?.set('userIdList', (res.userList || []).map((item: { id: string }) => item.id));
      current?.set('noticeTypeList', (res.noticeTypeList || []).filter((item: string) => item !== 'WEB_HOOK'));
      current?.set('webhook', Boolean((res.noticeTypeList || []).find((item: string) => item === 'WEB_HOOK')));
    });
  }, [selectedType, record, notifySettingDataSet, isOrganization]);
  useEffect(() => {
    const handleOk = async () => {
      const validate = await notifySettingDataSet.validate();
      const data = notifySettingDataSet.toData();
      const {
        // @ts-ignore
        specifier, userList = [], noticeTypeList, webhook,
      } = data && data[0];
      const userIdList = userList.map((i: User) => i.id);
      const userTypeList = [];
      if (validate) {
        for (const [key, value] of Object.entries(data[0])) {
          if (key !== 'userIdList' && key !== 'noticeTypeList' && key !== 'webhook') {
            if (typeof value === 'boolean' && value) {
              userTypeList.push(key);
            }
          }
        }

        if (noticeTypeList.length && !userTypeList.length) {
          return false;
        }
        if (specifier && (!userIdList || userIdList.length === 0)) {
          setHidden(false);
        }

        const updateData: IUpdateNotifySetting = {
          issueTypeId: selectedType,
          projectId: isOrganization ? 0 : getProjectId(),
          statusId: record.get('id'),
          userTypeList,
          noticeTypeList: webhook ? ['WEB_HOOK', ...noticeTypeList] : noticeTypeList,
          userIdList: specifier ? userIdList : undefined,
          objectVersionNumber: record.get('objectVersionNumber'),
        };
        try {
          await statusTransformApi[isOrganization ? 'orgUpdateNotifySetting' : 'updateNotifySetting'](updateData);
          customCirculationDataSet.query(customCirculationDataSet.currentPage);
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
  }, [customCirculationDataSet, isOrganization, modal, notifySettingDataSet, record, selectedType]);

  const data = notifySettingDataSet.toData() as any[];
  const selected = [];
  if (data[0]) {
    for (const [key, value] of Object.entries(data[0])) {
      if (key !== 'userIdList' && key !== 'noticeTypeList' && key !== 'specifier' && key !== 'webhook') {
        if (typeof value === 'boolean' && value) {
          const memberItem = memberOptionsDataSet.toData().find((item: {
            code: string, name: string
          }) => item.code === key);
          if (memberItem) {
            // @ts-ignore
            selected.push(memberItem.name);
          }
        }
      }
    }
    if (data[0].specifier) {
      (data[0].userList || []).forEach((userItem: User) => {
        if (userItem) {
          selected.push(userItem.realName);
        }
      });
    }
  }
  const memberIsRequired = data[0] && (
    // @ts-ignore
    data[0].noticeTypeList && data[0].noticeTypeList.length > 0
    // @ts-ignore
  );
  return (
    <div className={styles.notify_setting}>
      <OldLoading loading={loading} />
      <div className={styles.tip}>问题流转到当前状态后，将按照下方设置通知相关方。</div>
      <Form dataSet={notifySettingDataSet} className={styles.form}>
        <Select name="noticeTypeList" />
        <Dropdown
          // @ts-ignore
          
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
            className={`${styles.dropDown_trigger} ${memberIsRequired ? styles.dropDown_trigger_isRequired : styles.dropDown_trigger_isNotRequired} ${selected && selected.length ? styles.dropDown_trigger_hasSelected : styles.dropDown_trigger_hasNoSelected}`}
            role="none"
            onClick={(e) => {
              e.nativeEvent.stopImmediatePropagation();
              setHidden(!hidden);
            }}
            // eslint-disable-next-line jsx-a11y/no-noninteractive-tabindex
            tabIndex={0}
          >
            <span
              className={`${styles.trigger_label}  ${memberIsRequired ? styles.dropDown_member_isRequired : styles.dropDown_member_isNotRequired}`}
              style={{
                top: selected.length ? '3px' : '12px',
                left: '6px',
                fontSize: selected.length ? '12px' : '13px',
                lineHeight: '23px',
              }}
            >
              通知人员
            </span>
            <span className={styles.selected}>
              {selected.join(',')}
            </span>
            <span className={`${styles.trigger_tip} ${memberIsRequired && !selected.length ? styles.trigger_tip_visible : styles.trigger_tip_hidden}`}>请输入通知人员。</span>
            <Icon type="expand_more" className={styles.iconPicker} />
          </div>
        </Dropdown>
        {
          !isProgram && (
            <div>
              <div style={{ borderTop: '1px solid var(--divider)' }} />
              <CheckBox
                name="webhook"
                style={{
                  marginTop: 20,
                }}
              />
            </div>
          )
        }
      </Form>
    </div>
  );
};

export default observer(NotifySetting);
