/* eslint-disable jsx-a11y/tabindex-no-positive */
import React, {
  useMemo, useEffect, useRef, useCallback, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  find, filter, includes,
} from 'lodash';
import {
  Select, CheckBox, Form, DataSet, Dropdown,
} from 'choerodon-ui/pro';
import { Divider, Icon } from 'choerodon-ui';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import {
  getOrgUsersByIds, getProjectUsersByIds, ICondition, statusTransformApi, userApi,
} from '@/api';
import { getProjectId, getIsOrganization, getMenuType } from '@/utils/common';
import { User } from '@/common/types';
import styles from './index.less';
import SelectUser from '@/components/select/select-user';

interface Props {
  modal: any,
  record: any,
  selectedType: string,
  customCirculationDataSet: DataSet,
  selectedTypeCode?: string
}

interface IConditionInfo {
  issueTypeId: string
  lastUpdateDate: string
  objectVersionNumber: number
  statusId: string
  userId?: null | number[]
  userType: 'projectOwner' | 'participant' | 'specifier' | 'other',
  verifySubissueCompleted?: boolean
}

interface ConditionSelectProps {
  conditionDataSet: DataSet,
}

const ConditionSelect: React.FC<ConditionSelectProps> = ({ conditionDataSet }) => {
  const data = conditionDataSet.toData()[0];
  return (
    <div
      className={styles.condition_select}
    >
      <Form dataSet={conditionDataSet}>
        <CheckBox name="projectOwner" />
        <CheckBox name="participant" />
        <CheckBox name="specifier" />
        {
          // @ts-ignore
          data.specifier && (
            <SelectUser
              level={getMenuType() === 'project' ? 'project' : 'org'}
              name="assigners"
              maxTagCount={2}
              className={styles.condition_assigners}
            // @ts-ignore

            />
          )
        }
      </Form>
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

const Condition: React.FC<Props> = ({
  modal, record, selectedType, customCirculationDataSet, selectedTypeCode,
}) => {
  const [hidden, setHidden] = useState(true);
  const isOrganization = getIsOrganization();
  const handleClickOut = useCallback(() => {
    setHidden(true);
  }, []);
  const ref = useClickOut(handleClickOut);

  const conditionDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      { name: 'projectOwner', label: '项目所有者', type: 'boolean' as FieldType },
      { name: 'participant', label: '参与人', type: 'boolean' as FieldType },
      { name: 'specifier', label: '被指定人', type: 'boolean' as FieldType },
      {
        name: 'assigners',
        label: '指定人',
        type: 'object' as FieldType,
        textField: 'realName',
        valueField: 'id',
        multiple: true,
        dynamicProps: {
          // eslint-disable-next-line no-shadow
          required: ({ record }) => record.get('specifier'),
        },
      },
      {
        name: 'verifySubissueCompleted',
        label: '任务项子级需全部到达已解决状态',
        type: 'boolean' as FieldType,
      },
    ],
  }), []);

  useEffect(() => {
    const { current } = conditionDataSet;
    async function loadData() {
      const res: IConditionInfo[] = await statusTransformApi.getCondition(selectedType, record.get('id'));
      if (res) {
        const assigners = filter(res, (item: IConditionInfo) => item.userType === 'specifier');
        const projectOwnerItem = find(res, (item: IConditionInfo) => item.userType === 'projectOwner');
        const participantItem = find(res, (item: IConditionInfo) => item.userType === 'participant');
        const subIssueCompletedItem = find(res, (item: IConditionInfo) => item.userType === 'other' && !!item.verifySubissueCompleted);

        if (assigners && assigners.length) {
          current?.set('specifier', true);
          const userIds = assigners.filter((item) => item.userId).map((item: IConditionInfo) => String(item.userId));
          const users = await (getMenuType() === 'project' ? getProjectUsersByIds(userIds) : getOrgUsersByIds(userIds));
          current?.set('assigners', users);
        }
        if (projectOwnerItem) {
          current?.set('projectOwner', true);
        }
        if (participantItem) {
          current?.set('participant', true);
        }
        if (subIssueCompletedItem) {
          current?.set('verifySubissueCompleted', true);
        }
      }
    }
    loadData();
  }, [conditionDataSet, record, selectedType]);

  useEffect(() => {
    const handleOk = async () => {
      const data = conditionDataSet.toData();
      const validate = await conditionDataSet.validate();
      const {
      // @ts-ignore
        projectOwner, specifier, assigners, participant, verifySubissueCompleted,
      } = (data && data[0]) || {};
      if (validate) {
        const updateData: ICondition[] = [];
        if (projectOwner) {
          updateData.push({
            type: 'projectOwner',
          });
        }
        if (participant) {
          updateData.push({
            type: 'participant',
          });
        }
        if (specifier) {
          updateData.push({
            type: 'specifier',
            userIds: assigners?.map((user: User) => user.id),
          });
        }
        if (verifySubissueCompleted) {
          updateData.push({
            type: 'other',
            verifySubissueCompleted: true,
          });
        }
        await statusTransformApi[isOrganization ? 'orgUpdateCondition' : 'updateCondition'](selectedType, record.get('id'), record.get('objectVersionNumber'), updateData);
        customCirculationDataSet.query(customCirculationDataSet.currentPage);
        return true;
      }
      setHidden(false);
      return false;
    };
    modal.handleOk(handleOk);
  }, [conditionDataSet, customCirculationDataSet, isOrganization, modal, record, selectedType]);

  const data = conditionDataSet.toData()[0] as any;
  const selected = [];
  if (data && data.projectOwner) {
    selected.push('项目所有者');
  }
  if (data && data.participant) {
    selected.push('参与人');
  }
  if (data && data.specifier) {
    const assigners = data.assigners || [];
    assigners.forEach((user: any) => {
      if (user) {
        selected.push(user.realName);
      }
    });
  }
  return (
    <div className={styles.condition}>
      <div className={styles.tip}>当工作项流转到此状态应满足的条件设置。</div>
      <div className={styles.setting}>
        <p className={styles.memberSelectTip}>仅以下成员可移动工作项到此状态</p>
        <Dropdown
        // @ts-ignore
          visible={!hidden}
          getPopupContainer={() => document.getElementsByClassName(styles.form)[0] as any}
          overlay={(
            <div
            // @ts-ignore
              ref={ref}
              role="none"
              onMouseDown={(e) => e.stopPropagation()}
              onClick={(e) => {
                e.stopPropagation();
              }}
            >
              <ConditionSelect conditionDataSet={conditionDataSet} />
            </div>
        )}
          trigger={['click'] as Action[]}
        >
          <div
          // eslint-disable-next-line jsx-a11y/no-noninteractive-tabindex
            tabIndex={1}
            className={`${styles.dropDown_trigger} ${selected && selected.length ? styles.dropDown_trigger_hasSelected : styles.dropDown_trigger_hasNoSelected}`}
            role="none"
            onClick={(e) => {
              e.nativeEvent.stopImmediatePropagation();
              setHidden(!hidden);
            }}
          >
            <span
              className={styles.trigger_label}
              style={{
                top: selected.length ? '3px' : '14px',
                left: '6px',
                fontSize: selected.length ? '12px' : '13px',
              }}
            >
              选择成员
            </span>
            <span className={styles.selected}>
              {selected.join(',')}
            </span>
            <Icon type="expand_more" className={styles.iconPicker} />
          </div>
        </Dropdown>
        {
        includes(['story', 'task', 'bug'], selectedTypeCode) && (
          <>
            <Divider className={styles.divider} />
            <Form dataSet={conditionDataSet}>
              <div className={styles.completeSetting}>
                <CheckBox name="verifySubissueCompleted" />
              </div>
            </Form>
          </>
        )
      }
      </div>
    </div>
  );
};

export default observer(Condition);
