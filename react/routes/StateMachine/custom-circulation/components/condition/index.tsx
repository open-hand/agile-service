import React, { useMemo, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { find, filter, uniq } from 'lodash';
import {
  Select, CheckBox, Form, DataSet,
} from 'choerodon-ui/pro';
import { Divider } from 'choerodon-ui';
import SelectUser from '@/components/select/select-user';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { statusTransformApi } from '@/api';
import styles from './index.less';

interface Props {
  modal: any,
  record: any,
  selectedType: string,
  customCirculationDataSet: DataSet,
}

interface ICondition {
  type: 'specifier' | 'projectOwner',
  userIds?: string[],
}

interface IConditionInfo {
  issueTypeId: string
  lastUpdateDate: string
  objectVersionNumber: number
  statusId: string
  userId: null | number[]
  userType: 'projectOwner' | 'specifier',
}
const Condition:React.FC<Props> = ({
  modal, record, selectedType, customCirculationDataSet,
}) => {
  const memberOptionDataSet = useMemo(() => new DataSet({
    data: [
      { code: 'projectOwner', name: '项目所有者' },
      { code: 'specifier', name: '被指定人' },
    ],
    fields: [
      {
        name: 'code',
        type: 'string' as FieldType,
      },
      {
        name: 'name',
        type: 'string' as FieldType,
      },
    ],
  }), []);
  const conditionDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'member',
        label: '成员',
        type: 'array' as FieldType,
        textField: 'name',
        valueField: 'code',
        options: memberOptionDataSet,
        multiple: true,
      },
      {
        name: 'assigners',
        label: '指定人',
        type: 'array' as FieldType,
        multiple: true,
        textField: 'realName',
        valueField: 'id',
        dynamicProps: {
          // eslint-disable-next-line no-shadow
          required: ({ record }) => find(record.get('member') || [], (item: string) => item === 'specifier'),
        },
      },
      // {
      //   name: 'needCompleted',
      //   label: '任务项子级需全部到达已解决状态',
      //   type: 'boolean' as FieldType,
      // },
    ],
  }), [memberOptionDataSet]);

  useEffect(() => {
    const { current } = conditionDataSet;
    statusTransformApi.getCondition(selectedType, record.get('id')).then((res: IConditionInfo[]) => {
      if (res) {
        current?.set('member', uniq(res.map((item) => item.userType)));
        const assigners = filter(res, (item: IConditionInfo) => item.userType === 'specifier');
        if (assigners) {
          current?.set('assigners', assigners.map((item: IConditionInfo) => item.userId));
        }
      }
    });
    const handleOk = async () => {
      const data = conditionDataSet.toData();
      const validate = await conditionDataSet.validate();
      // @ts-ignore
      const { member, assigners, needCompleted } = data && data[0];
      if (validate) {
        const updateData: ICondition[] = [];
        member.forEach((item: 'specifier' | 'projectOwner') => {
          if (item === 'specifier') {
            updateData.push({
              type: item,
              userIds: assigners,
            });
          } else {
            updateData.push({
              type: item,
            });
          }
        });
        await statusTransformApi.updateCondition(selectedType, record.get('id'), record.get('objectVersionNumber'), updateData);
        customCirculationDataSet.query();
        return true;
      }
      return false;
    };
    modal.handleOk(handleOk);
  }, [conditionDataSet, customCirculationDataSet, modal, record, selectedType]);

  const data = conditionDataSet.toData();

  return (
    <div className={styles.condition}>
      <div className={styles.tip}>当工作项流转到此状态应满足的条件设置。</div>
      <div className={styles.setting}>
        <Form dataSet={conditionDataSet}>
          <p className={styles.memberSelectTip}>移动工作项到此状态的成员为</p>
          <Select name="member" />
          {
            // @ts-ignore
            data && data[0].member.find((item: string) => item === 'specifier') && (
              <SelectUser name="assigners" />
            )
          }
          {/* <Divider className={styles.divider} />
          <div className={styles.completeSetting}>
            <CheckBox name="needCompleted" />
          </div> */}
        </Form>
      </div>
    </div>
  );
};

export default observer(Condition);
