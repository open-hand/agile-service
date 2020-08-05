import React, { useMemo, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { find } from 'lodash';
import {
  Select, CheckBox, Form, DataSet,
} from 'choerodon-ui/pro';
import { Divider } from 'choerodon-ui';
import SelectUser from '@/components/select/select-user';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import styles from './index.less';

interface Props {
  modal: any,
  record: any,
}
const Condition:React.FC<Props> = ({ modal, record }) => {
  const memberOptionDataSet = useMemo(() => new DataSet({
    data: [
      { code: 'owner', name: '项目所有者' },
      { code: 'assigners', name: '被指定人' },
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
        name: 'needCompleted',
        label: '任务项子级需全部到达已解决状态',
        type: 'boolean' as FieldType,
      }],
  }), [memberOptionDataSet]);

  useEffect(() => {
    const handleOk = async () => {
      const data = conditionDataSet.toData();
      const validate = await conditionDataSet.validate();
      // @ts-ignore
      const { member, assigners, needCompleted } = data && data[0];
      console.log(`validate： ${validate}`);
      console.log('data：');
      console.log(data);
      if (validate || (member.length && member.findIndex((item: string) => item === 'assigners') === -1)) {
        return true;
      }
      return false;
    };
    modal.handleOk(handleOk);
  }, [conditionDataSet, modal]);

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
            data && data[0].member.find((item: string) => item === 'assigners') && (
              <SelectUser name="assigners" />
            )
          }
          <Divider className={styles.divider} />
          <div className={styles.completeSetting}>
            <CheckBox name="needCompleted" />
          </div>
        </Form>
      </div>
    </div>
  );
};

export default observer(Condition);
