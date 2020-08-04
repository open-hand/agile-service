import React, { useMemo, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { find } from 'lodash';
import {
  Select, CheckBox, Form, DataSet,
} from 'choerodon-ui/pro';
import { Divider } from 'choerodon-ui';
import SelectUser from '@/components/select/select-user';
import ConditionDataSet from './ConditionDataSet';
import style from './index.less';

// @ts-ignore
const Condition = ({ modal, record }) => {
  const conditionDataSet = useMemo(() => new DataSet(ConditionDataSet), []);
  

  useEffect(() => {
    const handleOk = async () => {
      const data = conditionDataSet.toData();
      const validate = await conditionDataSet.validate();
      // @ts-ignore
      const { member, assigners, needCompleted } = data && data[0];
      // console.log(`validate： ${validate}`);
      // console.log('data：');
      // console.log(data);
      if (validate || (member.length && member.find((item: string) => item !== 'assigners'))) {
        return true;
      } else {
        return false;
      }
    };
    modal.handleOk(handleOk);
  }, [conditionDataSet, modal]);

  const data = conditionDataSet.toData();

  return (
    <div className={style.condition}>
      <div className={style.tip}>当工作项流转到此状态应满足的条件设置。</div>
      <div className={style.setting}>
        <Form dataSet={conditionDataSet}>
          <p className={style.memberSelectTip}>移动工作项到此状态的成员为</p>
          <Select name="member" />
          {
            // @ts-ignore
            data && data[0].member.find((item: string) => item === 'assigners') && (
              <SelectUser name="assigners" />
            )
          }
          <Divider className={style.divider} />
          <div className={style.completeSetting}>
            <CheckBox name="needCompleted" />
          </div>
        </Form>
      </div>
    </div>
  ); 
};

export default observer(Condition);
