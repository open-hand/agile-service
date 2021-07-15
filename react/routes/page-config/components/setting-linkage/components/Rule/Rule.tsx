import React from 'react';
import { Form, Select, CheckBox } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import styles from './Rule.less';

interface Props {
  record: Record
}

const Rule: React.FC<Props> = ({ record }) => {
  console.log('render：Rule');
  return (
    <div className={styles.rule}>
      <Form record={record}>
        <Select name="fieldRelOptionList" />
        <Select name="defaultValue" />
        <CheckBox name="hidden" />
        <CheckBox name="required" />
      </Form>
    </div>
  );
};

export default observer(Rule);
