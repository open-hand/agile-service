import React from 'react';
import { Form, Select, CheckBox } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { includes } from 'lodash';
import styles from './Rule.less';
import { renderFieldRelSelect, renderDefaultValue } from './utils';

const selectTypes = ['radio', 'multiple', 'checkbox', 'single'];
interface Props {
  record: Record
}

const Rule: React.FC<Props> = ({ record }) => (
  <div className={styles.rule}>
    <Form record={record}>
      {record.get('chosenField')?.fieldType && includes(selectTypes, record.get('chosenField')?.fieldType) ? renderFieldRelSelect({ field: record.get('chosenField') }) : null}
      {renderDefaultValue({ field: record.get('chosenField'), fieldOptions: record.get('fieldRelOptionList') })}
      <CheckBox name="hidden" />
      <CheckBox name="required" />
    </Form>
  </div>
);

export default observer(Rule);
