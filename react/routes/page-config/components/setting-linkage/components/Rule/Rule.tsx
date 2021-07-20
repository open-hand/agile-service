import React, { useEffect } from 'react';
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

const Rule: React.FC<Props> = ({ record }) => {
  useEffect(() => {
    const currentFieldRelOptionList = [...(record?.get('fieldRelOptionList') || [])];
    console.log('请求可见选项', currentFieldRelOptionList);
    if (currentFieldRelOptionList.find((item: { value: string, meaning?: string}) => !item.meaning)) {
      const res: {id: string, name: string }[] = []; // 请求结果
      currentFieldRelOptionList.map((item) => {
        if (item.value && item.meaning) {
          return item;
        }
        return { value: item.value, meaning: res?.find((option) => option.id === item.value)?.name };
      });
      record.set('fieldRelOptionList', currentFieldRelOptionList);
    }
  }, [record]);

  console.log(record?.get('fieldRelOptionList'));
  return (
    <div className={styles.rule}>
      <Form record={record}>
        {record.get('chosenField')?.fieldType && includes(selectTypes, record.get('chosenField')?.fieldType) ? renderFieldRelSelect({ field: record.get('chosenField') }) : null}
        {renderDefaultValue({ field: record.get('chosenField'), fieldOptions: record.get('fieldRelOptionList') })}
        <CheckBox name="hidden" />
        <CheckBox name="required" />
      </Form>
    </div>
  );
};

export default observer(Rule);
