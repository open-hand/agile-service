import React, { useEffect } from 'react';
import { Form, Select, CheckBox } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { includes } from 'lodash';
import { toJS } from 'mobx';
import { pageConfigApi } from '@/api';
import styles from './Rule.less';
import { renderFieldRelSelect, renderDefaultValue } from './utils';

const selectTypes = ['radio', 'multiple', 'checkbox', 'single'];
interface Props {
  record: Record
}

const Rule: React.FC<Props> = ({ record }) => {
  useEffect(() => {
    const getRelOptionList = async () => {
      const currentFieldRelOptionList = [...(toJS(record?.get('fieldRelOptionList')) || [])];
      if (currentFieldRelOptionList.find((item: { value: string, meaning?: string}) => !item.meaning)) {
        const res: {cascadeOptionName: string, cascadeOptionId: string }[] = await pageConfigApi.getCascadeRelOptionList(record.get('id')); // 请求结果
        const newRelOptionList = currentFieldRelOptionList.map((item) => {
          if (item.value && item.meaning) {
            return item;
          }
          const meaning = res?.find((option) => option.cascadeOptionId === item.value)?.cascadeOptionName;
          return ({ value: item.value, meaning });
        });
        record.set('fieldRelOptionList', newRelOptionList);
      }
    };
    if (record.get('id')) {
      getRelOptionList();
    }
  }, [record]);

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
