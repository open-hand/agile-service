import React, { useEffect } from 'react';
import { Form, Select, CheckBox } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { includes } from 'lodash';
import { toJS } from 'mobx';
import { useCreation } from 'ahooks';
import { pageConfigApi } from '@/api';
import styles from './Rule.less';
import { IAgileBaseSearchFieldInstance } from '@/components/field-pro/layouts/search';

import { renderFieldRelSelect, renderDefaultValue } from './utils';

const selectTypes = ['radio', 'multiple', 'checkbox', 'single'];
interface Props {
  record: Record
  getFieldInstance: IAgileBaseSearchFieldInstance['fieldInstance']
}

const Rule: React.FC<Props> = ({ record, getFieldInstance }) => {
  useEffect(() => {
    const getRelOptionList = async () => {
      const currentFieldRelOptionList = [...(toJS(record?.get('fieldRelOptionList')) || [])];
      if (currentFieldRelOptionList.find((item: { value: string, meaning?: string }) => !item.meaning)) {
        const res: { cascadeOptionName: string, cascadeOptionId: string }[] = await pageConfigApi.getCascadeRelOptionList(record.get('id'));
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
        {record.get('chosenField')?.fieldType && includes(selectTypes, record.get('chosenField')?.fieldType) ? renderFieldRelSelect({ field: record.get('chosenField'), getFieldInstance }) : null}
        <div
          className={styles.rule_default}
          style={{
            marginTop: record.get('chosenField')?.fieldType && includes(selectTypes, record.get('chosenField')?.fieldType) ? -6 : 0,
          }}
        >
          {renderDefaultValue({ field: record.get('chosenField'), fieldOptions: record.get('fieldRelOptionList'), getFieldInstance })}
        </div>
        <CheckBox name="hidden" />
        <CheckBox name="required" style={{ marginTop: -13 }} />
      </Form>
    </div>
  );
};

export default observer(Rule);
