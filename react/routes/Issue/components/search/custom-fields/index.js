import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import IssueStore from '@/stores/project/sprint/IssueStore';
import SelectField from './field/SelectField';
import Store from '../../../stores';

function renderField(field, dataSet) {
  const { fieldType } = field;
  const { chosenFields } = IssueStore;
  const value = chosenFields.get(field.code) ? chosenFields.get(field.code).value : undefined;
  const handleChange = (v) => {
    IssueStore.setFieldFilter(field.code, v);
    dataSet.query();
  };
  switch (fieldType) {
    case 'single':
    case 'multiple':
    case 'radio':
      return (
        <SelectField
          field={field} 
          value={value}
          onChange={handleChange}
        />
      );
    default: return null;
  }
}
function CustomFields() {
  const { chosenFields } = IssueStore;
  const {
    dataSet, 
  } = useContext(Store);
  return [...chosenFields.entries()].map(([, field]) => renderField(field, dataSet));
}

export default observer(CustomFields);
