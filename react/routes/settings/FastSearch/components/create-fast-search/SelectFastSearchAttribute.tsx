import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { fieldApi, quickFilterApi } from '@/api';
import {
  Table, DataSet, Form, Select, TextField, Modal, Button, Col, Row,
} from 'choerodon-ui/pro';
import React from 'react';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';

interface Props extends Partial<SelectProps> {
    afterLoad?: SelectConfig['afterLoad']
    colSpan?:number

}
const SelectFastSearchAttribute: React.FC<Props> = ({ afterLoad, ...otherProps }) => {
  const isInProgram = true;
  async function loadData() {
    const data = await Promise.all<any[], any[]>([quickFilterApi.loadField(), fieldApi.getCustomFields()])
      .then(([preDefinedField, customField]) => ([...preDefinedField, ...isInProgram ? [{ fieldCode: 'feature', type: 'long', name: '特性' }] : [], ...customField].map((field) => ({ ...field, fieldCode: field.code || field.fieldCode, type: field.fieldType || field.type })) || []));
    return data;
  }

  const selectProps = useSelect({
    name: 'fastSearchAttribute',
    valueField: 'fieldCode',
    textField: 'name',
    request: () => loadData(),
    paging: false,
    afterLoad,
  });
  return <Select {...selectProps} {...otherProps} />;
};
export default SelectFastSearchAttribute;
