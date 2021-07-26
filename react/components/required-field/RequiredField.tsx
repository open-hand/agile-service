import React, { useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Form, DataSet,
} from 'choerodon-ui/pro';
import useIsInProgram from '@/hooks/useIsInProgram';
import { IField } from '@/common/types';
import { toJS } from 'mobx';
import renderField from './renderField';

export interface IFieldsValueVo {
  issueIds: string[],
  predefinedFields: object
  customFields: {
    fieldId: string,
    fieldType: string,
    value: any,
  }[]
}

export interface IRequiredField {
  dataSet: DataSet,
  fieldsValueVo: IFieldsValueVo | null
}
export type IRequiredFieldRef = React.MutableRefObject<IRequiredField | null>

interface Props {
  requiredFields: IField[]
  requiredFieldDataSet: DataSet
}

const RequiredField: React.FC<Props> = ({
  requiredFields, requiredFieldDataSet,
}) => {
  const { isInProgram } = useIsInProgram();
  useEffect(() => {
    requiredFields.filter((item) => item.defaultValue).forEach(({ fieldCode, defaultValue }) => {
      requiredFieldDataSet.current?.set(fieldCode as string, defaultValue);
    });
  }, [requiredFieldDataSet, requiredFields]);

  console.log(toJS(requiredFieldDataSet.current?.data));
  return (
    <Form dataSet={requiredFieldDataSet} columns={2} style={{ marginLeft: -5 }}>
      {
        requiredFields.map((item) => (renderField({
          field: item, otherComponentProps: {}, dataSet: requiredFieldDataSet, isInProgram,
        })))
      }
    </Form>
  );
};

export default observer(RequiredField);
