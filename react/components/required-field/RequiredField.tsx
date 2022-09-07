import React, { useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { DataSet, Form } from 'choerodon-ui/pro';
import useIsInProgram from '@/hooks/useIsInProgram';
import { IField } from '@/common/types';
import renderField from './renderField';

export interface IFieldsValueVo {
  issueIds: string[];
  predefinedFields: any;
  customFields: {
    fieldId: string;
    fieldType: string;
    value: any;
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
  projectId?: string
}

const RequiredField: React.FC<Props> = ({
  requiredFields, requiredFieldDataSet, projectId,
}) => {
  const { isInProgram } = useIsInProgram({ projectId });
  useEffect(() => {
    requiredFields.filter((item) => item.defaultValue).forEach(({ fieldCode, defaultValue }) => {
      requiredFieldDataSet.current?.set(fieldCode as string, defaultValue);
    });
  }, [requiredFieldDataSet, requiredFields]);

  return (
    <Form dataSet={requiredFieldDataSet} columns={2} style={{ marginLeft: -5 }}>
      {
        requiredFields.map((item) => (renderField({
          field: item, otherComponentProps: { projectId }, dataSet: requiredFieldDataSet, isInProgram,
        })))
      }
    </Form>
  );
};

export default observer(RequiredField);
