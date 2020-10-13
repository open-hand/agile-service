import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { AxiosRequestConfig } from 'axios';
import Field, { FieldProps } from 'choerodon-ui/pro/lib/data-set/Field';
import { IChosenFieldField } from '@/components/chose-field/types';

interface Props {
  fields: IChosenFieldField[],
  systemFields?: FieldProps[],
}
const IssueFilterFormDataSet = ({ fields, systemFields }: Props): DataSetProps => ({
  autoQuery: false,
  autoCreate: true,
  paging: false,
  selection: undefined,
  fields: [
    ...fields.filter((field) => field.fieldType === 'member').map((filed) => ({
      name: filed.code,
      label: filed.name,
      textField: 'realName',
      valueField: 'id',
    })),
    ...(systemFields || []),
  ],

});
export default IssueFilterFormDataSet;
