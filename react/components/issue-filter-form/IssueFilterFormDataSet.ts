import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { FieldProps } from 'choerodon-ui/pro/lib/data-set/Field';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
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
    ...fields.filter((field) => field.fieldType && ['datetime', 'date', 'time'].includes(field.fieldType)).map((field) => ({
      name: field.code,
      label: field.name,
      validator: async (v: any, name: string, record: Record) => {
        if (!v) {
          return true;
        }
        if (!v[0] || !v[1]) {
          record.init(name, undefined);
        }
        return true;
      },
    }
    )),
    ...(systemFields || []),
  ],
});
export default IssueFilterFormDataSet;
