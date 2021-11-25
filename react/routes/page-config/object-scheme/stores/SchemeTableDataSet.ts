import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import type { IntlShape } from 'react-intl';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { pageConfigApiConfig } from '@/api';

interface Props {
  schemeCode: string,
  formatMessage: IntlShape['formatMessage'],
}
const SchemeTableDataSet = ({
  formatMessage, schemeCode,
}: Props): DataSetProps => ({
  selection: false,
  paging: false,
  autoQuery: true,
  dataKey: 'content',
  queryFields: [
    {
      name: 'name',
      type: 'string' as FieldType,
      label: formatMessage({ id: 'field' }),
    },
  ],
  fields: [
    {
      name: 'name',
      type: 'string' as FieldType,
      label: formatMessage({ id: 'field' }),
    },
    {
      name: 'contextName',
      type: 'string' as FieldType,
      label: formatMessage({ id: 'field.range' }),
    },
    {
      name: 'fieldTypeName',
      type: 'string' as FieldType,
      label: formatMessage({ id: 'field.type' }),
    },
    {
      name: 'required',
      type: 'boolean' as FieldType,
      label: formatMessage({ id: 'field.required' }),
    },
  ],
  transport: {
    read: () => pageConfigApiConfig.load(schemeCode),
    destroy: ({ data: [data] }) => pageConfigApiConfig.delete(data.id),
  },
});
export default SchemeTableDataSet;
