import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { pageConfigApiConfig } from '@/api';
import type { UseFormatMessageFunction } from '@/hooks/useFormatMessage';

interface Props {
  schemeCode: string,
  formatMessage: UseFormatMessageFunction<string>,
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
      label: formatMessage({ id: 'agile.common.field' }),
    },
  ],
  fields: [
    {
      name: 'name',
      type: 'string' as FieldType,
      label: formatMessage({ id: 'agile.common.field' }),
    },
    {
      name: 'contextName',
      type: 'string' as FieldType,
      label: formatMessage({ id: 'agile.page.field.range' }),
    },
    {
      name: 'fieldTypeName',
      type: 'string' as FieldType,
      label: formatMessage({ id: 'agile.page.field.type' }),
    },
    {
      name: 'required',
      type: 'boolean' as FieldType,
      label: formatMessage({ id: 'agile.page.field.required' }),
    },
  ],
  transport: {
    read: () => pageConfigApiConfig.load(schemeCode),
    destroy: ({ data: [data] }) => pageConfigApiConfig.delete(data.id),
  },
});
export default SchemeTableDataSet;
