import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { DataSet } from 'choerodon-ui/pro';
import { omit } from 'lodash';
import { statusApiConfig } from '@/api';

interface TableProps {
  stageDs: DataSet,
  formatMessage: any
}

const TableDataSet = ({ stageDs, formatMessage }: TableProps): DataSetProps => ({
  autoQuery: false,
  autoCreate: false,
  selection: false,
  expandField: 'expand',
  modifiedCheck: false,
  transport: {
    read: ({ params, data }) => {
      const searchData = omit(data, 'params');
      searchData.param = data.params;
      return statusApiConfig.loadStatusList({ params, data: searchData } as any);
    },
  },
  fields: [
    { name: 'name', label: formatMessage({ id: 'agile.stateMachine.name' }) },
    { name: 'description', label: formatMessage({ id: 'agile.common.description' }) },
    { name: 'type', label: formatMessage({ id: 'agile.stateMachine.stage' }) },
  ],
  queryFields: [
    { name: 'name', label: formatMessage({ id: 'agile.stateMachine.name' }) },
    { name: 'description', label: formatMessage({ id: 'agile.common.description' }) },
    {
      name: 'type', label: formatMessage({ id: 'agile.stateMachine.stage' }), textField: 'name', valueField: 'code', options: stageDs,
    },
  ],
});

export default TableDataSet;
