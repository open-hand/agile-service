import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';

const AddUnselectedDataSet = (): DataSetProps => ({
  autoCreate: true,
  autoQuery: false,
  paging: false,
  fields: [
    {
      name: 'field', label: '字段', required: true,
    },
  ],
});
export default AddUnselectedDataSet;
