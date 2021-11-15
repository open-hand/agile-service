import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import {
  DataSet,
} from 'choerodon-ui/pro';
import { ganttApiConfig } from '@/api';

const GanntDependencyModalDataSet = (editData?: any[]): DataSetProps => ({
  autoCreate: !editData?.length,
  autoQuery: false,
  data: editData,
  selection: false,
  fields: [
    {
      name: 'predecessorType',
      label: '依赖关系',
      required: true,
      type: 'string' as FieldType,
      textField: 'name',
      valueField: 'valueCode',
      options: new DataSet({
        paging: false,
        autoQuery: true,
        transport: {
          read: ganttApiConfig.loadIssueDependencyTypes(),
        },
      }),
    },
    {
      name: 'predecessorId', label: '工作项', required: true, type: 'string' as FieldType, multiple: true,
    },

  ],
  transport: {
  },
});
export default GanntDependencyModalDataSet;
